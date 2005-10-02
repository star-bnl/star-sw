//--
//-- runEEmcPointMaker.C
//--
//-- example script for running the point maker.  The macro
//-- will process the first nevents of the MuDst (or list of MuDst's).
//-- if nevents is negative, the macro will process all events in the
//-- mudst.
//--

//-- switch should be commented out when analysing real data
//#define MONTE_CARLO
#define NEW 

class StChain;
class St_db_Maker;
class StEEmcDbMaker;
class StMuDstMaker;
class StEEmcA2EMaker;
class StEEmcClusterMaker;
class StEEmcPointMaker;
class StEEmcPointFitMaker;
class StEEmcMixMaker;
class StEEmcMixQAMaker;

//--
//-- globals
//--
StChain            *mChain        = 0;
St_db_Maker        *mStarDatabase = 0;
StEEmcDbMaker      *mEEmcDatabase = 0;
StMuDstMaker       *mMuDstMaker   = 0;
StEEmcA2EMaker     *mEEanalysis   = 0;
StEEmcClusterMaker *mEEclusters   = 0;
StEEmcPointMaker   *mEEpoints     = 0;
//StEEmcPointFitMaker *mEEpoints = 0;

StEEmcMixMaker     *mEEmixer      = 0;
StEEmcMixQAMaker   *mEEmixqa      = 0;

Int_t               count         = 0;
Int_t               stat          = 0; 

Int_t prescale = 100; 

void runEEmcPi0Analysis( Int_t nevents = 50, 
			 Char_t *name = "mcpi0_5000_06TC05_15.MuDst.root",
			 Char_t *ofile= "mcpi0_5000_06TC05_15.root",
			 Char_t *path = "/star/data04/sim/jwebb/MonteCarlo/single_gamma/", 
			 Int_t nfiles = 100
			 )
{

  TString pathname = path; 
  pathname += name;

  //--
  //-- Load shared libraries
  //--
  LoadLibs();
  

  //--
  //-- Create the analysis chain
  //--
  mChain = new StChain("eemcAnalysisChain");


  //--
  //-- MuDst maker for reading input
  //--
  mMuDstMaker = new StMuDstMaker(0,0,path,name,"MuDst",nfiles);
  mMuDstMaker->SetStatus("*",0);
  mMuDstMaker->SetStatus("MuEvent",1);
  mMuDstMaker->SetStatus("EmcAll",1);


  //--
  //-- Connect to the STAR databse
  //--
  mStarDatabase = new St_db_Maker("StarDb", "MySQL:StarDb");

  
#ifdef MONTE_CARLO  
  //--
  //-- Setup ideal gains for processing MC data
  //--
  mStarDatabase->SetFlavor("sim","eemcPMTcal");
  mStarDatabase->SetFlavor("sim","eemcPIXcal");
  mStarDatabase->SetFlavor("sim","eemcPMTped");
  mStarDatabase->SetFlavor("sim","eemcPMTstat");
  mStarDatabase->SetFlavor("sim","eemcPMTname");
  mStarDatabase->SetFlavor("sim","eemcADCconf");
  mStarDatabase->SetDateTime(20050101,0);
#endif

  //--
  //-- Initialize EEMC database
  //--
  mEEmcDatabase = new StEEmcDbMaker("eemcDb");
  //mEEmcDatabase -> setSectors(1,7); 
  gMessMgr -> SwitchOff("D");
  gMessMgr -> SwitchOn("I");


#ifdef MONTE_CARLO
  //--
  //-- Initialize slow simulator
  //--
  StEEmcSlowMaker *slowSim = new StEEmcSlowMaker("slowSim");
  slowSim->setDropBad(0);   // 0=no action, 1=drop chn marked bad in db
  slowSim->setAddPed(0);    // 0=no action, 1=ped offset from db
  slowSim->setSmearPed(0);  // 0=no action, 1=gaussian ped, width from db
  slowSim->setOverwrite(1); // 0=no action, 1=overwrite muDst values
#endif
 

  //--
  //-- Energy to ADC maker
  //--
  mEEanalysis=new StEEmcA2EMaker("AandE");
  mEEanalysis->database("eemcDb");          // sets db connection
  mEEanalysis->source("MuDst",1);           // sets mudst as input
  mEEanalysis->threshold(3.0,0);            // tower threshold
  mEEanalysis->threshold(3.0,1);            // pre1 threshold 
  mEEanalysis->threshold(3.0,2);            // pre2 threshold
  mEEanalysis->threshold(3.0,3);            // post threshold
  mEEanalysis->threshold(3.0,4);            // smdu threshold
  mEEanalysis->threshold(3.0,5);            // smdv threshold
//mEEanalysis->scale(1.2);                  // scale energies by x1.2
/*
  mEEanalysis->scale(1./1.56,4);            // smd-u
  mEEanalysis->scale(1./1.56,5);            // smd-v 
*/

   
  //--
  //-- Some simple QA histograms
  //--
  StEEmcQAMaker *eemcQA=new StEEmcQAMaker("eeqa");
  eemcQA->analysis("AandE");
  eemcQA->mudst("MuDst");
  eemcQA->nVertexMin=0;        // cut on min number of verticies
  eemcQA->nVertexMax=999;      // cut on max number of verticies
  eemcQA->trigger(96261);      // HT2 slow
  eemcQA->softTrigger(3.36); 


  //--
  //-- Cluster maker.  Generates tower, preshower, postshower clusters
  //-- (using Minesweeper algo) and smd clusters (seed strip w/ truncation).
  //--
  mEEclusters=new StEEmcClusterMaker("mEEclusters");
  mEEclusters->analysis("AandE");
  mEEclusters->seedEnergy(0.8,0);       // tower seed energy
  mEEclusters->seedEnergy(5./1000.,4); // 2 MeV smd-u strip
  mEEclusters->seedEnergy(5./1000.,5); // 2 MeV smd-v strip
  mEEclusters->setSeedFloor(2.0);       // floating seed threshold near clusters
  mEEclusters->setMaxExtent(5);         // maximum distance from seed strip
  mEEclusters->suppress(0);             // disallows seeds in two strips adjacent to any cluster


  //--
  //-- Point maker.  Matches pairs of smd clusters to active towers.
  //-- Energy is divided between points using a tower energy-sharing
  //-- vs position function.
  //--
  mEEpoints=new StEEmcPointMaker("mEEpoints");
  mEEpoints->analysis("AandE");
  mEEpoints->clusters("mEEclusters");
  mEEpoints->setEnergyMode(0);
  
  //--
  //-- Pi0 mixer, takes the points identified above and mixes pi0 pairs.
  //--
  mEEmixer = new StEEmcMixMaker("mEEmixer");
  mEEmixer -> mudst("MuDst");
  mEEmixer -> analysis("AandE");
  mEEmixer -> points("mEEpoints");
  for ( Int_t i=0;i<12;i++ ) // activate all 12 sectors
    mEEmixer->sector(i); 
  mEEmixer->trigger(96261); // EHT2

  mEEpi0analysis=new StEEmcPi0Analysis("pi0analy");
  mEEpi0analysis->trigger(96261);
  mEEpi0analysis->minbias(96011);
  mEEpi0analysis->mudst("MuDst");
  mEEpi0analysis->points("mEEpoints");
  mEEpi0analysis->mixer("mEEmixer");
  mEEpi0analysis->analysis("AandE"); 


  mEEmixer2 = new StEEmcMixMaker("mEEmixer2");
  mEEmixer2 -> mudst("MuDst");
  mEEmixer2 -> analysis("AandE");
  mEEmixer2 -> points("mEEpoints");
  for ( Int_t i=0;i<12;i++ ) // activate all 12 sectors
    mEEmixer2->sector(i); 
  mEEmixer2->trigger(96251); // EHT2

  mEEpi0analysis2=new StEEmcPi0Analysis("pi0analy2");
  mEEpi0analysis2->trigger(96251);
  mEEpi0analysis2->minbias(96011);
  mEEpi0analysis2->mudst("MuDst");
  mEEpi0analysis2->points("mEEpoints");
  mEEpi0analysis2->mixer("mEEmixer");
  mEEpi0analysis2->analysis("AandE"); 


  mEEmixer3 = new StEEmcMixMaker("mEEmixer3");
  mEEmixer3 -> mudst("MuDst");
  mEEmixer3 -> analysis("AandE");
  mEEmixer3 -> points("mEEpoints");
  for ( Int_t i=0;i<12;i++ ) // activate all 12 sectors
    mEEmixer3->sector(i); 
  mEEmixer3->trigger(26); // EHT2
  mEEmixer3->fixedVertex(0.,0.,-30.0); 

  mEEpi0analysis3=new StEEmcPi0Analysis("pi0analy3");
  mEEpi0analysis3->trigger(26);
  mEEpi0analysis3->minbias(96011);
  mEEpi0analysis3->mudst("MuDst");
  mEEpi0analysis3->points("mEEpoints");
  mEEpi0analysis3->mixer("mEEmixer");
  mEEpi0analysis3->analysis("AandE"); 

  mChain->ls(3);
  mChain->Init();

  //-----------------------------------------------------------------
  //--
  //-- This is where the business happens.  We loop over all events.
  //-- when mChain -> Make() is called, ::Make() will be called on 
  //-- all of the makers created above.
  //--

  Int_t stat  = 0;    // error flag
  Int_t count = 0;    // event count
  while ( stat == 0 ) {


    //--
    //-- Terminate once we reach nevents --
    //--
    if ( count++ >= nevents ) if ( nevents > 0 ) break;

    //--
    //-- Call clear on all makers 
    //--
    mChain -> Clear();

    
    //--
    //-- Process the event through all makers 
    //--
    stat = mChain -> Make();

    //--
    //-- Set to printout on every 10th event
    //--
    if ( (count%prescale) ) continue;

    std::cout << "------------------------------------------------";
    std::cout << "event=" << count << std::endl;

    //--
    //-- Print the number of hits in the towers, pre/postshower layers
    //--
    Int_t nhits[]={0,0,0,0,0,0};
    for ( int i = 0; i < 4; i++ ) {
      //      std::cout << " layer=" << i 
      //		<< " nhits=" << mEEanalysis->numberOfHitTowers(i) << std::endl;
      nhits[i]+=mEEanalysis->numberOfHitTowers(i);
    }

    //--
    //-- Print the total number of smd strips which fired
    //--
    Int_t nu=0,nv=0;
    for ( Int_t sec=0;sec<12;sec++ )
      {
	nu+=mEEanalysis->numberOfHitStrips(sec,0);
	nv+=mEEanalysis->numberOfHitStrips(sec,1);
      }
    nhits[4]=nu;
    nhits[5]=nv;


    //--
    //-- Print number of clusters in each layer
    //--
    Int_t ncl[6]={0,0,0,0,0,0};
    for ( Int_t i=0;i<12;i++ )
      {
	ncl[0]+=mEEclusters->numberOfClusters(i,0);
	ncl[1]+=mEEclusters->numberOfClusters(i,1);
	ncl[2]+=mEEclusters->numberOfClusters(i,2);
	ncl[3]+=mEEclusters->numberOfClusters(i,3);
	ncl[4]+=mEEclusters->numberOfSmdClusters(i,0);
	ncl[5]+=mEEclusters->numberOfSmdClusters(i,1);
      }

    const Char_t *lay[]={"tower:","pre1: ", "pre2: ", "post: ", "smdu: ", "smdv: "};
    for ( Int_t i=0;i<6;i++ )
      {
	std::cout << lay[i] << " " << nhits[i] << " " << ncl[i] << std::endl;
      }

    //    std::cout << "number of points: " << mEEpoints -> numberOfPoints() << std::endl;
    std::cout << "number of pairs:  " << mEEmixer -> numberOfCandidates() << std::endl;


	    
  }
  //--
  //-----------------------------------------------------------------


  //--
  //-- For debugging purposes, it's often useful to print out the 
  //-- database 
  //--
  mEEmcDatabase->exportAscii("dbdump.dat"); 

  //--
  //-- Calls the ::Finish() method on all makers
  //--
  mChain -> Finish(); 


  //--
  //-- Output the QA histograms to disk
  //--
  TFile *file=new TFile(ofile,"RECREATE");
  file->mkdir("QA");
  file->cd("QA");
  eemcQA -> GetHistList() -> Write();
  file->mkdir("eht2");
  file->cd("eht2"); 
  mEEpi0analysis->GetHistList()->Write(); 
  file->cd(); 
  file->mkdir("eht1");
  file->cd("eht1");
  mEEpi0analysis2->GetHistList()->Write(); 
  file->cd();
  file->mkdir("eht1f");
  file->cd("eht1f"); 
  mEEpi0analysis3->GetHistList()->Write(); 
  file->Close();
  
  
  delete file;


  return;
    
}

void LoadLibs()
{
  //-- Load muDst shared libraries --
  gROOT -> LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcSimulatorMaker");
  
#ifdef NEW
  gSystem->Load("StEEmcA2EMaker");
  gSystem->Load("StEEmcClusterMaker");
  gSystem->Load("StEEmcPointMaker");
  gSystem->Load("StEEmcPi0Mixer");
#else
  gSystem->Load("StMaxStripPi0");
#endif 
}

