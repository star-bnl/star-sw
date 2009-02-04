//--
//-- runEEmcPointMaker.C
//--
//-- example script for running the point maker.  The macro
//-- will process the first nevents of the MuDst (or list of MuDst's).
//-- if nevents is negative, the macro will process all events in the
//-- mudst.
//--

//-- switch should be commented out when analysing real data
#define MONTE_CARLO
#define NEW 

class StChain;
class St_db_Maker;
class StEEmcDb;
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
StEEmcDb           *mEEmcDatabase = 0;
StMuDstMaker       *mMuDstMaker   = 0;
StEEmcA2EMaker     *mEEanalysis   = 0;
StEEmcClusterMaker *mEEclusters   = 0;
StEEmcPointMaker   *mEEpoints     = 0;
//StEEmcPointFitMaker *mEEpoints = 0;

StEEmcMixMaker     *mEEmixer      = 0;
StEEmcMixQAMaker   *mEEmixqa      = 0;

Int_t               count         = 0;
Int_t               stat          = 0; 

Int_t prescale = 10; 

void runEEmcMcPi0Mixer( Int_t nevents = 50, 
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
  new StEEmcDbMaker("eemcDb");
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
  mEEanalysis->threshold(0,3.0);            // tower threshold
  mEEanalysis->threshold(1,3.0);            // pre1 threshold 
  mEEanalysis->threshold(2,3.0);            // pre2 threshold
  mEEanalysis->threshold(3,3.0);            // post threshold
//mEEanalysis->threshold(4,5.0);            // smdu threshold
//mEEanalysis->threshold(5,5.0);            // smdv threshold
  mEEanalysis->scale(1.2);                  // scale energies by x1.2

   
  //--
  //-- Some simple QA histograms
  //--
  StEEmcQAMaker *eemcQA=new StEEmcQAMaker("eeqa");
  eemcQA->analysis("AandE");
  eemcQA->mudst("MuDst");
//eemcQA->trigger(96261);      // add specified trigger to list  
  eemcQA->nVertexMin=0;        // cut on min number of verticies
  eemcQA->nVertexMax=999;      // cut on max number of verticies


  //--
  //-- Cluster maker.  Generates tower, preshower, postshower clusters
  //-- (using Minesweeper algo) and smd clusters (seed strip w/ truncation).
  //--
  mEEclusters=new StEEmcClusterMaker("mEEclusters");
  mEEclusters->analysis("AandE");
  mEEclusters->seedEnergy(0.8,0);       // tower seed energy
  mEEclusters->seedEnergy(2.0/1000.,4); // 2 MeV smd-u strip
  mEEclusters->seedEnergy(2.0/1000.,5); // 2 MeV smd-v strip
  mEEclusters->setSeedFloor(3.0);       // floating seed threshold near clusters
  mEEclusters->setMaxExtent(3);         // maximum distance from seed strip
//mEEclusters->suppress();              // disallows seeds in two strips adjacent to any cluster


  //--
  //-- Point maker.  Matches pairs of smd clusters to active towers.
  //-- Energy is divided between points using a tower energy-sharing
  //-- vs position function.
  //--
  mEEpoints=new StEEmcPointMaker("mEEpoints");
  //mEEpoints=new StEEmcPointFitMaker("mEEpoints");
  mEEpoints->analysis("AandE");
  mEEpoints->clusters("mEEclusters");
  mEEpoints->setEnergyMode(0);
  //mEEpoints->setLimit(10);
  //  mEEpoints -> doPermutations(false);


  //--
  //-- Pi0 mixer, takes the points identified above and mixes pi0 pairs.
  //--
  mEEmixer = new StEEmcMixMaker("mEEmixer");
  mEEmixer -> mudst("MuDst");
  mEEmixer -> analysis("AandE");
  mEEmixer -> points("mEEpoints");
  for ( Int_t i=0;i<12;i++ ) // activate all 12 sectors
    mEEmixer->sector(i); 
//mEEmixer->trigger(96251); // specify trigger id(s) to process
  mEEmixer->fixedVertex(0.,0.,0.);  // fixes the vertex positiion

  //--
  //-- QA/analysis histograms for pi0's
  //--
  mEEmixqa = new StEEmcMixQAMaker("mEEmixqa");
  mEEmixqa -> mixer( "mEEmixer", 0.1, 0.18 ); // specify mixer and mass range for gated histograms
  mEEmixqa -> points( "mEEpoints" );
  mEEmixqa -> maxPerCluster =   1;  // max number of pairs matched to a cluster of towers
  mEEmixqa -> maxPerSector  =   1;  // max number of pairs allowed per sector
  mEEmixqa -> maxPerEvent   = 100;  // max number of pairs allowed per event

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

    std::cout << "number of points: " << mEEpoints -> numberOfPoints() << std::endl;
    std::cout << "number of pairs:  " << mEEmixer -> numberOfCandidates() << std::endl;


	    
  }
  //--
  //-----------------------------------------------------------------


  //--
  //-- For debugging purposes, it's often useful to print out the 
  //-- database 
  //--
  mEEmcDatabase = (StEEmcDb*)mChain->GetDataSet("StEEmcDb");
  if (mEEmcDatabase) mEEmcDatabase->exportAscii("dbdump.dat"); 

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
  file->cd();
  file->mkdir("pions");
  file->cd("pions");
  mEEmixqa -> GetHistList() -> Write();
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
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
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

