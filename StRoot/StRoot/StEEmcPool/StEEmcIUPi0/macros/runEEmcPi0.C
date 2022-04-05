//--
//-- runEEmcPi0.C
//-- used by real data
//-- example script for running the EEMC pi0 finder.  The macro
//-- will process the first nevents of the MuDst (or list of MuDst's).
//-- if nevents is negative, the macro will process all events in the
//-- mudst.
//--
//-- author: Weihong He
//-- switch should be commented out when analysing real data
//#define MONTE_CARLO
#define NEW 

class StChain;
class St_db_Maker;
class StEEmcDb;
class StMuDstMaker;
class StEEmcA2EMaker;
class StEEmcIUClusterMaker;
class StEEmcIUPointMaker;
//class StEEmcPointFitMaker;
class StEEmcIUMixMaker;
//class StEEmcMixQAMaker;
class StSpinDbMaker;
//--
//-- globals
//--
StChain            *mChain        = 0;
St_db_Maker        *mStarDatabase = 0;
StEEmcDb           *mEEmcDatabase = 0;
StMuDstMaker       *mMuDstMaker   = 0;
StEEmcA2EMaker     *mEEanalysis   = 0;
StEEmcIUClusterMaker *mEEclusters   = 0;
StEEmcIUPointMaker   *mEEpoints     = 0;
//StEEmcPointFitMaker *mEEpoints = 0;

StEEmcIUMixMaker     *mEEmixer      = 0;
//StEEmcMixQAMaker   *mEEmixqa      = 0;
StSpinDbMaker      *mSpinDb       = 0;

Int_t               count         = 0;
Int_t               stat          = 0; 

Int_t prescale = 100;

void runEEmcPi0( Int_t nevents =-1, 
			Char_t *name = "st_physics_adc_7136033_raw_1060001.MuDst.root",
			Char_t *ofile= "test.root ",
			Char_t *path = "/star/institutions/iucf/hew/2006ppLongRuns/7136033/", 
			Int_t trigID=137641, 
			Int_t nfiles = 100,)
		      
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

  mSpinDb = new StSpinDbMaker("mSpinDb");

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
  //mEEanalysis->scale(1.3);                  // scale energies by x1.2
  //mEEanalysis->FUNC(1.,0.);
   

  //--
  //-- Cluster maker.  Generates tower, preshower, postshower clusters
  //-- (using Minesweeper algo) and smd clusters (seed strip w/ truncation).
  //--
  mEEclusters=new StEEmcIUClusterMaker("mEEclusters");
  mEEclusters->analysis("AandE");
  mEEclusters->seedEnergy(0.8,0);       // tower seed energy
  mEEclusters->seedEnergy(3.0/1000.,4); // 2 MeV smd-u strip
  mEEclusters->seedEnergy(3.0/1000.,5); // 2 MeV smd-v strip
  mEEclusters->setSeedFloor(1.0);       // floating seed threshold near clusters
  mEEclusters->setMaxExtent(3);         // maximum distance from seed strip
  mEEclusters->suppress(0);              // disallows seeds in two strips adjacent to any cluster


  //--
  //-- Point maker.  Matches pairs of smd clusters to active towers.
  //-- Energy is divided between points using a tower energy-sharing
  //-- vs position function.
  //--
  mEEpoints=new StEEmcIUPointMaker("mEEpoints");
  //mEEpoints=new StEEmcPointFitMaker("mEEpoints");
  mEEpoints->analysis("AandE");
  mEEpoints->clusters("mEEclusters");
  mEEpoints->setEnergyMode(0);
  //mEEpoints->setLimit(10);
  //  mEEpoints -> doPermutations(false);

 
  
  //--
  //-- Pi0 mixer, takes the points identified above and mixes pi0 pairs.
  //--
  mEEmixer = new StEEmcIUMixMaker("mEEmixer");
  mEEmixer -> mudst("MuDst");
  mEEmixer -> analysis("AandE");
  mEEmixer -> points("mEEpoints");
  for ( Int_t i=0;i<12;i++ ) // activate all 12 sectors
    mEEmixer->sector(i); 
  mEEmixer->trigger(trigID); // specify trigger id(s) to process

 
  //pi0 analysis starts
  mEEpi0analysis=new StEEmcIUPi0Analysis("pi0analy");
  mEEpi0analysis->trigger(trigID);
  mEEpi0analysis->minbias(117001);
  mEEpi0analysis->mudst("MuDst");
  mEEpi0analysis->points("mEEpoints");
  mEEpi0analysis->mixer("mEEmixer");
  mEEpi0analysis->analysis("AandE"); 
  mEEpi0analysis->spin("mSpinDb");
  //--
  //-- QA/analysis histograms for pi0's
  //--
  //mEEmixqa = new StEEmcMixQAMaker("mEEmixqa");
  //mEEmixqa -> mixer( "mEEmixer", 0.1, 0.18 ); // specify mixer and mass range for gated histograms
  //mEEmixqa -> points( "mEEpoints" );
  //mEEmixqa -> maxPerCluster =   1;  // max number of pairs matched to a cluster of towers
  //mEEmixqa -> maxPerSector  =   100;  // max number of pairs allowed per sector
  //mEEmixqa -> maxPerEvent   = 100;  // max number of pairs allowed per event

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
  int Tnumsmdu=0,Tnumsmdv=0,Tnumpoints=0,Tnumpairs=0,Tnumfp=0,n2clusteru=0,n2clusterv=0,n2point=0,j=0;
  //FILE *fout=fopen("lowratio.txt","w");assert(fout);
  while ( stat == 0 ) {


    //--
    //-- Terminate once we reach nevents --
    //--
    if ( count++ >= nevents && nevents>0 ) break;

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
    if ( (count%prescale)==0 ) //continue;
      {
	std::cout << "------------------------------------------------";
	std::cout << "event=" << count << std::endl;
      }
    //--
    //-- Print the number of hits in the towers, pre/postshower layers
    //--
    Int_t nhits[]={0,0,0,0,0,0};
    float umeandiff=0,vmeandiff;
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
    

  }
 
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
 
  //file->mkdir("pions");
  //file->cd("pions");
  //mEEmixqa -> GetHistList() -> Write();
  mEEclusters -> GetHistList() -> Write();
  mEEpoints -> GetHistList() -> Write();
  mEEpi0analysis->GetHistList()->Write(); 
 
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
  gSystem->Load("StEEmcA2EMaker");
#ifdef NEW
  gSystem->Load("StEEmcIUPi0");
#else
  gSystem->Load("StMaxStripPi0");
#endif 
  gSystem->Load("StSpinDbMaker");

}
