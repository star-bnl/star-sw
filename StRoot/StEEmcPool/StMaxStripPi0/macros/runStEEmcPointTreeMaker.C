#define MONTE_CARLO

class StChain;
class StMuEmcCollection;

class St_db_Maker;
class StEEmcDb;
class StMuDstMaker;
class StEventMaker; 

class StEEmcA2EMaker;
class StEEmcClusterMaker;

StChain        *mChain        = 0;
St_db_Maker    *mStarDatabase = 0;
StEEmcDb       *mEEmcDatabase = 0;
StMuDstMaker   *mMuDstMaker   = 0;
StEventMaker   *mStEventMaker = 0; 

StEEmcA2EMaker *mEEanalysis=0;
StEEmcClusterMaker *mEEclusters=0;

void runStEEmcPointTreeMaker( Int_t nevents = 500,
			    Char_t *name = "mc_06TC02_eemc_2500_1_pt10.event.root", 
			    Char_t *ofile = "mc_06TC02_eemc_2500_1_pt10.mix.root", 
			    Char_t *ttree = "",
			    Char_t *path = "/star/data04/sim/jwebb/MonteCarlo/TowerScan/", 
			    Int_t nfiles = 100,
			    Bool_t useSlow=1
			    )
{

  // Processes muDst's through the EEmcAnalysis Maker and produces
  // some example histograms

  TString pathname = path; 
  pathname += name;

  //-- Load muDst shared libraries --
  gROOT -> LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  //-- Load shared libraries specific to this analysis --
  gROOT->LoadMacro("StRoot/StEEmcPool/StMaxStripPi0/macros/loadlibs.C");
  loadlibs();

  //-- Create the analysis chain --
  mChain = new StChain("eemcAnalysisChain");

  //-- Create IO maker
  StIOMaker* ioMaker = new StIOMaker();
  ioMaker->SetFile( pathname ); 
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");             //deactivate all branches
  ioMaker->SetBranch("eventBranch",0,"r");   //activate Event Branch


  //-- Initialize database connection --
  mStarDatabase = new St_db_Maker("StarDb", "MySQL:StarDb");
  
  //-- DATE TIME FOR MONTE CARLO --
#ifdef MONTE_CARLO  // flags for M-C events
  TDatime *time = new TDatime();
  mStarDatabase -> SetDateTime( time -> GetDate(), time -> GetTime() );
  mStarDatabase->SetDateTime(20031120,0);
  mStarDatabase->SetFlavor("sim","eemcPMTcal");
  mStarDatabase->SetFlavor("sim","eemcPIXcal");
  mStarDatabase->SetFlavor("sim","eemcPMTped");
  mStarDatabase->SetFlavor("sim","eemcPMTstat");
  mStarDatabase->SetFlavor("sim","eemcPMTname");
  mStarDatabase->SetFlavor("sim","eemcADCconf");
#endif

  //-- Initialize EEMC database --
  new StEEmcDbMaker("eemcDb");
  std::cout <<  "mEEmcDatabase=" << mEEmcDatabase << std::endl;
  gMessMgr -> SwitchOff("D");
  gMessMgr -> SwitchOn("I");

  //-- Create the slow simulator for the endcap
  /*
  if ( useSlow ) {
    StEEmcSlowMaker *slowSim = new StEEmcSlowMaker("slowSim");
    slowSim->setDropBad(0);   // 0=no action, 1=drop chn marked bad in db
    slowSim->setAddPed(0);    // 0=no action, 1=ped offset from db
    slowSim->setSmearPed(0);  // 0=no action, 1=gaussian ped, width from db
    slowSim->setOverwrite(1); // 0=no action, 1=overwrite muDst values
  }
  */ 
  
  //-- The EEMC analysis maker --  
  mEEanalysis = new StEEmcA2EMaker("StEEmcA2EMaker");
  mEEanalysis -> source("StEventMaker",2);
  mEEanalysis -> database ( "eemcDb" );
  

  mEEclusters=new StEEmcClusterMaker("StEEmcClusterMaker");
  mEEclusters->analysis("StEEmcA2EMaker");
  mEEclusters->setMaxExtent(3);
  mEEclusters->setSeedFloor(2.5);
  mEEclusters->setFillStEvent();

  //mEEpoints=new StEEmcPointMaker("mEEpoints");
  mEEpoints=new StEEmcPointTreeMaker("mEEpoints");
  mEEpoints->analysis("StEEmcA2EMaker");
  mEEpoints->clusters("StEEmcClusterMaker");
  mEEpoints->setFillStEvent();
  mEEpoints->setFilename(ofile); 



  mChain->ls(3);
  mChain->Init();


  //-- Loop over all events in the muDst --
  Int_t stat  = 0;
  Int_t count = 0;

  while ( stat == 0 ) {

    //-- Terminate once we reach nevents --
    if ( count++ >= nevents ) break;

    //-- Call clear on all makers --
    mChain -> Clear();

    //-- Process the event through all makers --
    stat = mChain -> Make();
    if ( stat ) break;

    //-- Output so the user know we are still alive --
    if ( !(count%1) ) {
      std::cout << "Event = " << count << " ";
      std::cout << "N hit = " << mEEanalysis -> numberOfHitTowers(0) << " ";
      std::cout << "N cl = " << mEEclusters->numberOfClusters(5,0) << " "
		<< mEEclusters->numberOfClusters(5,1) << " "
		<< mEEclusters->numberOfClusters(5,2) << " "
		<< mEEclusters->numberOfClusters(5,3) << " "
		<< mEEclusters->numberOfSmdClusters(5,0) << " "
		<< mEEclusters->numberOfSmdClusters(5,1) << " ";
      StEvent* mEvent = (StEvent*)mChain->GetInputDS("StEvent");
      assert(mEvent);// fix your chain or open the right event file
      StEmcCollection* emc =(StEmcCollection*)mEvent->emcCollection(); 
      assert(emc);
      StEmcDetector *tow=emc->detector(13);
      StEmcDetector *pqr=emc->detector(14);
      StEmcDetector *smdu=emc->detector(15);
      StEmcDetector *smdv=emc->detector(16);
      std::cout << "N stcl = " << " "
		<< tow->cluster()->numberOfClusters() << " "
		<< pqr->cluster()->numberOfClusters() << " "
		<< smdu->cluster()->numberOfClusters() << " "
		<< smdv->cluster()->numberOfClusters() << " ";    
      std::cout << std::endl;  


          
    }






  }

  //-- Finish up --
  mChain -> Finish();

  return;
  
  
}
