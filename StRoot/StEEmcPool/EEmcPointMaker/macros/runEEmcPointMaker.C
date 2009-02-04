#define MONTE_CARLO

class StChain;
class StMuEmcCollection;

class St_db_Maker;
class StEEmcDb;
class StMuDstMaker;

class EEmcAnalysisMaker;
class EEmcClusterMaker;
class StEEmcPi0Maker;
class EEmcSmdClusterMaker;
class EEmcPointMaker;
class EEmcClusterMaker2;

StChain        *mChain        = 0;
St_db_Maker    *mStarDatabase = 0;
StMuDstMaker   *mMuDstMaker   = 0;

EEmcAnalysisMaker *mEEanalysis = 0;
EEmcClusterMaker  *mEEclusters = 0;

EEmcSmdClusterMaker *mEEsmdClusters = 0;
EEmcPointMaker *mEEpoints = 0;
EEmcClusterMaker2 *mEEclusters2 = 0;


const Float_t MeV=0.001;


void runEEmcPointMaker( Int_t nevents = 500,
                        Char_t *name = "mc_06TC02_all_2500_1_pt15.MuDst.root",
                        Bool_t useSlow=1,
                        Char_t *path = "/star/data04/sim/jwebb/MonteCarlo/TowerScan/",
			Int_t nfiles = 100
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
  LoadLibs();

  //-- Create the analysis chain --
  mChain = new StChain("eemcAnalysisChain");

  //-- Create micro-dst maker and load in MuDsts --
  mMuDstMaker = new StMuDstMaker(0,0,path,name,"MuDst",nfiles);

  //-- Initialize database connection --
  mStarDatabase = new St_db_Maker("StarDb", "MySQL:StarDb");
  
  //-- DATE TIME FOR MONTE CARLO --
  TDatime *time = new TDatime();
  mStarDatabase -> SetDateTime( time -> GetDate(), time -> GetTime() );
#if 1  // flags for M-C events
  mStarDatabase->SetDateTime(20031120,0);
  mStarDatabase->SetFlavor("sim","eemcPMTcal");
  mStarDatabase->SetFlavor("sim","eemcPIXcal");
  mStarDatabase->SetFlavor("sim","eemcPMTped");
  mStarDatabase->SetFlavor("sim","eemcPMTstat");
  mStarDatabase->SetFlavor("sim","eemcPMTname");
  mStarDatabase->SetFlavor("sim","eemcADCconf");
#endif

  //-- Initialize EEMC database --
  StEEmcDbMaker *eemcDbMaker = new StEEmcDbMaker("eemcDb");
  gMessMgr -> SwitchOff("D");
  gMessMgr -> SwitchOn("I");

  //-- Create the slow simulator for the endcap
  if ( useSlow ) {
    StEEmcSlowMaker *slowSim = new StEEmcSlowMaker("slowSim");
    slowSim->setDropBad(0);   // 0=no action, 1=drop chn marked bad in db
    slowSim->setAddPed(0);    // 0=no action, 1=ped offset from db
    slowSim->setSmearPed(0);  // 0=no action, 1=gaussian ped, width from db
    slowSim->setOverwrite(1); // 0=no action, 1=overwrite muDst values
  }
  
  //-- The EEMC analysis maker --  
  mEEanalysis = new EEmcAnalysisMaker("eemcAnalysisMaker");
  mEEanalysis -> setMuDstMaker("MuDst");
  mEEanalysis -> setSeedEnergy(0.5);
   
  //-- Tower cluster maker --
  mEEclusters = new EEmcClusterMaker("eemcClusterMaker");
  mEEclusters -> setAnalysis("eemcAnalysisMaker");

  mEEclusters2 = new EEmcClusterMaker2("eemcClusterMaker2");
  mEEclusters2 -> setSeedThreshold(0.5);
  mEEclusters2 -> setExpandThreshold(0.35);
 
  mEEsmdClusters = new EEmcSmdClusterMaker("eemcSmdClusterMaker");
  mEEsmdClusters -> setAnalysis("eemcAnalysisMaker");
  //mEEsmdClusters -> setClusters("eemcClusterMaker2");
  mEEsmdClusters -> setSeedEnergy(5.0*MeV);
  mEEsmdClusters -> setThresholdEnergy(0.5*MeV);
  mEEsmdClusters -> setMaxExtent(3);
  mEEsmdClusters -> setMinStrips(3);

  /*
  mEEsmdClusters -> setTerminalFraction(0.075);
  mEEsmdClusters -> setMinCoreFraction(0.3);
  mEEsmdClusters -> setMaxCoreFraction(0.9);
  mEEsmdClusters -> setSmdSeedFraction(0.1); // Fraction of expected energy deposition for a symmetric pi0
  */

    
  /// Turns tower response + smd response into points
  mEEpoints = new EEmcPointMaker("eemcPointMaker");
  //mEEpoints -> setSmdAverage(); 
    
  //----
    
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

    //-- Output so the user know we are still alive --
    if ( !(count%1) ) {
      std::cout << "Event = " << count << " ";
      std::cout << "N hit = " << mEEanalysis -> getNHitTowers() << " ";
      std::cout << "N tow cl= " << mEEclusters -> getNClusters() << " ";
      std::cout << "N tow cl2= " << mEEclusters2->numberOfClusters() << " "; 
      std::cout << "N smd cl= " << mEEsmdClusters-> getNumClusters() << " ";
      std::cout << "nPoints= " << mEEpoints->numberOfPoints() << " ";
      std::cout << std::endl;
          
    }


  }

  //-- Finish up --
  mChain -> Finish();

  return;
  
  
}

void LoadLibs()
{
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");

  gSystem->Load("StEEmcPoolEEmcAnalysisMaker");
  gSystem->Load("StEEmcPoolEEmcClusterMaker");
  gSystem->Load("StEEmcPoolEEmcSmdClusterMaker");
  gSystem->Load("StEEmcSimulatorMaker");
  gSystem->Load("StEEmcPi0Maker");
  gSystem->Load("StEEmcPoolEEmcPointMaker");

  gSystem->Load("StEEmcSimulatorMaker");

}
