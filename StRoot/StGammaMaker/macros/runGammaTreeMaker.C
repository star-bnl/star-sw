//-- switch should be commented out when analysing real data
//#define MONTE_CARLO

class StChain;
class St_db_Maker;
class StEEmcDbMaker;
class StMuDstMaker;
class StEEmcA2EMaker;

//--
//-- globals
//--
StChain            *mChain        = 0;
St_db_Maker        *mStarDatabase = 0;
StEEmcDbMaker      *mEEmcDatabase = 0;
StMuDstMaker       *mMuDstMaker   = 0;
StEEmcA2EMaker     *mEEanalysis   = 0;

Int_t               count         = 0;
Int_t               stat          = 0; 

Int_t prescale = 1; 

void runGammaTreeMaker( Int_t nevents = -1, 
			 //			 Char_t *name = "6149020.lis", 
			 //			 Char_t *ofile= "6149020.root",
			 Char_t *name="7146009.list",
			 Char_t *ofile="test.root",
			 Char_t *path = "", 
			 Int_t nfiles = 100
			 )
{
 
  // Char_t *gname="*.geant.root";

  gROOT->LoadMacro("StRoot/StGammaMaker/macros/loadGammaLibs.C"); 
  loadGammaLibs(); 
  gMessMgr -> SwitchOff("D");
  gMessMgr -> SwitchOff("I");
  TString pathname = path; 
  pathname += name;
  mChain = new StChain("chain");

#ifdef MONTE_CARLO
  StIOMaker* ioMaker = new StIOMaker();
  ioMaker->SetFile(gname);
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");             //deactivate all branches
  ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
  
  class StMcEventMaker *mcEventMaker = new StMcEventMaker();
  mcEventMaker->doPrintEventInfo = false;
  mcEventMaker->doPrintMemoryInfo = false;
#endif MONTE_CARLO


  mMuDstMaker = new StMuDstMaker(0,0,path,name,"MuDst",nfiles);
#if 0
  mMuDstMaker->SetStatus("*",0);
  mMuDstMaker->SetStatus("MuEvent",1);
  mMuDstMaker->SetStatus("EmcAll",1);
  mMuDstMaker->SetStatus("PrimaryTracks",1);
#endif 

  //StMuDbReader *db = StMuDbReader::instance();
  //StDetectorDbMaker *detdb = new StDetectorDbMaker();  
  mStarDatabase = new St_db_Maker("StarDb", "MySQL:StarDb");
  
#ifdef MONTE_CARLO  
  // Setup ideal gains for processing MC data
  mStarDatabase->SetFlavor("sim","eemcPMTcal");
  mStarDatabase->SetFlavor("sim","eemcPIXcal");
  mStarDatabase->SetFlavor("sim","eemcPMTped");
  mStarDatabase->SetFlavor("sim","eemcPMTstat");
  mStarDatabase->SetFlavor("sim","eemcPMTname");
  mStarDatabase->SetFlavor("sim","eemcADCconf");
  mStarDatabase->SetDateTime(20050101,0);
#endif

  // Initialize EEMC database
  mEEmcDatabase = new StEEmcDbMaker("eemcDb");


#ifdef MONTE_CARLO
  // endcap slow simulator
  StEEmcSlowMaker *slowSim = new StEEmcSlowMaker("slowSim");
#endif
 

  // ADC to energy
  mEEanalysis=new StEEmcA2EMaker("mEEanalysis");
  mEEanalysis->database("eemcDb");    // sets db connection
  mEEanalysis->source("MuDst",1);     // sets mudst as input
  mEEanalysis->threshold(3.0,0);      // tower threshold (ped+N sigma)
  mEEanalysis->threshold(3.0,1);      // pre1 threshold 
  mEEanalysis->threshold(3.0,2);      // pre2 threshold
  mEEanalysis->threshold(3.0,3);      // post threshold
  mEEanalysis->threshold(3.0,4);      // smdu threshold
  mEEanalysis->threshold(3.0,5);      // smdv threshold
#ifdef MONTE_CARLO
  mEEanalysis->scale(1.2);                  // scale tower energies by x1.2
#endif

  // endcap clusters
  StMyClusterMaker * EEclusters=new StMyClusterMaker("mEEclusters", mEEanalysis, mMuDstMaker );
  EEclusters->setSeedEnergy(1.5);
  EEclusters->setMinimumEnergy(0.5);



  //get BEMC calibration 
  StEmcADCtoEMaker *bemcAdc2E = new StEmcADCtoEMaker(); // this will just convert what's in MuDst to ADC, use for data only!
  bemcAdc2E->setPrint(true);
#ifdef MONTE_CARLO
  StEmcSimulatorMaker* emcSim = new StEmcSimulatorMaker(); //use this instead to "redo" converstion from geant->adc
  StPreEclMaker* preEcl = new StPreEclMaker(); //need this to fill new StEvent information
#endif

  StGammaEventMaker *gemaker = new StGammaEventMaker();

  StGammaRawMaker       *raw    = new StGammaRawMaker(); 
  StBarrelEmcClusterMaker* ecl  = new StBarrelEmcClusterMaker;
  StGammaCandidateMaker *gcm    = new StGammaCandidateMaker();
  StGammaTreeMaker      *gtm    = new StGammaTreeMaker();

  mChain->Init();
  mChain->ls(3);

  //-----------------------------------------------------------------
  //--
  Int_t stat  = 0;    // error flag
  Int_t count = 0;    // event count
  while ( stat == 0 ) {

    std::cout << "------------------------------------------------";
    std::cout << "event=" << count << std::endl;

    if ( count++ >= nevents ) if ( nevents > 0 ) break;

    mChain -> Clear();
    stat = mChain -> Make();

  }
  //--
  //-----------------------------------------------------------------

  mChain -> Finish(); 

  return;
    
}

void LoadLibs()
{
  //-- Load muDst shared libraries --
  gROOT -> LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  
  gSystem->Load("StDbUtilities");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDetectorDbMaker");

  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("libgeometry_Tables");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");
  gSystem->Load("StEmcSimulatorMaker");
  gSystem->Load("StEmcUtil");

  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcSimulatorMaker");
  
  gSystem->Load("StEEmcA2EMaker");
  gSystem->Load("StEEmcClusterMaker");
  gSystem->Load("StEEmcPointMaker");
  gSystem->Load("StEEmcPi0Mixer");
  gSystem->Load("StGammaMaker");

}

