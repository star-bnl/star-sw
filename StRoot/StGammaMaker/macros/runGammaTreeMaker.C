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
			Char_t *name = "/star/institutions/iucf/hew/2006ppLongRuns/7136022/st_physics_7136022_raw_1010001.MuDst.root",
			Char_t *ofile = "st_physics_7136022_raw_1010001.gtree.root",
			Char_t *path = "", 
			Int_t nfiles = 100
			)
{
 
  gROOT->Macro("StRoot/StGammaMaker/macros/loadGammaLibs.C"); 
  TString pathname = path; 
  pathname += name;
  mChain = new StChain("chain");

#ifdef MONTE_CARLO
  TString gname=name;
  if ( !gname.Contains("MuDst.root") )
    {
      std::cout << "We need to be running over a single MuDst for MC" << std::endl;
      return;
    }
  gname.ReplaceAll("MuDst.root","geant.root");
  StIOMaker* ioMaker = new StIOMaker;
  ioMaker->SetFile(gname);
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");             //deactivate all branches
  ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
  
  StMcEventMaker* mcEventMaker = new StMcEventMaker;
  mcEventMaker->doPrintEventInfo = false;
  mcEventMaker->doPrintMemoryInfo = false;
#endif



  mMuDstMaker = new StMuDstMaker(0,0,path,name,"MuDst",nfiles);

#if 0
  mMuDstMaker->SetStatus("*",0);
  mMuDstMaker->SetStatus("MuEvent",1);
  mMuDstMaker->SetStatus("EmcAll",1);
  mMuDstMaker->SetStatus("PrimaryTracks",1);
#endif

  //StMuDbReader *db = StMuDbReader::instance();
  //StDetectorDbMaker *detdb = new StDetectorDbMaker();  

  StTriggerFilterMaker *filterMaker = new StTriggerFilterMaker;
  filterMaker->addTrigger(137641);

  //  mStarDatabase = new St_db_Maker("StarDb", "MySQL:StarDb");
  mStarDatabase = new St_db_Maker("StarDb", "MySQL:StarDb", "$STAR/StarDb");

#ifdef MONTE_CARLO  
  // Setup ideal gains for processing MC data
  mStarDatabase->SetFlavor("sim","eemcPMTcal");
  mStarDatabase->SetFlavor("sim","eemcPIXcal");
  mStarDatabase->SetFlavor("sim","eemcPMTped");
  mStarDatabase->SetFlavor("sim","eemcPMTstat");
  mStarDatabase->SetFlavor("sim","eemcPMTname");
  mStarDatabase->SetFlavor("sim","eemcADCconf");
  //  mStarDatabase->SetDateTime(20050101,0);
  mStarDatabase->SetDateTime(20060616, 173801);
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



  // Get BEMC calibration 
#ifdef MONTE_CARLO
  // Use this instead to "redo" conversion from GEANT to ADC
  StEmcSimulatorMaker* emcSim = new StEmcSimulatorMaker;
  emcSim->setCheckStatus(kBarrelEmcTowerId,false); //this returns hits regardless of offline tower status
  emcSim->setCalibSpread(kBarrelEmcTowerId,0.15);//spread gains by 15%
#else
  // This will just convert what's in MuDst to ADC, use for data only!
  StEmcADCtoEMaker* bemcAdc2E = new StEmcADCtoEMaker;
  bemcAdc2E->setPrint(true);
#endif



#ifdef MONTE_CARLO
  StMCAsymMaker* asym = new StMCAsymMaker;
  StGammaPythiaEventMaker* pythia = new StGammaPythiaEventMaker;
#endif

  StGammaEventMaker* gemaker = new StGammaEventMaker;
  StGammaRawMaker* raw = new StGammaRawMaker; 
  StBarrelEmcClusterMaker* ecl = new StBarrelEmcClusterMaker;
  StGammaCandidateMaker* gcm = new StGammaCandidateMaker;
  gcm->SetCompressLevel(0); // 0=No compression, 1=Compress SMD, 2=Compress All

#ifndef MONTE_CARLO
  StSpinDbMaker* spinDb = new StSpinDbMaker;
  StGammaSpinMaker* gspmaker = new StGammaSpinMaker;
#endif

  StGammaTreeMaker* gtm = new StGammaTreeMaker;
  gtm->SetFilename(ofile);

  mChain->ls(3);
  mChain->Init();

  //-----------------------------------------------------------------
  //--
  Int_t count = 0;    // event count
  for (Int_t ev = 1; ev <= nevents || nevents == -1; ++ev) {

    cout << "------------------------------------------------";
    cout << "event=" << count++ << endl;

    mChain -> Clear();
    Int_t stat = mChain -> Make(ev);

    if (stat % 10 == kStEOF || stat % 10 == kStFatal) break;

  }
  //--
  //-----------------------------------------------------------------
}
