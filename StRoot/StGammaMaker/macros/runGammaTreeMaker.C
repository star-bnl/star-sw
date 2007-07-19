//-- switch should be commented out when analysing real data
#define MONTE_CARLO

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
			 //Char_t *name = "7146009.list", 
			 //Char_t *ofile= "7146009.root",
			 Char_t *name="/star/institutions/mit/betan/Simulation/photon_9_11_1.MuDst.root",
			 Char_t *ofile="photon_9_11_1.gtree.root",
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
  TString gname=name;
  if ( !gname.Contains("MuDst.root") )
    {
      std::cout << "We need to be running over a single MuDst for MC" << std::endl;
      return;
    }
  gname.ReplaceAll("MuDst.root","geant.root");
  StIOMaker* ioMaker = new StIOMaker();
  ioMaker->SetFile(gname);
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");             //deactivate all branches
  ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
  
  class StMcEventMaker *mcEventMaker = new StMcEventMaker();
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



  //get BEMC calibration 
#ifndef MONTE_CARLO
  StEmcADCtoEMaker *bemcAdc2E = new StEmcADCtoEMaker(); // this will just convert what's in MuDst to ADC, use for data only!
  bemcAdc2E->setPrint(true);
#endif
#ifdef MONTE_CARLO
  StEmcSimulatorMaker* emcSim = new StEmcSimulatorMaker(); //use this instead to "redo" converstion from geant->adc
  StPreEclMaker* preEcl = new StPreEclMaker(); //need this to fill new StEvent information
#endif



#ifdef MONTE_CARLO
  StMCAsymMaker* asym = new StMCAsymMaker;
  StGammaPythiaEventMaker* pythia = new StGammaPythiaEventMaker;
#endif

  StGammaEventMaker *gemaker = new StGammaEventMaker();
  StGammaRawMaker       *raw    = new StGammaRawMaker(); 
  StBarrelEmcClusterMaker* ecl  = new StBarrelEmcClusterMaker;
  StGammaCandidateMaker *gcm    = new StGammaCandidateMaker();
  StGammaTreeMaker      *gtm    = new StGammaTreeMaker();
  gtm->SetFilename(ofile);

  mChain->ls(3);

  mChain->Init();


#ifdef MONTE_CARLO
  // Note: ------------ Must do this stuff after Init()
  const int controlVal = 2;
  controlEmcSimulatorMaker_st* simControl = emcSim->getControlSimulator()->GetTable();
  simControl->calibSpread[0] = 0.15;
  simControl->keyDB[0] = controlVal;
  simControl->keyDB[1] = 0;
  simControl->keyDB[2] = controlVal;
  simControl->keyDB[3] = controlVal;
  //keyDB[det] = 0 -> NO database (default value)
  //           = 1 - only gains are applied
  //           = 2 - gains and pedestals are applied
  // In other words, for pure MC should be 2, and
  // for embedding should be 1.

#endif





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
