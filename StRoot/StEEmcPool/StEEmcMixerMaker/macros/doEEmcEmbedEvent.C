////////////////////////////////////////////////////////////////////////////////////////////////////
/*!\fn doEmcEmbedEvent
\author Alexandre Suaide
Modified for EEmc      WMZ   4/27/2005
*/
class StChain;
StChain *chain=0;
//void doEEmcEmbedEvent(int nevents = 10,char* file="*.event.root", char* file1="*.geant.root",Bool_t print = kTRUE)

void doEEmcEmbedEvent(
int nevents = 3,
char* file="/star/data45/reco/productionPP/ReversedFullField/P04ik/2004/134/st_physics_5134003_raw_2030010.event.root",
char *file1="/star/data04/sim/jwebb/MonteCarlo/single_gamma/mcpi0_5000_06TC05_15.geant.root",
Bool_t print = true )
{
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StMagF");
  gSystem->Load("StUtilities");  
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities"); 
  gSystem->Load("StMcEvent"); 
  gSystem->Load("StMcEventMaker"); 
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StMcAnalysisMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("libgeometry_Tables");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
    
  gSystem->Load("StEEmcSimulatorMaker");
  gSystem->Load("StMaxStripPi0");
  gSystem->Load("StEEmcMixerMaker");
  gSystem->Load("StEEmcAssociationMaker");
  gSystem->Load("StEEmcMatchMaker");

// create chain    
  chain = new StChain("bfc");  
  if(print) chain->SetDebug(1);
    
// open data
  StIOMaker* io = new StIOMaker("IO");
  io->SetFile(file);
  io->SetIOMode("r"); 
  io->SetBranch("*",0,"0");           //deactivate all branches
  io->SetBranch("eventBranch",0,"r");



// EEmc database 
  St_db_Maker *stDb = new St_db_Maker("StarDb", "MySQL:StarDb");
  StEEmcDbMaker* myDb=new StEEmcDbMaker("eemcDb");

  //int firstSec=1;
  //int lastSec=12;
  //myDb->setSectors(firstSec, lastSec);

#if 0  // Set this flag equal to 1 if you're mixing two Monte Carlo files... though why would you want to?
  stDb->SetDateTime(20031120,0);
  stDb->SetFlavor("sim","eemcPMTcal");
  stDb->SetFlavor("sim","eemcPIXcal");
  stDb->SetFlavor("sim","eemcPMTped");
  stDb->SetFlavor("sim","eemcPMTstat");
  stDb->SetFlavor("sim","eemcPMTname");
  stDb->SetFlavor("sim","eemcADCconf");
#endif
  gMessMgr->SwitchOn("D");
  gMessMgr->SwitchOn("I");
  gMessMgr->SwitchOn("W");

// open MC (must be after database so we find correct timestamp)
  StIOMaker* io1 = new StIOMaker("IO1");
  io1->SetFile(file1);
  io1->SetIOMode("r"); 
  io1->SetBranch("*",0,"0");           //deactivate all branches
  io1->SetBranch("geantBranch",0,"r");



// StMakers 
  StEEmcPreMixerMaker *preMixer = new StEEmcPreMixerMaker("preEmbed");

  StMcEventMaker *mcEvent = new StMcEventMaker(); // McEventMaker
//  mcEvent->SetDebug(1);
  
  StEEmcSimulatorMaker *eemcSim = new StEEmcSimulatorMaker();// EEmc simulator
//  eemcSim->SetDebug(1);
//  eemcSim->setSimEsmd(0);

  StEEmcMixerMaker *embed = new StEEmcMixerMaker(); // EEmc mixer
//  embed->SetDebug(1);

  mEEanalysis = new StEEmcA2EMaker("StEEmcA2EMaker"); // EEmc adc2Energy
  mEEanalysis -> source("StEventMaker",2);
  mEEanalysis -> database ( "eemcDb" );

  mEEclusters=new StEEmcClusterMaker("StEEmcClusterMaker"); // EEmc cluster
  mEEclusters->analysis("StEEmcA2EMaker");
// default setup of JWebb
  mEEclusters->setMaxExtent(4);
  mEEclusters->setSeedFloor(2.5);
  mEEclusters->setFillStEvent();

  mEEpoints=new StEEmcPointTreeMaker("mEEpoints");  // EEmc point
  mEEpoints->analysis("StEEmcA2EMaker");
  mEEpoints->clusters("StEEmcClusterMaker");
  mEEpoints->setFillStEvent();
  mEEpoints->setFilename("eemcPoint.root");

  // TPC association 
  StAssociationMaker    *association = new StAssociationMaker();  

  // EEmc association 
  StEEmcAssociationMaker *eemcAssociation = new StEEmcAssociationMaker(); 
//  eemcAssociation->SetDebug(1);

/*
 USER's McTrack-hits matching maker added here.
 example given below, replace it with your own.   
*/
  StEEmcMatchMaker *match = new StEEmcMatchMaker(); 


  chain->ls(3);
  chain->Init();
  int iev = 0;
  int istat = 0; 
  
// do the event loop    
  while ( istat!=2 && istat!=3 && istat!=4 && iev<=nevents ) {
    chain->Clear();
    istat = chain->Make();
    if(iev%20==0) cout << "Finished processing event number "<<iev <<endl;
    iev++;
//  eemcAssociation->printMaps();
  }

  chain->Finish();
         
}
