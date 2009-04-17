////////////////////////////////////////////////////////////////////////////////////////////////////
/*!\fn doEmcEmbedEvent
\author Alexandre Suaide
*/
class StChain;
StChain *chain=0;
void doEmcEmbedEvent(int nevents = 10,char* file="*.event.root",Bool_t print = kTRUE, const char* outDir = "./")
{
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StBFChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StarMagField"); // needed for StMagF
  gSystem->Load("StMagF");
  gSystem->Load("StUtilities");  // new addition 22jul99
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities"); 
  gSystem->Load("StMcEvent"); 
  gSystem->Load("StMcEventMaker"); 
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StAssociationMaker");
  //gSystem->Load("StMcAnalysisMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("libgeometry_Tables");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");
    
  gSystem->Load("StEmcSimulatorMaker");     
  gSystem->Load("StMuDSTMaker");
  gSystem->Load("StEmcMixerMaker");

  gSystem->Load("StMiniMcEvent");
  gSystem->Load("StMiniMcMaker");
  
  cout << "Loading done" << endl;
// create chain    
  chain = new StChain("bfc");  
  if(print) chain->SetDebug(1);
    
  StIOMaker* io = new StIOMaker("IO");
  io->SetFile(file);
  io->SetIOMode("r"); 
  io->SetBranch("*",0,"0");           //deactivate all branches
  io->SetBranch("eventBranch",0,"r");
  io->SetBranch("geantBranch",0,"r");
  
  St_db_Maker *db1 = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
   
  StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();
  // this line is important to propagate all the hits into StEvent
  // so, even the pedestals are propagated. In this case
  // the second AdcToEMaker will be responsible for making the
  // cuts (after the simulated hits are embedded)
  adc->saveAllStEvent(kTRUE);
  if(!print) adc->setPrint(kFALSE);
  
  StEmcPreMixerMaker *preMixer = new StEmcPreMixerMaker("preEmbed");
  
  StMcEventMaker *mcEvent = new StMcEventMaker();
  mcEvent->SetDebug();
  StEmcSimulatorMaker *emcSim = new StEmcSimulatorMaker();
  if(!print) emcSim->setPrint(kFALSE);
  
  StEmcMixerMaker *emb = new StEmcMixerMaker();
  // include the next line if you want to embedd all simuated hits
  // even the ones that do not have a hit in the real data
  //emb->setEmbedAll(kTRUE);
  if(!print) emb->setPrint(kFALSE);
  
  StEmcADCtoEMaker *adc1 = new StEmcADCtoEMaker("EReadEmbed");      
  adc1->setEmbeddingMode(kTRUE);
  if(!print) adc1->setPrint(kFALSE);
  
  StPreEclMaker *pre = new StPreEclMaker();
  if(!print) pre->setPrint(kFALSE);
  
  StEpcMaker *epc = new StEpcMaker();
  if(!print) epc->setPrint(kFALSE);
    
  StAssociationMaker    *association = new StAssociationMaker(); // TPC association maker
  association->SetDebug(2);
  association->useInTracker();
  association->useIdAssoc();
  StEmcAssociationMaker *emcAssociation = new StEmcAssociationMaker(); // EMC association maker
  emcAssociation->setPrint(print);
  
  ///////////////////////////////////////////////////////////////
  //
  // put your analysis maker here
  //
  ///////////////////////////////////////////////////////////////
  StMiniMcMaker* minimcmk = new StMiniMcMaker;
  minimcmk->SetDebug(2);
  minimcmk->SetMode(1);
  
  TString outDirName = outDir;
  TString filename   = file;
  TString embedrun   = file;
  
  minimcmk->setOutDir(outDirName.Data());
  int embedRunIndex = embedrun.Index("_",0);
  embedrun.Remove(0,embedRunIndex+1);
  embedRunIndex = embedrun.Index("_",0);
  embedrun.Remove(embedRunIndex);
  int fileBeginIndex = filename.Index("st_zerobias",0);
  filename.Remove(0,fileBeginIndex+3);
  fileBeginIndex = filename.Index("st_zerobias",0);
  filename.Remove(0,fileBeginIndex);
  //filename.Prepend(embedrun);
  //filename.Prepend("emb");
  minimcmk->setFileName(filename);
  minimcmk->setFilePrefix("st_zerobias");
  cout << "outdir : " << outDirName << endl;
  cout << "Input Filename : " << filename << endl;

  
  int commonHits=3;
  StMcParameterDB* parameterDB = StMcParameterDB::instance();  
  // TPC
  parameterDB->setXCutTpc(.5); // 5 mm
  parameterDB->setYCutTpc(.5); // 5 mm
  parameterDB->setZCutTpc(.5); // 5 mm
  parameterDB->setReqCommonHitsTpc(commonHits); // Require 3 hits in common for tracks to be associated
  // FTPC
  parameterDB->setRCutFtpc(.3); // 3 mm
  parameterDB->setPhiCutFtpc(5*(3.1415927/180.0)); // 5 degrees
  parameterDB->setReqCommonHitsFtpc(3); // Require 3 hits in common for tracks to be associated
  // SVT
  parameterDB->setXCutSvt(.08); // 800 um
  parameterDB->setYCutSvt(.08); // 800 um
  parameterDB->setZCutSvt(.08); // 800 um
  parameterDB->setReqCommonHitsSvt(1); // Require 1 hits in common for tracks to be associated
  
  chain->Init();
  int iev = 0;
  int istat = 0; 
  
  controlEmcSimulatorMaker_st* simControl = emcSim->getControlSimulator()->GetTable();
  simControl->keyDB[0] = 1;
  simControl->keyDB[1] = 0;
  simControl->keyDB[2] = 1;
  simControl->keyDB[3] = 1;
  
// do the event loop    
  while ( istat!=2 && istat!=3 && istat!=4 && iev<=nevents ) {
    chain->Clear();
    istat = chain->Make();
    emcAssociation->printMaps();
    if(iev%20==0) cout << "Finished processing event number "<<iev <<endl;
    iev++;
  }
  chain->Finish();
         
}
