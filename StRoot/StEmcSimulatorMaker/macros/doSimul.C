////////////////////////////////////////////////////////////////////////////////////////////////////
/*!\fn doEmcEmbedEvent
\author Alexandre Suaide
*/
class StChain;
StChain *chain=0;
void doSimul(int nevents = 10,char* file="*.event.root",Bool_t print = kFALSE)
{
  gSystem->Load("St_base");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StMagF");
  gSystem->Load("StUtilities");  // new addition 22jul99
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities"); 
  gSystem->Load("StMcEvent"); 
  gSystem->Load("StMcEventMaker"); 
  gSystem->Load("StEmcUtil");
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StMcAnalysisMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("libgeometry_Tables");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");
    
  gSystem->Load("StEmcSimulatorMaker");     
  gSystem->Load("StEmcMixerMaker");
    
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
  db1->SetDateTime(20040101,0);
  
  StMcEventMaker *mcEvent = new StMcEventMaker();
  
  StEmcSimulatorMaker *emcSim = new StEmcSimulatorMaker();
  if(!print) emcSim->setPrint(kFALSE);
    
  StPreEclMaker *pre = new StPreEclMaker();
  if(!print) pre->setPrint(kFALSE);
  
  StEpcMaker *epc = new StEpcMaker();
  if(!print) epc->setPrint(kFALSE);
      
  ///////////////////////////////////////////////////////////////
  //
  // put your analysis maker here
  //
  ///////////////////////////////////////////////////////////////
  
  chain->Init();
  int iev = 0;
  int istat = 0; 
  
  controlEmcSimulatorMaker_st* simControl = emcSim->getControlSimulator()->GetTable();
  simControl->keyDB[0] = 2;
  simControl->keyDB[1] = 0;
  simControl->keyDB[2] = 2;
  simControl->keyDB[3] = 2;
  simControl->makeFullDet[0] = 1;
  simControl->makeFullDet[1] = 0;
  simControl->makeFullDet[2] = 1;
  simControl->makeFullDet[3] = 1;
  simControl->pedCutOff[2] = 1.5;
  simControl->pedCutOff[3] = 1.5;
  
// do the event loop    
  while ( istat!=2 && istat!=3 && istat!=4 && iev<=nevents ) {
    chain->Clear();
    istat = chain->Make();
    if(iev%20==0) cout << "Finished processing event number "<<iev <<endl;
    iev++;
  }
  chain->Finish();
         
}
