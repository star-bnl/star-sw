class StEvent;
StEvent *rEvent1 = 0, *rEvent2 = 0;
class StBFChain;
StBFChain *chain;
//________________________________________________________________________________
void PrintInfo() {
  rEvent1 = (StEvent*) chain->GetDataSet("IO1/.make/IO1_Root/.data/bfcTree/eventBranch/StEvent");
  rEvent2 = (StEvent*) chain->GetDataSet("IO2/.make/IO2_Root/.data/bfcTree/eventBranch/StEvent");
  cout << "Pointers obtained from GetDataSet" << endl;
  cout << "Event1 At: " << rEvent1 << endl;
  cout << "Event2 At: " << rEvent2 << endl;
  if (!rEvent1 || !rEvent2) return;
  cout << "Event1: Run "<< rEvent1->runId() << " Event1 No: " << rEvent1->id() << endl;
  cout << "Event2: Run "<< rEvent2->runId() << " Event2 No: " << rEvent2->id() << endl;
 }
//________________________________________________________________________________
void Read2StEvents(//const char *eventFile1="tpt_p05ie/st_physics_6174070_raw_2020010.event.root",
		   //const char *eventFile2="ittf_p05fi/st_physics_6174070_raw_2020010.event.root") {
		   const char *eventFile1="/star/data07/calib/fisyak/test/DevDevComparision/year_2005_CuCu200_MinBias_daq_sl302/st_physics_6048025_raw_1020002.event.root",
		   const char *eventFile2="/star/data07/calib/fisyak/test/g77Comparision/year_2005_CuCu200_MinBias_daq_sl302/st_physics_6048025_raw_1020002.event.root"){
  //		   const char *eventFile2="/star/data07/calib/fisyak/test/DevDevComparision/year_2005_CuCu200_MinBias_daq_sl302/st_physics_6048025_raw_1020002.event.root"){
  gROOT->LoadMacro("bfc.C");
  TString Chain("in,StEvent,nodefault,debug");
  bfc(-2,Chain.Data(),0,0,0);
  StIOMaker* ioMaker1 = new StIOMaker("IO1","r",eventFile1);//,"bfcTree");
  ioMaker1->SetDebug(2);
  ioMaker1->SetIOMode("r");
  ioMaker1->SetBranch("*",0,"0");                 //deactivate all branches
  ioMaker1->SetBranch("eventBranch",0,"r"); //activate geant Branch
  StIOMaker* ioMaker2 = new StIOMaker("IO2","r",eventFile2);//,"bfcTree");
  ioMaker2->SetDebug(2);
  ioMaker2->SetIOMode("r");
  ioMaker2->SetBranch("*",0,"0");                 //deactivate all branches
  ioMaker2->SetBranch("eventBranch",0,"r"); //activate geant Branch
  Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
  if (initStat) return;
  chain->Make();
  PrintInfo();
 }
