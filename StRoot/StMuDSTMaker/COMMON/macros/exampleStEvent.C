// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
class StMuDstMaker;
class StEvent;

StMuDstMaker* maker;
StEvent* ev;


void exampleStEvent() {
   if (gClassTable->GetID("TTable") < 0) gSystem->Load("libStar");
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
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StMcAnalysisMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");

  cout << " loading done " << endl;
  
  StMuDebug::setLevel(0);  // switch of some debug output
 
  int iret=0;
  maker = new StMuDstMaker(0,0,"","AuAu200.lis","MuDst.root:st_physics",3);   // set up maker in read mode

  StMuDbReader* db = StMuDbReader::instance();
  db->addDb("/star/u/laue/afsWork/P02g.db");

  StEvent* ev=0;
  int n;
  while ( maker->Make()==0 ) {
    cout << n++ << " ";
    cout.flush();
    StMuDst* mu = maker->muDst();
    if (mu) ev = mu->createStEvent();
    if (ev) delete ev; ev=0;
  }
  cout << endl;

}



