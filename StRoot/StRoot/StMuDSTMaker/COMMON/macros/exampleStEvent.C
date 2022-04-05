// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
class StMuDstMaker;
class StEvent;

StMuDstMaker* maker;
StEvent* ev;


void exampleStEvent() {
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

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



