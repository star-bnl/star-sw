// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
class StMuDstMaker;

StMuDstMaker* maker;

void example() {
   if (gClassTable->GetID("TTable") < 0) {
     gSystem->Load("libTable");
     gSystem->Load("libPhysics");
   }     
  gROOT->LoadMacro("StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  
  StMuDebug::setLevel(0);  // switch of some debug output

  int iret=0;
  maker = new StMuDstMaker(0,0,"","test.lis","MuDst.root",2);   // set up maker in read mode
  StMuDbReader* db = StMuDbReader::instance();
  //  db->addDb("/star/u/laue/afsWork/P02gc.db");
  //  db->addDb("/star/u/laue/afsWork/P02gd.db");

  iret = maker->Make();  // read an event 
  iret = maker->Make();  


  StMuEvent* e = maker->muDst()->event();
  StL0Trigger t;
  StEventInfo info;
  if (e) {
  t=e->l0Trigger();
  info=e->eventInfo();
  }

  int n = maker->muDst()->primaryTracks()->GetEntries();  // get number of primary tracks
  
  for (int i=0; i<n; i++) {
    StMuTrack* primaryTrack = maker->muDst()->primaryTracks(i);     // get pointer to primary track
    StMuTrack* global = primaryTrack->globalTrack();
    cout << endl;
    StMuTrack*  tt = primaryTrack;
    printf("primary momentum=%8f first-helix=%8f  last-outerHelix=%8f length=%8f lengthMeasured=%8f charge=%d \n",
	   tt->p().mag(), (tt->firstPoint() - tt->helix().origin()).mag(), (tt->lastPoint() - tt->outerHelix().origin()).mag(), tt->length(), tt->lengthMeasured(), tt->charge() );
    StMuTrack*  tt = global;
    printf("global  momentum=%8f first-helix=%8f  last-outerHelix=%8f length=%8f lengthMeasured=%8f  charge=%d \n",
	   tt->p().mag(), (tt->firstPoint() - tt->helix().origin()).mag(), (tt->lastPoint() - tt->outerHelix().origin()).mag(), tt->length(), tt->lengthMeasured(), tt->charge() );
  }
  printf("EVENT id=%d, runId=%d  unix time=%d trigWord=0x%0x bXing=%d spinBits=%d nPrim=%d\n", info.id(), info.runId(), info.time(), t.triggerWord(),t.bunchCrossingId(), t.spinBits(),n);
  

}



