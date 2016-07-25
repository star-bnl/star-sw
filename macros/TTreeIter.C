void load(){
    if (gClassTable->GetID("TTable") < 0) {
      gSystem->Load("libGeom");
      gSystem->Load("libTable");
    }
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StUtilities");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StTriggerDataMaker");
    gSystem->Load("StEvent");
    gSystem->Load("StEventUtilities");
    gSystem->Load("StBFChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StMcEvent");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StTpcMcAnalysisMaker");
}
void TTreeIter() {
  load();
  TTreeIter iter;
  iter.AddFile(gDirectory->GetName());
// init of user variables
  const int &fNoPixels       = iter("fNoPixels");
  const int &fNoMcHit        = iter("fNoMcHit");   
  const int &fNoRcHit        = iter("fNoRcHit");   
  const int &fNoMcPad        = iter("fNoMcPad");   
  const int &fNoRcPad        = iter("fNoRcPad");   
  const int &fNoRcTrack      = iter("fNoRcTrack"); 
  const int &fAdcSum         = iter("fAdcSum");    
  const int &fPixels         = iter("fPixels");    
  const Float_t *&pfx        = iter("fRcTrack.fpx"); 
  const Float_t *&pfy        = iter("fRcTrack.fpy"); 
  const Float_t *&pfz        = iter("fRcTrack.fpz"); 
  TH1F *hz = new TH1F("pz","pz distribution",50,-5.,+5.);
  //         Now iterations
  while (iter.Next()) {
    for (int itr=0;itr<fNoRcTrack;itr++) {
      hz->Fill( pfz[itr]); 
    }
  }
  //    iter.Reset(); //ready for next loop                                 
}
