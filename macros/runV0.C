void runV0() {
  TString Chain("in,y2012a,StiCA,BAna,KFVertex,ReadAll,analysis,nodefault");
  gROOT->LoadMacro("bfc.C");
  chain = bfc(0,Chain.Data(),"/star/institutions/bnl/fisyak/V0/gtrack.event.root");
#if 0
  //  chain->Init();
  //  V0Filter();
  // Old Sti
  StiKalmanTrackNode::setDebug(8+32+16);
  StiKalmanTrackFinder::setDebug(2);
  StiKalmanTrackFitter::setDebug(1);
  StiKalmanTrack::setDebug(2);
  StiTrackNodeHelper::setDebug(8);
#endif
  chain->Maker("KFVertex")->SetDebug(2);
}
