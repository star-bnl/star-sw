void runMuD0(Int_t noEvents = 10000000, const Char_t *input = "./*MuDst.root", const Char_t *output = "testD0.root") {
  gROOT->LoadMacro("bfc.C");
  TString Chain("RMuDst,MuD0Anal,nodefault");
  TString RootFile(output);
  if (RootFile == "") {
    RootFile = gSystem->BaseName(input);
    RootFile.ReplaceAll("*","");
    RootFile.ReplaceAll("MuDst","");
  }
  bfc(-1,Chain.Data(),input,0,RootFile.Data());
  StBFChain *chain = (StBFChain *) StMaker::GetTopChain();
  StMuDstMaker *MuDstMaker = (StMuDstMaker *) chain->Maker("MuDst");
  MuDstMaker->SetStatus("*",0);
  const Char_t *ActiveBranches[] = {
    "MuEvent"
    //   ,"PrimaryVertices"
    //    ,"PrimaryTracks"
    ,"GlobalTracks"
    //   ,"StStMuMcVertex"
    //   ,"StStMuMcTrack"
    //   ,"CovPrimTrack"
    ,"CovGlobTrack"
    //   ,"StStMuMcVertex"
    //   ,"StStMuMcTrack"
    //    ,"KFTracks"
    ,"KFVertices"
    ,"StBTofHit"
    ,"StBTofHeader"
  }; 
  Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
  for (Int_t i = 0; i < Nb; i++) MuDstMaker->SetStatus(ActiveBranches[i],1); // Set Active braches
  chain->Init();
  chain->EventLoop(noEvents);
}
