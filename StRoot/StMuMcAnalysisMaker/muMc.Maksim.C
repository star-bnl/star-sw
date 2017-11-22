/*
  root.exe -q -b -x 'muMc.C(1e6,"../*MuDst.root")'
  root.exe 'muMc.C(-1,"")'
  StMuMcAnalysisMaker::instance()->DrawAll();
*/
void muMc(Int_t N = 10, const Char_t *input = "/net/l404/data/fisyak/reco/2016/Embedding/D0KpimTsq.TFG17i/st_physics_adc_17128018_raw_1000007.MuDst.root", const Char_t *output = "muMc.root", bool isSignal = 1) {
#if !defined(__CINT__) && !defined(__CLING__)
  std::cout << "This code cannot be compiled" << std::endl;
#else
  //  gSystem->SetFPEMask(kInvalid | kDivByZero | kOverflow );
  gROOT->LoadMacro("lMuDst.C");
  Char_t *file =  gSystem->Which("./",output,kReadPermission);
  if (! file) {
    lMuDst(0,input,"RMuDst,mysql,MuMc,quiet,nodefault",output);
    StBFChain *chain = (StBFChain *) StMaker::GetTopChain();
    StMuDstMaker* maker = (StMuDstMaker *) chain->Maker("MuDst");
    maker->SetStatus("*",0);
    const Char_t *ActiveBranches[] = {"MuEvent"
				      ,"PrimaryVertices"
				      ,"PrimaryTracks"
				      ,"GlobalTracks"
				      ,"CovGlobTrack"
				      ,"StStMuMcVertex"
				      ,"StStMuMcTrack"
    };
    Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
    for (Int_t i = 0; i < Nb; i++) maker->SetStatus(ActiveBranches[i],1); // Set Active braches
    StMuDebug::setLevel(0);  
    StMuMcAnalysisMaker *maker = chain->Maker("MuMcAnalysis");
    maker->StoreCutNTuples();
    if(isSignal)
      maker->ProcessSignal(); //TODO comment when processing background, uncomment for signal
    chain->EventLoop(N);
  } else {
    lMuDst(-1,"","RMuDst,mysql,MuMc,nodefault");
    TFile *f = new TFile(file);
    //    StBFChain *chain = StMaker::GetTopChain();
    chain->SetTFile(f);
    chain->Init();
    StMuMcAnalysisMaker::instance()->DrawAll();
  }
  delete [] file;
#endif
  
}
