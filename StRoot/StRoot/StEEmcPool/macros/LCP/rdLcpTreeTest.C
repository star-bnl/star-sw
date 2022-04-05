  float vz;
  int nTr;
  TChain *chain = new TChain("Tjas","jDst");

rdLcpTree() {
  //  f=TFile("htree.root");
  //f->ls();
  //TH1F *h=f->Get("hpx");
  // h->Draw();


  chain->AddFile("./wrkLcp/R3011049.tree.root", -1);
  chain->AddFile("./wrkLcp/R3011050.tree.root", -1);
  chain->AddFile("./wrkLcp/R3011051.tree.root", -1);
  chain->AddFile("./wrkLcp/R3011052.tree.root", -1);
  chain->AddFile("./wrkLcp/R3012001.tree.root", -1);
  chain->AddFile("./wrkLcp/R3012002.tree.root", -1);
  chain->AddFile("./wrkLcp/R3012008.tree.root", -1);
  
  chain->SetBranchAddress("vz",&vz);  
  chain->SetBranchAddress("nPrim",&nTr);  

  
  int N=chain->GetEntries();
  printf("N=%d\n",N);
  system("date");
  int k;
  for(k=0;k<N;k++) {
    int ret=chain->GetEntry(k);  
    assert(ret);
    if(k%2000==0)printf("%d %d %d %f\n",k,ret,nTr,vz);
  }
  system("date");

}
