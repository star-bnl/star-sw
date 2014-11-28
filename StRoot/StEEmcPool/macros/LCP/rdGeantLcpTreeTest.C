// process .geant.root with my maker, finds generated LCP
//
TChain *chain = new TChain("G-LCP");

rdGeantLcpTreeTest(){
  // f=TFile("./MC7777,tree.root");
  // f->ls();
  // return;
  //TH1F *h=f->Get("hpx");
  // h->Draw();

  TString fItem="recoEffStudy/G2032.tree.root";

  chain->AddFile(fItem, -1);

  chain->ls();
  float pt;
  int nPKPi;
  chain->SetBranchAddress("pt",&pt);  
  chain->SetBranchAddress("nPKPi",&nPKPi);  

  
  int N=chain->GetEntries();
  printf("N=%d\n",N);
  system("date");
  int k;
  for(k=0;k<N;k++) {
    int ret=chain->GetEntry(k);  
    assert(ret);
    //if(k%2000==0)
    printf("%d %d %d %f\n",k,ret,nPKPi,pt);
  }
  system("date");

}
