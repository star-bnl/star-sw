/* allows to concatent TTree's from many
 runs given by runL="R3012010 R3012011 ..."

projects LCP phi vs. bX120, for specified cuts

read TTree & saves histo in the wrkDir

use fill="F2201" as the core for the output histo

*/

rdLcpTree(const char *runL="R3012008", const char *wrkDir="./wrkLcpX/", const char *fill="F9999"){

  printf("rdLcpTree:: chain runs: '%s' --> '%s'\n",runL,fill);

  TString outF=wrkDir; outF+=fill; outF+=".hits.root";

  hFile=new TFile(outF,"RECREATE");
  assert(hFile->IsOpen());
  //create histograms
  TH1F *h1[8];

  h1[0]=new TH1F("bx","rates vs. bXing",129,-0.5,128.5);
  h1[1]=new TH1F("pt","Leading Particle pT (GeV/c)",200,0.,20.);
  h1[2]=new TH1F("Zv","Z vertex/cm",100,-250.,250.);
  h1[3]=new TH1F("nPrim","No.of valid prim TPC tracks ",50,-0.5,49.5);
  h1[4]=new TH1F("cosm", "Mike's Cosmics  Rejector return value",5,-0.5,4.5);
  h1[5]=new TH1F("phi","LCP phi/rad",100,-3.15.,3.15);
 
  
  printf("finds all input TTree's...\n");

  TChain *chain = new TChain("T-LCP","dummName");
  int i=0;
  system("date");
  char *run=strtok(runL," "); // init 'strtok'
  while(run){
    i++;
    TString item=run;
    item=wrkDir+item;
    item=item+".tree.root";
    chain->AddFile(item,-1);
    printf("found run%2.2d '%s' -->%s\n",i,run,item.Data());
    run=strtok(0," ");  // advance by one name
  }
  int N=chain->GetEntries();
  printf("N=%d\n",N);
  system("date");


  // associate varibales with  the tree
  float vz,pt,phi;
  int nPrim,bx,cosm,id,nFit;
  chain->SetBranchAddress("bx120",&bx);  
  chain->SetBranchAddress("vz",&vz);  
  chain->SetBranchAddress("id",&id);  
  chain->SetBranchAddress("pt",&pt);  
  chain->SetBranchAddress("phi",&phi);  
  chain->SetBranchAddress("nPrim",&nPrim);  
  chain->SetBranchAddress("nFit",&nFit);  
  chain->SetBranchAddress("cosm",&cosm);  

  printf("\n::::::::::::::: L O O P   O V E R   E V E N T S  :::::\n");
  int k;
  int nLcp=0;
  for(k=0;k<N;k++) {
    int ret=chain->GetEntry(k);  
    assert(ret);
    if(k%1000==0)printf("%d %d %d %f id=%d nFit=%d\n",k,ret,nPrim  ,vz,id,nFit);
    if(nPrim<=0) continue;
    h1[3]->Fill(nPrim);
    h1[0]->Fill(bx);
    h1[2]->Fill(vz);
    h1[4]->Fill(cosm);
    if(nFit<=0) continue;
    nLcp++;
    // cuts on LCP 
    if(pt<1.2) continue;
    h1[1]->Fill(pt);
    h1[5]->Fill(phi);

  }
  system("date");
  printf("nLcp=%d\n",nLcp);
  
  hFile->Write();// Save all objects in this file

  c=new TCanvas();
  c->Divide(2,2);
  
  c->cd(1); h1[5]->Draw(); //gPad->SetLogy(); 
  c->cd(2); h1[1]->Draw();
  c->cd(3); h1[2]->Draw();
  c->cd(4); h1[3]->Draw();

}
