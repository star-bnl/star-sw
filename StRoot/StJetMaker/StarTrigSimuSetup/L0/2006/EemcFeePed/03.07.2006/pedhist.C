pedhist ( int crate=3, int group=2){

  //crate is crate number
  //group is group of 16 FEE ch in crate
  // group 0=ch0-15
  // group 1=ch16-31

  gStyle->SetOptStat(1111111);
  char *name="feePed.hist.root";

  TFile *f=new TFile(name);
  TCanvas *c=new TCanvas("bb","ccc",800,800);

  int start=group*16;
  int stop=start+16;

  c->Divide(4,4);
  int r=0;
  int i=crate-3;
  int j;
  for(j=start;j<stop;j++) {
    r++;
    c->cd(r); 
    char tt1[100];
    sprintf(tt1,"cr%d_ch%03d",crate,j);
    TH1F* h= (TH1F*) f->Get(tt1); 
    printf("=%s= %p\n",tt1,h);
    h->Draw();
    h->SetAxisRange(0,100); 
    gPad->SetLogy();   
  }	
}
