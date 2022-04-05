plFeePed ( int crate=4, int group=1){

  //crate=1,2,3,4,5,6
  //crate is crate number
  //group is group of 16 FEE ch in crate
  // group 0=ch0-15
  // group 1=ch16-31

  gStyle->SetOptStat(1111111);
  char *name="feePed4/feePed.hist.root";

  TFile *f=new TFile(name);
  if(!f->IsOpen()){
    printf("Error opening file=%d, STOP\n",name);
    return 1;
  }
  // f->ls();

  int start=group*16;
  int stop=start+16;

  char tt1[100];
  sprintf(tt1,"cr%d-ch%03d-%03d",crate,start,stop-1);

  TCanvas *c=new TCanvas(tt1,tt1,800,800);


  c->Divide(4,4);
  int r=0;
  int i=crate-3;
  int j;
  for(j=start;j<stop;j++) {
    r++;
    c->cd(r); 

    sprintf(tt1,"cr%d_ch%3.3d",crate,j);
    printf("plot %s\n",tt1);
    TH1F* h= (TH1F*) f->Get(tt1); 
    h->Draw();
    h->SetAxisRange(0,50); 
    ( h->Integral()>0)gPad->SetLogy();   
  }
}
