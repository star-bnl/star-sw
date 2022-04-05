// manual pedestal fit to one channel
pedMan ( char *name="out3-5b.hist.root"){

  name="mini1001.hist.root";
  TFile *f=new TFile(name);
 

  TCanvas *c=new TCanvas("bb","ccc",600,800);
    c->Divide(1,2);
  int sec=6,isub=3,eta=4;
 
  char tt1[100];
	
  sprintf(tt1,"%2.2dT%c%2.2dc%d",sec,'A'+isub,eta,0);
  TH1F*h= (TH1F*) f->Get(tt1); // find histrogram in file with this name

  h->Draw(); h->SetAxisRange(0,100); gPad->SetLogy();   
  char text[100];  sprintf(text,"eta%2.2d.gif",eta);
  getPed(h);
  // c->Print(text);
}


void getPed(TH1F* h){
  
  float *x=h->GetArray();

  // find max ADC in first 100 channels

  float ym=0;
  int nb=h->GetNbinsX();
  int i;
  int j=-1;
  for(i=1;i<=100;i++){
    if(ym>x[i]) continue;
    ym=x[i];
    j=i;
  }

  float x0=h->GetBinCenter(j);
  h->Fit("gaus","","",x0-5,x0+4); 

  TF1 *f=h->GetFunction("gaus");
  assert(f);
  
  f->SetLineColor(kRed);
  float g=f->GetParameter(1);
  float eg=f->GetParError(1);
  printf("#%s  mean gain  %f +/- %f \n",h->GetTitle(),g,eg);
}







