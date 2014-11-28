TH2F *h2D;
TH1F *hg;

void plBtowGainPerCrate() {
  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(10);

  hg=new TH1F("hg","threshold ADC; crate ID",30,0.5,30.5);

  TFile *_file0 = TFile::Open("/star/data05/scratch/balewski/2009-Wana-outB4.3-bckgNoEtow/data/run9setABCD.wana.hist.root");

 c=new TCanvas();
 c->Divide(2,1);
 c->cd(1);
 muTr2D1->Draw("colz");
 c->cd(2);
 h2D= pubCrR; h2D->Draw("colz");
 // 
gPad->SetLogz();



 int iEW=0;
 for(iEW=0;iEW<2; iEW++) {
   if(iEW==0) 
     c=new TCanvas("gainEastB" ,"gainEastB" ,600,1000);
   else
     c=new TCanvas("gainWestB" ,"gainWestB" ,600,1000);
   c->Divide(2,8);
   for(int i=1;i<=15;i++) {
     c->cd(i);
     int cr=i;
     if(iEW)cr+=15; 
     TH1F *h=getSlice(cr);
     h->Draw(); gPad->SetLogy();
     h->SetAxisRange(0,2000); h->SetTitleSize(0.4);
     float adc1=findFraction(h,0.06);
     hg->Fill(cr,adc1);

   } // over 15 crates
 } // over East/West
 c=new TCanvas();
 hg->Draw();
 hg->Fit("pol0");

 h2D->Rebin2D(2,1);

}


//======================
TH1F * getSlice(  int crateID){
  TAxis* axX=h2D->GetXaxis();
  Taxis* axY=h2D->GetYaxis();
  float x1=axX->GetXmin();
  float x2=axX->GetXmax();
  int nbX=axX->GetNbins();
  char tit1[100];

  sprintf(tit1,"CR=%d; %s",crateID,axX->GetTitle());

  TH1F*h=new TH1F(tit1,tit1,nbX,x1,x2);
  h->SetLineColor(kBlue);  h->SetFillColor(18);
  h->GetXaxis()->SetLabelSize(0.09);
  h->SetTitleSize(0.4);

  // copy contecnt of 2D histo to 1D
  for(int i=1;i<=nbX;i++) h->SetBinContent(i,h2D->GetBinContent(i,crateID));
  h->SetEntries(h->Integral());
  return h;
}


//======================
float findFraction( TH1F * h, float frac){
  TAxis* axX=h->GetXaxis();
  int nbX=axX->GetNbins();
  float total=h->Integral();
  float thres=total*(1-frac);
  float sum=0;


  for(int i=1;i<=nbX;i++) {
    sum+=h->GetBinContent(i);
    if(sum< thres) continue;
    float adc1=h->GetBinCenter(i);
    printf(" got sum=%.0f i=%d  adc=%.0f\n",sum,i,adc1);
    Lx=h->GetListOfFunctions();
    ln=new TLine(adc1,0,adc1,1.e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
    return adc1; 
  }
  retur -1;
}
