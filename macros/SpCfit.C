//_______________________________________________________________________________
Double_t LogLandau(Double_t *x, Double_t *par)
{
   Double_t yy =x[0];
   Double_t xx = TMath::Exp(yy); 
   Double_t f = xx*TMath::Landau(xx,TMath::Exp(par[0]),par[1]);
   return f;
}
//________________________________________________________________________________
void Func()
{
   TF1 *f1 = new TF1("Func",LogLandau,-3,7,2);
   f1->SetParameters(0,1);
   f1->SetParNames("mean","sigma");
   f1->Draw();
}
//________________________________________________________________________________
void SpCfit(TH2F *hist=SpC, Int_t i1=1, Int_t i2=2)
{
  Func();
  TH1D *h1 = hist->ProjectionY("bin",i1,i2);
  //  TF1 *f1=gROOT->GetFunction("g");
  //  f1->SetParameters(-4,1);
  h1.Fit("g","R");
}
