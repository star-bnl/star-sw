class Bichsel;
Bichsel *gBichsel = 0;
//____________________________________
Double_t bdEdx(Double_t *xx, Double_t *par) {
  Double_t zz = TMath::Log(xx[0]);
  Double_t x = par[0]; // log10bg
  Double_t y = par[1]; // log2dx
  Double_t zprob = gBichsel->GetMostProbableZ(x,y);//-5.07402529167365057e-01;
  Double_t sigma = gBichsel->GetRmsZ(x,y);
  Double_t z = (zz - zprob)/sigma;
  return gBichsel->GetProbability(x,y,z)/xx[0]/sigma;
}
void Draper() {
  if (! gBichsel) {
    gSystem->Load("StBichsel.so");
    gBichsel = Bichsel::Instance();
  }
  TF1 *Bichsel = new TF1("Bichsel",bdEdx,0,10,2);
  Bichsel->SetParameters(1.,0.);
  Bichsel->Draw();
}
