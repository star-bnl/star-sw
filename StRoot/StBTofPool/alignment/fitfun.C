Double_t fitfun(Double_t *x, Double_t *par)
{
  Double_t y = (TMath::Abs(x[0]-par[0])-par[2])/par[3];
  return par[1]/(1.+TMath::Exp(y))+par[4];
}
