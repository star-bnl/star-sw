Double_t Voltage(Double_t *xy, Double_t *pars) {
  Double_t x = xy[0];
  Double_t y = xy[1];
  Double_t s = pars[0];
  Double_t sx = TMath::Pi()*x/s;
  Double_t sy = TMath::Pi()*y/s;
  Double_t cx = TMath::Sin(sx);
  cx *= cx;
  Double_t cy = TMath::SinH(sy);
  cy *= cy;
  Double_t V = 0;
  if (cx + cy > 0) V = TMath::Log(4*(cx + cy));
  //  printf("x %f y %f V %f\n",x,y,V);
  return V;
}
