void GattiFWHM() {
  TF1 *f = new TF1("f","4*TMath::ATanH(1./TMath::Sqrt(2+x))/(TMath::Pi()*(1-0.5*TMath::Sqrt(x)));K3;FWHM",0,2);
  f->Draw();
}
Double_t K3F(Double_t *x, Double_t *par)  {
  // Calculation of K_3 coefficient for E.Gatti formula
  // It is used parametrization from Fig.2 from E.Mathieson, J.S.Gordon, 
  // "Cathode charge distributions in multiwire chambers", NIM 227 (1984) 277-282
  //  h2s - ratio Anode-Cathode gap to wire spacing
  //  a2s - ratio Anode wire radius to wire spacing 
  Double_t params[5] = {.1989337e-02, -.6901542e-04,  .8665786, 154.6177, -.6801630e-03};
  const Double_t anodeWireRadius = 1e-3;
  const Double_t anodeWirePitch  = 0.4;
  const Double_t innerSectorPadWidth = 0.285;
  const Double_t innerSectorPadPitch = 0.335;
  const Double_t innerSectorPadLength = 1.15;
  const Double_t outerSectorPadWidth = 0.620;
  const Double_t outerSectorPadPitch = 0.675;
  const Double_t outerSectorPadLength = 1.95;
  Double_t a = anodeWireRadius; // a = Anode wire radius
  Double_t s = anodeWirePitch;  // s = wire spacing
  Double_t h = 0.2;// gStTpcDb->WirePlaneGeometry()->innerSectorAnodeWirePadPlaneSeparation(); // h = Anode-Cathode gap 
  //  Double_t a2s = a/s;
  Double_t a2s = par[0];
  //  Double_t h2s = h/s;
  Double_t h2s = x[0];
  Double_t K3 = (params[0]/ h2s + params[1])*(params[2]/a2s + params[3] + params[4]/(a2s*a2s));
  //  cout << "a2s\t" << a2s << "\th2s\t" << h2s << "\tK3\t" << K3 << endl;
#if 0
  h = 0.4; //gStTpcDb->WirePlaneGeometry()->outerSectorAnodeWirePadPlaneSeparation(); // h = Anode-Cathode gap 
  h2s = h/s;
  K3OP = (params[0]/ h2s + params[1])*(params[2]/a2s + params[3] + params[4]/(a2s*a2s));
#endif
  return K3;
}
void K3() {
  TF1 *k3 = new TF1("k3",K3F,2e-1,4,1);
  k3->SetParameter(0,2.5e-3);
  k3->Draw();
}
