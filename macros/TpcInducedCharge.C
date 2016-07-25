void InducedCharge(Double_t s = 0.4, Double_t h = 0.4, Double_t ra = 1e-3, Double_t Va = 1359) {
  // Calculate variation of induced charge due to different arrived angles 
  // alpha = -26 and -70 degrees
  cout << "wire spacing = " << s << " cm"
       << "\tcathode anode gap = " << h << " cm"
       << "\tanode wire radius = " << ra << " cm"
       << "\tpotential on anode wire = " << Va << " V" << endl;
  const Double_t B  = 30e-3; // 1/V
  const Double_t E0 = 20e3; // V/cm
  const Double_t mu = 2.26; // cm**2/V/sec CH4+ mobility 
  // const Double_t mu = 1.87; // cm**2/V/sec Ar+ mobility 
  Double_t alpha[2] = {-26., -70.};
  Double_t pi = TMath::Pi();
  Double_t rc = s/(2*pi)*TMath::Exp(pi*h/s); cout << "rc = " << rc << " cm" << endl;
  Double_t C  = 1./(2*TMath::Log(rc/ra)); cout << "C = " << C << endl;
  Double_t E  = 2*pi*C*Va/s; cout << "E = " << E << " V/cm" << endl;
  // Gain variation: M = M0*(1 - k*cos(2*alpha))
  Double_t k = 2*B/3.*TMath::Power((pi/E0/s),2)*TMath::Power(C*Va,3); cout << "k = " << k << endl;
  // Induced charge variation
  Double_t t0 = ra*ra/(4*mu*C*Va); cout << "t0 = " << 1e9*t0 << " ns" << endl;
  Double_t Tav = t0*h/s/(2*pi*C);  cout << "Tav = " << 1e9*Tav << " ns" << endl;
  //  Double_t t = 5*55e-9;             cout << "t = " << 1e9*t << " ns" << endl;
  Double_t t = 180e-9;             cout << "t = " << 1e9*t << " ns" << endl;
  Double_t rp = TMath::Sqrt(1. + t/t0); cout << "r' = " << rp << endl;
  // qc = rp*ra*sin(alpha)/(2*h) + C/2*log(1 + t/t0) = A*sin(alpha) + B
  Double_t A = rp*ra/(2*h);        cout << "A = " << A << endl;
  Double_t B = C/2*TMath::Log(1 + t/t0); cout << "B = " << B << endl;
  Double_t Gains[2];
  for (Int_t i = 0; i < 2; i++) {
    Gains[i] = A*TMath::Sin(pi/180*alpha[i]) + B; cout << "Gain = " << Gains[i] << " at alpha = " << alpha[i] << " degree" << endl;
  }
  Double_t GainsAv = 0.5*(Gains[0] + Gains[1]);
  for (Int_t i = 0; i < 2; i++) {
    Double_t r = Gains[i]/GainsAv - 1; cout << "Relative gain " << r << " at alpha = " << alpha[i] << endl;
  }
}
void TpcInducedCharge() {
  cout << "Outer Sector ======================" << endl;
  InducedCharge(0.4,0.4,1e-3,1359);
  cout << "Inner Sector ======================" << endl;
  InducedCharge(0.4,0.2,1e-3,1129);
}
