Double_t CH4(Float_t percentMethaneOut, Float_t barometricPressure) {
#if 0
  Double_t x = percentMethaneOut-(1.61096e-02+1.00800e-02*barometricPressure);//percentMethaneOut - 1.61096e-02+1.00800e-02*barometricPressure;
  Double_t dv = 5.53488e+00+1.53462e-01*x;
  //  cout << "percentMethaneOut " << percentMethaneOut << "\tbarometricPressure" << barometricPressure
  //       << "\tx " << x << "\tdv " <<  dv << endl; 
#endif
#if 0
  static const Double_t c[4] = {
     4.39258e+03,                            
    -1.30411e+03,                            
     1.29200e+02,                            
    -4.26593e+00};
#endif
  static const Double_t c[4] = {
     4.408867e+03, 
    -1.308944e+03, 
     1.296776e+02, 
    -4.281674e+00};

  Double_t x = 1e3*percentMethaneOut/barometricPressure;
  Double_t dv = c[3];
  for (Int_t i = 2; i >= 0; i--) dv = c[i] + x*dv;
  return dv; 
}
