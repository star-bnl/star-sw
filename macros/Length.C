Double_t Length(Double_t *x, Double_t *par) {
#if 0
    Double_t COEFF[7] = {-0.29414629E+00,-0.65181924E-15,-0.19702754E-09,
			 0.20519117E-01,-0.28383705E-03, 0.59486243E-12,
			 0.25575770E-07};
    Int_t IBASFT[7]   = {0, 7, 5, 1, 2, 6, 4};
    Double_t X = x[0];
    if (X <  15.) X = 15.;
    if (X > 140.) X = 140.;
    Double_t xx[8];
    Double_t FPARAM=0.;
    Double_t sirrf = 1.;
    xx[0] = 1.; 
    xx[1] = X;
    for (int J=2; J<8;J++) {
      double j = J;
      xx[J] = (2*j-1)/j*xx[J-1]*X-(j-1)/j*xx[J-2];
    }
    for (int i=0; i<7; i++) {
      int k = IBASFT[i];
      FPARAM += COEFF[i]*xx[k];
    }
    sirrf *= TMath::Exp(FPARAM);
    //    sirrf = FPARAM;
  return sirrf;
#else
  static Double_t coeff[9] = { 
//      4.66646e-01, -4.93579e-02,  2.27040e-03, -5.73637e-05,  
//      8.40239e-07, -7.04925e-09,  3.13373e-11, -5.71612e-14
    4.84187e-01, -3.87216e-02,  1.63463e-03, -3.91121e-05,
    5.52376e-07, -4.49661e-09,  1.94136e-11, -3.43526e-14
  };
  Double_t X = x[0];
  if (X <  20.5) X =  20.5;
  if (X > 120  ) X = 120;
  Double_t FPARAM = 0;
  for (int i = 8; i >= 0; i--) FPARAM = coeff[i] + X*FPARAM;
  return par[0]+FPARAM-7.01522e-02;
#endif
}
