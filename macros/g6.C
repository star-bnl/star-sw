static const Int_t NpGaus = 18;  // hist108
static const Double_t parGaus[2][NpGaus] = {
  {2.57760e-02, 1.44032e+00, 2.45484e+00, // inner  FCN=827.986
   7.05363e-02,-7.36314e-01, 1.12509e+00,
   2.27172e-02, 1.76234e+00, 8.55844e-01,
   1.29267e-02, 2.48337e+00, 9.63194e-01,
   2.33560e-01, 1.12666e-01, 9.40526e-01,
   3.68642e-03,-2.55756e+00, 1.28595e+00},
  {3.98911e-01, 3.62377e-02, 1.39581e+00, // outer FCN=1299.15 
   1.85302e-01,-7.82685e-02, 8.77156e-01,
   8.66260e-03, 2.37183e+00, 1.06643e+00,
   1.08442e-03, 3.07282e+00, 2.79578e+00,
  -2.64447e-01,-8.55225e-02, 1.40087e+00,
   1.70728e-02, 2.29372e+00, 2.21135e+00}
};
Double_t g6(Double_t *x, Double_t *par) {
  Double_t val = 0;
  Int_t l  = par[1];
  if (l < 0) l = 0;
  if (l > 1) l = 1;
  //  Double_t xx = (x[0] - par[1]);//*par[2];
  Double_t xx = x[0];//*par[2];
  for (Int_t j=0; j<NpGaus; j+=3){
    Double_t dev = (xx - parGaus[l][j+1])/parGaus[l][j+2];
    //      printf ("dev = %f\n",dev);
    val += parGaus[l][j]*exp(-0.5*dev*dev);
  }
  //  printf("x: %f l%i val:%f\n",xx,l,val);
  return par[0]*val;
  //  return val;
}
