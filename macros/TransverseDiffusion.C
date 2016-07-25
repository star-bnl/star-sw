TGraph *gr[4] = {0,0,0,0};
void TransverseDiffusion() {
  /* http://www.star.bnl.gov/public/tpc/tpc.html
    .                E (V/cm)
    B (kG)	 50	 100	150	 200
    0.0	        534	 542	552	 518
    1.0	        410	 466	479	 473
    2.0	        320	 369	385	 402
    3.0	        259	 289	326	 344
    4.0	        225	 254	296	 315
    5.0	        209	 233	263	 279 */
  Double_t E[4] = {50,	 100,	150,	 200};
  Double_t B[6] = { 0., 1., 2., 3., 4., 5.};
  Double_t TrasDiff[6][5] = {
    {534,	 542,	552,	 518},
    {410,	 466,	479,	 473},
    {320,	 369,	385,	 402},
    {259,	 289,	326,	 344},
    {225,	 254,	296,	 315},
    {209,	 233,	263,	 279}
  };
  /* root.exe [23] gr[2]->Fit("pol2")
Fitting results:
Parameters:
NO.             VALUE           ERROR
0       5.572857e+02    9.063270e-01
1      -9.937143e+01   8.525173e-01
2       8.142857e+00    1.636634e-01
  */
  for (Int_t k = 0; k < 4; k++) {
    Double_t y[6] = {TrasDiff[0][k], TrasDiff[1][k], TrasDiff[2][k], 
		     TrasDiff[3][k], TrasDiff[4][k], TrasDiff[5][k]};
    for (Int_t i = 1; i < 6; i++) y[i] /= y[0];
    y[0] = 1;
    gr[k] = new TGraph(6,B,y);
    gr[k]->SetMarkerStyle(20);
    gr[k]->SetMarkerColor(k+1);
  }
}
