TGraph *gr = 0;
void laserPositions() {
  Double_t offset = -178.3;
  Double_t y[14] = {-172.505,-147.916,-113.402,-84.066, -57.136, -26.868,  7.346,
		    174.547, 142.156, 114.160,999.999,  49.760,  24.273, -6.576};
  Double_t x[7] = {  -0.947,  26.752,  57.808, 87.667, 118.674, 146.232, 178.3};
  Double_t X[14];
  Double_t Y[14];
  Int_t N = 0;
  for (Int_t i = 0; i < 7; i++) {
    for (Int_t j = 0; j < 2; j++) {
      Int_t ij = 7*j + i;
      if (y[ij] > 999.0) continue;
      Y[N] = y[ij];
      if (j == 0) Y[N] -= (6.576 + 0.5);
      else        Y[N] += (7.346 - 0.5);
      if (j == 0) {
	X[N] = x[i] + offset;
      } else {
	X[N] = - x[i] - offset;
      }
      //      if (j == 0) X[N] += 6.576;
      //      else        X[N] -= 7.346;
      Y[N] -= X[N];
      cout << N << "\tX = " << X[N] << "\tdelta = " << Y[N] << endl;
      N++;
    }
  }
  gr = new TGraph(N,X,Y);
  gr->SetMarkerStyle(20);
  gr->Draw("axp");
}
