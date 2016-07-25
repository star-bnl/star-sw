// See TMultiDimFit class documentation for more information 
// 
//
// Static data variables
//
int    gNVariables    = 4;
int    gNCoefficients = 20;
double gDMean         = 0.131325;
// Assignment to mean vector.
double gXMean[] = {
  0.0451218, 0.284734, 0.181683, 0.0135319 };

// Assignment to minimum vector.
double gXMin[] = {
  -0.613105, -0.760204, 0, -0.453125 };

// Assignment to maximum vector.
double gXMax[] = {
  0.483055, 1.55741, 190, 0.453125 };

// Assignment to coefficients vector.
double gCoefficient[] = {
  0.0702396,
  -0.082846,
  0.0412671,
  -0.0211888,
  0.0224215,
  -0.0198677,
  -0.00304716,
  0.00514692,
  0.00343496,
  0.000435832,
  -0.00227305,
  0.00317144,
  -0.00315852,
  0.00553869,
  -0.00219481,
  0.00201784,
  -0.00831418,
  0.00848524,
  -0.00105014,
  -0.00245119
 };

// Assignment to error coefficients vector.
double gCoefficientRMS[] = {
  0.00188901,
  0.000772179,
  0.0026403,
  0.00213037,
  0.000619187,
  0.000783915,
  0.000683779,
  0.000255451,
  0.00308829,
  0.000201129,
  0.000371541,
  0.000131454,
  0.000384939,
  0.000658038,
  0.000367001,
  0.000154973,
  0.000964771,
  0.00118133,
  9.82577e-05,
  0.000156617
 };

// Assignment to powers vector.
// The powers are stored row-wise, that is
//  p_ij = gPower[i * NVariables + j];
int    gPower[] = {
  1,  1,  1,  1,
  1,  1,  2,  1,
  3,  1,  1,  1,
  1,  2,  1,  1,
  1,  1,  1,  3,
  1,  1,  2,  3,
  3,  2,  3,  1,
  2,  1,  2,  1,
  3,  2,  1,  1,
  1,  1,  4,  1,
  5,  1,  2,  1,
  1,  5,  2,  1,
  1,  3,  1,  1,
  1,  2,  3,  1,
  1,  3,  1,  3,
  1,  1,  5,  1,
  5,  1,  1,  1,
  5,  2,  1,  1,
  1,  1,  6,  1,
  1,  4,  3,  1
};

// 
// The function   double MDF(double *x)
// 
double MDF(double *x) {
  double returnValue = gDMean;
  int    i = 0, j = 0, k = 0;
  for (i = 0; i < gNCoefficients ; i++) {
    // Evaluate the ith term in the expansion
    double term = gCoefficient[i];
    for (j = 0; j < gNVariables; j++) {
      // Evaluate the polynomial in the jth variable.
      int power = gPower[gNVariables * i + j]; 
      double p1 = 1, p2 = 0, p3 = 0, r = 0;
      double v =  1 + 2. / (gXMax[j] - gXMin[j]) * (x[j] - gXMax[j]);
      // what is the power to use!
      switch(power) {
      case 1: r = 1; break; 
      case 2: r = v; break; 
      default: 
        p2 = v; 
        for (k = 3; k <= power; k++) { 
          p3 = p2 * v;
          p3 = 2 * v * p2 - p1; 
          p1 = p2; p2 = p3; 
        }
        r = p3;
      }
      // multiply this term by the poly in the jth var
      term *= r; 
    }
    // Add this term to the final result
    returnValue += term;
  }
  return returnValue;
}

// EOF for MDFSpX_s_1_r_1_N_3_B_1.C
Double_t func(Double_t *x, Double_t *p) {
  Double_t X[4] = {0,0,0,0};
  Int_t i = (Int_t) p[0];
  Int_t ix = i%10; 
  Int_t iy = i/10;
  if (ix > 0) X[ix-1] = x[0];
  if (iy > 0) X[iy-1] = x[1];
  return MDF(X);
}
//________________________________________________________________________________
TF1 *DrawMDF(Int_t ix = 1, Int_t iy = 2) {
  TF1 *f = 0;
  if (ix > 0 && iy > 0) {
    f = new TF2(Form("f%i%i",ix,iy),func,gXMin[ix-1],gXMax[ix-1],gXMin[iy-1],gXMax[iy-1],1);
  } else {
    if (ix > 0) f = new TF1(Form("f%i%i",ix,iy),func,gXMin[ix-1],gXMax[ix-1],1);
    if (iy > 0) f = new TF1(Form("f%i%i",ix,iy),func,gXMin[iy-1],gXMax[iy-1],1);
  }
  if (f) {
    f->SetParameter(0,10*ix+iy);
    f->Draw("colz");
  }
  return f;
}
