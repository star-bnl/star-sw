#ifndef __CINT__
#include <Rtypes.h> // needed for Double_t etc
#endif

//
// Static data variables
//
static Int_t    gINVariables    = 2;
static Int_t    gINCoefficients = 6;
static Double_t gIDMean         = -0.191891;
// Assignment to minimum vector.
static Double_t gIXMin[] = {
  5e-07, 1e-06 };

// Assignment to maximum vector.
static Double_t gIXMax[] = {
  0.0001995, 0.000353 };

// Assignment to coefficients vector.
static Double_t gICoefficient[] = {
  0.0242257,
  0.105702,
  0.306149,
  -0.0305344,
  0.27914,
  0.327843
 };

// Assignment to powers vector.
// The powers are stored row-wise, that is
//  p_ij = gPower[i * NVariables + j];
static Int_t    gIPower[] = {
  1,  1,
  1,  2,
  2,  1,
  3,  1,
  1,  3,
  2,  2
};

// 
// The function   Double_t II3padI(Double_t *x)
// 
Double_t II3padI(Double_t *x) {
  Double_t Value = gIDMean;
  Int_t i,j,k;
  for (i = 0; i < gINCoefficients ; i++) {
    // Evaluate the ith term in the expansion
    Double_t term = gICoefficient[i];
    for (j = 0; j < gINVariables; j++) {
      // Evaluate the polynomial in the jth variable.
      Int_t power = gIPower[gINVariables * i + j]; 
      Double_t p1 = 1, p2 = 0, p3 = 0, r = 0;
      Double_t v =  1 + 2. / (gIXMax[j] - gIXMin[j]) * (x[j] - gIXMax[j]);
      // what is the power to use!
      switch(power) {
      case 1: r = 1; break; 
      case 2: r = v; break; 
      default: 
        p2 = v; 
        for (k = 3; k <= power; k++) { 
          p3 = p2 * v;
          p1 = p2; p2 = p3; 
        }
        r = p3;
      }
      // multiply this term by the poly in the jth var
      term *= r; 
    }
    // Add this term to the final result
    Value += term;
  }
  return Value;
}
//
// Static data variables
//
static Int_t    gONVariables    = 2;
static Int_t    gONCoefficients = 6;
static Double_t gODMean         = -0.104529;
// Assignment to minimum vector.
static Double_t gOXMin[] = {
  5e-07, 1e-06 };

// Assignment to maximum vector.
static Double_t gOXMax[] = {
  0.0001995, 0.000399 };

// Assignment to coefficients vector.
static Double_t gOCoefficient[] = {
  -0.0248125,
  -0.129327,
  0.00883517,
  0.00717224,
  -0.00352795,
  -0.00171865
 };

// Assignment to powers vector.
// The powers are stored row-wise, that is
//  p_ij = gPower[i * NVariables + j];
static Int_t    gOPower[] = {
  1,  1,
  1,  2,
  2,  1,
  2,  2,
  1,  3,
  3,  1
};

// 
// The function   Double_t II3padO(Double_t *x)
// 
Double_t II3padO(Double_t *x) {
  Double_t Value = gODMean;
  Int_t i,j,k;
  for (i = 0; i < gONCoefficients ; i++) {
    // Evaluate the ith term in the expansion
    Double_t term = gOCoefficient[i];
    for (j = 0; j < gONVariables; j++) {
      // Evaluate the polynomial in the jth variable.
      Int_t power = gOPower[gONVariables * i + j]; 
      Double_t p1 = 1, p2 = 0, p3 = 0, r = 0;
      Double_t v =  1 + 2. / (gOXMax[j] - gOXMin[j]) * (x[j] - gOXMax[j]);
      // what is the power to use!
      switch(power) {
      case 1: r = 1; break; 
      case 2: r = v; break; 
      default: 
        p2 = v; 
        for (k = 3; k <= power; k++) { 
          p3 = p2 * v;
          p1 = p2; p2 = p3; 
        }
        r = p3;
      }
      // multiply this term by the poly in the jth var
      term *= r; 
    }
    // Add this term to the final result
    Value += term;
  }
  return Value;
}
Double_t II3padC(Double_t x, Double_t y, Int_t IO){
  Double_t xx[2];
  xx[0] = x;
  xx[1] = y;
  if (!IO) return II3padI(xx);
  else     return II3padO(xx);
}
