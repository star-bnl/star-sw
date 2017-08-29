
#include <cmath>
#include "Math/SVector.h"
#include "Math/SMatrix.h"

#include <iostream>
#include <vector>
#include <string>
#include <limits>
#include "TMath.h"
#include "TString.h"
#include "TGeoMatrix.h"
using namespace ROOT::Math;

using std::cout;
using std::endl;


//_____________________________________________________________________________
Double_t IsOrtogonal(const Double_t *r) {
// Perform orthogonality test for rotation.
  Double_t cmax = 0;
  Double_t cij;
  for (Int_t i=0; i<2; i++) {
    for (Int_t j=i+1; j<3; j++) {
      // check columns
      cij = TMath::Abs(r[i]*r[j]+r[i+3]*r[j+3]+r[i+6]*r[j+6]);
      if (cij>1E-4) cmax = cij;
      // check rows
      cij = TMath::Abs(r[3*i]*r[3*j]+r[3*i+1]*r[3*j+1]+r[3*i+2]*r[3*j+2]);
      if (cij>cmax) cmax = cij;
    }
  }
  return cmax;   
}
//________________________________________________________________________________
void SMatrixTest() {
  Double_t r[9] = {-0.996140,   -0.083523,   -0.001228, 
		    0.083647,   -0.995945,    0.003832,
		   -0.001499  ,  0.003442 ,   0.999633};
  TGeoRotation R; R.SetMatrix(r); R.Print();
  cout << "Determinant = " << R.Determinant() << endl;
  cout << "Ortogonality " << IsOrtogonal(r) << endl;
  if ( TMath::Abs(R.Determinant() - 1) < 1e-14 && IsOrtogonal(r) < 1e-14) return;
  SMatrix<double,3,3> A(r,9); // A.Print();
  cout << "A: " << endl << A << endl;
  SMatrix<double,3,3> B = A;
  double det = 0.;
  A.Det(det);
  cout << "Determinant - 1: " << det-1 << endl;
  cout << "A again: " << endl << A << endl;
  A = B;
  A.Invert();
  cout << "A^-1: " << endl << A << endl;
  // check if this is really the inverse:
  cout << "A^-1 * B: " << endl << A * B << endl;
  // the Babylonian method for extracting the square root of a matrix :  Q_{n+1} = 2 * M * ((Q_{n}^{-1} * M) + (M^{T} *Q_{n}))^{-1}
  
  SMatrix<double,3,3> Qn1;
  SMatrix<double,3,3> Qn2;
  SMatrix<double,3,3> M = B;
  SMatrix<double,3,3> Qn = M;
  
  Int_t ifail = 0;
  Int_t N = 0;
  Qn.Det(det); cout << "N " << N << "\tQn Determinant - 1: " << Form("%15.5g",det-1) << endl;
  Qn = M;
  while (TMath::Abs(TMath::Abs(det) - 1) > 1e-14) {
    SMatrix<double,3> QnInv = Qn.Inverse(ifail);
    if (ifail) {
      cout << "Qn inversion failed" << endl;
      break;
    }
    SMatrix<double,3,3> C1 = QnInv * M;
    SMatrix<double,3,3> C2 = Transpose(M) * Qn;
    SMatrix<double,3,3> C  = C1 + C2;
    SMatrix<double,3,3> CInv = C.Inverse(ifail);
    if (ifail) {
      cout << "C inversion failed" << endl;
      break;
    }
    Qn1 = 2 * M * CInv; 
    Qn2 = Qn1;
    Qn2.Det(det);  cout << "N " << N << "\tQn2 Determinant - 1: " << Form("%15.5g",det-1) << endl;
    N++;
    if (N > 13) break;
    Qn = Qn1;
    cout << "Qn:" << endl << Qn << endl;
  }
  R.SetMatrix(Qn.Array()); R.Print();
  const Double_t *rr = R.GetRotationMatrix();
  cout << "Determinant = " << R.Determinant() << endl;
  cout << "Ortogonality " << IsOrtogonal(rr) << endl;
}
