#include <iomanip>
#include "TRVector.h"
#include "TRDiagMatrix.h"
#include "TError.h"
ClassImp(TRDiagMatrix);
//________________________________________________________________________________
TRDiagMatrix::TRDiagMatrix(Int_t nrows,Double_t a0, ...) : 
  TRArray(nrows), fNrows(nrows) {
  __VA_LIST__(a0);
}
//________________________________________________________________________________
TRDiagMatrix::TRDiagMatrix(const TRDiagMatrix& S,ETRMatrixCreatorsOp kop) {
  Int_t i;
  switch (kop) {
  case kInvertedPosDef:
  case kInverted:
  case kInvertedA:
    fNrows = S.GetNcols();
    Set(fNrows*(fNrows+1)/2);
    for (i = 0; i < fNrows;  i++) if (fArray[i] != 0) fArray[i] = 1./fArray[i];
    break;
  default:
    Error("TRDiagMatrix(ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
  }
}
//________________________________________________________________________________
Double_t TRDiagMatrix::Product(const TRVector& A,ETRMatrixCreatorsOp kop) {
  Int_t M, N; // N == 1
  Double_t Value = 0;
  Int_t i;
  switch (kop) { // 
  case kAxSxAT: //A[M,N]*D[N,N]*AT[M,N] => R[M,M]; 
  case kATxSxA: //BT[N,M]*S[N,N]*B[N,M] => R[M,M]; 
    M = A.GetNrows();
    N = GetNrows();
    assert(N == A.GetNcols() || M == N);
    for (i = 0; i < N; i++) Value += A[i]*fArray[i]*A[i];
    break;
    break;
  default:
    Error("TRDiagMatrix(ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
  }
  return Value;
}
//________________________________________________________________________________
ostream& operator<<(ostream& s,const TRDiagMatrix &target) {
  static const int width = 10;
  static const Double_t zero = 0;
  Int_t Nrows = target.GetNrows();
  const Double_t *Array = target.GetArray();
  s << "Diagonal Matrix Size \t[" 
    << Nrows << "," << Nrows << "]" << endl;
  if (Array) {
    s.setf(std::ios::fixed,std::ios::scientific);
    s.setf(std::ios::showpos);
    for (int i = 0; i< Nrows; i++) {
      for (int j = 0; j <= i; j++)
	if (i == j)
	  s << std::setw(width) << std::setprecision(width-3) << Array[i] << ":\t";
	else
	  s << std::setw(width) << std::setprecision(width-3) << zero << ":\t";
      s << endl;
    }
    s.unsetf(std::ios::showpos);
  }
  else s << " Empty";   
  return s;
}
//________________________________________________________________________________
void TRDiagMatrix::Print(Option_t *opt) const {if (opt) {}; cout << *this << endl;}
