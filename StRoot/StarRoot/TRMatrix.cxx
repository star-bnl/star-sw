#include <iomanip>
#include "TRMatrix.h"
#include "TMath.h"
#include "TError.h"
#include "TString.h"
ClassImp(TRMatrix);
//________________________________________________________________________________
TRMatrix::TRMatrix(const TRMatrix &S, Int_t NI, Int_t NJ, Int_t I, Int_t J){
  if (NI == 0) NI = S.NI();
  if (NJ == 0) NJ = S.NJ();
  if (NI > S.NI()) NI = S.NI();
  if (NJ > S.NJ()) NJ = S.NJ();
  if (I == 0) {::Error("TRMatrix::TRMatrix(const TRMatrix &)", "index i %d out of bounds (size: %d, this: %p)", 
		       I, S.NI(), this); I = 1;}
  if (J == 0) {::Error("TRMatrix::TRMatrix(const TRMatrix &)", "index j %d out of bounds (size: %d, this: %p)", 
		       J, S.NJ(), this); J = 1;}
  if (I+NI-1 > S.NI()) {::Error("TRMatrix::TRMatrix(const TRMatrix &)", "index i %d out of bounds (size: %d, this: %p)", 
		       I+NI-1, S.NI(), this); I = 1;}
  if (J+NJ-1 > S.NJ()) {::Error("TRMatrix::TRMatrix(const TRMatrix &)", "index j %d out of bounds (size: %d, this: %p)", 
		       J+NJ-1, S.NJ(), this); J = 1;}
  fNrows = NI; 
  fNcols = NJ;
  Set(fNrows*fNcols);
  const Double_t *Array = S.GetArray();
  for (int i = 0; i < fNrows; i++) 
    for (int j = 0; j < fNcols; j++) 
      fArray[j + i*fNcols] = Array[J+j-1 + (I+i-1)*S.NJ()];
}
//________________________________________________________________________________
TRMatrix &TRMatrix::operator=(const TRMatrix &rhs) {
    if (this != &rhs) SetMatrix(rhs.GetNrows(),rhs.GetNcols(),rhs.GetArray());
    return *this;
  }
//________________________________________________________________________________
TRMatrix::TRMatrix(Int_t nrows,Int_t ncols,Double_t a0, ...) : 
  TRArray(nrows*ncols), fNrows(nrows), fNcols(ncols) {
  __VA_LIST__(a0);
}
//________________________________________________________________________________
void TRMatrix::SetMatrix(Int_t nrows,Int_t ncols,const  Double_t *array) {   
  fNrows = nrows; fNcols = ncols; Set(fNrows*fNcols,array);
}
//________________________________________________________________________________
TRMatrix::TRMatrix(ETRMatrixCreatorsOp kop,Int_t nrows):
TRArray(nrows*nrows), fNrows(nrows), fNcols(nrows) {
  switch (kop) {
  case kZero:
    break;
  case kUnit:
    for (int i=0; i<fNrows; i++) fArray[i+fNrows*i] = 1;
    break;
  default:
    Error("TRMatrix(ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
  }
}
//________________________________________________________________________________
TRMatrix::TRMatrix(const TRMatrix& A, ETRMatrixCreatorsOp kop) {
  switch (kop) {
  case kTransposed:
    fNrows = A.GetNcols();
    fNcols = A.GetNrows();
    Set(fNrows*fNcols);
    TCL::mxtrp(A.GetArray(),fArray,fNcols,fNrows);
    break;
  default:
    Error("TRMatrix(ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
  }
}
//________________________________________________________________________________
TRMatrix::TRMatrix(const TRMatrix& A, ETRMatrixCreatorsOp kop,const TRMatrix& B):  TRArray(0){
  Int_t NI, NJ, NK;
  switch (kop) {
  case kAxB:
    NI = A.GetNrows(); fNrows = NI;
    NJ = A.GetNcols();
    assert (NJ == B.GetNrows());
    NK = B.GetNcols(); fNcols = NK;
    Set(NI*NK);
    TCL::mxmpy(A.GetArray(),B.GetArray(),fArray,NI,NJ,NK);
    break;
  case kAxBT:
    NI = A.GetNrows(); fNrows = NI;
    NJ = A.GetNcols();
    assert (NJ == B.GetNcols());
    NK = B.GetNrows(); fNcols = NK;
    Set(NI*NK);
    TCL::mxmpy1(A.GetArray(),B.GetArray(),fArray,NI,NJ,NK);
    break;
  case kATxB:
    NI = A.GetNcols(); fNrows = NI;
    NJ = A.GetNrows();
    assert (NJ == B.GetNrows());
    NK = B.GetNcols(); fNcols = NK;
    Set(NI*NK);
    TCL::mxmpy2(A.GetArray(),B.GetArray(),fArray,NI,NJ,NK);
    break;
  case kATxBT:
    NI = A.GetNcols(); fNrows = NI;
    NJ = A.GetNrows();
    assert (NJ == B.GetNcols());
    NK = B.GetNrows(); fNcols = NK;
    Set(NI*NK);
    TCL::mxmpy3(A.GetArray(),B.GetArray(),fArray,NI,NJ,NK);
    break;
#if 1
  case kAxBxAT: //[NI,NJ]x[NJ,NJ]x[NI,NJ]T => [NI,NI]
    NI = A.GetNcols();
    NJ = A.GetNrows();
    assert (NJ == B.GetNcols());
    Set(NI*NI);
    TCL::mxmlrt(A.GetArray(),B.GetArray(),fArray,NI,NJ);
    break;
  case kATxBxA: //[NI,NJ]Tx[NI,NI]x[NI,NJ] => [NJ,NJ]
    NI = A.GetNcols(); 
    NJ = A.GetNrows(); fNrows = NJ; fNcols = NJ;
    assert (NI == B.GetNcols());
    Set(NJ*NJ);
    TCL::mxmltr(A.GetArray(),B.GetArray(),fArray,NJ,NI);
    break;
#endif
  default:
    Error("TRMatrix(ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
    
  }
}
//________________________________________________________________________________
void TRMatrix::Add(const TRMatrix& A, ETRMatrixCreatorsOp kop,const TRMatrix& B) {
  Int_t NI, NJ, NK;
  switch (kop) {
  case kAxB:
    NI = A.GetNrows();
    NJ = A.GetNcols();
    assert (NJ == B.GetNrows());
    NK = B.GetNcols(); 
    assert(NI == fNrows && NK == fNcols);
    TCL::mxmad(A.GetArray(),B.GetArray(),fArray,NI,NJ,NK);
    break;
  case kAxBT:
    NI = A.GetNrows();
    NJ = A.GetNcols();
    assert (NJ == B.GetNcols());
    NK = B.GetNrows(); 
    assert(NI == fNrows && NK == fNcols);
    TCL::mxmad1(A.GetArray(),B.GetArray(),fArray,NI,NJ,NK);
    break;
  case kATxB:
    NI = A.GetNcols();
    NJ = A.GetNrows();
    assert (NJ == B.GetNrows());
    NK = B.GetNcols(); 
    assert(NI == fNrows && NK == fNcols);
    TCL::mxmad2(A.GetArray(),B.GetArray(),fArray,NI,NJ,NK);
    break;
  case kATxBT:
    NI = A.GetNcols();
    NJ = A.GetNrows();
    assert (NJ == B.GetNcols());
    NK = B.GetNrows(); 
    assert(NI == fNrows && NK == fNcols);
    TCL::mxmad3(A.GetArray(),B.GetArray(),fArray,NI,NJ,NK);
    break;
    
  default:
    Error("TRMatrix(ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
  }
}
//________________________________________________________________________________
void TRMatrix::Substruct(const TRMatrix& A, ETRMatrixCreatorsOp kop,const TRMatrix& B) {
  Int_t NI, NJ, NK;
  switch (kop) {
  case kAxB:
    NI = A.GetNrows();
    NJ = A.GetNcols();
    assert (NJ == B.GetNrows());
    NK = B.GetNcols(); 
    assert(NI == fNrows && NK == fNcols);
    TCL::mxmub(A.GetArray(),B.GetArray(),fArray,NI,NJ,NK);
    break;
  case kAxBT:
    NI = A.GetNrows();
    NJ = A.GetNcols();
    assert (NJ == B.GetNcols());
    NK = B.GetNrows(); 
    assert(NI == fNrows && NK == fNcols);
    TCL::mxmub1(A.GetArray(),B.GetArray(),fArray,NI,NJ,NK);
    break;
  case kATxB:
    NI = A.GetNcols();
    NJ = A.GetNrows();
    assert (NJ == B.GetNrows());
    NK = B.GetNcols(); 
    assert(NI == fNrows && NK == fNcols);
    TCL::mxmub2(A.GetArray(),B.GetArray(),fArray,NI,NJ,NK);
    break;
  case kATxBT: //[NJ,NI]T x [NK,NJ]T => [NI,NK]
    NI = A.GetNcols();
    NJ = A.GetNrows();
    assert (NJ == B.GetNcols());
    NK = B.GetNrows(); 
    assert(NI == fNrows && NK == fNcols);
    TCL::mxmub3(A.GetArray(),B.GetArray(),fArray,NI,NJ,NK);
    break;
    
  default:
    Error("TRMatrix(ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
  }
}
//________________________________________________________________________________
TRMatrix::TRMatrix(const TRSymMatrix &S, ETRMatrixCreatorsOp kop,const TRMatrix& A){
  Int_t M,N; // A[M,N], B[N,M]], C[M,N], S[M,M], R[N,N]
  switch (kop) { 
  case kSxA: // S[M,M]*A[M,N] => C[M,N]
    M = A.GetNrows();
    N = A.GetNcols();
    assert(M == S.GetNrows());
    fNrows = M;
    fNcols = N;
    Set(fNrows*fNcols);
    TCL::trsa(S.GetArray(),A.GetArray(),fArray,M,N);
    break;
  case kSxAT: // S[M,M]*BT[N,M] => C[M,N]
    N = A.GetNrows();
    M = A.GetNcols();
    assert(M == S.GetNrows());
    fNrows = M;
    fNcols = N;
    Set(fNrows*fNcols);
    TCL::trsat(S.GetArray(),A.GetArray(),fArray,M,N);
    break;
  default:
    Error("TRMatrix SxA (ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
  }
}
//________________________________________________________________________________
TRMatrix::TRMatrix(const TRMatrix& A, ETRMatrixCreatorsOp kop,const TRSymMatrix &S){
  Int_t M,N; // A[M,N], B[N,M]], C[M,N], S[M,M], R[N,N]
  switch (kop) { 
  case kAxS: // A[M,N]*R[N,N] => C[M,N]
    M = A.GetNrows();
    N = A.GetNcols();
    assert(N == S.GetNrows());
    fNrows = M;
    fNcols = N;
    Set(fNrows*fNcols);
    TCL::tras(A.GetArray(),S.GetArray(),fArray,M,N);
    break;
  case kATxS: // BT[N,M]*R[N,N] => C[M,N]
    M = A.GetNcols();
    N = A.GetNrows();
    assert(N == S.GetNrows());
    fNrows = M;
    fNcols = N;
    Set(fNrows*fNcols);
    TCL::trats(A.GetArray(),S.GetArray(),fArray,M,N);
    break;
  default:
    Error("TRMatrix AxS (ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
  }
}
//________________________________________________________________________________
TRMatrix::TRMatrix(const TRSymMatrix &S) {
  Int_t M = S.GetNcols();
  fNrows = M;
  fNcols = M;
  Set(fNrows*fNcols);
  TCL::trupck(S.GetArray(),fArray,M);
}
//________________________________________________________________________________
std::ostream& operator<<(std::ostream& s,const TRMatrix &target) {
  Int_t Nrows = target.GetNrows();
  Int_t Ncols = target.GetNcols();
  const Double_t *Array = target.GetArray();
  s << "Rectangular Matrix Size \t[" << Nrows << "," << Ncols << "]" << std::endl;
  if (Array) { 
    s.setf(std::ios::fixed,std::ios::scientific);
    s.setf(std::ios::showpos);
    for (int i = 0; i< Nrows; i++) {
      Int_t Nzeros = 0;
      for (int j = Ncols-1; j >= 0; j--) if (Array[j + i*Ncols] == 0.0) {Nzeros++;} else break;
      if (Nzeros == 1) Nzeros = 0;
      for (int j = 0; j < Ncols-Nzeros; j++) s << Form("%10.3f",Array[j + i*Ncols]);
      if (Nzeros) s << Form("%8i*0",Nzeros);
      s << std::endl;
    }
    s.unsetf(std::ios::showpos);
  }
  else s << " Empty";
  return s;
}
//________________________________________________________________________________
void TRMatrix::Print(Option_t *opt) const {if (opt) {}; std::cout << *this << std::endl;}
