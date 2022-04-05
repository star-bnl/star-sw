#include <iomanip>
#include "TMath.h"
#include "TRVector.h"
#include "TRSymMatrix.h"
#include "TRDiagMatrix.h"
#include "TError.h"
ClassImp(TRSymMatrix);
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(Int_t nrows,Double_t a0, ...) : 
  TRArray(nrows*(nrows+1)/2), fNrows(nrows) {
  __VA_LIST__(a0);
}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(Int_t nrows,const Double_t *Array) : TRArray() {
  fNrows = TMath::Abs(nrows);
  if (nrows > 0) Set(fNrows*(fNrows+1)/2,Array);
  else {// Upper to Lower
    Set(fNrows*(fNrows+1)/2);
    Int_t ij;
    Int_t i = 0, j = 0;
    for (Int_t l = 0; l < fN; l++) {
      ij = i*(i+1)/2 + j;
      fArray[ij] = Array[l];
      if (i < fNrows - 1) i++;
      else {
	j++;
	i = j;
      }
    }
  }
}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(Int_t nrows,const Float_t *Array) : TRArray() {
  fNrows = TMath::Abs(nrows);
  if (nrows > 0) {
    Set(fNrows*(fNrows+1)/2);
    TCL::ucopy(Array,fArray,fNrows*(fNrows+1)/2);
  }
  else {// Upper to Lower
    Set(fNrows*(fNrows+1)/2);
    Int_t ij;
    Int_t i = 0, j = 0;
    for (Int_t l = 0; l < fN; l++) {
      ij = i*(i+1)/2 + j;
      fArray[ij] = Array[l];
      if (i < fNrows - 1) i++;
      else {
	j++;
	i = j;
      }
    }
  }
}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(Int_t nrows,const Char_t *s) : TRArray(nrows*(nrows+1)/2,s), fNrows(nrows) {}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(ETRMatrixCreatorsOp kop,Int_t nrows) :
  TRArray(nrows*(nrows+1)/2), fNrows(nrows){
 switch (kop) {
  case kZero:
    break;
  case kUnit:
    for (int i=0; i<fNrows; i++) fArray[i*(i+1)/2+i] = 1;
    break;
  default:
    Error("TRSymMatrix(ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
  }
  
}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(const TRSymMatrix& S,ETRMatrixCreatorsOp kop) {
  switch (kop) {
  case kInvertedPosDef:
    SpmInv(S);
    break;
  case kInverted:
    fNrows = S.GetNcols();
    Set(fNrows*(fNrows+1)/2);
    TCL::trsinv(S.GetArray(),fArray, fNrows);
    break;
  case kInvertedA:
    fNrows = S.GetNcols();
    Set(fNrows*(fNrows+1)/2);
    fValid = ! TrsInv(S.GetArray(),fArray, fNrows);
    break;
  default:
    Error("TRSymMatrix(ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
  }
}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(const TRMatrix& A) {
  Int_t NI = A.GetNrows(); fNrows = NI;
  Int_t NJ = A.GetNcols();
  assert(NI == NJ);
  Set(fNrows*(fNrows+1)/2);
  TCL::trpck(A.GetArray(),fArray,NI);
}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(const TRMatrix& A,ETRMatrixCreatorsOp kop,const TRSymMatrix& S) {
  Int_t M, N;
  switch (kop) { // 
  case kAxSxAT: //A[M,N]*S[N,N]*AT[M,N] => R[M,M]; 
    M = A.GetNrows();
    N = S.GetNrows();
    assert(N == A.GetNcols());
    fNrows = M;
    Set(fNrows*(fNrows+1)/2);
    TCL::trasat(A.GetArray(),S.GetArray(),fArray,M,N);
    break;
  case kATxSxA: //BT[N,M]*S[N,N]*B[N,M] => R[M,M]; 
    M = A.GetNcols();
    N = S.GetNrows();
    assert(N == A.GetNrows());
    fNrows = M;
    Set(fNrows*(fNrows+1)/2);
    TCL::tratsa(A.GetArray(),S.GetArray(),fArray,M,N);
    break;
  default:
    Error("TRSymMatrix(ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
  }
}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(const TRSymMatrix& Q,ETRMatrixCreatorsOp kop,const TRSymMatrix& T){
  assert (kop == kRxSxR);
  Int_t M = Q.GetNcols();
  assert(M == T.GetNcols());
  fNrows = M;
  Set(fNrows*(fNrows+1)/2);
  TCL::trqsq(Q.GetArray(),T.GetArray(),fArray,M);
}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(const TRMatrix& A,ETRMatrixCreatorsOp kop) {
  Int_t M, N;
  switch (kop) {
  case kAxAT: // A[M,N]*AT[M,N] => S[M,M]
    M = A.GetNrows();    
    N = A.GetNcols();    
    fNrows = M;
    Set(fNrows*(fNrows+1)/2);
    TCL::traat(A.GetArray(),fArray,M,N);
    break;
  case kATxA: // AT[N,M]*A[N,M] => S[M,M]
    N = A.GetNrows();    
    M = A.GetNcols();    
    fNrows = M;
    Set(fNrows*(fNrows+1)/2);
    TCL::trata(A.GetArray(),fArray,M,N);
    break;
  default:
    Error("TRSymMatrix(ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
  }
}
//________________________________________________________________________________
Double_t TRSymMatrix::Product(const TRVector& A,ETRMatrixCreatorsOp /* kop */) {
  Int_t M, N; // N == 1
  Double_t Value;
  //  case kAxSxAT: //A[M,N]*S[N,N]*AT[M,N] => R[M,M]; 
  //  case kATxSxA: //BT[N,M]*S[N,N]*B[N,M] => R[M,M]; 
  M = A.GetNcols();
  N = GetNrows();
  assert(N == A.GetNrows());
  TCL::tratsa(A.GetArray(),GetArray(),&Value,M,N);
  return Value;
}
//________________________________________________________________________________
ostream& operator<<(ostream& s,const TRSymMatrix &target) {
  static const int width = 10;
  Int_t Nrows = target.GetNrows();
  const Double_t *Array = target.GetArray();
  s << "Semi Positive DefinedSymMatrix Size \t[" 
    << Nrows << "," << Nrows << "]" << endl;
  if (Array) {
    s.setf(std::ios::fixed,std::ios::scientific);
    s.setf(std::ios::showpos);
    for (int i = 0; i< Nrows; i++) {
      for (int j = 0; j <= i; j++)
	s << std::setw(width) << std::setprecision(width-3) << Array[i*(i+1)/2 + j] << ":\t";
      s << endl;
    }
    s.unsetf(std::ios::showpos);
  }
  else s << " Empty";   
  return s;
}
//________________________________________________________________________________
void TRSymMatrix::Print(Option_t *opt) const {if (opt) {}; cout << *this << endl;}
//________________________________________________________________________________
Int_t TRSymMatrix::SpmInv(const TRSymMatrix &S, TRVector *B) {
  if (&S != this) *this = S;
  Double_t *diag = new Double_t[fNrows];
  Bool_t   *flag = new Bool_t[fNrows];
  Double_t *b = 0;
  if (B) b = B->GetArray();
  Int_t nrank = 0;
  spminv(fArray, b, fNrows, nrank, diag, flag);
  delete [] diag;
  delete [] flag;
  return nrank;
}
//________________________________________________________________________________
Int_t TRSymMatrix::spminv(Double_t *v, Double_t *b, Int_t n, 
			  Int_t &nrank, Double_t *diag, Bool_t *flag) {
  // taken from Millepede (V.Blobel)
  /* Local variables */
  static Int_t i, j, k, l, jj, jk, kk, jl, lk;
  static Double_t vjk, vkk;
  
  /*     obtain solution of a system of linear equations with symmetric */
  /*     matrix and the inverse. */
  
  /*                    - - - */
  /*        CALL SPMINV(V,B,N,NRANK,...,...)      solve  V * X = B */
  /*                    - -   ----- */
  
  /*           V = symmetric N-by-N matrix in symmetric storage mode */
  /*               V(1) = V11, V(2) = V12, V(3) = V22, V(4) = V13, . . . */
  /*               replaced by inverse matrix */
  /*           B = N-vector, replaced by solution vector */
  
  /*     DIAG(N) =  double precision scratch array */
  /*     FLAG(N) =  Bool_t scratch array */
  
  /*     Method of solution is by elimination selecting the  pivot  on  the */
  /*     diagonal each stage. The rank of the matrix is returned in  NRANK. */
  /*     For NRANK ne N, all remaining  rows  and  cols  of  the  resulting */
  /*     matrix V and the corresponding elements of  B  are  set  to  zero. */
  
  /*     ... */
  /* Parameter adjustments */
  --v;
  --diag;
  if (b) --b;
  --flag;
  
  /* Function Body */
  for (i = 1; i <= n; ++i) {
    flag[i] = kTRUE; /* reset flags */
    diag[i] = TMath::Abs(v[(i * i + i) / 2]); /* save TMath::Abs of diagonal elements */
  }
  nrank = 0;
  for (i = 1; i <= n; ++i) {/* start of loop */
    k = jj = kk = 0;
    vkk = 0.;
    for (j = 1; j <= n; ++j) {/* search for pivot */
      jj += j;
      if (flag[j]) {/* not used so far  Computing MAX */
	if (TMath::Abs(v[jj]) > TMath::Max(TMath::Abs(vkk),diag[j] * 1e-10)) {
	  vkk = v[jj]; /* pivot (candidate) */
	  k = j;       /* index of pivot */
	  kk = jj;     /* index of diagonal element */
	}
      }
    }
    if (k != 0) {            /* pivot found */
      ++nrank;               /* increase rank and ... */
      flag[k] = kFALSE;      /* ... reset flag */
      vkk = 1. / vkk;
      v[kk] = -vkk;
      if (b) b[k] *= vkk;
      jk = kk - k;
      jl = 0;
      for (j = 1; j <= n; ++j) {/* elimination */
	if (j == k) {
	  jk = kk;
	  jl += j;
	} else {
	  if (j < k) {
	    ++jk;
	  } else {
	    jk = jk + j - 1;
	  }
	  vjk = v[jk];
	  v[jk] = vkk * vjk;
	  if (b) b[j] -= b[k] * vjk;
	  lk = kk - k;
	  for (l = 1; l <= j; ++l) {
	    ++jl;
	    if (l == k) {
	      lk = kk;
	    } else {
	      if (l < k) {
		++lk;
	      } else {
		lk = lk + l - 1;
	      }
	      v[jl] -= v[lk] * vjk;
	    }
	  }
	}
      }
    } else {
      for (k = 1; k <= n; ++k) {
	if (flag[k]) {
	  if (b) b[k] = 0.; /* clear vector element */
	  for (j = 1; j <= k; ++j) {
	    if (flag[j]) {
	      v[(k * k - k) / 2 + j] = 0.;
	    } /* clear matrix row/col */
	  }
	}
      }
      goto L10;
    }
  } /* end of loop */
 L10:  Int_t nn = (n * n + n) / 2;
    for (Int_t ij = 1; ij <= nn; ++ij) {
    v[ij] = -v[ij]; /* finally reverse sign of all matrix elements */
  }
  return 0;
} 
//________________________________________________________________________________
Int_t TRSymMatrix::TrsInv(const Double_t *g, Double_t *gi, Int_t n) {
  Int_t fail = TrchLU(g, gi, n);
  fail += TrInv(gi, gi, n);
  TrsmUL(gi, gi,n);
  return fail;
}
//________________________________________________________________________________
Int_t TRSymMatrix::TrchLU(const double *a, double *b, int n) {
   Int_t fail = 0;
   /* Local variables */
   int ipiv, kpiv, i__, j;
   double r__, dc;
   int id, kd;
   double sum;


   /* CERN PROGLIB# F112    TRCHLU          .VERSION KERNFOR  4.16  870601 */
   /* ORIG. 18/12/74 W.HART */


   /* Parameter adjuTments */
   --b;    --a;

   /* Function Body */
   ipiv = 0;

   i__ = 0;

   do {
      ++i__;
      ipiv += i__;
      kpiv = ipiv;
      r__ = a[ipiv];

      for (j = i__; j <= n; ++j) {
         sum = 0.;
         if (i__ == 1)       goto L40;
         if (r__ == 0.)      goto L42;
         id = ipiv - i__ + 1;
         kd = kpiv - i__ + 1;

         do {
            sum += b[kd] * b[id];
            ++kd;   ++id;
         } while (id < ipiv);

L40:
         sum = a[kpiv] - sum;
L42:
         if (j != i__) b[kpiv] = sum * r__;
         else {
	   if (sum > 0) dc = TMath::Sqrt(sum);
	   else         dc = 0;
	   b[kpiv] = dc;
	   if (r__ > 0. && dc > 0)  r__ = (double)1. / dc;
	   else                    {r__ = 0; fail++;}
         }
         kpiv += j;
      }

   } while  (i__ < n);
   return fail;
}
//________________________________________________________________________________
Int_t TRSymMatrix::TrInv(const Double_t *g, Double_t *gi, Int_t n)  {TCL::trinv(g, gi, n); return 0;}
//________________________________________________________________________________
Int_t TRSymMatrix::TrsmUL(const Double_t *g, Double_t *gi, Int_t n) {TCL::trsmul(g, gi, n); return 0;}
#if 0
//________________________________________________________________________________
Double_t &TRSymMatrix::operator()(Int_t i,Int_t j){
  //  assert(! (j < 0 || j >= fNrows));
  if (j < 0 || j >= fNrows) {
    ::Error("TRSymMatrix::operator()", "index j %d out of bounds (size: %d, this: %p)", 
	    j, fNrows, this); 
    j = 0;
  }
  //  assert(! (i < 0 || i >= fNrows));
  if (i < 0 || i >= fNrows) {
    ::Error("TRSymMatrix::operator()", "index i %d out of bounds (size: %d, this: %p)", 
	    i, fNrows, this); 
    i = 0;
  }
  Int_t m = i;
  Int_t l = j;
  if (i > j) {m = j; l = i;}
  return TArrayD::operator[](m + (l+1)*l/2);
}
#endif
