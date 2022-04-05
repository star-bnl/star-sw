#include <stdarg.h>
#include <iomanip>
#include "TRArray.h"
#include "TString.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include "TObjString.h"
#include "TObjArray.h"
using namespace std;
ClassImp(TRArray);
//________________________________________________________________________________
TRArray::TRArray(Int_t N,const Float_t *Array):  TArrayD(N), fValid(kTRUE), fIsNotOwn(kFALSE) {
  TCL::ucopy(Array,fArray,N);
}
//________________________________________________________________________________
TRArray::TRArray(Int_t N,Double_t va_(a0), ...) : TArrayD(N), fValid(kTRUE), fIsNotOwn(kFALSE) {
  __VA_LIST__(a0);
}
//________________________________________________________________________________
TRArray::TRArray(Int_t N,const Char_t *s): TArrayD(N), fValid(kTRUE), fIsNotOwn(kFALSE) {
  static TString separator = "([^\t ;,]+)";
  TString opt(s);
  TObjArray *array = opt.Tokenize(separator);
  TIter next(array);
  TObjString *objs;
  Int_t i = 0;
  while ((objs = (TObjString *) next()) && i < N) {fArray[i++] = objs->GetString().Atof();}
  delete array;
}
//________________________________________________________________________________
Double_t TRArray::Mag2() const {
  return TCL::vdot(fArray,fArray,fN);
}
//________________________________________________________________________________
ostream& operator<<(ostream& s,const TRArray &target) {
  s << "Size \t" << target.fN << endl;
  if (target.fArray) 
    for (int i = 0; i< target.fN; i++) {
      s << Form("%10.3f", target.fArray[i]); 
      if ((i+1)%10 == 0) s << endl;
    }
  else s << " Empty";
  s << endl;
  return s;
}
//________________________________________________________________________________
istream & operator>>(istream &s, TRArray &target) {
  Int_t N;
  s >> N;
  if (N != target.fN) target.Set(N);
  for (int i = 0; i < N; i++) s >> target.fArray[i];
  return s;
}
//________________________________________________________________________________
TRArray &TRArray::operator=(const TRArray &rhs) {   // TRArray assignment operator.
  if (this != &rhs) {
    if (! fIsNotOwn) Set(rhs.fN, rhs.fArray);
    else {
      fN = rhs.fN;
      memcpy(fArray,rhs.fArray, fN*sizeof(Double_t));
    }
  }
  return *this;
}
//________________________________________________________________________________
Bool_t TRArray::Verify(const TRArray &A, Double_t zeru, Int_t Level) const {
  // TRUE if test failed
  Int_t fails = 0;
  if (fN != A.GetSize()) {
    if (Level) cout << "Check length is inconsistent:" << fN << " != " << A.GetSize() << endl;
    return kTRUE;
  }
  const Double_t *aArray = A.GetArray();
  for (int i=0; i<fN; i++) {
    Double_t diff = TMath::Abs(aArray[i] - fArray[i]);
    Double_t sum =  TMath::Abs(aArray[i] + fArray[i]);
    if (diff > zeru || (sum > 2. && (2 * diff ) / sum > zeru)) {
      fails++;
      if (Level) 
	cout << "Failed:[" << i << "]\t" << aArray[i] << "\t" << fArray[i] << "\tdiff\t" << diff << endl;
      continue;
    }
    else if (Level > 1) 
      cout << "Passed:[" << i << "]\t" << aArray[i] << "\t" << fArray[i] << "\tdiff\t" << diff << endl;
  }
  if (fails) {
    cout << "Failed " << fails << " times" << endl;
  }
  return fails != 0;
}
//________________________________________________________________________________
void TRArray::Print(Option_t *opt) const {if (opt) {}; cout << *this << endl;}
//______________________________________________________________________________
void TRArray::AdoptA(Int_t n, Double_t *arr) {
   // Adopt array arr into TRArray, i.e. don't copy arr but use it directly
   // in TRArray. User may delete arr, TRArray dtor will not do it.
   fN     = n;
   if (fArray == arr) return;
   if (fArray && ! fIsNotOwn) delete [] fArray;
   fIsNotOwn = kTRUE;
   fArray = arr;
}
//______________________________________________________________________________
void TRArray::Set(Int_t n) {
   // Set size of this array to n doubles.
   // A new array is created, the old contents copied to the new array,
   // then the old array is deleted.
   // This function should not be called if the array was declared via Adopt.

   if (n < 0) return;
   if (fIsNotOwn) {
     memset(&fArray[fN],0,(n-fN)*sizeof(Double_t));
     fN = n;
     return;
   }
   if (n != fN) {
      Double_t *temp = fArray;
      if (n != 0) {
	fArray = new Double_t[n];
	if (n < fN) memcpy(fArray,temp, n*sizeof(Double_t));
	else {
	  memcpy(fArray,temp,fN*sizeof(Double_t));
	  memset(&fArray[fN],0,(n-fN)*sizeof(Double_t));
	}
      } else {
         fArray = 0;
      }
      if (fN) delete [] temp;
      fN = n;
   }
}
//________________________________________________________________________________
void TRArray::Set(Int_t n, const Float_t *array) {
  // Set size of this array to n doubles and set the contents
  // This function should not be called if the array was declared via Adopt.
  if (fArray && fN != n && ! fIsNotOwn) {
    delete [] fArray;
    fArray = 0;
   }
  fN = n;
  if (fN == 0) return;
  if (array == 0) return;
  if (!fArray) {
    fIsNotOwn = kFALSE;
    fArray = new Double_t[fN];
  }
  TCL::ucopy(array,fArray,n);
}
//______________________________________________________________________________
void TRArray::Set(Int_t n, const Double_t *array) {
   // Set size of this array to n doubles and set the contents
   // This function should not be called if the array was declared via Adopt.
   if (fArray && fN != n && ! fIsNotOwn) {
      delete [] fArray;
      fArray = 0;
   }
   fN = n;
   if (fN == 0) return;
   if (array == 0) return;
   if (!fArray) {
     fIsNotOwn = kFALSE;
     fArray = new Double_t[fN];
   }
   memcpy(fArray,array, n*sizeof(Double_t));
}
