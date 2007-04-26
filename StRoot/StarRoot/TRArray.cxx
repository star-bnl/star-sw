#include <stdarg.h>
#include <iomanip>
#include "TRArray.h"
#include "TCL.h"
#include "TString.h"
ClassImp(TRArray);
//________________________________________________________________________________
TRArray::TRArray(Int_t N,const Float_t *Array):  TArrayD(N), fValid(kTRUE) {
  TCL::ucopy(Array,fArray,N);
}
//________________________________________________________________________________
TRArray::TRArray(Int_t N,Double_t va_(a0), ...) : TArrayD(N), fValid(kTRUE) {
  __VA_LIST__(a0);
}
//________________________________________________________________________________
TRArray::TRArray(Int_t N,const Char_t *s): TArrayD(N), fValid(kTRUE) {
#ifdef R__SSTREAM
      istringstream ins((char *) s);
#else
      istrstream ins((char *) s);
#endif
    for (int i=0; i < N; i++) ins >> fArray[i]; 
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
    }
    else if (Level > 1) 
      cout << "Passed:[" << i << "]\t" << aArray[i] << "\t" << fArray[i] << "\tdiff\t" << diff << endl;
  }
  return fails != 0;
}
//________________________________________________________________________________
void TRArray::Print(Option_t *opt) const {if (opt) {}; cout << *this << endl;}
