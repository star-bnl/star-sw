#ifndef ROOT_TRArray
#define ROOT_TRArray
#include <assert.h>
#include "Riostream.h"
#include "Rstrstream.h"
#include "Rtypes.h" 
#include "TObject.h"
#include "TArrayD.h"
#include "TMath.h"
#include "Varargs.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#ifndef __CINT__
#define __VA_LIST__(name) \
  va_list args;     \
  va_start(args,va_(name)); \
  for (Int_t num=0; num<fN; num++) { \
    if (! num) fArray[0] = name; \
    else fArray[num] = (Double_t) va_arg(args, Double_t); \
  } \
  va_end(args); 
#endif



class TRArray : public TArrayD {
 public:
  enum ETRMatrixType {kUndefined, kVector, kRectangular, kSemiPosDefinedSymMatrix, kDiagonalMatrix};
  enum ETRMatrixCreatorsOp { kZero, kUnit, kTransposed, kInverted, kInvertedPosDef, kInvertedA,
			     kMult, 
			     kAxB, kAxBT, kATxB, kATxBT,
			     kAxBxAT, kATxBxA, 
			     kSxA, kAxS, kSxAT, kATxS,
			     kAxAT, kATxA,
			     kAxSxAT, kATxSxA, kRxSxR
  };
  TRArray(Int_t N=0):  TArrayD(N), fValid(kTRUE), fIsNotOwn(kFALSE) {}
  //  TRArray(Int_t N,Double_t scalar):  TArrayD(N) {if (scalar != 0) Reset(scalar);}
#ifndef __CINT__
  TRArray(Int_t N,Double_t a0, ...);
#endif
  TRArray(Int_t N,const Double_t *Array):  TArrayD(N,Array), fValid(kTRUE), fIsNotOwn(kFALSE) {}
  TRArray(Int_t N,const Float_t *Array);
  TRArray(const TRArray &A,const Double_t fA, TRArray &B,const Double_t fB): TArrayD(0), fValid(kTRUE), fIsNotOwn(kFALSE)  {
    Int_t N = A.GetSize(); assert (N == B.GetSize()); Set(N); 
    TCL::vlinco(A.GetArray(),fA,B.GetArray(),fB,fArray,N);  
  }
  TRArray(Int_t N,const Char_t *s);  
  virtual ~TRArray() {if (fIsNotOwn) fArray = 0;}
  virtual Int_t GetNrows()                           const {return GetSize();} 
  virtual Int_t GetNcols()                           const {return 1;}
  virtual ETRMatrixType GetMatrixType()              const {return kUndefined;}
  virtual Bool_t IsValid()                           const {return fValid;}
  virtual Double_t Mag2()                            const;
  virtual Double_t Mag()                             const {return TMath::Sqrt(Mag2());}
  virtual void   SetValid(Bool_t Valid=kTRUE)              {fValid = Valid;}
  void           Set(Int_t n);
  void           Set(Int_t n, const Double_t *array);
  void           Set(Int_t n, const Float_t *array);
  void           AdoptA(Int_t n, Double_t *arr);
  void           reset() {Reset();}
  TRArray& operator=(const TRArray &rhs);
  virtual Double_t &operator()(Int_t i)                    {return operator[](i);}
  virtual Double_t operator()(Int_t i) const               {return operator[](i);}
  friend TRArray &operator-=(TRArray &target, Double_t scalar) {
    for (int i=0; i<target.fN; i++) target.fArray[i] -= scalar; return target;}
  friend TRArray &operator+=(TRArray &target, Double_t scalar) {
    for (int i=0; i<target.fN; i++) target.fArray[i] += scalar; return target;
  }
  friend Double_t operator*(const TRArray &target, const TRArray &source) {
    assert(target.fN == source.GetSize());
    Double_t sum = 0;
    const Double_t *sArray = source.GetArray();
    for (int i=0; i<target.fN; i++) sum += target.fArray[i]*sArray[i]; return sum;
  }
  friend TRArray &operator*=(TRArray &target, Double_t scalar) {
    for (int i=0; i<target.fN; i++) target.fArray[i] *= scalar; return target;
  }
  friend TRArray &operator/=(TRArray &target, Double_t scalar) {
    for (int i=0; i<target.fN; i++) target.fArray[i] /= scalar; return target;
  }
  friend TRArray &operator-=(TRArray &target, const TRArray &A) {
    assert(target.fN == A.GetSize());
    const Double_t *fA  = A.GetArray();
    for (int i=0; i<target.fN; i++) target.fArray[i] -= fA[i]; 
    return target;
  }
  friend TRArray &operator+=(TRArray &target, const TRArray &A) {
    assert(target.fN == A.GetSize());
    const Double_t *fA  = A.GetArray();
    for (int i=0; i<target.fN; i++) target.fArray[i] += fA[i]; 
    return target;
  }
  
  friend Bool_t operator==(TRArray &target, Double_t scalar) {
    for (int i=0; i<target.fN; i++) if (target.fArray[i] != scalar) return kFALSE; 
    return kTRUE;
  }
  friend Bool_t operator==(TRArray &target, const TRArray &A) {
    if (target.fN != A.GetSize()) return kFALSE; 
    const Double_t *fB  = A.GetArray();
    for (int i=0; i<target.fN; i++) if (target.fArray[i] != fB[i]) return kFALSE; return kTRUE;
  }
  friend TRArray operator + (const TRArray &A, const TRArray &B) {TRArray C(A); C += B; return C;}
  friend TRArray operator - (const TRArray &A, const TRArray &B) {TRArray C(A); C -= B; return C;}
  
  Bool_t Verify(const TRArray &A, const Double_t zeru=5.e-7, Int_t Level=1) const;
  virtual void   Print(Option_t *opt="") const;
 protected:
  Bool_t fValid;
  Bool_t fIsNotOwn; /* == kTRUE If TRArray created via AdoptA(Int_t n, Double_t *array) method 		  	
		       then TRArray is not owner of fArray and user has to care to its size and delete
		       in contrary TArrayD                                                            */
 public:
  ClassDef(TRArray,1)  // TRArray class (double precision)
};
std::ostream& operator<<(std::ostream& s,const TRArray &target);
std::istream & operator>>(std::istream &s, TRArray &target);

#endif
