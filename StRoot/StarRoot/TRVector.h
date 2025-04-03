#ifndef ROOT_TRVector
#define ROOT_TRVector
#include "TRMatrix.h"
#include "TRSymMatrix.h"
#include "TVector3.h"
#include "StThreeVectorD.hh"
class TRVector : public TRMatrix {
 public:
  TRVector(Int_t nrows=0);
  TRVector(Int_t nrows,const Double_t *Array);
  TRVector(Int_t nrows,const Float_t *Array);
  TRVector(const TRVector& A, ETRMatrixCreatorsOp kop,const TRMatrix& B);
  TRVector(const TRMatrix& A, ETRMatrixCreatorsOp kop,const TRVector& B);
  TRVector(const TRMatrix& A, Int_t I=1);
  TRVector(Int_t nrows,const Char_t *s);
  TRVector(const TRSymMatrix &S, ETRMatrixCreatorsOp kop,const TRVector& A);
  TRVector(const TRVector& A, ETRMatrixCreatorsOp kop,const TRSymMatrix &S);
  TRVector       &operator=(const TVector3 &rhs);
  TRVector       &operator=(const StThreeVectorD &rhs);
  TRVector(const TVector3 &rhs); 
  TRVector(const StThreeVectorD &rhs); 
  TRVector       Cross(const TRVector& v) const;
  TRVector       Unit() const;
  friend TRVector operator*(const TRVector &source, Double_t scalar) {TRVector s(source); s *= scalar; return s;}
  friend TRVector operator*(Double_t scalar, const TRVector &source) {TRVector s(source); s *= scalar; return s;}
  friend TRVector operator/(const TRVector &source, Double_t scalar) {TRVector s(source); s /= scalar; return s;}
  friend TRVector operator+(const TRVector &source, Double_t scalar) {TRVector s(source); s += scalar; return s;}
  friend TRVector operator+(Double_t scalar, const TRVector &source) {TRVector s(source); s += scalar; return s;}
  friend TRVector operator-(const TRVector &source, Double_t scalar) {TRVector s(source); s -= scalar; return s;}
  friend TRVector operator-(Double_t scalar, const TRVector &source) {TRVector s(source); s -= scalar; return s;}
#ifndef __CINT__
  TRVector(Int_t nrows,Double_t a0, ...);
#endif
  virtual ~TRVector(){}
  ETRMatrixType GetMatrixType() const {return kVector;}
  //  void Add(const TRMatrix& A, ETRMatrixCreatorsOp kop,con.xst  TRVector& B); 
  //  void Substruct(const TRMatrix& A, ETRMatrixCreatorsOp kop,const  TRVector& B); 
  virtual void   Print(Option_t *opt="") const;
  ClassDef(TRVector,1)  // TRMatrix class (double precision)
};
ostream& operator<<(ostream& s,const TRVector &target);
#endif
