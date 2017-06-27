#ifndef __FitParameters_h__
#define __FitParameters_h__
//#include <assert.h>
#include "Stiostream.h"
#include "TRVector.h"
#include "TRSymMatrix.h"
class TGeoHMatrix;
class FitParameters;
ostream& operator<<(ostream& s,const FitParameters &target);
//________________________________________________________________________________
class FitParameters { // base class for fit paramters
 public:
  //                                SC       SD       Sti       Dca
  enum eTrParType {kUndefined=-1,  kSC = 0, kSD = 1, kSti = 2, kDca = 3, kAll};
  struct ParamSC_t {// 
    Double_t pInv, Lambda, Phi, yT, zT, xT; // at fixed track length, pInv = q/p;
  };
  struct CovMatrixSC_t {// covariance matrix of  SC parameters
    Double_t  pInpIn;
    Double_t  LampIn, LamLam;
    Double_t  PhipIn,   PhiZ, PhiPhi;
    Double_t   yTpIn,    yTZ,  yTPhi, yTyT;
    Double_t   zTpIn,    zTZ,  zTPhi, zTyT, zTzT;
  };
  struct ParamSD_t {// spline paramters
    Double_t pInv, tY, tZ, y, z; // on a plane, pInv = q/p;
  };
  struct CovMatrixSD_t {
    Double_t  pInpIn;
    Double_t  tYpIn, tYtY;
    Double_t  tZpIn, tZtY, tZtZ;
    Double_t   ypIn,  ytY,  ytZ, yy;
    Double_t   zpIn,  ztY,  ztZ, zy, zz;
  };
  struct ParamSti_t {// Sti parameters
    //                    -q/pT
    Double_t x, y, z, eta, ptin, tanl; // at fixed x (eta == Phi)
  };
  
  struct CovMatrixSti_t {// covariance matrix of  Sti parameters
    Double_t 
      cXX,
      cYX, cYY,
      cZX, cZY, cZZ,
      cEX, cEY, cEZ, cEE,
      cPX, cPY, cPZ, cPE, cPP,
      cTX, cTY, cTZ, cTE, cTP, cTT;
  };
  struct ParamDca_t {
    Double_t Imp, Z, Psi, pTInv, tanL, Curv;
  };
  struct CovMatrixDca_t {
    Double_t  ImpImp;
    Double_t    ZImp,   ZZ;
    Double_t  PsiImp, PsiZ, PsiPsi;
    Double_t  PtiImp, PtiZ, PtiPsi, PtiPti;
    Double_t  TanImp, TanZ, TanPsi, TanPti, TanTan;
  };
 protected:
  void Setup( const TRVector *P = 0, const TRSymMatrix *C = 0);
 public:
  FitParameters(Int_t type=kUndefined, Double_t spu = 0, TGeoHMatrix *rot=0) : 
    _type(SetType(type)), _spu(spu), _rot(rot) {Setup();}
  FitParameters(Int_t type, TGeoHMatrix *rot) : 
    _type(SetType(type)), _spu(0), _rot(rot) {Setup();}
  FitParameters(Int_t type, TGeoHMatrix *rot, const TRVector &P, const TRSymMatrix &C) :
    _type(SetType(type)), _spu(0), _rot(rot) {Setup(&P,&C);}
  FitParameters(Int_t type, Double_t spu, TGeoHMatrix *rot, const TRVector &P, const TRSymMatrix &C) :
    _type(SetType(type)), _spu(spu), _rot(rot) {Setup(&P,&C);}
  FitParameters(Int_t type, const TRVector &P, const TRSymMatrix &C) :
    _type(SetType(type)), _spu(0), _rot(0) {Setup(&P,&C);}
  FitParameters(Int_t type, Double_t spu, const TRVector &P, const TRSymMatrix &C) :
    _type(SetType(type)), _spu(spu), _rot(0) {Setup(&P,&C);}
  virtual ~FitParameters() {}
  eTrParType         Type()   const {return _type;}
  const TRVector     P()      const {return _P;}		       
  const TRSymMatrix  C()      const {return _C;}		       
  TRVector&          P()            {return *&_P;}	       
  TRSymMatrix&       C()            {return *&_C;}	       
  void               Reset()        {_P.Reset(), _C.Reset();}    
  Double_t          *A()            {return _P.GetArray();}      
  const Double_t    *A()      const {return _P.GetArray();}      
  const TGeoHMatrix *Rot()    const {return _rot;}	       
  const TRMatrix     F()      const {return _F;}		       
  TRMatrix          &F()            {return *&_F;}               
  const TRVector     BField() const {//assert(_BField.GetSize()); 
    return _BField;}
  TRVector          &BField()       {//assert(_BField.GetSize()); 
    return *&_BField;}
  Double_t           Spu()    const {return _spu;}
  Double_t          &Spu()          {return *&_spu;}
  static  eTrParType SetType(Int_t type=0);        
  static void        SetDebug(Int_t l) {_debug = l;}
  void               SetBField(Double_t *bfield) {_BField = TRVector(3,bfield);}
  void               SetBField(Float_t  *bfield) {_BField = TRVector(3,bfield);}
  void               Print(const Char_t *opt) const {cout << *this << endl;}
  const Char_t      *PrintpTasString() const;
  void               SetRotation(TGeoHMatrix *rot) {_rot = rot;}
  FitParameters     &operator=(const    FitParameters &source);    

  FitParameters     &SC2SD  (const FitParameters &source);	      
  FitParameters     &SC2Sti (const FitParameters &source);	      
  FitParameters     &SC2Dca (const FitParameters &source);	      

  FitParameters     &SD2SC  (const FitParameters &source);	      
  FitParameters     &SD2Sti (const FitParameters &source);	      
  FitParameters     &SD2Dca (const FitParameters &source);        

  FitParameters     &Sti2SC (const FitParameters &source);	      
  FitParameters     &Sti2SD (const FitParameters &source);	      
  FitParameters     &Sti2Dca (const FitParameters &source);	      

  FitParameters     &Dca2SC (const FitParameters &source);	      
  FitParameters     &Dca2Sti(const FitParameters &source);	      
  FitParameters     &Dca2SD(const FitParameters &source);	      

  TRVector          &PxyzG() const;
  TRSymMatrix       &PxyzGCovMatrix()    const;
  TRVector          &XyzG() const;
  TRSymMatrix       &XyzGCovMatrix()     const;
  TRVector           PxyzGSC() const;
  TRSymMatrix        PxyzGCovMatrixSC()  const;
  TRVector           XyzGSC() const;
  TRSymMatrix        XyzGCovMatrixSC()   const;
  TRVector           PxyzGSD() const;
  TRSymMatrix        PxyzGCovMatrixSD()  const;
  TRVector           XyzGSD() const;
  TRSymMatrix        XyzGCovMatrixSD()   const;
  TRVector           PxyzGSti() const;
  TRSymMatrix        PxyzGCovMatrixSti() const;
  TRVector           XyzGSti() const;
  TRSymMatrix        XyzGCovMatrixSti()  const;
  TRVector           PxyzGDca() const;
  TRSymMatrix        PxyzGCovMatrixDca() const;
  TRVector           XyzGDca() const;
  TRSymMatrix        XyzGCovMatrixDca()  const;
  void               Momentum(Double_t p[3], Double_t e[6]) const;
  Int_t              Charge() const;
  Double_t           Y()      const;	
  Double_t 	     Z()      const;	
  Double_t           X_g()    const {return XyzG()[0];}
  Double_t           Y_g()    const {return XyzG()[1];}	
  Double_t 	     Z_g()    const {return XyzG()[2];}	
  Double_t 	     Eta()    const;	
  Double_t 	     TanL()   const;
  Double_t           Pt()     const {return pT();}
  Double_t           pT()     const;
  Double_t           dpTOverpT()     const;
  const TRMatrix    &H()      const {return *_H[_type];}
  const Char_t      *beg()    const {return _beg;}
 protected:
  Char_t       _beg[1];
  Double_t     _par[6];
  Double_t     _cov[21];
  Double_t     _bfield[3];
  Char_t       _end[1];
  eTrParType   _type;
  Double_t     _spu;
  TGeoHMatrix *_rot;
  TRVector     _BField;
  TRVector     _P;
  TRSymMatrix  _C;
  static Int_t _debug;
  static TRMatrix _F; // Conversion matrix of the latest transformation
  static TRMatrix **_H;
};
#endif
