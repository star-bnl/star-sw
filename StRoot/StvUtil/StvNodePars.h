#ifndef __StvNodePars_h_
#define __StvNodePars_h_
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <vector>
#include "THelixTrack.h"

class StvNodePars;
class StvFitPars;
class StvFitErrs;
class StvImpact;
class StvELossTrak;

//------------------------------------------------------------------------------
typedef double Mtx55D_t[5][5];
void Multiply(Mtx55D_t &res, const Mtx55D_t &A,const Mtx55D_t &B);
void Multiply(double res[5], const Mtx55D_t &A,const double B[5]);
inline void Copy(Mtx55D_t &to,const Mtx55D_t &fr){memcpy(to[0],fr[0],5*5*sizeof(to[0][0]));}

//------------------------------------------------------------------------------
class StvFitDers 	//Derivative matrix of StvFitPars
{
public:
operator const Mtx55D_t &() const {return mMtx;};
operator       Mtx55D_t &()       {return mMtx;};
StvFitDers &operator=( const StvFitDers &fr) 
         	{memcpy(mMtx[0],fr[0],sizeof(mMtx)); return *this;};
      void  Reverse();  
protected:
Mtx55D_t mMtx;
};
//------------------------------------------------------------------------------
class StvHlxDers 	//Derivative matrix of THelixTrack
{
public:
operator const Mtx55D_t &() const {return mMtx;};
operator       Mtx55D_t &()       {return mMtx;};
StvHlxDers &operator=( const StvFitDers &fr) 
         	{memcpy(mMtx[0],fr[0],sizeof(mMtx)); return *this;};
protected:
Mtx55D_t mMtx;
};

void Invert(Mtx55D_t &to,const Mtx55D_t &fr);
void Testik(const Mtx55D_t &tt);
double EmxSign(int n,const double *a); 
double EmxSign(int n,const float  *a); 
//------------------------------------------------------------------------------
class StvFitPars
{
public:	
  StvFitPars():mH(0),mZ(0),mA(0),mL(0),mP(0){}
  StvFitPars(double h,double z):mH(h),mZ(z),mA(0),mL(0),mP(0){}
  StvFitPars(const double *arr) 		{memcpy(&mH,arr,5*sizeof(mH));}
const StvFitPars &operator*(const StvFitDers &t) const;    
        void Print(const char *tit=0) const;
         int Check(const char *tit=0) const;
         int TooBig(const StvNodePars &np) const;
      double *Arr() 				{return &mH;}
const double *Arr()  const 			{return &mH;}
operator const double *() const			{return &mH;}
operator       double *() 			{return &mH;}
StvFitPars &operator*=(double f) 
    		{for(int i=0;i<5;i++){(*this)[i]*=f;} return *this;}

public:	
// Let (Dx,Dy,Dz) vector track direction
// It could be also represented:
// (cos(L)*cos(A),cos(L)*sin(A),sin(L))

// mH: movement along (-Dy    ,Dx     ,           0) vector
// mZ: movement along (-Dx*Dz , -Dz*Dy, Dy*Dy+Dx*Dx)
// Or mH: along (-cos(A),sin(A),0)
//    mZ: along (-sin(L)*cos(A),-sin(L)*sin(A), cos(L))

double mH;	// direction perpendicular movement and Z
double mZ;	// Pseudo Z, direction perpendicular movement & H
double mA;	// Angle in XY. cos(A),sin(A),T moving direction
double mL;	// Angle lambda in Rxy/Z
double mP;	// 1/pt with curvature sign
};


//------------------------------------------------------------------------------
class StvNodePars {
public:	
  enum eNodePars {kNPars=5};
  StvNodePars()			{reset();}
  void reset();
  void ready();
   int isReady() const;
  void set(const THelixTrack *ht, double Hz);
const double *pos() const {return &_x;}
      double *pos()       {return &_x;}
  void get(      THelixTrack *ht) const;
double getPt() const			{ return 1./(fabs(_ptin)+1e-6); }
  void getMom(double p[3]) const; 
  void getDir(double d[3]) const; 
double getP2()  const;
double getP()  const;
double getRxy() const;
double getCos2L() const 		{return 1./(1.+_tanl*_tanl);}
double getCosL() const;
  void reverse(); 
  void Deriv(double len,             StvFitDers &der) const;
  void Deriv(double len,double dPdP0,StvFitDers &der) const;
   int isValid() const 	{return  (_hz && _cosCA);};
///		convert THelixTrack derivativ matrix into StvFitPar one
  void convert( StvFitDers &fitDer , const StvHlxDers &hlxDer) const;
//		move point along helix  
  void move(double dLen); 
double move(const double v[3],double dPP,int dir); 
//		move point along helix  up to given radius in xy
  void moveToR(double Rxy); 
StvNodePars &merge(double wt,StvNodePars &other);
//		typical variations of parametrs
StvFitPars delta() const;
StvFitErrs deltaErrs() const;
double diff(const StvNodePars &other) const;
double diff(const StvNodePars &other, const StvFitErrs &otherr) const;
double diff(const float hit[3]) const;

operator const double  *() const {return P;}
operator       double  *()       {return P;}
    int getCharge() const {return (_ptin > 0) ? -1 : 1;}
    int     check(const char *pri=0) const;
void  operator+=(const StvFitPars &add);
StvNodePars &operator=(const StvNodePars &fr);
const StvFitPars &operator-(const StvNodePars& sub) const;
void    print() const;
  void GetRadial (double radPar[6],double *radErr=0,const StvFitErrs *fE=0)  const;
  void GetPrimial(double radPar[6],double *radErr=0,const StvFitErrs *fE=0)  const;
  void GetImpact(StvImpact *imp,const StvFitErrs *fE=0)  const;

  enum {kX=0,kY,kZ,kPhi,kCurv,kTanL};
public:	
  /// sine and cosine of cross angle
  double _cosCA;
  double _sinCA;
  union{double P[1];double _x;};
  double _y; 
  double _z;
  double _psi;
  /// signed invert pt [sign = sign(-qB)]
  double _ptin;  
  /// tangent of the track momentum dip angle
  double _tanl;
  /// signed curvature [sign = sign(-qB)]
  double _curv;  
  /// Z component magnetic field in units Pt(Gev) = Hz * RCurv(cm)
  double _hz;  
};
class StvNodeErrs {
public:	
StvNodeErrs(){reset();assert(&_cTT-&_cXX+1==21);}
void reset()				{memset(this,0,sizeof(StvNodeErrs));}
operator const double *() const { return &_cXX;}
operator       double *()       { return &_cXX;}
public:	
  double _cXX;
  double _cYX,_cYY;                       
  double _cZX,_cZY, _cZZ;                 
  double _cEX,_cEY, _cEZ, _cEE;           
  double _cPX,_cPY, _cPZ, _cPE, _cPP;     
  double _cTX,_cTY, _cTZ, _cTE, _cTP, _cTT;
};  

//------------------------------------------------------------------------------
class StvFitErrs
{
public:	
  enum eFitErrs {kNErrs=15};
  StvFitErrs(double hh=0,double hz=0,double zz=0);
  StvFitErrs(const StvFitErrs &fr) {*this = fr;}
  void Set(const THelixTrack *he,double hz);
  void Get(      THelixTrack *he)     const;
  void Get(const StvNodePars *np,  StvNodeErrs *ne)     const;
double GetHz() const   ;//?? 	{ return mHz ;}
  void SetHz(double hz);//??  	{ mHz=hz     ;}
  const double *Arr() const 	{ return &mHH;}
        double *Arr()       	{ return &mHH;}
operator const double *() const { return &mHH;}
operator       double *()       { return &mHH;}
StvFitErrs &operator=(const StvFitErrs &fr) ;
StvFitErrs &operator*=(double f) {for (int i=0;i<kNErrs;i++){(*this)[i]*=f;};return *this;}
  void Add(const StvELossTrak *el,const StvNodePars &pa,double len=0);
  void Backward();
const StvFitErrs &operator*(const StvFitDers &mtx) const; 
double Sign() const;
   int Check(const char *tit=0) const;
   int Recov();
double Diff(const StvFitErrs &errs,int *idx=0) const;// difference 

double MaxCorr() const   ;//  maximal correlation
  void SetMaxCorr(double maxCorr)  ;//  set maximal correlation
  void Print(const char *tit=0) const;
static double EmxSign(int n,const double *S);
static double EmxSign(int n,const float  *S);

public:	
//  dH: along ort to dir and in Track/Z plane
//  dA: delta azimuth angle; 
//  dP: == d(1./Pt) where Pt is signed as curvature;  
//  dZ: ort to dH and in plane dH,Zaxis;When lamda=0 it is Zaxis 
//  dL = dLambda, angle between track and X,Y plane

double
mHH,
mHZ, mZZ,
mHA, mZA, mAA,
mHL, mZL, mAL, mLL,
mHP, mZP, mAP, mLP, mPP;
//protected:
double mHz;
};  

//------------------------------------------------------------------------------
class StvHitErrs{
public:
StvHitErrs()			 {reset();}
void reset()			 {memset(this,0,sizeof(*this));}
StvHitErrs &operator*=(double f) {for (int i=0;i<6;i++){(&hXX)[i]*=f;};return *this;}
void rotate(double angle);
  double hXX;		
  double hXY,hYY;                       
  double hXZ,hYZ,hZZ;                 
};
//------------------------------------------------------------------------------
class StvImpact {
public:    
StvImpact();
void Print(const char *opt) const;
public:    
    /// signed impact parameter; Signed in such a way that:
    ///     x =  -impact*sin(Psi)
    ///     y =   impact*cos(Psi)
    float  mImp;
    ///  Z-coordinate of this track (reference plane)
    float  mZ;
    ///  Psi angle of the track
    float  mPsi;
    /// signed invert pt [sign = sign(-qB)]
    float  mPti;
    /// tangent of the track momentum dip angle
    float  mTan;
    /// signed curvature
    float  mCurv;
    
    /// pars errors
    float  mImpImp;
    float  mZImp, mZZ;
    float  mPsiImp, mPsiZ, mPsiPsi;
    float  mPtiImp, mPtiZ, mPtiPsi, mPtiPti;
    float  mTanImp, mTanZ, mTanPsi, mTanPti, mTanTan;
};

//------------------------------------------------------------------------------
class TGeoMaterial; 

class StvELossData 
{
public:
class Aux 
{
public:
const TGeoMaterial* mMat;
double mLen,mA,mZ,mDens,mX0;
};

public:
  double mTheta2;	//multiple scattering angle error
  double mOrt2;		//multiple scattering position error
  double mELoss;	//Energy loss
  double mELossErr2;	//Square error of energy loss
  double mdLogEdXdLogP;       	//d(log(dEdX))/d(log(P))
  double mTotLen;		//Total length where errors accumulated
  double mE;			//Total energy
  double mP;			//Total momentum
  double mM;			//Mass
     int mTally;		//Counter for debug only, remove later
std::vector<Aux> mMats;
};
//------------------------------------------------------------------------------
class StvNodeParsTest
{
public:
static void Test();
static void TestGetRadial(int nEv=10000);
static void TestErrProp  (int nEv=10000);
static void TestMtx      ();
static void TestImpact   (int nEv=10000) ;
ClassDef(StvNodeParsTest,0)
};

//------------------------------------------------------------------------------
// 		StvNodePars::inlines
inline void StvNodePars::reset(){memset(this,0,sizeof(StvNodePars));}
//------------------------------------------------------------------------------
inline void StvNodePars::ready(){_cosCA=cos(_psi);_sinCA=sin(_psi);_curv = _hz*_ptin;}
//------------------------------------------------------------------------------
inline void StvNodePars::getMom(double p[3]) const 
{ 
  double pt = 1./(fabs(_ptin)+1e-6); 
  p[0]=pt*_cosCA;p[1]=pt*_sinCA;p[2]=pt*_tanl;
}
//------------------------------------------------------------------------------
inline double StvNodePars::getRxy() const 
{ 
   return sqrt(_x*_x+_y*_y);
}
//------------------------------------------------------------------------------
inline void StvNodePars::getDir(double d[3]) const 
{ 
  double nor = sqrt(1.+_tanl*_tanl);
  d[0]=_cosCA/nor;d[1]=_sinCA/nor;d[2]=_tanl/nor;
}
//------------------------------------------------------------------------------
inline double StvNodePars::getP() const 
{ 
  double t =_tanl*_tanl;
  t = (t<0.01)? (1.+t*(0.5-t*0.125)) : sqrt(1.+t);
  return t/(fabs(_ptin)+1e-12);
}

//------------------------------------------------------------------------------
inline double StvNodePars::getP2() const 
{ return 1./(_ptin*_ptin+1e-12)*(1.+_tanl*_tanl);}

//------------------------------------------------------------------------------
inline void StvNodePars::reverse() 
{
 _cosCA = -_cosCA; _sinCA=-_sinCA; _psi+= M_PI; 
 while (_psi> M_PI) {_psi-=2*M_PI;}
 while (_psi<-M_PI) {_psi+=2*M_PI;}
 _tanl  = -_tanl ; _curv = -_curv ; _ptin = -_ptin;
}
//------------------------------------------------------------------------------
inline double StvNodePars::getCosL() const
{    
   double tt = _tanl*_tanl;
   return (tt<0.1)? 1./(1. +tt*(0.5 - tt*0.25)) :1./sqrt(1+tt);
}

#endif
