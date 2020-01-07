#ifndef __StvNodePars_h_
#define __StvNodePars_h_
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <vector>
#include "THelixTrack.h"
#include "THelix3d.h"

class StvNodePars;
class StvFitPars;
class StvImpact;
class StvELossTrak;
class TRungeKutta;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
class StvFitDers: public THDer3d_t
{
public:
StvFitDers():THDer3d_t(){};
StvFitDers &operator=(const THDer3d_t &fr) {THDer3d_t::operator=(fr); return *this;}
StvFitDers(const THDer3d_t &fr){*this = fr;};

};
//------------------------------------------------------------------------------

class  StvFitErrs  : public THEmx3d_t 
{
public:
 StvFitErrs(){};
 StvFitErrs(double errs[15],TkDir_t *tkd=0)
           { THEmx3d_t::Set(errs);if (tkd) *this = *tkd; };
 StvFitErrs(double UU,double UV,double VV) {THEmx3d_t::Set(UU,UV,VV);};
 void Add(const StvELossTrak *el,double len=0);
 double Sign() const;
    int Sigre() const;	//Check correctness of signes 
const StvFitErrs& operator*(const StvFitDers &der) const;
      StvFitErrs& operator= (const THEmx3d_t  &emx  );
      StvFitErrs& operator= (const TkDir_t   &tkdir);
//????             void Update(const TkDir_t &tkdir);
 void Set(const THelixTrack *he);
 void Set(const THelix3d *he);
 void Get(      THelix3d *he) const;
 void Set(const TRungeKutta *he);
 void Get(      TRungeKutta *he) const;
  int Check(const char  *tit) const;
  int Recov();
 void Print(const char  *tit) const;
 
 double PtErr(const StvNodePars &pars) const;

static double EmxSign(int n,const float  *e);
static double EmxSign(int n,const double *e);
}; 

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
class StvFitPars
{
public:	
  StvFitPars();
  StvFitPars(double u,double v);
  StvFitPars(const double *arr);
const StvFitPars &operator*(const StvFitDers &t) const;    
        void Print(const char *tit=0) const;
         int Check(const char *tit=0) const;
operator const double *() const				{return &mU;}
operator       double *() 				{return &mU;}
void operator*=(double f) {mU*=f;mV*=f;mFita*=f;mLama*=f;mPinv*=f;}

public:	
// Let (Dx,Dy,Dz) vector track direction
// It could be also represented:
// T = ( cos(Lamda)*cos(Phi),  cos(Lamda)*sin(Phi), sin(Lamda))
// U = (-           sin(Phi),             cos(Phi),          0)
// V = (-sin(Lamda)*cos(Phi), -sin(Lamda)*sin(Phi), cos(Lamda))
//
// In this coordinate system define new spherical system
// Any vector could be represented as a sum:
// T*cos(Lama)*cos(Fita) +
// U*cos(Lama)*sin(Fita) + 
// V*sin(Lama)
//
// OR if the angles are small
//
// T +
// U*Fita + 
// V*Lama
//
double mU;	// 
double mV;	// 
double mFita;	// 
double mLama;	// 
double mPinv;	// -iQ/P 

};


//------------------------------------------------------------------------------
class StvNodePars {
public:	
  enum eNodePars {kNPars=5};
  StvNodePars()				{reset();}
  void reset();
  void set(const double h[3]);
  void set(const double x[3],const double d[3],double pinv,const double h[3]);
  void set(const THelixTrack *ht);
  void set(const THelix3d *ht);
  void get(      THelix3d *ht) const;
  void set(const TRungeKutta *ht);
  void get(      TRungeKutta *ht) const;
  void move(double len, StvFitErrs *errs=0 );
  void move(const double xyz[3]);
  void merge(const StvNodePars &other,double fak=0.5 );
  void add(const StvELossTrak *el,double len);
  
  void getMom(double p[3]) const; 
  void getDir(double d[3]) const; 
const double *dir() const {return _d;}
const double *pos() const {return _x;}
      double *pos()       {return _x;}
const double *mag() const {return _h;}
double getSign() const	  {return _x[0]*_d[0]+_x[1]*_d[1];}
double getP()    const;
double getP2()   const;
double getRxy2() const;
double getRxy()  const;
double getZ()    const { return _x[2];};
double getSinL() const { return _d[2];};
double getCosL() const { return sqrt((1.-_d[2])*(1.+_d[2]));};
double getTanL() const { return _d[2]/getCosL();};
double getSinP() const { return _d[1]/getCosL();};
double getCosP() const { return _d[0]/getCosL();};
double getPsi()  const { return atan2(_d[1],_d[0]);};
double getPtin() const { return _pinv/getCosL()   ;};
double getPt()   const { return 1./fabs(getPtin());};
double getCurv() const ;
   int getDir() const;
const TkDir_t &getTkDir() const {return _tkdir;}
      TkDir_t &getTkDir()       {return _tkdir;}
  void reverse(); 
  void make2nd(); 
  void operator+=(const StvFitPars &fp);
  const StvFitPars &operator-(const StvNodePars &fp) const;
StvFitPars delta() const;
//		typical variations of parametrs
// StvFitPars delta() const;
// StvFitErrs deltaErrs() const;
double diff(const StvNodePars &other, const StvFitErrs &otherr) const;

operator const double  *() const {return _x;}
operator       double  *()       {return _x;}
    int getCharge() const { return (_pinv > 0) ? -1 : 1;}

   int check(const char *pri=0  ) const;
  void print(const char *name="") const;
  void GetRadial (double radPar[6],double *radErr=0  ,const StvFitErrs *fE=0,double der[5][5]=0) const;
  void GetImpact (StvImpact *imp                     ,const StvFitErrs *fE=0,double der[5][5]=0) const;
private:
  void update();
public:	
  double _x[3];
  double _d[3];
  double _pinv;
  /// magnetic field in units Pt(Gev) = Hz * RCurv(cm)
  double _h[4];  //mag field + _h[3]= module of mag field
TkDir_t _tkdir;    


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
enum {kImp=0,kImpZ=1,kImpPsi=2,kImpPti=3,kImpTan=4,kImpCur=5};
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
    float  mZImp,   mZZ;
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
class StvFitParsCentral
{
public:	
  StvFitParsCentral(){;}
operator const double *() const	{return &mH;}
operator       double *() 	{return &mH;}
public:	
// Let (Dx,Dy,Dz) vector track direction
// It could be also represented:
// (cos(L)*cos(A),cos(L)*sin(A),sin(L))

// mH: movement along (-Dy    ,Dx     ,           0) vector
// mZ: movement along (-Dx*Dz , -Dz*Dy, Dy*Dy+Dx*Dx)
// Or mH: along (-cos(A),sin(A),0)
//    mZ: along (-sin(L)*cos(A),-sin(L)*sin(A), cos(L))
void Set(TkDir_t &tkd,StvFitPars &fitPars);


double mH;	// direction perpendicular movement and Z
double mZ;	// Pseudo Z, direction perpendicular movement & H
double mA;	// Angle in XY. cos(A),sin(A),T moving direction
double mL;	// Angle lambda in Rxy/Z
double mP;	// 1/pt with curvature sign
};

//------------------------------------------------------------------------------
class StvFitErrsCentral
{
public:	
  enum eFitErrs {kNErrs=15};
  StvFitErrsCentral(){;}
operator const double *() const { return &mHH;}
operator       double *()       { return &mHH;}
void Set(const StvNodePars &p,const StvFitErrs &e);
public:	
//  dH: along ort to dir and in Track/Z plane
//  dZ: ort to dH and in plane dH,Zaxis;When lamda=0 it is Zaxis 
//  dA: delta azimuth angle; 
//  dP: == d(1./Pt) where Pt is signed as curvature;  
//  dL = dLambda, angle between track and X,Y plane


public:
double 
mHH,
mHZ, mZZ,
mHA, mZA, mAA,
mHL, mZL, mAL, mLL,
mHP, mZP, mAP, mLP, mPP;
};  



//------------------------------------------------------------------------------
class StvNodeParsTest
{
public:
virtual ~StvNodeParsTest(){}
static void Test();
static void TestDerRadial();
static void TestGetRadial(int nEv=10000);
static void TestErrProp  (int nEv=10000);
static void TestDerImpact();
static void TestGetImpact(int nEv=10000) ;
static void TestCentral(int nEv=1000);
ClassDef(StvNodeParsTest,0)
};

//------------------------------------------------------------------------------
// 		StvNodePars::inlines
inline void StvNodePars::reset(){memset(this,0,sizeof(StvNodePars));}
//------------------------------------------------------------------------------
inline void StvNodePars::getMom(double p[3]) const 
{ 
  double mom = getP();
  p[0]=_d[0]*mom; p[1]=_d[1]*mom; p[2]=_d[2]*mom; 
}
//------------------------------------------------------------------------------
inline double StvNodePars::getRxy2() const 
{ 
   return (_x[0]*_x[0]+_x[1]*_x[1]);
}
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
inline double StvNodePars::getRxy() const 
{ 
   return sqrt(_x[0]*_x[0]+_x[1]*_x[1]);
}
inline void StvNodePars::getDir(double d[3]) const 
{ 
  memcpy(d,_d,sizeof(_d));
}
//------------------------------------------------------------------------------
inline double StvNodePars::getP() const 
{ 
return fabs(1./_pinv);
}
//------------------------------------------------------------------------------
inline double StvNodePars::getP2() const 
{ 
return fabs(1./(_pinv*_pinv));
}


//------------------------------------------------------------------------------
inline void StvNodePars::reverse() 
{
  _pinv = -_pinv;
  for (int i=0;i<3;i++) {
   _d[i]       =-_d[i]; 
   _tkdir.Backward();
  }
}
//------------------------------------------------------------------------------
inline double vsuma(double *a,int na) 
{ double sum = 0.; 
  for (int i=0;i<na;i++) { sum += fabs(a[i]);} 
  return sum;
}
#endif
