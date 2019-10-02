#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "TVector3.h"
#include "TCernLib.h"
#include "TMath.h"
#include "TMath.h"
#include "StarRoot/THelixTrack.h"
#include "StarRoot/THelix3d.h"
#include "StarRoot/TRungeKutta.h"
#include "StvUtil/StvNodePars.h"
#include "StvUtil/StvELossTrak.h"
#include "StvUtil/StvDebug.h"
#include "Stv/StvToolkit.h"
static const double kMaxPti=200,kMaxCurv=(0.000299792458 * 4.98478)*kMaxPti,kMaxEta = 6;
//static const double kMaxLamda = 3.14159265358/2-atan(exp(-kMaxEta))*2;
//static const double kMaxTanL  = tan(kMaxLamda);

enum {kHf,kZf,kAf,kLf,kPf};
enum {kHh,kAh,kCh,kZh,kLh};
  static const int idx66[6][6] =
  {{ 0, 1, 3, 6,10,15},{ 1, 2, 4, 7,11,16},{ 3, 4, 5, 8,12,17}
  ,{ 6, 7, 8, 9,13,18},{10,11,12,13,14,19},{15,16,17,18,19,20}};
static const double recvCORRMAX  = 0.99999;
static const double chekCORRMAX  = 0.99999;

//                               X   Y   Z Dx Dy Dz Pinv
static double MAXNODPARS[]   ={555,555,555, 1, 1, 1, 100};
//                                   h    z   a     l   ptin
static const double MAXFITPARS[]   ={1.0 ,1.0,0.5 ,0.5 ,kMaxPti  };
static const double BIGFITPARS[]   ={0.1 ,0.1,0.1 ,0.1 ,0.01},BIGFITPART=0.01;
static const double kERRFACT     = 3*3;
static const double kFitErrs[5]   ={3,3 
                                   ,10./180*M_PI
				   ,10./180*M_PI
				   ,kMaxPti};
static const double kPiMass=0.13956995;
static const double kMinP = 0.01,kMinE = sqrt(kMinP*kMinP+kPiMass*kPiMass);
static const double kMaxCorr = 0.1;

//_____________________________________________________________________________
inline static double dot(const double *a,const double *b)
{return a[0]*b[0]+a[1]*b[1]+a[2]*b[2];}

//_____________________________________________________________________________
inline static double dot2(const double *a,const double *b)
{return a[0]*b[0]+a[1]*b[1];}

//_____________________________________________________________________________
inline static void sub(const double a[3],const double b[3],double c[3])
{
  c[0] = a[0]-b[0]; c[1] = a[1]-b[1]; c[2] = a[2]-b[2];
}
//_____________________________________________________________________________
inline static void mpy(const double a[3],double fak,double c[3])
{
  c[0] = fak*a[0]; c[1] = fak*a[1]; c[2] = fak*a[2];
}

//______________________________________________________________________________ 
void Multiply(Mtx55D_t &res, const Mtx55D_t &A,const Mtx55D_t &B)
{
  memset(res[0],0,5*5*sizeof(res[0][0]));
  for (int j=0;j<5;j++) {
    const double *Bj = B[j];
    for (int i=0;i<5;i++) {
    const double &aij = A[i][j];
    if (!aij) continue;
          double *resi =res[i];
    for (int k=0;k<5;k++) {
      const double &bjk = Bj[k];
      resi[k] += aij*bjk;
  } } }
}
//______________________________________________________________________________ 
void Multiply(double *res, const Mtx55D_t &A,const double *B)
{
  for (int i=0;i<5;i++) {
    double S = 0;
    const double *Ai = A[i];
    for (int j=0;j<5;j++) {if (!Ai[j]) continue;S += Ai[j]*B[j];}
    res[i]=S;
  }
}
//______________________________________________________________________________ 
void Invert(Mtx55D_t &to, const Mtx55D_t &fr)
{
assert(0);
}
//______________________________________________________________________________ 
void Testik(const Mtx55D_t &tt)
{
   TMatrixD T(5,5,tt[0]);
   TMatrixD Tt(T); Tt.T();
   TMatrixD T1 = Tt*T;
   printf("Det = %g ============================\n",T.Determinant());
   T.Print();
   T1.Print();
}
//______________________________________________________________________________
int StvNodePars::check(const char *pri) const
{
  if (fabs(_x[0]+_x[1]+ _x[2])<=0) return 0;
  assert(_h[2]);
  int ierr=0;
  for (int i=0;i<7;i++){if (fabs(_x[i]) > MAXNODPARS[i]){ierr = i+ 1;	goto FAILED;}} 
  return 0;
FAILED: 
  assert(ierr<1000);
  if (!pri ) return ierr;
  printf("StvNodePars::check(%s) == FAILED(%d)\n",pri,ierr);  print();
  return ierr;
} 

//______________________________________________________________________________
double StvNodePars::getCurv() const 
{
  double DH = dot(_d,_h)/_h[3]; double myCosL = sqrt((1-DH)*(1+DH));
  return  _h[3]*_pinv/myCosL;
}
//______________________________________________________________________________
void StvNodePars::set(const double h[3]) //set mag field
{

  _h[0] = h[0]; _h[1] = h[1];_h[2] = h[2];
  _h[3] = sqrt(h[0]*h[0]+h[1]*h[1]+h[2]*h[2]);
assert(_h[3]);
  THelix3d::MakeTkDir(_d,_h,_tkdir);
}
//______________________________________________________________________________
void StvNodePars::set(const double x[3],const double d[3],double pinv,const double h[3])
{
 if (x) {_x[0] = x[0]; _x[1] = x[1];_x[2] = x[2];}
 if (d) {
   _d[0] = d[0]; _d[1] = d[1];_d[2] = d[2];
   double nor = d[0]*d[0]+d[1]*d[1]+d[2]*d[2];
   if (fabs(nor-1)>1e-6) {
     nor = sqrt(nor);
     _d[0]/=nor;_d[1]/=nor;_d[2]/=nor;
  } }
 if (pinv) _pinv = pinv;
 
 if (h)set(h);
}
//______________________________________________________________________________
void StvNodePars::set(const THelix3d *th)
{
  memcpy(_x,th->Pos(),sizeof(_x));   
  memcpy(_d,th->Dir(),sizeof(_d));   
  memcpy(_h,th->Mag(),sizeof(_h));   
assert(fabs(dot(_d,_d)-1)<1e-4);
  int iQ = th->Charge();
  _pinv = -iQ/th->MomTot();
  THelix3d::MakeTkDir(_d,_h,_tkdir);

}
//______________________________________________________________________________
void StvNodePars::set(const THelixTrack *th)
{
static StvToolkit *kit = StvToolkit::Inst();
  memcpy(_x,th->Pos(),sizeof(_x));   
  memcpy(_d,th->Dir(),sizeof(_d));   
  kit->GetMag(_x,_h); set(_h);
  _pinv = th->GetRho()*th->GetCos()/(_h[2]);
  THelix3d::MakeTkDir(_d,_h,_tkdir);
}
//______________________________________________________________________________
void StvNodePars::set(const TRungeKutta *th)
{
  memcpy(_x,th->Pos(),sizeof(_x));   
  memcpy(_d,th->Dir(),sizeof(_d));   
assert(fabs(dot(_d,_d)-1)<1e-4);
  set(th->Mag());   
  int iQ = th->Charge();
  _pinv = -iQ/th->MomTot();
  THelix3d::MakeTkDir(_d,_h,_tkdir);

}
//______________________________________________________________________________
//______________________________________________________________________________
void StvNodePars::get(TRungeKutta *th) const
{
  int iQ = getCharge();
  double P = fabs(1./_pinv);

  double mom[3]= { _d[0]*P, _d[1]*P, _d[2]*P };
  th->Set(iQ,_x,mom);
  th->ZetMag(_h);
}
//______________________________________________________________________________
void StvNodePars::get(THelix3d *th) const
{
  int iQ = getCharge();
  double P = fabs(1./_pinv);

  double mom[3]= { _d[0]*P, _d[1]*P, _d[2]*P };
  th->Set(iQ,_x,mom,_h);
}
//______________________________________________________________________________
void StvNodePars::move(double len, StvFitErrs *err) 
{
static THelix3d th;
  get(&th); 
  if (err) err->Get(&th);
  th.Move(len);
  set(&th); 
  if (err) err->Set(&th);
}

//______________________________________________________________________________
void StvNodePars::move(const double xyz[3]) 
{
static THelix3d th;
  get(&th); 
  double len = th.Path(xyz);
  th.Move(len);
  set(&th); 
}
//______________________________________________________________________________
const StvFitPars &StvNodePars::operator-(const StvNodePars& what) const
{
static StvFitPars fp;
  double dif[3];
  sub(_x,what._x,dif);
  fp.mU = dot(dif,_tkdir[kKU]);
  fp.mV = dot(dif,_tkdir[kKV]);
  sub(_d,what._d,dif);
  fp.mFita = dot(dif,_tkdir[kKU]);
  fp.mLama = dot(dif,_tkdir[kKV]);
  fp.mPinv = _pinv - what._pinv;
  return fp;
}
//______________________________________________________________________________
void StvNodePars::operator+=(const StvFitPars &fp)
{
static StvToolkit *kit = StvToolkit::Inst();  

  if (_tkdir[0][0]>1) THelix3d::MakeTkDir(_d,_h,_tkdir);
  for (int i=0;i<3;i++) {
    _x[i]+= _tkdir[kKU][i]*fp.mU    + _tkdir[kKV][i]*fp.mV; 
    _d[i]+= _tkdir[kKU][i]*fp.mFita + _tkdir[kKV][i]*fp.mLama;  
  }
  _pinv += fp.mPinv;  
  double nor = dot(_d,_d);
  if (fabs(nor-1)>1e-5) {
    nor = sqrt(nor);
    _d[0]/=nor; _d[1]/=nor; _d[2]/=nor;
  }
  kit->GetMag(_x,_h); set(_h);
  THelix3d::MakeTkDir(_d,_h,_tkdir);
}
//_____________________________________________________________________________
void StvNodePars::add(const StvELossTrak *el,double len)
{    
  if (!el) return;;
  double fakLen = fabs(len/el->Len());
  double pinv = fabs(_pinv);
  double dpin = el->PinvLoss()*fakLen;
  if (len>0) dpin = -dpin;
  pinv += dpin;
  _pinv = (_pinv>0) ? pinv:-pinv;
}
//______________________________________________________________________________
void StvNodePars::print(const char *name) const
{
static const char* tit[]={"X","Y","Z","Dx","Dy","Dz","Pinv",0};
  printf("StvNodePars(%s) Rxy = %g",name,getRxy());
  for (int i=0;i<kNPars+1;i++) {printf("%s = %g, ",tit[i],(*this)[i]);}
  printf("\n");
}   
//______________________________________________________________________________
void StvNodePars::update() 
{
}   
//_____________________________________________________________________________
StvFitPars StvNodePars::delta() const
{
   double spaceR = 0.1+(fabs(_x[0])+fabs(_x[1]))/200;
   double spaceZ = 0.1+fabs(_x[2])/200;
   StvFitPars fp;
   fp.mU = spaceR;    fp.mV = spaceZ; 
   fp.mFita = fp.mLama = 3.14/180*10;	//ten degree
   fp.mPinv = fabs(_pinv)*0.1+1e-2;
   return fp;
}
//_____________________________________________________________________________
int StvNodePars::getDir() const
{
  return (_x[0]*_d[0]+_x[1]*_d[1] <0)? 0:1;
}
// //_____________________________________________________________________________
// StvFitPars StvNodePars::delta() const
// {
//    double spaceR = 0.1+sqrt(_x[0]*_x[0]+_x[1]*_x[1])/200;
//    double spaceZ = 0.1+fabs(_x[2])/200;
//    StvFitPars fp;
//    fp.mH = spaceR;    fp.mZ = spaceZ; 
//    fp.mA = fp.mL = 3.14/180*30;	//ten degree
//    fp.mP = fabs(_ptin)*0.2;
//    if (fp.mP<0.1) fp.mP = 0.1;
//    return fp;
// }
// //_____________________________________________________________________________
// StvFitErrs StvNodePars::deltaErrs() const
// {
//    StvFitPars dlt = delta();
//    StvFitErrs fe;
//    for (int i=0,li=0;i< 5;li+=++i) {fe[li+i] = kERRFACT*dlt[i]*dlt[i];}
//    fe.mHz = _hz;
//    return fe;
// }


//_____________________________________________________________________________
void StvNodePars::merge(const StvNodePars &other,double wt)
{
  int sizeD = &_tkdir[0][0] - &_x[0];
  TCL::vlinco(_x,wt,other._x,1.-wt,_x,sizeD);
  double nor = sqrt(TCL::vdot(_d,_d,3));
  TCL::vscale(_d,1./nor,_d,3);
  THelix3d::MakeTkDir(_d,_h,_tkdir);
}
//_____________________________________________________________________________
void StvFitErrs::Add(const StvELossTrak *el,double len)
{    
  if (!el) return;
  double fakLen = (len)? fabs(len/el->Len()):1.;
  mFF+= el->Theta2() 		*fakLen;
  mLL+= el->Theta2()  		*fakLen;
  mUU+= el->Ort2() 		*fakLen;
  mVV+= el->Ort2() 		*fakLen;
  mPP+= el->PinvErr2()		*fakLen;
}
//_____________________________________________________________________________
double StvNodePars::diff(const StvNodePars &other,const StvFitErrs &otherr) const 
{ 
  double dx[3],dn[3],p[5];
  TCL::vsub(other._x,_x,dx,3);
  TCL::vsub(other._d,_d,dn,3);
  p[0] = TCL::vdot(dx,(_tkdir)[kKU],3);
  p[1] = TCL::vdot(dx,(_tkdir)[kKV],3);
  p[2] = TCL::vdot(dn,(_tkdir)[kKU],3);
  p[3] = TCL::vdot(dn,(_tkdir)[kKV],3);
  p[4] = other._pinv - _pinv;

  double myMax=0;
  const double *ee = otherr;
  for (int i=0,li=0;i< 5;li+=++i) {
    double d = p[i]*p[i]/(ee[li+i]+1e-11);
    if (myMax<d) myMax=d;
  }
  return myMax;
}

//______________________________________________________________________________
//______________________________________________________________________________
void StvHitErrs::rotate(double angle)
{  double t[2][2];
  t[0][0] = cos(angle); t[0][1] = -sin(angle);
  t[1][0] = -t[0][1]  ; t[1][1] = t[0][0];
  double r[3];
  TCL::trasat(t[0],&hXX,r,2,2);
  TCL::ucopy(r,&hXX,3);
}


//______________________________________________________________________________
// //______________________________________________________________________________
// StvFitErrs::StvFitErrs(double hh,double hz,double zz)
// {
//   memset(this,0,sizeof(*this));
//   mHH=hh;mHZ=hz;mZZ=zz;mHz = 3e33;
//   assert(&mPP-&mHH+1==15);
// }
//______________________________________________________________________________
double StvFitErrs::Sign() const {return EmxSign(5,*this);}
//______________________________________________________________________________
StvFitErrs& StvFitErrs::operator= (const THEmx3d_t &emx) 
{
  (THEmx3d_t&)*this = emx;
  return *this;
}      
//______________________________________________________________________________
StvFitErrs& StvFitErrs::operator= (const TkDir_t &tkdir) 
{
  memcpy(mTkDir[0],tkdir[0],sizeof(mTkDir));
  return *this;
}      
//______________________________________________________________________________
const StvFitErrs &StvFitErrs::operator*(const StvFitDers &how) const
{
static StvFitErrs myFitErrs;
  TCL::trasat(how[0],*this,myFitErrs,5,5);
  TCL::ucopy(how.TkDir(1)[0],myFitErrs.mTkDir[0],3*3);
  myFitErrs.Recov();
  return myFitErrs;
}  
//______________________________________________________________________________
 double StvFitErrs::PtErr(const StvNodePars &pars) const
{
  double pinv = pars._pinv;
  double cosL = pars.getCosL();
  double sinL = pars.getSinL();
  double dir[5]={0};
  dir[3] = sinL/pinv;
  dir[4] =-cosL/(pinv*pinv);
  double ptrr=0;
  TCL::trasat(dir,*this,&ptrr,1,5);
  return ptrr;
}

// //______________________________________________________________________________
// void StvFitErrs::Reset(double hz)
// {
//   memset(this,0,sizeof(*this));
//   mHH =  kFitErrs[0]*kFitErrs[0];
//   mZZ =  kFitErrs[1]*kFitErrs[1];
//   mAA =  kFitErrs[2]*kFitErrs[2];
//   mLL =  kFitErrs[3]*kFitErrs[3];
//   mPP =  kFitErrs[4]*kFitErrs[4];
//   mHz =  hz;
// }

//______________________________________________________________________________
void StvFitErrs::Set(const THelixTrack *he)
{
  static StvToolkit *kit = StvToolkit::Inst();
  double h[3];
  kit->GetMag(he->Pos(),h);
  THelix3d::MakeTkDir(he->Dir(),h,mTkDir);
  THelix3d::ConvertErrs(he,h[2],*this);
}  
//______________________________________________________________________________
void StvFitErrs::Set(const THelix3d *he)
{
const THEmx3d_t *emx = he->Emx();
 assert(emx);
 *this = *emx;
 assert(mUU>0);
 assert(mVV>0);
 Recov();
}  
//______________________________________________________________________________
void StvFitErrs::Get(THelix3d *he) const
{
  he->SetEmx();
  THEmx3d_t *emx = he->Emx();
  assert(emx);
  *emx = *this;
}  
//______________________________________________________________________________
void StvFitErrs::Get(TRungeKutta *he) const
{
  he->SetEmx();
  THEmx3d_t *emx = he->Emx();
  assert(emx);
  *emx = *this;
}  
//______________________________________________________________________________
void StvFitErrs::Set(const TRungeKutta *he) 
{
const THEmx3d_t *emx = he->Emx();
 assert(emx);
 *this = *emx;
 assert(mUU>0);
 assert(mVV>0);
 Recov();
}  
//_____________________________________________________________________________
int StvFitErrs::Check(const char *tit) const
{
//  if (mUU+mVV<=0) 		return 0;
//  ((StvFitErrs*)((void*)this))->Recov();
  int ierr=0;
  double dia[5];const double *e=&mUU;
  for (int i=0,li=0;i< 5;li+=++i) {
    dia[i]=e[li+i];
    if (dia[i]> 1e+4*kFitErrs[i]*kFitErrs[i]) {ierr = i+1; goto ERR;}
    for (int j=0;j<i;j++) {
       if (e[li+j]*e[li+j]>=dia[i]*dia[j]){ierr = 100+10*i+j;goto ERR;}
    } }
  return 0;

ERR: if (!tit) return ierr;
  printf("StvFitErrs::Check(%s)=%d\n",tit,ierr);
  Print(tit);
  return ierr;
}     
//_____________________________________________________________________________
int StvFitErrs::Recov()
{

  double dia[5],fak[5];double *e=*this;

  int nerr=0;
//		Check diag errs
  for (int i=0,li=0;i< 5;li+=++i) {
    fak[i]=1;
    if (e[li+i] < kFitErrs[i]*kFitErrs[i]) continue;
    fak[i] = 0.99*kFitErrs[i]/sqrt(e[li+i]); nerr++;
  };
  if (nerr) {  		//Recovery
    for (int i=0,li=0;i< 5;li+=++i) {
      for (int j=0;j<=i;j++) {
        e[li+j]*=fak[i]*fak[j];
  } } }
static int jkl=0;
if (jkl) return nerr;
  int jerr=0;
//		Check correlations & Recovery
  for (int i=0,li=0;i< 5;li+=++i) {
    fak[i]=1;
    dia[i]=e[li+i];
    for (int j=0;j<i;j++) {
       if (fabs(e[li+j]*e[li+j])<=dia[i]*dia[j]*chekCORRMAX) continue ;
       double qwe = sqrt(fabs(e[li+j]*e[li+j])/(dia[i]*dia[j]*chekCORRMAX));
       jerr++;
       if (fak[i]<qwe)fak[i]=qwe;
       if (fak[j]<qwe)fak[j]=qwe;
  } }
  if (!jerr) return nerr;
  for (int i=0,li=0;i< 5;li+=++i) {
    if (fak[i]<=1) continue;
    e[li+i]*=fak[i];
  }
  
  return nerr+jerr;
}     
//_____________________________________________________________________________
int StvFitErrs::Sigre() const
{
/// signature of error(covariant) matrix. 0=correct
  int n = 0;
  if (mUF<0) n+=1;
  if (mUP<0) n+=10;
  if (mVL<0) n+=100;
  if (mFP<0) n+=1000;
  return n;
}
//_____________________________________________________________________________
 void StvFitErrs::Print(const char *tit) const
{
static const char *N="UVFLP";
  if (!tit) tit = "";
  printf("StvFitErrs::Print(%s) ==\n",tit);
  const double *e = *this;
  for (int i=0,li=0;i< 5;li+=++i) {
    printf("%c ",N[i]);
    for (int j=0;j<=i;j++) {
    printf("%g\t",e[li+j]);} 
    printf("\n");
  }
}
//_____________________________________________________________________________
StvFitPars::StvFitPars():mU(0),mV(0),mFita(0),mLama(0),mPinv(0)
{
}
//_____________________________________________________________________________
StvFitPars::StvFitPars(double u,double v):mU(u),mV(v),mFita(0),mLama(0),mPinv(0)
{
}
//_____________________________________________________________________________
StvFitPars::StvFitPars(const double *arr) 
{
 memcpy(&mU,arr,5*sizeof(mU));
}
//_____________________________________________________________________________
int StvFitPars::Check(const char *tit) const
{
  int ifail = 0;
  const double *p = *this; 
  for (int i=0;i<5;i++) {if (fabs(p[i]) > MAXFITPARS[i]){ifail=i+1;break;}};
  if (!ifail) return 0;
  if (!tit || !tit[0]) return ifail;
  TString ts(tit);ts +=" *** Check = "; ts+=ifail;ts +=" ***";
  Print(ts.Data());
  return 0;
}
//_____________________________________________________________________________
void StvFitPars::Print(const char *tit) const
{
static const char* Nams[]={"mU","mV","mFita","mLama","mPinv",0};
  if (tit && tit[0]) printf("StvFitPars::Print(%s)\n",tit);
  for (int i=0;Nams[i]; i++) {printf("%s=%g ",Nams[i],(*this)[i]);}
  printf("\n");
}
//_____________________________________________________________________________

//_____________________________________________________________________________
/**
   returns the node information
   double x[6],  : state, for a definition, in radial implementation
                   rad  - radius at start (cm). See also comments
                   phi  - azimuthal angle  (in rad)      
                   z    - z-coord. (cm)                 
                   tanl - tan(dip) =pz/pt               
                   psi  - azimuthal angle of pT vector (in rads)     
                   pti  - signed invert Pt.//  pti = curv/hz 
   double cc[15] : error matrix of the state "x" rad is fixed
                       code definition adopted here, where:
   PhiPhi;
   ZPhi     ,ZZ;                       
   TanlPhi  ,TanlZ ,TanlTanl,                 
   PhiPsi   ,ZPsi  ,TanlPsi , PsiPsi ,           
   PhiPti  ,ZPti ,TanlPti, PsiPti, PtiPti     

*/
//_____________________________________________________________________________
void StvNodePars::GetRadial(double radPar[6],double radErr[15],const StvFitErrs *fitErr, double der[5][5]) const
{
/// This is GetRadial for primary track. Radial representation of errors
/// is senseless for primary. But our oldfashiond TPT format demanding it. 
/// All space errors supposed to be zeros.

//Remind StvFitPars:
//double mU;	// direction perpendicular movement and Z
//double mV;	// Pseudo Z, direction perpendicular movement & H
//double mFita;	// Angle in XY. cos(A),sin(A),T moving direction
//double mLama;	// Angle lambda in Rxy/Z
//double mPinv;	// 1/p with curvature sign

// X = X0 + tkDir[kKU][0]*mU+tkDir[kKV][0]*mV
// Y = Y0 + tkDir[kKU][1]*mU+tkDir[kKV][1]*mV
// Z = Z0 + tkDir[kKU][2]*mU+tkDir[kKV][2]*mV
// Phi = atan2(Y,X)
// 
// Dx = tkDir[kKT][0] + tkDir[kKU][0]*mFita+ tkDir[kKV][0]*mLama
// Dy = tkDir[kKT][1] + tkDir[kKU][1]*mFita+ tkDir[kKV][1]*mLama
// Dz = tkDir[kKT][2] + tkDir[kKU][2]*mFita+ tkDir[kKV][2]*mLama
// 
// Tan = Dz/sqrt(Dx*Dx+Dy*Dy)
// Psi = atan2(Dy,Dx);
// cosL = 1./sqrt(1+tanL*tanL)
// Pti = Pinv/cosL;

//   dXdU = tkDir[kKU][0]
//   dXdV = tkDir[kKV][0]
//   dYdU = tkDir[kKU][1]
//   dYdV = tkDir[kKV][1]
//   dZdU = tkDir[kKU][2]
//   dZdV = tkDir[kKV][2]
//   dPhi_dX = -Y/(X*X+Y*Y)
//   dPhi_dY =  X/(X*X+Y*Y)
//   dPhi_dU = dPhi_dX*dXdU + dPhi_dY*dYdU
//   dPhi_dV = dPhi_dX*dXdV + dPhi_dY*dYdV
// 
//   dDx_dFita = tkDir[kKU][0]
//   dDx_dLama = tkDir[kKV][0]
// 
//   dDy_dFita = tkDir[kKU][1]
//   dDy_dLama = tkDir[kKV][1]
// 
//   dDz_dFita = tkDir[kKU][2]
//   dDz_dLama = tkDir[kKV][2]
// 
//   D2xy = (Dx*Dx+Dy*Dy);  Dxy = sqrt(D2xy)
//   Dxy_dFita = -(Dx_dFita*Dx+Dy_dFita*Dy)/Dxy
//   Dxy_dLama = -(Dx_dLama*Dx+Dy_dLama*Dy)/Dxy
// 
//   dTan_dFita = Dz_dFita/Dxy - Dz*Dxy_dFita/(D2xy)
//   dTan_dLama = Dz_dLama/Dxy - Dz*Dxy_dLama/(D2xy)

//   dPsi_dDx = -Dy/(Dx*Dx+Dy*Dy)
//   dPsi_dDy =  Dx/(Dx*Dx+Dy*Dy)
//   dPsi_dFita = dPsi_dDx*dDx_dFita + dPsi_dDy*dDy_dFita
//   dPsi_dLama = dPsi_dDx*dDx_dLama + dPsi_dDy*dDy_dLama

//   CosInv = sqrt(1+Tan*Tan)
//   dCosInv_dFita = Tan/CosInv*dTan_dFita
//   dCosInv_dLama = Tan/CosInv*dTan_dLama
// 
//   dPti_dPinv = CosInv
//   dPti_dFita = Pinv*dCosInv_dFita
//   dPti_dLama = Pinv*dCosInv_dLama
// 

  enum {jRad =0,jPhi   ,jZ  ,jTan,jPsi,jPti};

  double r2xy = _x[0]*_x[0]+_x[1]*_x[1], rxy=sqrt(r2xy);
  double d2xy = _d[0]*_d[0]+_d[1]*_d[1], dxy=sqrt(d2xy);
  double tanL = _d[2]/dxy;
  double psi  = atan2(_d[1],_d[0]);
  double cosL = 1./sqrt(1+tanL*tanL);

  radPar[jRad] = rxy;
  radPar[jPhi] = atan2(_x[1],_x[0]);
  radPar[jZ  ] = _x[2];
  radPar[jTan] = tanL;
  radPar[jPsi] = psi;
  radPar[jPti] = _pinv/cosL;
  if (!radErr && !der) return;

  double  X  = _x[0],Y =_x[1];
  double  Dx = _d[0],Dy=_d[1],Dz=_d[2];
  double  Tan  = tanL, Pinv = _pinv;
  double  XTk0 = dot2(_x,_tkdir[kKU]);
  double  XTk1 = dot2(_x,_tkdir[kKV]);
  double  XTk2 = dot2(_x,_tkdir[kKT]);
  double  sub0 = XTk0/XTk2;
  double  sub1 = XTk1/XTk2;

  double  dXdU = _tkdir[kKU][0]-sub0*_tkdir[kKT][0];
  double  dYdU = _tkdir[kKU][1]-sub0*_tkdir[kKT][1];
  double  dZdU = _tkdir[kKU][2]-sub0*_tkdir[kKT][2];
 
  double  dXdV = _tkdir[kKV][0]-sub1*_tkdir[kKT][0];
  double  dYdV = _tkdir[kKV][1]-sub1*_tkdir[kKT][1];
  double  dZdV = _tkdir[kKV][2]-sub1*_tkdir[kKT][2];

  double  dPhi_dX = -Y/(X*X+Y*Y);
  double  dPhi_dY =  X/(X*X+Y*Y);
  double  dPhi_dU = dPhi_dX*dXdU + dPhi_dY*dYdU;
  double  dPhi_dV = dPhi_dX*dXdV + dPhi_dY*dYdV;

  double  dDx_dFita = _tkdir[kKU][0];
  double  dDx_dLama = _tkdir[kKV][0];

  double  dDy_dFita = _tkdir[kKU][1];
  double  dDy_dLama = _tkdir[kKV][1];

  double  dDz_dFita = _tkdir[kKU][2];
  double  dDz_dLama = _tkdir[kKV][2];

  double  D2xy = (Dx*Dx+Dy*Dy),  Dxy = sqrt(D2xy);
  double  dDxy_dFita = (dDx_dFita*Dx+dDy_dFita*Dy)/Dxy;
  double  dDxy_dLama = (dDx_dLama*Dx+dDy_dLama*Dy)/Dxy;

  double  dPsi_dDx   = -Dy/D2xy;
  double  dPsi_dDy   =  Dx/D2xy;
  double  dPsi_dFita = dPsi_dDx*dDx_dFita + dPsi_dDy*dDy_dFita;
  double  dPsi_dLama = dPsi_dDx*dDx_dLama + dPsi_dDy*dDy_dLama;


  double  dTan_dFita = dDz_dFita/Dxy - Dz*dDxy_dFita/(D2xy);
  double  dTan_dLama = dDz_dLama/Dxy - Dz*dDxy_dLama/(D2xy);
  double  CosInv = sqrt(1+Tan*Tan);
  double  dCosInv_dFita = Tan/CosInv*dTan_dFita;
  double  dCosInv_dLama = Tan/CosInv*dTan_dLama;

  double  dPti_dPinv = CosInv;
  double  dPti_dFita = Pinv*dCosInv_dFita;
  double  dPti_dLama = Pinv*dCosInv_dLama;
 

double T[5][5] = 
/*                       U,       V,       Fita,       Lama,      Pinv*/
/*--------------------------------------------------------------------*/
/*jRPhi*/	{{ dPhi_dU, dPhi_dV,          0,          0,          0}
/*jZ   */	,{    dZdU,    dZdV,          0,          0,          0}
/*jTan */	,{       0,       0, dTan_dFita, dTan_dLama,          0}
/*jPsi */	,{       0,       0, dPsi_dFita, dPsi_dLama,          0}
/*jPti */	,{       0,       0, dPti_dFita, dPti_dLama, dPti_dPinv}};
/*--------------------------------------------------------------------*/
  if (der) TCL::ucopy(T[0],der[0],5*5);
  if (radErr) TCL::trasat(T[0],*fitErr,radErr,5,5); 

}
//_____________________________________________________________________________
void StvNodePars::make2nd()
{
///	make second order
  double curv = getCurv();
  double sinL = dot(_h,_tkdir[kKT])/_h[3];
  double cosL = sqrt((1.+sinL)*(1-sinL));
  double alfa = cosL/_h[3];
  double ort[3];
  for (int i=0;i<3;i++) {ort[i] = _tkdir[kKT][i]-_h[i]*alfa;}
  double nor = sqrt(dot(ort,ort));
  for (int k=0;k<3;k++) {
    _tkdir[kKdDdL][k] = 0.5*dot(ort,_tkdir[k])/nor*curv*cosL*cosL;
  }

}

//______________________________________________________________________________
const StvFitPars &StvFitPars::operator*(const StvFitDers &t) const  
{
static StvFitPars myPars;
  TCL::vmatl(t[0],*this,myPars,5,5);
  return myPars;
}
//______________________________________________________________________________
StvImpact::StvImpact() {memset(this,0,sizeof(*this));}

//______________________________________________________________________________
void StvImpact::Print(const char *opt) const
{
  if (!opt) opt = "";
  printf("StvImpact::Print(%s) ==\n",opt);

static const char* tit[]={"Imp","Z  ","Psi","Pti","Cur",0};
  const auto* P=&mImp;
  for (int i=0;i<5;i++) {printf("%s = %g, ",tit[i],P[i]);}
  printf("\n");
  if (mImpImp<=0) return;
  const auto *e = &mImpImp;
  for (int i=0,li=0;i< 5;li+=++i) {
    printf("%s ",tit[i]);
    for (int j=0;j<=i;j++) {
    printf("%g\t",e[li+j]);} 
    printf("\n");
  }


}   
#if 0 //
//____________________________________________________________
double EmxSign(int n,const double *a) 
{
   double ans=3e33;
   double buf[55];
   double *B = (n<=10) ? buf : new double[n];
   double *b = B;
   // trchlu.F -- translated by f2c (version 19970219).
   //
   //see original documentation of CERNLIB package F112 

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
            if (sum<ans) ans = sum;
            if (sum<0.) goto RETN;
            dc = sqrt(sum);
            b[kpiv] = dc;
            if (r__ > 0.)  r__ = (double)1. / dc;
         }
         kpiv += j;
      }

   } while  (i__ < n);

RETN: if (B!=buf) delete B; 
   return ans;
} /* trchlu_ */
#endif //0

#if 1
//_____________________________________________________________________________
void StvFitErrsCentral::Set(const StvNodePars &p,const StvFitErrs &e)
{ 
  enum {kkH,kkZ, kkA  ,kkL  ,  kkPti};	//
  
const TkDir_t tkd = p.getTkDir();


  TVector3 T(tkd[kKT]);
  TVector3 U(tkd[kKU]);
  TVector3 V(tkd[kKV]);

  TVector3 Z(0,0,1);
  TVector3 H(T[1],-T[0],0); H.SetMag(1.);

// U*u + V*v + T*t= H*h + Z*z
// 
// H = (Hx,Hy,0)= (Ty,-Tx,0)*k = (Ty,-Tx,0)/sqrt(1-Tz*Tz)
// (H*T) ==0
// t = Tz*z 
// U*u + V*v = H*h + (Z-T*Tz)*z
// U*u + V*v + T*Tz*z = H*h + Z*z

// (H*U)*u + (H*V)*v  = h
// (Z*U)*u + (Z*V)*v  = z * (1-Tz*Tz)
//
// h =  (H*U)*u + (H*V)*v
// z = ((Z*U)*u + (Z*V)*v)/(1-Tz*Tz)

  double HxU = (H*U);
  double HxV = (H*V);
  double ZxU = (Z*U)/(T[0]*T[0]+T[1]*T[1]);
  double ZxV = (Z*V)/(T[0]*T[0]+T[1]*T[1]);
  double D[5][5] = {{0}};
  TCL::vzero(D[0],25); 
  D[kkH][kU] = HxU;
  D[kkH][kV] = HxV;
  D[kkZ][kU] = ZxU;
  D[kkZ][kV] = ZxV;

// U*fita + V*lama = 
  double sinL = T[2];
  double cos2L = (1-sinL)*(1+sinL),cosL = sqrt(cos2L);
  double cosF = T[0]/cosL;
  double sinF = T[1]/cosL;
assert(fabs(sinF*sinF+cosF*cosF-1)<1e-4);
  TVector3 PH(-cosL*sinF, cosL*cosF,   0);
  TVector3 DL(-sinL*cosF,-sinL*sinF,cosL);

// U*fita+V*lama = PH*dPhi + DL*dLam
// (PH*U)*fita +(PH*V)*lama = cosL**2*dPhi
// (DL*U)*fita +(DL*V)*lama =         dLam

  double PHxU = (PH*U),PHxV = (PH*V);
  double DLxU = (DL*U),DLxV = (DL*V);

  D[kkA][kFita] = PHxU/cos2L;
  D[kkA][kLama] = PHxV/cos2L;;
  D[kkL][kFita] = DLxU;
  D[kkL][kLama] = DLxV;


  D[kkPti][kPinv] = 1./cosL;
  double qwe = p._pinv/cos2L*sinL;
  D[kkPti][kFita]  = qwe*DLxU;
  D[kkPti][kLama]  = qwe*DLxV;

  TCL::trasat(D[0],e,*this,5,5);

}

//_____________________________________________________________________________
void StvFitParsCentral::Set(TkDir_t &tkd,StvFitPars &fitPars)
{

  TVector3 T(tkd[kKT]);
  TVector3 U(tkd[kKU]);
  TVector3 V(tkd[kKV]);
  double sinL = T[2];
  double cos2L = (1-sinL)*(1+sinL),cosL = sqrt(cos2L);

  TVector3 Z(0,0,1);
  TVector3 H(T[1],-T[0],0); H.SetMag(1.);
  mH = (U*fitPars.mU+ V*fitPars.mV).Dot(H); 
  mZ = (U*fitPars.mU+ V*fitPars.mV).Dot(Z)/(T[0]*T[0]+T[1]*T[1]); 

  double cosF = T[0]/cosL;
  double sinF = T[1]/cosL;
assert(fabs(sinF*sinF+cosF*cosF-1)<1e-4);
  TVector3 PH(-cosL*sinF, cosL*cosF,   0);
  TVector3 DL(-sinL*cosF,-sinL*sinF,cosL);

// PH*dPhi + DL*dLam = U*u + V*v
  mA = (U*fitPars.mFita + V*fitPars.mLama).Dot(PH)/cos2L;
  mL = (U*fitPars.mFita + V*fitPars.mLama).Dot(DL);
  mP = fitPars.mPinv/cosL;
}
  
#endif
//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
#include "TSystem.h"
#include "TCanvas.h"
#include "TH1F.h"
#include "TRandom.h"
#include "TMatrixDSym.h"
#include "TMatrixD.h"
#include "TVectorD.h"
#include "TVector3.h"
#include "StarRoot/TRandomVector.h"
//_____________________________________________________________________________
ClassImp(StvNodeParsTest);
void StvNodeParsTest::Test()
{
  int iQ =1;
  double thPars[7+15],Hz[3] ={ 0.,0.,0.000299792458 * 4.98478};
  for (int i=0;i<7+15;i++) {thPars[i]=gRandom->Rndm();}
  THelix3d th(iQ,thPars,thPars+3,Hz),thh;
  memcpy(thPars+3,th.Dir(),3*sizeof(double));

  THEmx3d_t emx(thPars+7,0);
  th.SetEmx(&emx);
  
  StvNodePars pars;
  StvFitErrs  errs;
  pars.set(&th);
  errs.Set(&th);
  pars.reverse();
  errs.Backward();
  pars.get(&thh);
  errs.Get(&thh);
  thh.Backward();

  double thhPars[7+15];
  memcpy(thhPars,thh.Pos(),7*sizeof(double));
  memcpy(thhPars+7,*thh.Emx(),15*sizeof(double));
  int nerr=0;
  for (int i=0;i<7+15;i++) {
    if (fabs(thhPars[i]-thPars[i]) <1e-6) continue;
    nerr++;printf("%d = %g %g \n",i,thPars[i],thhPars[i]);}
  printf("nmErrs = %d\n",nerr);
}
//_____________________________________________________________________________
void StvNodeParsTest::TestDerRadial()
{
  double PtGev = 3.,Curv = 1./100, Hz = PtGev*Curv;
  double HZ[3] = {0,0,Hz};
  double Pbeg[3],Xbeg[3]={50,60,70};

  int charge = 1;
  TVector3 PbegV(PtGev,0,PtGev/3*4);
  PbegV.RotateZ(gRandom->Rndm()*3.1415*2); PbegV.GetXYZ(Pbeg);
  TVector3 XbegV(Xbeg);

  double Mag[3]={HZ[0],HZ[1],HZ[2]};
#if 1
  TVector3 magV(HZ); 
  double r1 = gRandom->Rndm(),r2 = gRandom->Rndm(); 
   magV.RotateX(r1);  magV.RotateY(r2);  magV.GetXYZ(Mag);
  PbegV.RotateX(r1); PbegV.RotateY(r2); PbegV.GetXYZ(Pbeg);
#endif
  
  THelix3d TH0(charge,Xbeg,Pbeg,Mag);
  StvNodePars pars;
  pars.set(&TH0);
  double Rxy2beg = pars.getRxy2();

  double radPar[6],der[5][5];
  pars.GetRadial(radPar,0,0,der);
//		Event loop
  for (int ip=0;ip<5;ip++) {
    StvNodePars myPars(pars);
    StvFitPars  myFit;
    double eps = 1e-3;
    ((double*)myFit)[ip]+=eps;
    myPars+=myFit;
    for (int it =0; it<2;it++) {
      double Rxy2 = myPars.getRxy2();
      double tau = -0.5*(Rxy2-Rxy2beg)/dot2(myPars._x,myPars._d);
      TCL::vlinco(myPars._x,1,myPars._d,tau,myPars._x,3);
    }
    double myRadPar[6];
    myPars.GetRadial(myRadPar);
    TCL::vsub(myRadPar+1,radPar+1,myRadPar+1,5);
    
    TVectorD difV(5,myRadPar+1),derV(5);     
    for (int j=0;j<5;j++) {derV[j] = der[j][ip];}
    difV*=1./eps;
    const static char *tit[5] =  {"U","V","Fita","Lama","Pinv"};
    printf("d/d%s :\n",tit[ip]);
    derV.Print("Analitic  "); 
    difV.Print("Arithmetic"); 
  }   

}

//_____________________________________________________________________________
void StvNodeParsTest::TestGetRadial(int nEv)
{
//    PhiPhi;
//    ZPhi     ,ZZ;                       
//    TanlPhi  ,TanlZ ,TanlTanl,                 
//    PhiPsi   ,ZPsi  ,TanlPsi , PsiPsi ,           
//    PhiPti  ,ZPti ,TanlPti, PsiPti, PtiPti     
  double PtGev = 1.,Curv = 1./100, Hz = PtGev*Curv;
  double HZ[3] = {0,0,Hz};
  double Pbeg[3],Xbeg[3]={50,60,70};

//		Canvas + Histograms
enum {kNCanvs=10};
static TCanvas *myCanvas[kNCanvs] = {0};

  myCanvas[0] = new TCanvas("C_Xi2","C_Xi2",600,800);
  myCanvas[0]->Divide(1,1); 
  static TH1F *hXi2[2] = {0};
  for (int jk=0;jk<1;jk++) {
    TString ts("Xi2_"); ts += jk;
    delete hXi2[jk];
    hXi2[jk]= new TH1F(ts.Data(),ts.Data(),100,0,0);
    myCanvas[0]->cd(jk+1);  hXi2[jk]->Draw(); 
  }

//		Now pulls
  myCanvas[1] = new TCanvas("C_Pull","C_Pull",600,800);
  myCanvas[1]->Divide(1,5); 
  static TH1F * hpu[5]={0};
{
static const char *tit[]={"Phi","Z","Tan","Psi","Pti"};
  for (int ih=0;ih<5;ih++) {
    delete hpu[ih];
    hpu[ih] = new TH1F(tit[ih],tit[ih],100,0,0);
    myCanvas[1]->cd(ih+1  ); 
    hpu[ih]->Draw();
}}


//		Now test of Corr
static TH1F * hcr[10]={0};
{
  myCanvas[2] = new TCanvas("C_Corr1","C_Corr1",600,800);
  myCanvas[3] = new TCanvas("C_Corr2","C_Corr2",600,800);
  myCanvas[2]->Divide(1,5); 
  myCanvas[3]->Divide(1,5); 
  static const char *tit[]={
 "ZPhi"
,"TanlPhi","TanlZ"
,"PhiPsi","ZPsi","TanlPsi"
,"PhiPti","ZPti","TanlPti","PsiPti"};

  for (int ih=0;ih<10;ih++) {
    delete hcr[ih];
    hcr[ih] = new TH1F(tit[ih],tit[ih],100,0,0);
    if (ih<5) myCanvas[2]->cd(ih+1  ); 
    else      myCanvas[3]->cd(ih+1-5);
    hcr[ih]->Draw();
  }}
  
    
  int charge = 1;
  TVector3 PbegV(PtGev,0,PtGev*3);
  double Ptot = PbegV.Mag();
  PbegV.RotateZ(gRandom->Rndm()*3.1415*2); PbegV.GetXYZ(Pbeg);
  TVector3 XbegV(Xbeg);

  double Mag[3]={HZ[0],HZ[1],HZ[2]};
#if 1
  TVector3 magV(HZ); 
  double r1 = gRandom->Rndm(),r2 = gRandom->Rndm(); 
   magV.RotateX(r1);  magV.RotateY(r2);  magV.GetXYZ(Mag);
  PbegV.RotateX(r1); PbegV.RotateY(r2); PbegV.GetXYZ(Pbeg);
#endif
  TVectorD dia(5);
  dia[0]= 0.1; dia[1]= 0.2; dia[2]= 0.1/Ptot; dia[3]= 1./360; dia[4]= 2./360;
  
  TRandomVector RV(dia);
  auto &EMX = RV.GetMtx();
  auto &val = RV.GetLam();
  dia.Print("DIA");
  val.Print("VAL");


  double Gbeg[15];
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i;j++) {
      Gbeg[li+j] = EMX[i][j];
  } }


  double GbegI[15];
  TCL::trsinv(Gbeg,GbegI,5);  
  
  THelix3d TH0(charge,Xbeg,Pbeg,Mag);
  TH0.SetEmx();
  THEmx3d_t *emx = TH0.Emx();
  emx->Set(Gbeg);
  StvNodePars pars;
  pars.set(&TH0);
  double Rxy2beg = pars.getRxy2();
  StvFitErrs errs;  
  errs.Set(&TH0);
  double radPar[6],radErr[15],radErrI[15],radErrD[5];
  pars.GetRadial(radPar,radErr,&errs);
  TCL::trsinv(radErr,radErrI,5);  
  for (int i=0,li=0;i<5;li+=++i) {radErrD[i] = sqrt(radErr[li+i]);}
  
//		Event loop
  for (int iev=0;iev<nEv;iev++) {
    TVectorD delta  = RV.Gaus();
    StvNodePars myPars(pars);
    StvFitPars  myFit(delta.GetMatrixArray());
    myPars+=myFit;
    for (int it =0; it<2;it++) {
      double Rxy2 = myPars.getRxy2();
      double tau = -0.5*(Rxy2-Rxy2beg)/dot2(myPars._x,myPars._d);
      TCL::vlinco(myPars._x,1,myPars._d,tau,myPars._x,3);
    }
    assert(fabs(myPars.getRxy2()-Rxy2beg)<1e-4*Rxy2beg);
    double myRadPar[6];
    myPars.GetRadial(myRadPar);
    TCL::vsub(myRadPar+1,radPar+1,myRadPar+1,5);
    


    double chi2;
    TCL::trasat(myRadPar+1,radErrI,&chi2,1,5);
//    TCL::trasat(delta.GetMatrixArray(),GbegI,&chi2,1,5);
    assert(chi2>0);
    hXi2[0]->Fill(chi2);


    int jk=0;
    for (int i=0,li=0;i<5;li+=++i) {
      hpu[i]->Fill(myRadPar[i+1]/radErrD[i]);
//      hpu[i]->Fill(delta[i]/GbegD[i]);
      for (int j=0;j<i;j++) {
	double d = myRadPar[i+1]*myRadPar[j+1] - radErr[li+j];
	d/= radErrD[i]*radErrD[j];
	hcr[jk++]->Fill(d);
    } }
  }

  for (int i=0;myCanvas[i];i++) {
    if (!myCanvas[i]) continue;
    myCanvas[i]->Modified();myCanvas[i]->Update();}

  while(!gSystem->ProcessEvents()){gSystem->Sleep(200);}; 
}

//_____________________________________________________________________________
static void Add(THelixTrack &ht,const double add[5]) 
{
// add = H,A,C,Z,L
  TVector3  pos(ht.Pos()),dir(ht.Dir()),ort(-dir[1],dir[0],0.);
  ort = ort.Unit();
  double rho=ht.GetRho();
  pos+=ort*add[0]; pos[2]+=add[3];
  dir.SetMagThetaPhi(1.,(M_PI/2-dir.Theta())-add[4],dir.Phi()+add[1]);
  double wk[7]={pos[0],pos[1],pos[2],dir[0],dir[1],dir[2],rho+add[2]};
  ht.Set(wk,wk+3,wk[6]);
}


//_____________________________________________________________________________
void StvNodeParsTest::TestErrProp(int nEv)
{

StvNodePars iP,iPR,oP,oPR;
THelix3d iH,iHR,oH,ht;
StvFitErrs iE,oE,oER;

StvFitErrs oHE,oHER;
double dia[5],*e,*er,*vtx;
  double x[3] = {153.15599597873157, 61.139971243562862, -165.47715347045698};
  double lam = atan(9), psi = 0.0039755004554277579, ptin = 3.000133812200076;
  double d[3] = {cos(lam)*cos(psi),cos(lam)*sin(psi),sin(lam)};
  double h[3]={0,0, 0.0014989667215176954};
  iP.set(h);iP.set(x,d,ptin*cos(lam),h);

  double G[15] = {
    0.008422599212098222
  ,-0.009191683296246307, 0.019038123366318632
  ,-0.000396250809100565, 0.000276017159398224, 3.865461868668168-05
  , 0.000294318186665566,-0.000606674087156203,-1.20459566280978e-05, 2.69467912060978e-05
  , 0.013476203596402746,-0.000583805967045623,-0.002148732218088687, 0.000163580439637613, 0.17711658543201794 
  };

  oER*=0.;
  oHER.Clear();
//		Prepare error matrix for TRandomVector
  TRandomVector RV(5,G);
//
  iE.Print("Input StvFitErrs");

  iP.get(&iH);	// nodePar => Helix
  iE.Get(&iH);	// fitErr  => HelixErr
  iE.Set(&iH);
  iE.Print("Input StvFitErrs => THEmx_t => StvFitErrs");


  iH.Emx()->Print("Input Helix Errs");
  oH = iH;
  double myDist = 33;
  oH.Move(myDist);		//Move helix to 100cm
  oP.set(&oH);		//helix100 => nodePar100
  oE.Set(&oH);		//helixErr100 => fitErr100
  oE.Print("Output StvFitErrs");
  oH.Emx()->Print("Output Helix Errs");

  vtx = oH.Pos();
  TVector3 Vtx(vtx);
  TVector3 Dir(oH.Dir());
  TVector3 Nxy(-Dir[1],Dir[0],0.); Nxy = Nxy.Unit();

static int iHELIX=0;


//		Event loop
  for (int ev=0;ev <= nEv;ev++) {
    iPR = iP;
    ht = iH;
//		Randomize fit parameters
    const TVectorD res = RV.Gaus();

    if (iHELIX) { //Randomize helix directly
      Add(ht,res.GetMatrixArray());
    } else {
      StvFitPars fp(res.GetMatrixArray()); iPR+=fp;
//		Create THelixTrack from StvNodePars
      iPR.get(&ht);
//		Set no error matrix to helix
      ht.SetEmx();
    }
      
// //		Change helix direction for fun
//     ht.Backward();
//		Move random helix to DCA point of etalon vertex
    double my100 = ht.Path(vtx);
    assert(fabs(my100-myDist) <20);
    ht.Move(my100);
    oPR.set(&ht);
    StvFitPars fp = (oPR-oP);
    double *e = oER;
    double *d = fp;
    for (int i=0,li=0;i< 5;li+=++i) {
      for (int j=0;j<=i;j++    ) {
        e[li+j]+=d[i]*d[j];
    } } 
    double dS = ht.Path(vtx[0],vtx[1]);
    ht.Move(dS);
    double D[5];
    TVector3 VPos(ht.Pos()),VDir(ht.Dir());
    D[0] = (VPos-Vtx)*Nxy;
    D[1] = VDir.DeltaPhi(Dir);
    D[2] = ht.GetRho()-oH.GetRho();
    D[3] = VPos[2]-Vtx[2];
    D[4] = -(VDir.Theta()-Dir.Theta());// "-" due to theta)= Pi/2-Lambda

    e = oHER;
    for (int i=0,li=0;i< 5;li+=++i) {
      for (int j=0;j<=i;j++    ) {
        e[li+j]+=D[i]*D[j];
    } } 


  }//EndEvts

  oER *= (1./nEv);
  oHER*= (1./nEv);
  
  printf("*** Check THelixTrack Error matrix ***\n");
  e = *oH.Emx();
  er = oHER;
  double qA=0,qAmax=0;
  for (int i=0,li=0;i< 5;li+=++i) {
    dia[i]=e[li+i];
    for (int j=0;j<=i;j++) {
    double dif = (er[li+j]-e[li+j])/sqrt(dia[i]*dia[j]);
    printf("(%d %d) \t%g = \t%g \t%g\n",i,j,er[li+j],e[li+j],dif);
    dif = fabs(dif);
    qA+= (dif); if (dif>qAmax) qAmax=dif;
  } }
  printf("Quality %g < %g < 1\n",qA,qAmax);


  printf("*** Check StvFitErr Error matrix ***\n");
  e = oE;
  er = oER;
  qA=0;qAmax=0;
  for (int i=0,li=0;i< 5;li+=++i) {
    dia[i]=e[li+i];
    for (int j=0;j<=i;j++) {
    double dif = (er[li+j]-e[li+j])/sqrt(dia[i]*dia[j]);
    printf("(%d %d) \t%g = \t%g \t%g\n",i,j,er[li+j],e[li+j],dif);
    dif = fabs(dif);
    qA+= (dif); if (dif>qAmax) qAmax=dif;
  } }
  qA/=15;
  printf("Quality %g < %g < 1\n",qA,qAmax);

}
//_____________________________________________________________________________

//_____________________________________________________________________________
// Some math for:
// 
// 	(cL*cP)
// T =  (cL*sP)
// 	(sL   )
// 
// 
// 	(-sP)
// P = 	( cP)
// 	( 0 )
// 
// 
// 
// 	(-sL*cP)
// L = 	(-sL*sP)
// 	( cL   )
// 
// 
// dX = T*t + P*h + L*l
// 
// 
// 	(cP)
// D =  (sP)
// 	(0 )
// 
// X0 = -imp*P
// dD = P*dPsi = P*(a/cL + rho*(cL*t -sL*l))
// 
// 
// 
// (Tk =  T*s/cL + P*(-Imp+h+dFi0*s +rho*s*s/2) + L*l
// 
// ACCOUNT ONLY 2D
// Tk = D*(s-sL*l) + P*(-Imp+h+dFi0*s+rho*s*s/2) 
// dTk= D          + P*(dFi0+rho*s)
// 
// (Tk*dTk) = 0
// 
// Now keep only the biggest order of magnitude
// 
// Tk = D*(s-sL*l) + P*(-Imp) 
// dTk= D          + P*(dFi0+rho*s)
// 
// (s-sL*l)-Imp*(dFi0+rho*s)= 0
// (1-Imp*rho)*s -sL*l -Imp*dFi0=0
// s= (sL*l+Imp*dFi0)/(1-Imp*rho)
// dFi = dFi0 +rho*(sL*l+Imp*dFi0)/(1-Imp*rho)
// dFi = (dFi0*(1+rho*Imp) +rho*sL*l)/(1-rho*Imp)
// 
// dFidFi0 = (1.)/(1-rho*Imp);
// dFidl   =      rho*sL/(1-rho*Imp);
// 
// dFidA = (1+rho*Imp)/(1-rho*Imp) /cL
// 
// dtdA = Imp/c2L/(1-Imp*rho)
// dtdl =     tL/(1-Imp*rho)
//  
// dZda = sL*dtdA; 
// dZdl = sL*dtdl+cL; 
//  

//_____________________________________________________________________________
void StvNodePars::GetImpact(StvImpact *imp,const StvFitErrs *fe,double der[5][5])  const
{
    /// signed impact parameter; Signed in such a way that:
    ///     x =  -impact*sin(Psi)
    ///     y =   impact*cos(Psi)
static const double kMaxStp=1e-4, kRefStp = 1e-2;

  double cosP = getCosP(),sinP = getSinP();

  imp->mImp  = _x[0]*(-sinP) + _x[1]*(cosP);
  double tst = _x[0]*( cosP) + _x[1]*(sinP);
  assert(fabs(tst)<kMaxStp || fabs(tst) <  kRefStp*fabs(imp->mImp));
  imp->mZ   = _x[2];
  imp->mPsi = getPsi();
  imp->mPti = getPtin();
  double cosL = getCosL();
  double sinL = getSinL();
  double tanL = sinL/cosL;
  imp->mTan = tanL;
  imp->mCurv= getCurv();
  if (!fe && !der) return;

//		StvFitPars
//      		Impacts
//     float  mImpImp;
//     float  mZImp,   mZZ;
//     float  mPsiImp, mPsiZ, mPsiPsi;
//     float  mPtiImp, mPtiZ, mPtiPsi, mPtiPti;
//     float  mTanImp, mTanZ, mTanPsi, mTanPti, mTanTan;
// ==================================


  double T[5][5]={{0}};

//		Now d/dU
  double dXdU = _tkdir[kKU][0];
  double dYdU = _tkdir[kKU][1];
  double dZdU = _tkdir[kKU][2];

  double dTdU = -((dXdU*cosP+dYdU*sinP))/cosL;

  dXdU+=_d[0]*dTdU;
  dYdU+=_d[1]*dTdU;
  dZdU+=_d[2]*dTdU;

  tst = dXdU*cosP + dYdU*sinP;
  assert(fabs(tst)<1e-6);
  
  T[kImp ][kU]= (dXdU *(-sinP) + dYdU*( cosP));
  T[kImpZ][kU]= dZdU;


//		Now d/dV
//===========================
  double dXdV = _tkdir[kKV][0];
  double dYdV = _tkdir[kKV][1];
  double dZdV = _tkdir[kKV][2];

  double dTdV = -((dXdV*cosP+dYdV*sinP));

  dXdV+=cosP*dTdV;
  dYdV+=sinP*dTdV;
  dZdV+=tanL*dTdV;

  tst = dXdV*cosP + dYdV*sinP;
  assert(fabs(tst)<1e-6);
  
  T[kImp ][kV]= (dXdV *(-sinP) + dYdV*( cosP));
  T[kImpZ][kV]= dZdV;

//		Now d/dFita
//===========================
  double dDx_dFita = _tkdir[kKU][0];
  double dDy_dFita = _tkdir[kKU][1];
  double dDz_dFita = _tkdir[kKU][2];


  double dPsi_dFita = (-dDx_dFita*sinP+dDy_dFita*cosP)/cosL;
  double dTdFita = -imp->mImp*dPsi_dFita;
  double dZdFita = dTdFita*sinL/cosL;
  double dLamda_dFita = dDz_dFita/cosL;
  double dTan_dFita   = dLamda_dFita/(cosL*cosL);
  double dCosLi_dFita = tanL/(cosL)*dLamda_dFita;

  T[kImp   ][kFita] = 0;
  T[kImpZ  ][kFita] = dZdFita;
  T[kImpPsi][kFita] = dPsi_dFita;
  T[kImpTan][kFita] = dTan_dFita;
  T[kImpPti][kFita] = _pinv*dCosLi_dFita;

  
//		Now d/dLama
//===========================
  double dDx_dLama = _tkdir[kKV][0];
  double dDy_dLama = _tkdir[kKV][1];
  double dDz_dLama = _tkdir[kKV][2];


  double dPsi_dLama = (-dDx_dLama*sinP+dDy_dLama*cosP)/cosL;
  double dTdLama = -imp->mImp*dPsi_dLama;
  double dZdLama = dTdLama*sinL/cosL;
  double dLamda_dLama = dDz_dLama/cosL;
  double dTan_dLama   = dLamda_dLama/(cosL*cosL);
  double dCosLi_dLama = tanL/(cosL)*dLamda_dLama;

  T[kImp   ][kLama] = 0;
  T[kImpZ  ][kLama] = dZdLama;
  T[kImpPsi][kLama] = dPsi_dLama;
  T[kImpTan][kLama] = dTan_dLama;
  T[kImpPti][kLama] = _pinv*dCosLi_dLama;

//		Now d/dPinv
//===========================
  T[kImpPti][kPinv] =    1./(cosL);

  if (der) TCL::ucopy(T[0],der[0],5*5);
  if (!fe) return;
  double qwe[15];
  TCL::trasat(T[0],*fe,qwe,5,5); 
  assert(qwe[0]>0);
  TCL::ucopy(qwe,&imp->mImpImp,15); 
}
//_____________________________________________________________________________
#include "TMatrixT.h"
#include "TMatrixTSym.h"
#include "TVectorT.h"
//_____________________________________________________________________________
double StvFitErrs::EmxSign(int n,const float *e)
{
  enum {maxN =10,maxE = (maxN*maxN-maxN)/2+maxN};
  double d[maxE];
  assert(n>0 && n<=maxN);
  TCL::ucopy(e,d,(n*(n+1))/2);
  return EmxSign(n,d);
}
//_____________________________________________________________________________
double StvFitErrs::EmxSign(int n,const double *e)
{
  TMatrixDSym S(n);  
  TVectorD coe(n);
  for (int i=0,li=0;i< n;li+=++i) {
    double qwe = e[li+i];
    if(qwe<=0) return qwe;
    qwe = pow(2.,-int(log(qwe)/(2*log(2))));
    coe[i]=qwe;
    for (int j=0;j<=i;j++    ) {
       S[i][j]=e[li+j]*coe[i]*coe[j]; S[j][i]=S[i][j];
  } }
  TMatrixD EigMtx(n,n);
  TVectorD EigVal(n);  
  EigMtx = S.EigenVectors(EigVal);

  double ans = 3e33;
  for (int i=0;i<n;i++) {if (EigVal[i]<ans) ans = EigVal[i];}
  return ans;
} 
//_____________________________________________________________________________
//_____________________________________________________________________________
void StvNodeParsTest::TestDerImpact()
{
  double PtGev = 3.,Curv = 1./100, Hz = PtGev*Curv;
  double HZ[3] = {0,0,Hz};
  double Pbeg[3],Xbeg[3]={5,6,0.7};

  int charge = 1;
  TVector3 PbegV(PtGev,0,PtGev/3*4);
#if 1
  PbegV.RotateZ(gRandom->Rndm()*3.1415*2); 
#endif
   PbegV.GetXYZ(Pbeg);
  TVector3 XbegV(Xbeg);

  double Mag[3]={HZ[0],HZ[1],HZ[2]};
#if 1
  TVector3 magV(HZ); 
  double r1 = gRandom->Rndm(),r2 = gRandom->Rndm(); 
   magV.RotateX(r1);  magV.RotateY(r2);  magV.GetXYZ(Mag);
  PbegV.RotateX(r1); PbegV.RotateY(r2); PbegV.GetXYZ(Pbeg);
#endif
  
  THelix3d TH0(charge,Xbeg,Pbeg,Mag);
  double l = TH0.Path(0.,0.);
  TH0.Move(l);
  l = TH0.Path(0.,0.);
  TH0.Move(l);

  StvNodePars pars;
  pars.set(&TH0);

  StvImpact imp;
  double impPar[5],impDer[5][5];
  pars.GetImpact(&imp,0,impDer);
  TCL::ucopy(&imp.mImp,impPar,5);
  
//		Event loop
  for (int ip=0;ip<5;ip++) {
    StvNodePars myPars(pars);
    StvFitPars  myFit;
    double eps = 1e-4;
    ((double*)myFit)[ip]+=eps;
    myPars+=myFit;
    THelix3d TH1;
    myPars.get(&TH1);
    l = TH1.Path(0.,0.); TH1.Move(l);
    l = TH1.Path(0.,0.); TH1.Move(l);

    myPars.set(&TH1);
    double myImpPar[6];
    StvImpact myImp;
    myPars.GetImpact(&myImp);
    TCL::ucopy(&myImp.mImp,myImpPar,5);
    TCL::vsub(myImpPar,impPar,myImpPar,5);
    
    TVectorD difV(5,myImpPar),derV(5);     
    for (int j=0;j<5;j++) {derV[j] = impDer[j][ip];}
    difV*=1./eps;
    double  pct=0; int ipct=-1;
    for (int i=0;i<5;i++) {
      if (fabs(difV[i])<1e-2
      &&  fabs(derV[i])<1e-2) continue;
      double bott = 0.5*(fabs(difV[i])+fabs(derV[i]));
      if (bott<1e-3) continue;
      double myPct = fabs((difV[i]-derV[i])/bott);
      if (myPct<pct) continue;
      pct = myPct; ipct = i;
    }
    if (pct >0.2) printf("################################  [%d] = %d%% \n",ipct,int(pct*100));

    const static char *tit[5] =  {"U","V","Fita","Lama","Pinv"};
    printf("d/d%s :\n",tit[ip]);
    derV.Print("Analitic  "); 
    difV.Print("Arithmetic"); 
  }   

}
//_____________________________________________________________________________
void StvNodeParsTest::TestGetImpact(int nEv)
{
//      		Impacts
//     float  mImpImp;
//     float  mZImp, mZZ;
//     float  mPsiImp, mPsiZ, mPsiPsi;
//     float  mPtiImp, mPtiZ, mPtiPsi, mPtiPti;
//     float  mTanImp, mTanZ, mTanPsi, mTanPti, mTanTan;
  double PtGev = 3.,Curv = 1./100, Hz = PtGev*Curv;
  double HZ[3] = {0,0,Hz};
  double Pbeg[3],Xbeg[3]={5,6,7};

//		Canvas + Histograms
enum {kNCanvs=10};
static TCanvas *myCanvas[kNCanvs] = {0};

  myCanvas[0] = new TCanvas("C_Xi2","C_Xi2",600,800);
  myCanvas[0]->Divide(1,1); 
  static TH1F *hXi2[2] = {0};
  for (int jk=0;jk<1;jk++) {
    TString ts("Xi2_"); ts += jk;
    delete hXi2[jk];
    hXi2[jk]= new TH1F(ts.Data(),ts.Data(),100,0,0);
    myCanvas[0]->cd(jk+1);  hXi2[jk]->Draw(); 
  }

//		Now pulls
  myCanvas[1] = new TCanvas("C_Pull","C_Pull",600,800);
  myCanvas[1]->Divide(1,5); 
  static TH1F * hpu[5]={0};
{
static const char *tit[]={"Phi","Z","Tan","Psi","Pti"};
  for (int ih=0;ih<5;ih++) {
    delete hpu[ih];
    hpu[ih] = new TH1F(tit[ih],tit[ih],100,0,0);
    myCanvas[1]->cd(ih+1  ); 
    hpu[ih]->Draw();
}}


//		Now test of Corr
static TH1F * hcr[10]={0};
{
  myCanvas[2] = new TCanvas("C_Corr1","C_Corr1",600,800);
  myCanvas[3] = new TCanvas("C_Corr2","C_Corr2",600,800);
  myCanvas[2]->Divide(1,5); 
  myCanvas[3]->Divide(1,5); 
  static const char *tit[]={
 "ZImp",
 "PsiImp", "PsiZ",
 "PtiImp", "PtiZ", "PtiPsi", 
 "TanImp", "TanZ", "TanPsi", "TanPti"};

  for (int ih=0;ih<10;ih++) {
    delete hcr[ih];
    hcr[ih] = new TH1F(tit[ih],tit[ih],100,0,0);
    if (ih<5) myCanvas[2]->cd(ih+1  ); 
    else      myCanvas[3]->cd(ih+1-5);
    hcr[ih]->Draw();
  }}
  
    
  int charge = 1;
  TVector3 PbegV(PtGev,0,PtGev/3*4);
  double Ptot = PbegV.Mag();
#if 1
  PbegV.RotateZ(gRandom->Rndm()*3.1415*2); 
#endif
  PbegV.GetXYZ(Pbeg);
  TVector3 XbegV(Xbeg);

  double Mag[3]={HZ[0],HZ[1],HZ[2]};
#if 1
  TVector3 magV(HZ); 
  double r1 = gRandom->Rndm(),r2 = gRandom->Rndm(); 
   magV.RotateX(r1);  magV.RotateY(r2);  magV.GetXYZ(Mag);
  PbegV.RotateX(r1); PbegV.RotateY(r2); PbegV.GetXYZ(Pbeg);
#endif
  TVectorD dia(5);
  dia[0]= 0.1; dia[1]= 0.2; dia[2]= 0.1/Ptot; dia[3]= 1./360; dia[4]= 2./360;
  
  TRandomVector RV(dia);
  auto &EMX = RV.GetMtx();
  auto &val = RV.GetLam();
  dia.Print("DIA");
  val.Print("VAL");


  double Gbeg[15];
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i;j++) {
      Gbeg[li+j] = EMX[i][j];
  } }


  double GbegI[15];
  TCL::trsinv(Gbeg,GbegI,5);  
  
  THelix3d TH0(charge,Xbeg,Pbeg,Mag);
  double l = TH0.Path(0.,0.);
  TH0.Move(l);
  TH0.SetEmx();
  THEmx3d_t *emx = TH0.Emx();
  emx->Set(Gbeg);
  assert(StvFitErrs::EmxSign(5,*emx)>0);
  StvNodePars pars;
  pars.set(&TH0);
  StvFitErrs errs;  
  errs.Set(&TH0);
  assert(StvFitErrs::EmxSign(5,errs)>0);
  StvImpact imp;
  double impPar[5],impErr[15],impErrI[15],impErrD[5];
  pars.GetImpact(&imp,&errs);
  TCL::ucopy(&imp.mImp,impPar,5);
  TCL::ucopy(&imp.mImpImp,impErr,15);

  TCL::trsinv(impErr,impErrI,5);  
  for (int i=0,li=0;i<5;li+=++i) {impErrD[i] = sqrt(impErr[li+i]);}
  
//		Event loop
  for (int iev=0;iev<nEv;iev++) {
    TVectorD delta  = RV.Gaus();
    StvNodePars myPars(pars);
    StvFitPars  myFit(delta.GetMatrixArray());
    myPars+=myFit;
    THelix3d TH1;
    myPars.get(&TH1);
    double l = TH1.Path(0.,0.);
    TH1.Move(l);
    myPars.set(&TH1);

    double myImpPar[6];
    StvImpact myImp;
    myPars.GetImpact(&myImp);
    TCL::ucopy(&myImp.mImp,myImpPar,5);
    TCL::vsub(myImpPar,impPar,myImpPar,5);

    


    double chi2;
    TCL::trasat(myImpPar,impErrI,&chi2,1,5);
    TCL::trasat(delta.GetMatrixArray(),GbegI,&chi2,1,5);
    assert(chi2>0);
    hXi2[0]->Fill(chi2);


    int jk=0;
    for (int i=0,li=0;i<5;li+=++i) {
      hpu[i]->Fill(myImpPar[i]/impErrD[i]);
      for (int j=0;j<i;j++) {
	double d = myImpPar[i]*myImpPar[j] - impErr[li+j];
	d/= impErrD[i]*impErrD[j];
	hcr[jk++]->Fill(d);
    } }
  }

  for (int i=0;myCanvas[i];i++) {
    if (!myCanvas[i]) continue;
    myCanvas[i]->Modified();myCanvas[i]->Update();}

  while(!gSystem->ProcessEvents()){gSystem->Sleep(200);}; 
}
//_____________________________________________________________________________
void StvNodeParsTest::TestCentral(int nEv)
{

//		Canvas + Histograms
enum {kNCanvs=10};
static TCanvas *myCanvas[kNCanvs] = {0};


//		Now pulls
  static const char *tit[]={"H","Z","Phi","Lam","Pti"};
  static TH1F * cntPull[15]={0};
  int ih = -1,kanv=-1; 
    for (int i=0,li=0;i<5;li+=++i) {
      for (int j=0;j<=i;j++)         {
        ih++;
        if ((ih%5)==0) {
          kanv++;
          TString ts("C_Kan"); ts+=kanv;
          myCanvas[kanv] = new TCanvas(ts,ts,600,800);
          myCanvas[kanv]->Divide(1,5); 
        }
	TString ts("Corr_"); ts+=tit[i];ts+=tit[j];
	delete cntPull[ih];
	cntPull[ih] = new TH1F(ts,ts,100,0,0);
	myCanvas[kanv]->cd((ih%5)+1); 
	cntPull[ih]->Draw();
  }}

  TRandom grnd;//????
  double PtGev = 1.,Curv = 1./100, Hz = PtGev*Curv;
  double HZ[3] = {0,0,Hz};
  double Pbeg[3],Xbeg[3]={0,0,0};

    
  TVector3 PbegV(PtGev,0,PtGev*3);
  double Ptot = PbegV.Mag();
  PbegV.RotateZ(gRandom->Rndm()*3.1415*2); PbegV.GetXYZ(Pbeg);
  TVector3 XbegV(Xbeg);

  double Mag[3]={HZ[0],HZ[1],HZ[2]};
#if 1
  TVector3 magV(HZ); 
  double r1 = gRandom->Rndm(),r2 = gRandom->Rndm(); 
   magV.RotateX(r1);  magV.RotateY(r2);  magV.GetXYZ(Mag);
  PbegV.RotateX(r1); PbegV.RotateY(r2); PbegV.GetXYZ(Pbeg);
#endif
  TVectorD dia(5);
  dia[0]= 0.1; dia[1]= 0.2; dia[2]= 1./360; dia[3]= 2./360,dia[4]= 0.1/Ptot;
  
  TRandomVector RV(dia);
  auto &EMX = RV.GetMtx();
  auto &val = RV.GetLam();
  dia.Print("DIA");
  val.Print("VAL");


  double Gbeg[15];
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i;j++) {
      Gbeg[li+j] = EMX[i][j];
      Gbeg[li+j]=0; //?????
      if (i==j) Gbeg[li+j] = dia[i]*dia[i]; //?????
  } }

//	Define Stv objects
  StvNodePars stvPars;
  int iQ = 1;
  stvPars.set(Xbeg, Pbeg,iQ/Ptot,Mag);
  TkDir_t tkd = stvPars.getTkDir(); 
  StvFitErrs stvFitErrs(Gbeg,&tkd);  
//	Define central objects
  StvFitErrsCentral cntFitErrs;
  cntFitErrs.Set(stvPars,stvFitErrs);
  StvFitParsCentral cntFitPars;
//		Event loop
  for (int iev=0;iev<nEv;iev++) {
    TVectorD delta  = RV.Gaus();
    for(int jk=0;jk<5; jk++){delta[jk]= grnd.Gaus(0,dia[jk]);}//??????

    StvFitPars  stvFitPars(delta.GetMatrixArray());
    cntFitPars.Set(tkd,stvFitPars);
    ih = -1;
    double diia[5];
    for (int i=0,li=0;i< 5;li+=++i) {
      diia[i] = cntFitErrs[li+i];
      double cntI = cntFitPars[i];
      for (int j=0;j<=i;j++)         {
        ih++;
        double cntJ = cntFitPars[j];
        double cntIJ = cntFitErrs[li+j];
        double pul = (cntI*cntJ-cntIJ)/sqrt(dia[i]*dia[j]);
        if (i==j) pul = cntI/sqrt(diia[i]);
        cntPull[ih]->Fill(pul);
      }
    }
  }
  for (int i=0;myCanvas[i];i++) {
    if (!myCanvas[i]) continue;
    myCanvas[i]->Modified();myCanvas[i]->Update();}

  while(!gSystem->ProcessEvents()){gSystem->Sleep(200);}; 
}
