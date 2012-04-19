#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "TCernLib.h"
#include "TMath.h"
#include "TMath.h"
#include "StvUtil/StvNodePars.h"
#include "Stv/StvToolkit.h"

  static const int idx66[6][6] =
  {{ 0, 1, 3, 6,10,15},{ 1, 2, 4, 7,11,16},{ 3, 4, 5, 8,12,17}
  ,{ 6, 7, 8, 9,13,18},{10,11,12,13,14,19},{15,16,17,18,19,20}};
static const double recvCORRMAX  = 0.99;
static const double chekCORRMAX  = 0.9999;

static double MAXTAN = 100;
//                              x   y   z  psi   pt  tan      cur
static double MAXNODPARS[]   ={555,555,555,6.66,111, MAXTAN+1, .1};
//                                   h    z   a     l   ptin
static const double MAXFITPARS[]   ={1.0 ,1.0,0.5 ,0.5 ,20  };
static const double BIGFITPARS[]   ={0.1 ,0.1,0.1 ,0.1 ,0.01},BIGFITPART=0.01;
static const double MAXFITERR[5]   ={0.5,0.5 ,0.3 ,.3 ,10};
static const double MAXERRFACT     = 1;
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
      if (!bjk) continue;
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
#if 0
//______________________________________________________________________________ 
double StvTrackNode::sinX(double x)
{
  double x2 = x*x;
  if (x2>0.5) return (sin(x)-x)/x2/x;
  double nom = -1./6;
  double sum = nom;
  for (int it=4;1;it+=2) {
    nom = -nom*x2/(it*(it+1));
    sum +=nom;
    if (fabs(nom) <= 1e-10*fabs(sum)) break;
  }
  return sum;
} 
//______________________________________________________________________________
void StvTrackNode::mult6(double Rot[kNPars][kNPars],const double Pro[kNPars][kNPars]) 
{
  double T[kNPars][kNPars];

  if (!Rot[0][0]) {memcpy(Rot[0],Pro[0],sizeof(T)); return;}

  memcpy(T[0],Pro[0],sizeof(T));

  for (int i=0;i<kNPars;i++) {
  for (int j=0;j<kNPars;j++) {
    if(!Rot[i][j]) continue;
    for (int k=0;k<kNPars;k++) {
      if (!Pro[k][i]) continue;
      T[k][j] += Pro[k][i]*Rot[i][j];
  }}} 
  for (int i=0;i<kNPars;i++) {
  for (int k=0;k<kNPars;k++) {
    Rot[i][k] += T[i][k];
}}
}     
#endif //0
//______________________________________________________________________________
int StvNodePars::check(const char *pri) const
{

  assert(_hz);
  int ierr=0;
//?? temp test
  double tmp = _curv - _ptin* _hz;
//		1km for 1GeV is a zero field
  if (fabs(_hz)>=1e-5 && fabs(tmp)> 1e-3*fabs(_curv)) {ierr=1001; 	   	goto FAILED;}
  for (int i=0;i<=kNPars;i++){if (fabs(P[i]) > MAXNODPARS[i]) {ierr = i+ 1;	goto FAILED;}} 

  for (int i=-2;i<0;i++)     {if (fabs(P[i]) > 1.0001)        {ierr = i+12;	goto FAILED;}} 
  tmp = fabs(cos(_psi)-_cosCA);
  ierr = 1002; if (tmp>1e-4) 							goto FAILED;
  tmp = fabs(sin(_psi)-_sinCA);
  ierr = 1003; if (tmp>1e-4) 							goto FAILED;
  ierr = 1004; if (fabs(_z) <= 1e-10) 						goto FAILED;
  return 0;
FAILED: 
  if (!pri ) return ierr;
  printf("StvNodePars::check(%s) == FAILED(%d)\n",pri,ierr);  print();
  return ierr;
} 
//______________________________________________________________________________
StvNodePars &StvNodePars::merge(double wt,StvNodePars &other)
{
  assert(_hz);
   double wt0 = 1.-wt;
   for (int i=0;i<kNPars+1;i++) {P[i] = wt0*P[i] + wt*other.P[i];}
   ready();
   return *this;
}

//______________________________________________________________________________
void StvNodePars::set(const THelixTrack *th, double Hz)
{
  memcpy(&_x,th->Pos(),3*sizeof(_x));
  _psi = atan2(th->Dir()[1],th->Dir()[0]);
  double sinL = th->Dir()[2];
  double cosL = sqrt((1-sinL)*(1+sinL));
  _tanl = sinL/cosL;
  
  _cosCA = th->Dir()[0]/cosL;
  _sinCA = th->Dir()[1]/cosL;
  _curv = th->GetRho();
  _hz =  Hz;
  assert(_hz);
   if (fabs(_curv) > fabs(1e-6*_hz)) 	{_ptin = _curv/_hz;              } 
   else 				{_ptin = 1e-6; _curv = _ptin*_hz;}
} 

//______________________________________________________________________________
void StvNodePars::get(THelixTrack *th) const
{
  assert(_hz);
  double dir[3]={_cosCA,_sinCA,_tanl};
  th->Set(&_x,dir,_curv);
}
//______________________________________________________________________________
void StvNodePars::move(double dLxy)
{
  double dcCA,dsCA,dC,dS,dCR,dSR,dX,dY,dZ;
  double dPhi = _curv*dLxy;

  assert(_hz);
  if (fabs(dPhi) < 0.1) {
    dCR = -dLxy*dPhi/2; dSR = dLxy*(1-dPhi*dPhi/6);
    dC  = dCR*_curv;    dS  = dSR*_curv;
  } else {
    dC = cos(dPhi)-1; dS = sin(dPhi);
    dCR = dC/_curv;   dSR = dS/_curv;
  }
  dX = (_cosCA*dSR + _sinCA*dCR);
  dY = (_sinCA*dSR - _cosCA*dCR);
  dZ = dLxy*_tanl;
  dcCA = _cosCA*dC - _sinCA*dS;
  dsCA = _sinCA*dC + _cosCA*dS;
  _cosCA+=dcCA; _sinCA+=dsCA; _x+=dX; _y+=dY; _z+=dZ; _psi+=dPhi;
}
//______________________________________________________________________________
void StvNodePars::moveToR(double R)
{
  double myR2  = _x*_x     + _y*_y;
  double myDot = _x*_cosCA + _y*_sinCA;
  double dR2 = R*R-myR2;
  double dis = myDot*myDot+dR2;
  assert(_hz);
  if (dis<0) dis = 0;
  dis = sqrt(dis);
  double dL = (dR2)/(dis+fabs(myDot));
  if (myDot<0) dL = -dL;
  move(dL);
  assert(fabs(_x*_x+ _y*_y-R*R)/(2*R)<1e-2);//??????????????????
}
//------------------------------------------------------------------------------
void StvNodePars::reverse( Mtx55D_t &fitDerI, const Mtx55D_t &fitDer) const
{
static const int mius[]= {1,2,5,8,9,10,13,14,0};
  assert(_hz);
  if (fitDerI != fitDer) memcpy(fitDerI[0],fitDer[0],sizeof(Mtx55D_t));
  for (int i=0;mius[i];i++) {fitDerI[0][mius[i]]=-fitDerI[0][mius[i]];}
}

//______________________________________________________________________________
StvNodePars &StvNodePars::operator=(const StvNodePars& fr) 
{
  if (&fr==this)	return *this;
  memcpy(this,&fr,sizeof(*this));
  assert(_hz);
  return *this;
}
//______________________________________________________________________________
const StvFitPars &StvNodePars::operator-(const StvNodePars& sub) const
{
static StvFitPars fp;
  double cos2L = 1./(1+sub._tanl*sub._tanl); 
  double cosL  = sqrt(cos2L);
  double sinL  = sub._tanl*cosL;
  double dx = _x-sub._x;
  double dy = _y-sub._y;
  double dz = _z-sub._z;
  
  fp.mH = dx*(     -sub._sinCA)+ dy*(      sub._cosCA);
  fp.mZ = dx*(-sinL*sub._cosCA)+ dy*(-sinL*sub._sinCA) +dz*cosL;
  fp.mA = (_psi -sub._psi );
  if (fp.mA < -M_PI) fp.mA += M_PI*2;
  if (fp.mA >  M_PI) fp.mA -= M_PI*2;
  fp.mP = (_ptin-sub._ptin);
  double tL = (_tanl-sub._tanl)/(1+_tanl*sub._tanl);
  if (fabs(tL)<0.05) { fp.mL = tL*(1-tL*tL/3); }
  else               { fp.mL = atan(tL)      ; }
  if (fp.mL < -M_PI) fp.mL += M_PI*2;
  if (fp.mL >  M_PI) fp.mL -= M_PI*2;

//  fp.Check("StvNodePars::operator-");
  return fp;
}
//______________________________________________________________________________
void StvNodePars::operator+=(const StvFitPars &fp)
{
  assert(_hz);
  double cos2L = 1./(1+_tanl*_tanl); 
  double cosL  = sqrt(cos2L);
  double sinL  = _tanl*cosL;
  _x += -_sinCA*fp.mH - sinL*_cosCA*fp.mZ;
  _y +=  _cosCA*fp.mH - sinL*_sinCA*fp.mZ;
  _z +=                 cosL       *fp.mZ;

  double a = fp.mA,cA,sA;
  if (fabs(a) < 0.1) { sA = a*(1-a*a/6); cA = 1-a*a/2;}
  else               { sA = sin(a)     ; cA = cos(a) ;}                
 _psi   += a;
  double cosCA = _cosCA;
  _cosCA = cosCA*cA-_sinCA*sA;
  _sinCA = cosCA*sA+_sinCA*cA;

  _ptin  += fp.mP;

  double l = fp.mL,tL;
  if (fabs(l) < 0.1) { tL = l*(1+l*l/3); }
  else               { tL = tan(l)      ;}                
  _tanl = (tL+_tanl)/(1.-_tanl*tL);
  if (_tanl < -MAXTAN) _tanl = -MAXTAN;
  if (_tanl >  MAXTAN) _tanl =  MAXTAN;
  _curv   = _hz *_ptin;
  if (fabs( _cosCA)>1 || fabs( _sinCA)>1) ready();
  assert(!check("StvNodePars::operator+=") || 1);
}
//______________________________________________________________________________
void StvNodePars::print() const
{
static const char* tit[]={"cosCA","sinCA","X","Y","Z","Eta","Ptin","TanL","Curv",0};
  for (int i=-2;i<kNPars+1;i++) {printf("%s = %g, ",tit[i+2],P[i]);}
  printf("\n");
}   
//______________________________________________________________________________
void StvNodePars::convert( Mtx55D_t &fitDer, const Mtx55D_t &hlxDer) const
{
enum {kHf,kZf,kAf,kLf,kPf};
enum {kHh,kAh,kCh,kZh,kLh};

  double cosL = 1/sqrt(1+_tanl*_tanl);
  double sinL = _tanl*cosL;
  double tanL = _tanl;
  double rho = _curv;
  double mHz = _hz;

  assert(_hz);
fitDer[kHf][kHf] = hlxDer[kHh][kHh];
fitDer[kHf][kZf] = hlxDer[kHh][kAh]*( tanL*rho) + hlxDer[kHh][kZh]*1/cosL;
fitDer[kHf][kAf] = hlxDer[kHh][kAh];
fitDer[kHf][kLf] = hlxDer[kHh][kLh];
fitDer[kHf][kPf] = hlxDer[kHh][kCh]*(mHz);
fitDer[kZf][kHf] = cosL*hlxDer[kZh][kHh];
fitDer[kZf][kZf] = cosL*(hlxDer[kZh][kAh]*( tanL*rho) + hlxDer[kZh][kZh]*1/cosL);
fitDer[kZf][kAf] = cosL*hlxDer[kZh][kAh];
fitDer[kZf][kLf] = cosL*hlxDer[kZh][kLh];
fitDer[kZf][kPf] = cosL*hlxDer[kZh][kCh]*(mHz);
fitDer[kAf][kHf] = hlxDer[kAh][kHh] + (-sinL*rho)*hlxDer[kZh][kHh];
fitDer[kAf][kZf] = hlxDer[kAh][kAh]*( tanL*rho) + hlxDer[kAh][kZh]*1/cosL + (-sinL*rho)*(hlxDer[kZh][kAh]*( tanL*rho) + hlxDer[kZh][kZh]*1/cosL);
fitDer[kAf][kAf] = hlxDer[kAh][kAh] + (-sinL*rho)*hlxDer[kZh][kAh];
fitDer[kAf][kLf] = hlxDer[kAh][kLh] + (-sinL*rho)*hlxDer[kZh][kLh];
fitDer[kAf][kPf] = hlxDer[kAh][kCh]*(mHz) + (-sinL*rho)*hlxDer[kZh][kCh]*(mHz);
fitDer[kLf][kHf] = hlxDer[kLh][kHh];
fitDer[kLf][kZf] = hlxDer[kLh][kAh]*( tanL*rho) + hlxDer[kLh][kZh]*1/cosL;
fitDer[kLf][kAf] = hlxDer[kLh][kAh];
fitDer[kLf][kLf] = hlxDer[kLh][kLh];
fitDer[kLf][kPf] = hlxDer[kLh][kCh]*(mHz);
fitDer[kPf][kHf] = (1/mHz)*hlxDer[kCh][kHh];
fitDer[kPf][kZf] = (1/mHz)*(hlxDer[kCh][kAh]*( tanL*rho) + hlxDer[kCh][kZh]*1/cosL);
fitDer[kPf][kAf] = (1/mHz)*hlxDer[kCh][kAh];
fitDer[kPf][kLf] = (1/mHz)*hlxDer[kCh][kLh];
fitDer[kPf][kPf] = (1/mHz)*hlxDer[kCh][kCh]*(mHz);


}
//______________________________________________________________________________
//______________________________________________________________________________
void StvHitErrs::rotate(double angle)
{
  double t[2][2];
  t[0][0] = cos(angle); t[0][1] = -sin(angle);
  t[1][0] = -t[0][1]  ; t[1][1] = t[0][0];
  double r[3];
  TCL::trasat(t[0],&hXX,r,2,2);
  TCL::ucopy(r,&hXX,3);
}


//______________________________________________________________________________
//______________________________________________________________________________
StvFitErrs::StvFitErrs(double hh,double hz,double zz)
{
  memset(this,0,sizeof(*this));
  mHH=hh;mHZ=hz;mZZ=zz;mHz = 3e33;
}
//______________________________________________________________________________
double StvFitErrs::Sign() const {return EmxSign(5,Arr());}
//______________________________________________________________________________
const StvFitErrs &StvFitErrs::operator*(const Mtx55D_t &how) const
{
static StvFitErrs myFitErrs;
  TCL::trasat(how[0],Arr(),myFitErrs.Arr(),5,5);
  myFitErrs.mHz = mHz;
  myFitErrs.Recov();
  return myFitErrs;
}  
//______________________________________________________________________________
void StvFitErrs::Reset()
{
  memset(this,0,sizeof(*this));
  mHH =  MAXFITERR[0]*MAXFITERR[0]*MAXERRFACT;
  mZZ =  MAXFITERR[1]*MAXFITERR[1]*MAXERRFACT;
  mAA =  MAXFITERR[2]*MAXFITERR[2]*MAXERRFACT;
  mLL =  MAXFITERR[3]*MAXFITERR[3]*MAXERRFACT;
  mPP =  MAXFITERR[4]*MAXFITERR[4]*MAXERRFACT;
  mHz = 3e33;
}

//______________________________________________________________________________
void StvFitErrs::Set(const THelixTrack *he, double hz)
{
mHz = hz;assert(fabs(hz)<0.002);
const THEmx_t *emx = he->Emx();
double  cosL = he->GetCos();
double  sinL = he->GetSin();
double  rho  = he->GetRho();
double  dAdZ = -sinL*cosL*rho;
mHH = emx->mHH;
mHZ = cosL*emx->mHZ;
mZZ = cosL*emx->mZZ*cosL;
mHA = emx->mHA + (dAdZ)*emx->mHZ;
mZA = (emx->mAZ + (dAdZ)*emx->mZZ)*cosL;
mAA = emx->mAA + (dAdZ)*emx->mAZ + (emx->mAZ + (dAdZ)*emx->mZZ)*(dAdZ);
mHL = emx->mHL;
mZL = emx->mZL*cosL;
mAL = emx->mAL + emx->mZL*(dAdZ);
mLL = emx->mLL;
mHP = (1/mHz)*emx->mHC;
mZP = (1/mHz)*emx->mCZ*cosL;
mAP = (1/mHz)*emx->mAC + (1/mHz)*emx->mCZ*(dAdZ);
mLP = (1/mHz)*emx->mCL;
mPP = (1/mHz)*emx->mCC*(1/mHz);
  Recov();
}  
//______________________________________________________________________________
void StvFitErrs::Get(THelixTrack *he) const
{
  assert(mHz && fabs(mHz)<.002);
  he->SetEmx(0);
  THEmx_t *emx = he->Emx();
  double  cosL = he->GetCos();
  double  sinL = he->GetSin();
  double  rho  = he->GetRho();
  double  dAdZeta = sinL*rho;
  emx->mHH = mHH;
  emx->mHA = ( dAdZeta)*mHZ + mHA;
  emx->mAA = ( dAdZeta)*(mZZ*( dAdZeta) + mZA) + mZA*( dAdZeta) + mAA;
  emx->mHC = (mHz)*mHP;
  emx->mAC = (mHz)*(mZP*( dAdZeta) + mAP);
  emx->mCC = (mHz)*mPP*(mHz);
  emx->mHZ = 1/cosL*mHZ;
  emx->mAZ = 1/cosL*(mZZ*( dAdZeta) + mZA);
  emx->mCZ = 1/cosL*mZP*(mHz);
  emx->mZZ = 1/cosL*mZZ*1/cosL;
  emx->mHL = mHL;
  emx->mAL = mZL*( dAdZeta) + mAL;
  emx->mCL = mLP*(mHz);
  emx->mZL = mZL*1/cosL;
  emx->mLL = mLL;

}  
//_____________________________________________________________________________
void StvFitErrs::Set(const StvFitErrs &fr,double errFactor)
{
  Reset();
  mHz = fr.mHz;  
  assert(mHz && fabs(mHz)<0.002);
  double const *e =fr.Arr();
  double       *ee=   Arr();
  for (int i=0,li=0;i< 5;li+=++i) {
    ee[li+i] = e[li+i]*errFactor;
    if (ee[li+i] > MAXFITERR[i]*MAXFITERR[i]) 
        ee[li+i] = MAXFITERR[i]*MAXFITERR[i];
  }
}
//_____________________________________________________________________________
void StvFitErrs::Backward()
{
  mHA*=-1; mAP*=-1; mHZ*=-1; mZP*=-1; mAL*=-1; mZL*=-1;
}
//_____________________________________________________________________________
int StvFitErrs::Check(const char *tit) const
{
  ((StvFitErrs*)((void*)this))->Recov();
  int ierr=0;
  double dia[5];const double *e=&mHH;
  for (int i=0,li=0;i< 5;li+=++i) {
    dia[i]=e[li+i];
    if (dia[i]< 1e-8*MAXFITERR[i]*MAXFITERR[i]) {ierr = i+1; goto ERR;}
    if (dia[i]> 1e+4*MAXFITERR[i]*MAXFITERR[i]) {ierr = i+6; goto ERR;}
    for (int j=0;j<i;j++) {
       if (e[li+j]*e[li+j]>=dia[i]*dia[j]){ierr = 100+10*i+j;goto ERR;}
    } }
  if (!(mHz && fabs(mHz) <.002)) {ierr = 1001; goto ERR;}
  return 0;
ERR: if (!tit) return ierr;
  printf("StvFitErrs::Check(%s)=%d\n",tit,ierr);
  Print();
  return ierr;
}     
//_____________________________________________________________________________
int StvFitErrs::Recov()
{

  double dia[5],fak[5];double *e=&mHH;

  int nerr=0;
//		Check diag errs
  for (int i=0,li=0;i< 5;li+=++i) {
    fak[i]=1;
    if (e[li+i] < MAXFITERR[i]*MAXFITERR[i]) continue;
    fak[i] = 0.99*MAXFITERR[i]/sqrt(e[li+i]); nerr++;
  };
  if (nerr) {  		//Recovery
    for (int i=0,li=0;i< 5;li+=++i) {
      for (int j=0;j<=i;j++) {
        e[li+j]*=fak[i]*fak[j];
  } } }

//		Check correlations & Recovery
  for (int i=0,li=0;i< 5;li+=++i) {
    dia[i]=e[li+i];
    for (int j=0;j<i;j++) {
       if (e[li+j]*e[li+j]<dia[i]*dia[j]) continue;
       double qwe = 0.98*sqrt(dia[i]*dia[j]);
       e[li+j] = (e[li+j]<0)? -qwe:qwe;
       nerr++;
  } }
  return nerr;
}     

//_____________________________________________________________________________
 void StvFitErrs::Print(const char *tit) const
{
static const char *N="HZALP";
  if (!tit) tit = "";
  printf("StvFitErrs::Print(%s) ==\n",tit);
  const double *e = &mHH;
  for (int i=0,li=0;i< 5;li+=++i) {
    printf("%c ",N[i]);
    for (int j=0;j<=i;j++) {
    printf("%g\t",e[li+j]);} 
    printf("\n");
  }
}
//_____________________________________________________________________________
int StvFitPars::TooBig(const StvNodePars &np) const
{
  
  double space = BIGFITPART*(fabs(np._x)+fabs(np._y)+fabs(np._z));
  for (int i=0;i<2;i++) {if (fabs(Arr()[i]) > BIGFITPARS[i]+space) return i+1;};
  for (int i=2;i<4;i++) {if (fabs(Arr()[i]) > BIGFITPARS[i]      ) return i+1;};

  if (fabs(mP) > BIGFITPARS[4]+fabs(np._ptin)*BIGFITPART)     	   return 4+1;
  return 0;
}
//_____________________________________________________________________________
int StvFitPars::Check(const char *tit) const
{
  int ifail = 0;
  for (int i=0;i<5;i++) {if (fabs(Arr()[i]) > MAXFITPARS[i]){ifail=i+1;break;}};
  if (!ifail) return 0;
  if (!tit || !tit[0]) return ifail;
  TString ts(tit);ts +=" *** Check = "; ts+=ifail;ts +=" ***";
  Print(ts.Data());
  return 0;
}
//_____________________________________________________________________________
void StvFitPars::Print(const char *tit) const
{
static const char* Nams[]={"mH","mZ","mA","mL","mP",0};
  if (tit && tit[0]) printf("StvFitPars::Print(%s)\n",tit);
  for (int i=0;Nams[i]; i++) {printf("%s=%g ",Nams[i],Arr()[i]);}
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
void StvNodePars::GetRadial(double radPar[6],double radErr[15],const StvFitErrs *fitErr) const
{
//Remind StvFitPars:
//double mH;	// direction perpendicular movement and Z
//double mZ;	// Pseudo Z, direction perpendicular movement & H
//double mA;	// Angle in XY. cos(A),sin(A),T moving direction
//double mL;	// Angle lambda in Rxy/Z
//double mP;	// 1/pt with curvature sign

  enum {jRad=0,jPhi,jZ,jTan,jPsi,jPti};
  enum {jRPhiRPhi=0
       ,jRPhiZ  ,jZZ
       ,jRPhiTan,jZTan,jTanTan
       ,jRPhiPsi,jZPsi,jTanPsi,jPsiPsi
       ,jRPhiPti,jZPti,jTanPti,jPsiPti,jPtiPti};
  double rxy = getRxy();
  radPar[jRad] = rxy;
  radPar[jPhi] = atan2(_y,_x);
  radPar[jZ  ] = _z;
  radPar[jTan] = _tanl;
  radPar[jPsi] = _psi;
  radPar[jPti] = _ptin;
  if (!radErr) return;

//double R2 = radPar[jRad]*radPar[jRad];
  double cos2L = 1./(1+_tanl*_tanl);
  double cosL = sqrt(cos2L);
  double sinL = _tanl*cosL;
			
  double dcaFrame[3][3]={{  cosL*_cosCA, cosL*_sinCA, sinL}	
                        ,{      -_sinCA,      _cosCA,    0}
                        ,{ -sinL*_cosCA,-sinL*_sinCA, cosL}};
			

  double radFrame[3][3]={{  _x/radPar[jRad],_y/radPar[jRad], 0}	
                        ,{ -_y/radPar[jRad],_x/radPar[jRad], 0}
                        ,{                0,              0, 1}};


  double radDca[3][3];

  TCL::mxmpy1(radFrame[0],dcaFrame[0],radDca[0],3,3,3);
  if (fabs(radDca[0][0])<1e-5) {
    radDca[0][0] = (radDca[0][0]<0)? -1e-5:1e-5;}

  double asd[2][2];
  for (int i=0;i<2;i++) {
  for (int j=0;j<2;j++) {
    asd[i][j] = radDca[i+1][j+1] - radDca[i+1][0]*radDca[0][j+1]/radDca[0][0];}}

  double T[5][5] = {{asd[0][0], asd[0][1], 0,        0, 0}
                   ,{asd[1][0], asd[1][1], 0,        0,	0}
                   ,{        0,         0, 0, 1./cos2L, 0}
                   ,{        0,         0, 1,        0, 0}
                   ,{        0,         0, 0,        0, 1}};

  TCL::trasat(T[0],fitErr->Arr(),radErr,5,5); 
  
  double dia[5];
  for (int i=0,li=0;i<5;li+=++i) {
    dia[i]= radErr[li+i];assert(dia[i]>0);
    for (int j=0;j<i;j++) {
      assert(radErr[li+j]*radErr[li+j]< dia[i]*dia[j]); }}


}
//_____________________________________________________________________________
void StvNodePars::GetImpact(StvImpact *imp,const StvFitErrs *fe)  const
{
    /// signed impact parameter; Signed in such a way that:
    ///     x =  -impact*sin(Psi)
    ///     y =   impact*cos(Psi)
  imp->mImp = _x*(-_sinCA) + _y*(_cosCA);
  double tst = _x*(_cosCA) + _y*(_sinCA);
  assert(fabs(tst)<1e-5 || fabs(imp->mImp) > 1000*fabs(tst));
  imp->mZ   = _z;
  imp->mPsi = _psi;
  imp->mPti = _ptin;
  imp->mTan = _tanl;
  imp->mCurv= _curv;
  if (!fe) return;

  double cos2L = 1./(1+_tanl*_tanl);
  double cosL  =sqrt(cos2L);
  double Rxy   = fabs(imp->mImp);
  double T[5][5] = {
  {1,    0,         0, 0,          0},
  {0, cosL, _tanl*Rxy, 0,          0},
  {0,    0,         1, 0,          0},
  {0,    0,         0, 0,          1},
  {0,    0,         0, 1/cos2L,    0}};
  double qwe[15];
  TCL::trasat(T[0],fe->Arr(),qwe,5,5); 
  TCL::ucopy(qwe,&imp->mImpImp,15);
}
//______________________________________________________________________________
const StvFitPars &StvFitPars::operator*(const Mtx55D_t &t) const  
{
static StvFitPars myPars;
  TCL::vmatl(t[0],Arr(),myPars.Arr(),5,5);
//  assert(!myPars.Check("StvFitPars::operator*") || 1);
  return myPars;
}
//_____________________________________________________________________________
void StvFitErrs::Get(const StvNodePars *np,  StvNodeErrs *ne) const
{
  double cos2L = 1./(np->_tanl*np->_tanl+1);
  double dTdL = 1./cos2L;
  double cosL = sqrt(cos2L);

//                       d/dH     d/dZ       d/dA     d/dLam d/dPti
  double T[6][5] = {{ np->_cosCA,   0,         0,         0,  0}   //dX
  		   ,{ np->_sinCA,   0,         0,         0,  0}   //dY
  		   ,{         0,    1/cosL,    0,         0,  0}   //dZ
  		   ,{         0,    0,         1,         0,  0}   //dEta
  		   ,{         0,    0,         0,         0,  1}   //dPti
  		   ,{         0,    0,         0,      dTdL,  0}}; //dTan

  assert(0);
  TCL::trasat(T[0],this->Arr(),ne->A,5,6); 

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

//_____________________________________________________________________________
//_____________________________________________________________________________
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
  double thPars[7+15],Hz=0.000299792458 * 4.98478;
  for (int i=0;i<7+15;i++) {thPars[i]=gRandom->Rndm();}
  THelixTrack th(thPars,thPars+3,thPars[6]),thh;
  memcpy(thPars+3,th.Dir(),3*sizeof(double));
  th.SetEmx(thPars+7);
  
  StvNodePars pars;
  StvFitErrs  errs;
  pars.set(&th,Hz);
  errs.Set(&th,Hz);
  pars.reverse();
  errs.Backward();
  pars.get(&thh);
  errs.Get(&thh);
  thh.Backward();

  double thhPars[7+15];
  memcpy(thhPars,thh.Pos(),7*sizeof(double));
  memcpy(thhPars+7,thh.Emx()->Arr(),15*sizeof(double));
  int nerr=0;
  for (int i=0;i<7+15;i++) {
    if (fabs(thhPars[i]-thPars[i]) <1e-6) continue;
    nerr++;printf("%d = %g %g \n",i,thPars[i],thhPars[i]);}
  printf("nmErrs = %d\n",nerr);
  
}
//_____________________________________________________________________________
void StvNodeParsTest::TestGetRadial(int nEv)
{
StvFitErrs fE;
  double f = 0.01;
  TVectorD D(5);
  D[0] = 1.0*1.0	*f;
  D[1] = 2.0*2.0	*f;
  D[2] = 0.03*0.01	*f;
  D[3] = 0.04*0.01	*f;
  D[4] = 0.07*0.07	*f;
  TRandomVector RV(D);

  TMatrixD S(RV.GetMtx());
  S.Print();

  double *e = &fE.mHH;
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i;j++    ) {
       e[li+j]= S[i][j];
  } }


  StvNodePars node;
  node.reset();
  double myRad = 100;
  double phi = gRandom->Rndm()*2*M_PI;
  node._x    = myRad*cos(phi);
  node._y    = myRad*sin(phi);
  node._z    = (gRandom->Rndm()-0.5)*200;
  node._psi  = phi + (gRandom->Rndm()-0.5);
  node._tanl = gRandom->Rndm();
  node._ptin = (gRandom->Rndm()-0.5);
  node._hz   = 0.000299792458 * 4.98478;
  node.ready();

  double radPar[6],radErr[15];
  node.GetRadial(radPar,radErr,&fE);
  TMatrixDSym SS(5);
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i;j++    ) {
       SS[i][j]=radErr[li+j]; SS[j][i]=radErr[li+j];
  } }
  SS.Print();

  double rE[15]={0};
  for (int ev=0;ev<nEv;ev++) 
  {
    const TVectorD &res = RV.Gaus();
    StvNodePars myNode(node);
    StvFitPars  fitPars(res.GetMatrixArray());
    myNode +=fitPars;
//		Project it to x*x+y*y=r*r
    myNode.moveToR(myRad);
    double rP[6];
    myNode.GetRadial(rP);
    
    for (int i=0,li=0;i< 5;li+=++i) {
      for (int j=0;j<=i;j++    ) {
      rE[li+j]+= (rP[i+1]-radPar[i+1])*(rP[j+1]-radPar[j+1]);
    } }
  }  //End events
  for (int j=0;j<15;j++) {rE[j]/=(nEv);}
  double qA=0,qAmax=0,dia[5];
  for (int i=0,li=0;i< 5;li+=++i) {
    dia[i]=radErr[li+i];
    for (int j=0;j<=i;j++) {
      double nor = sqrt(dia[i]*dia[j]);
      double dif = (rE[li+j]-radErr[li+j])/nor;
      printf("(%d %d) \t%g = \t%g \t%g\n",i,j,radErr[li+j]/nor,rE[li+j]/nor,dif);
      dif = fabs(dif);
      qA+= (dif); if (dif>qAmax) qAmax=dif;
  } }
  qA/=15;
  printf("Quality %g < %g < 1\n",qA,qAmax);
}

//_____________________________________________________________________________
static void Add(THelixTrack &ht,const double add[5]) 
{
// add = H,A,C,Z,L
  TVector3  pos(ht.Pos()),dir(ht.Dir()),ort(-dir[1],dir[0],0.);
  ort = ort.Unit();
  double rho=ht.GetRho();
  pos+=ort*add[0]; pos[2]+=add[3];
  dir.SetMagThetaPhi(1.,dir.Theta()-add[4],dir.Phi()+add[1]);
  double wk[7]={pos[0],pos[1],pos[2],dir[0],dir[1],dir[2],rho+add[2]};
  ht.Set(wk,wk+3,wk[6]);
}


//_____________________________________________________________________________
void StvNodeParsTest::TestErrProp(int nEv)
{

StvNodePars iP,iPR,oP,oPR;
THelixTrack iH,iHR,oH,ht;

THEmx_t oHE,oHER;
double dia[5],*e,*er,*vtx;

  iP._cosCA = 0.051522195951218416; iP._sinCA = -0.99867184876021664;  iP._x   = 56.80456301948584; 
  iP._y     = -179.95090442478528;  iP._z     = 16.833129146428401;    iP._psi = -1.5192513089402997; iP._ptin = -4.286089548109465; 
  iP._tanl  = -0.71077992742240803; iP._curv  = -0.0063779138641975935;iP._hz=(0.0014880496061989194);
  iP.ready();
StvFitErrs iE,oE,oER;
  iE.mHH = 0.0025928369042255385;  iE.mHZ = -4.9934860023454386e-11; iE.mZZ = 0.014598355970801268; iE.mHA = -0.00059887440419442305; 
  iE.mZA = 1.0958739205478152e-11; iE.mAA = 0.00026524379894739812;  iE.mHL = 3.463001237863329e-12; iE.mZL = -0.0016525557966380938; 
  iE.mAL = 8.3669926017237923e-13; iE.mLL = 0.00041855110437868546;  iE.mHP = 0.0043962440767417576; iE.mZP = -2.904206508909407e-11; 
  iE.mAP = -0.0041320793241820105; iE.mLP = -2.5031139398137018e-12; iE.mPP = 0.78568815092933286; iE.SetHz(0.0014880496061989194);

  oER*=0.;
  oHER.Clear();

//		Prepare error matrix for TRandomVector
  TMatrixDSym S(5);
  e = iE.Arr();
  for (int i=0,li=0;i< 5;li+=++i) {S[i][i] = e[li+i];}
  TRandomVector::RandRotate(S);

  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i;j++)        { e[li+j] = S[i][j];}}
//
  iE.Print("Input StvFitErrs");

  iP.get(&iH);	// nodePar => Helix
  iE.Get(&iH);	// fitErr  => HelixErr
  iE.Set(&iH,iP._hz);
  iE.Print("Input StvFitErrs => THEmx_t => StvFitErrs");


  iH.Emx()->Print("Input Helix Errs");
  oH = iH;
  double myDist = 33;
  oH.Move(myDist);		//Move helix to 100cm
  oP.set(&oH,iP._hz);		//helix100 => nodePar100
  oE.Set(&oH,iP._hz);		//helixErr100 => fitErr100
  oE.Print("Output StvFitErrs");
  oH.Emx()->Print("Output Helix Errs");

  vtx = oH.Pos();
  TVector3 Vtx(vtx);
  TVector3 Dir(oH.Dir());
  TVector3 Nxy(-Dir[1],Dir[0],0.); Nxy = Nxy.Unit();

static int iHELIX=0;

//		Prepare error matrix for TRandomVector
  e = (iHELIX) ? iH.Emx()->Arr() : iE.Arr();
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i;j++    ) {
       S[i][j]=e[li+j]; S[j][i]=e[li+j];
    } }
  TRandomVector RV(S);

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
      ht.SetEmx(0);
    }
      
// //		Change helix direction for fun
//     ht.Backward();
//		Move random helix to DCA point of etalon vertex
    double my100 = ht.Path(vtx);
    assert(fabs(my100-myDist) <20);
    ht.Move(my100);
    oPR.set(&ht,iP._hz);
    StvFitPars fp = (oPR-oP);
    double *e = oER.Arr();
    double *d = fp.Arr();
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
    D[4] = -(VDir.Theta()-Dir.Theta());// "-" due to theta)= Pi-Lambda

    e = oHER.Arr();
    for (int i=0,li=0;i< 5;li+=++i) {
      for (int j=0;j<=i;j++    ) {
        e[li+j]+=D[i]*D[j];
    } } 


  }//EndEvts

  oER *= (1./nEv);
  oHER*= (1./nEv);
  
  printf("*** Check THelixTrack Error matrix ***\n");
  e = oH.Emx()->Arr();
  er = oHER.Arr();
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
  e = oE.Arr();
  er = oER.Arr();
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
#include "TMatrixT.h"
#include "TMatrixTSym.h"
#include "TVectorT.h"
//_____________________________________________________________________________
double EmxSign(int n,const float *e)
{
  enum {maxN =10,maxE = (maxN*maxN-maxN)/2+maxN};
  double d[maxE];
  assert(n>0 && n<=maxN);
  TCL::ucopy(e,d,(n*(n+1))/2);
  return EmxSign(n,d);
}
//_____________________________________________________________________________
double EmxSign(int n,const double *e)
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
void StvFitErrs::SetHz(double hz)
{ mHz=hz; assert(mHz && fabs(mHz)<0.002) ;}
double StvFitErrs::GetHz() const
{assert(mHz && fabs(mHz)<0.002);  return mHz ;}
