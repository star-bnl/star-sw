#if 0
#include <stdlib.h>
#include <math.h>
#include "TError.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "TSystem.h"
#include "TMath.h"
#include "TCernLib.h"
#include "TVector3.h"
#include "TVectorD.h"
#include "TRungeKutta.h"
#include "THelixTrack.h"
#include "TRandom.h"
#include "TRandomVector.h"
#include "TH1.h"
#include "TCernLib.h"
TRKuttaMag *TRungeKutta::fgMag=0;


//_____________________________________________________________________________
//_____________________________________________________________________________
ClassImp(TRungeKutta)
//_____________________________________________________________________________
void TRungeKutta::TRKutaPars_t::Print(const char *tit) const
{
   if (tit && *tit) {printf("TRKutaPars_t::Print(%s)\n",tit);}
   printf("X= %g %g %g D= %g %g %g Pinv = %g\n"
         ,X[0],X[1],X[2],D[0],D[1],D[2],Pinv);
}
//_____________________________________________________________________________
void TRungeKutta::Print(const char *tit) const
{
   if (tit && *tit) {printf("TRungeKutta::Print(%s)\n",tit);}
   fInp.Print("");
}

//_____________________________________________________________________________
TRungeKutta::TRungeKutta(int charge,const double *xyz,const double *mom,TRKuttaMag *Mag)
{
//      Generalization of RungeKutta algorithm with derivative and error propagationf
//      V.Perevoztchikov
//
  memset(fBeg,0,fEnd-fBeg);
  SetMag(Mag);
  Set(charge,xyz,mom);
}
//_____________________________________________________________________________
TRungeKutta::TRungeKutta()
{
  memset(fBeg,0,fEnd-fBeg);
  SetMag(0);
}
//_____________________________________________________________________________
TRungeKutta::~TRungeKutta()
{
  delete fEmx3d;
  delete fDer3d;
}
//_____________________________________________________________________________
TRungeKutta::TRungeKutta(const THelixTrack &from,TRKuttaMag* mag)
{
  memset(fBeg,0,fEnd-fBeg);
  SetMag(mag);
  double B[3],P[3];
  (*fGUFLD)(from.Pos(),B);
  assert(fabs(B[0])+fabs(B[1])<1e-3*fabs(B[2]));
  double rho = from.GetRho();
  int iq = ((rho>0)==(B[2]>0)) ? -1:1;
  TCL::vscale(from.Dir(),B[2]/rho,P,3);
  Set(iq,from.Pos(),P);
}
//_____________________________________________________________________________
void TRungeKutta::SetMag(TRKuttaMag* mag)
{
  fGUFLD = (mag)? mag:fgMag;
  fBInp[2] = 1e11;
  assert(fGUFLD);
}
//_____________________________________________________________________________
void TRungeKutta::ZetMag(const double *mag)
{
  memcpy(fBInp,mag,sizeof(fBInp));
  memcpy(fBOut,mag,sizeof(fBOut));
}
//_____________________________________________________________________________
void TRungeKutta::Set(int charge,const double *xyz,const double *mom)
{
  if (charge) fCharge = charge;
  memcpy(fInp.X,xyz,sizeof(fInp.X));
  memcpy(fInp.P,mom,sizeof(fInp.P));
  fInp.Pinv = -fCharge/sqrt(TCL::vdot(fInp.P,fInp.P,3));
  TCL::vscale(fInp.P,fabs(fInp.Pinv),fInp.D,3);
  if (fBInp[2]<1e3) return;
  (*fGUFLD)(fInp.X,fBInp);

}
//_____________________________________________________________________________
TRungeKutta::TRungeKutta(const TRungeKutta  *from)
{
  *this = *from; fEmx3d=0,fDer3d=0;
}
//_____________________________________________________________________________
TRungeKutta::TRungeKutta(const TRungeKutta  &from)
{
  *this = from;
  if (fEmx3d) {fEmx3d = new THEmx3d_t; *fEmx3d = *from.fEmx3d;}
  if (fDer3d) {fDer3d = new THDer3d_t; *fDer3d = *from.fDer3d;}
}
//_____________________________________________________________________________
void TRungeKutta::Backward()
{
  TCL::vscale(fInp.P,-1.,fInp.P,3);
  TCL::vscale(fInp.D,-1.,fInp.D,3);
  fCharge = - fCharge;
  fInp.Pinv = -fInp.Pinv;
  if (fEmx3d) fEmx3d->Backward();
  if (fDer3d) fDer3d->Backward();

}
//_____________________________________________________________________________
double TRungeKutta::Move(double step)
{
  fLen = step;
  if (fabs(fLen)<=kMicron) return 0;
  grkuta(fCharge ,fLen,fInp,fOut) ;
  fOut.Update();
  Move();
  return fLen;
}
//______________________________________________________________________________
double  TRungeKutta::GetCurv()   const   //Full curvature in current point
{
  double H = sqrt(fBInp[0]*fBInp[0]+fBInp[1]*fBInp[1]+fBInp[2]*fBInp[2]);
  return fInp.Pinv*H;
}
//______________________________________________________________________________
int  TRungeKutta::GetDir()   const   //
{
   double qwe = TCL::vdot(Pos(),Dir(),3);
   return (qwe<0)? 0:1;
}
//______________________________________________________________________________
double TRungeKutta::Eval(double step, double xyz[3], double mom[3]) const
{
  fLen = step;
  if (fabs(step)<=0) {
    if (xyz) memcpy(xyz,fInp.X,sizeof(fInp.X));
    if (mom) memcpy(mom,fInp.P,sizeof(fInp.P));
    return fLen;
  }
  grkuta(fCharge ,fLen,fInp,fOut) ;
  fOut.Update();
  if (xyz) memcpy(xyz,fOut.X,sizeof(fOut.X));
  if (mom) memcpy(mom,fOut.P,sizeof(fOut.P));
  return fLen;
}
//______________________________________________________________________________
double TRungeKutta::Move()
{
  if (IsDerOn()) {
    MakeMtx();
  }
  fInp = fOut;
  return fLen;
}
//______________________________________________________________________________
double TRungeKutta::Path(const double point[3], int idir,int ndca) const
{
static const double kEps=1e-7,kRef=1e-6;
assert(ndca==2 || ndca==3);
  double dx[3],gate[4]={-1e11,1e11};
  int myDir = idir, fail = 13,myState=0;
  fLen = 0; fOut = fInp;
  for (int iter=0; iter<=100; iter++) {
    double radius = 1./(fabs(GetCurv())+1e-11);
    double maxStep = (radius<100)? radius:100;

    auto inp = fOut;
assert(fLen>=gate[0] && fLen<=gate[1]);
    TCL::vsub(point,inp.X,dx,ndca); 
    double dis = sqrt(TCL::vdot(dx,dx,ndca));
    if (maxStep>dis) maxStep=dis;
    double tau = TCL::vdot(dx,inp.D,ndca);
    if (ndca==2) tau/=sqrt((1.-inp.D[2])*(1.+inp.D[2]));
    double space = fabs(inp.X[0])+fabs(inp.X[1]);
    if (fabs(tau)<kEps || fabs(tau)<kRef*space) {
      fLen+=tau; fail=0; break;
    }


    if (myDir>0) {
      if (tau<0) { tau= maxStep;} else { myDir = 0;}
    } else if (myDir<0){ 
      if (tau>0) { tau=-maxStep;} else { myDir = 0;}
    }
    if (fabs(tau)>maxStep) tau = (tau<0)? -maxStep:maxStep;
    if (tau<0) 	{ myState|=2;
     if (gate[1]>fLen) {gate[1]=fLen;gate[3]=-tau;}
    }
    else	{ myState|=1;
      if (gate[0]<fLen) {gate[0]=fLen;gate[2] = tau;}
    }  
    if (myState==3) {
      tau = ((gate[0]-fLen)*gate[3]+(gate[1]-fLen)*gate[2])/(gate[3]+gate[2]);
      int myLen = fLen+tau;
      if (iter>20 || myLen<=gate[0] || myLen>=gate[1]) tau = (gate[0]-fLen+gate[1]-fLen)/2;
    } else {
      tau = (tau<0)?  -maxStep:maxStep;
    }
    grkuta(fCharge ,tau,inp,fOut) ;
    fLen += tau;
assert(fLen>=gate[0] && fLen<=gate[1]);
  }
  if (fail)	return 2e22;

  fOut.Update();
  return fLen;
}
//______________________________________________________________________________
double TRungeKutta::Path(double x,double y, int idir) const
{
   double gde[2];
   gde[0] = x;gde[1] = y;
   return Path(gde,idir,2);
}
#if 0
//______________________________________________________________________________
double TRungeKutta::Path(const double point[3], int idir) const
{
static const double kEps=1e-5,kRef=1e-5,kBigEps=1e-1;
  double qaLeft,posLeft=0,qaRite=0,posRite=0,qaMine,qaStep,dx[3];
  double radius = 1./(fabs(GetCurv())+1e-11);
  radius/= sqrt(1. - fOut.D[2]*fOut.D[2])+1e-11;
  double maxStep = (radius<100)? radius:100;
  double minStep = 0.1;
  
  fLen = 0; fOut = fInp;
  for (int iter=0; iter<=20; iter++) {
    auto inp = fOut;
    TCL::vsub(inp.X,point,dx,3); 
    qaMine = -TCL::vdot(dx,inp.D,3);
    if (!iter)                  {qaLeft = qaMine;}
    else if (qaMine*qaLeft>0)   {qaLeft = qaMine;}
    else                        {qaRite = qaMine; posLeft=-qaStep; break ;}
    qaStep = fabs(qaMine); if (iter) qaStep*=2;
    if (qaStep > maxStep) qaStep = maxStep;
    if (qaStep < minStep) qaStep = minStep;
    if (idir) { qaStep*=idir;} else if (qaMine<0) {qaStep*=-1;};

    grkuta(fCharge ,qaStep,inp,fOut) ;
    fLen += qaStep;
    if (fabs(fLen)>6*radius)		return 1e11;
    assert(fabs(fLen)<6*radius);
  }
  if (!qaRite)				return 2e22;
  assert(qaRite);
  for (int iter=0; iter<=20; iter++) {
    auto inp = fOut;
    double denom = qaRite-qaLeft;
    if ((iter&3)==3 || fabs(denom) < 1e-4) { //Half step
      qaStep = 0.5*(posRite+posLeft);
    } else {				//Linear approach
      qaStep = -(qaLeft*posRite-qaRite*posLeft)/denom;
    }
    grkuta(fCharge ,qaStep,inp,fOut) ;
    fLen += qaStep;
    TCL::vsub(fOut.X,point,dx,3); 
    qaMine = -TCL::vdot(dx,fOut.D,3);

    if (qaLeft*qaMine>0) { qaLeft = qaMine; posLeft=0; posRite-=qaStep;}
    else                 { qaRite = qaMine; posRite=0; posLeft-=qaStep;}
    if (fabs(qaMine)>kBigEps)	continue;
    double dis2 = TCL::vdot(dx,dx,3);;
    if (fabs(qaMine) < kEps || qaMine*qaMine < kRef*kRef*dis2) break;
  }

  fOut.Update();
  return (fabs(qaMine)<kBigEps)? fLen:1e11;
}
#endif
//______________________________________________________________________________
double TRungeKutta::Dca(const double point[3],double *dcaErr) const
{
if(point || dcaErr){};
assert(!"TRungeKutta::Dca Not implemented");
return 0;
}
#if 0
//______________________________________________________________________________
double TRungeKutta::Path(double x,double y, int idir) const
{
static const double kEps=1e-5,kRef=1e-5,kBigEps=1e-1;
  double qaLeft,posLeft=0,qaRite=0,posRite=0,qaMine,qaStep,dx[2];
  double minStep = 0.1;
  double radius = 1./(fabs(GetCurv())+1e-11);
  radius/= sqrt(1. - fOut.D[2]*fOut.D[2])+1e-11;
  double maxStep = (radius<100)? radius:100;

  fLen = 0; fOut = fInp;
  for (int iter=0; iter<=20; iter++) {
    auto inp = fOut;
    dx[0] = inp.X[0]-x;dx[1] = inp.X[1]-y; 
    qaMine = -(dx[0]*inp.D[0] + dx[1]*inp.D[1]);
    qaMine /=(1.-inp.D[2])*(1.+inp.D[2]);
    if (!iter)                  {qaLeft = qaMine;}
    else if (qaMine*qaLeft>0)   {qaLeft = qaMine;}
    else                        {qaRite = qaMine; posLeft=-qaStep; break ;}
    qaStep = fabs(qaMine); if (iter) qaStep*=2;
    if (qaStep > maxStep) qaStep = maxStep;
    if (qaStep < minStep) qaStep = minStep;
    if (idir) { qaStep*=idir;} else if (qaMine<0) {qaStep*=-1;};


    grkuta(fCharge ,qaStep,inp,fOut) ;
    fLen += qaStep;
    if (fabs(fLen)>6*radius) 	return 1e11;
    assert(fabs(fLen)<6*radius);
    if (fabs(fLen)>6*radius) 	return 2e22;
  }
  if(!qaRite)			return 3e33;;
  assert(qaRite);
  for (int iter=0; iter<=20; iter++) {
    auto inp = fOut;
    double denom = qaRite-qaLeft;
    if ((iter&3)==3 || fabs(denom) < 1e-4) { //Half step
      qaStep = 0.5*(posRite+posLeft);
    } else {				//Linear approach
      qaStep = -(qaLeft*posRite-qaRite*posLeft)/denom;
    }
    grkuta(fCharge ,qaStep,inp,fOut) ;
    fLen += qaStep;
    dx[0] = fOut.X[0]-x;dx[1] = fOut.X[1]-y; 
    qaMine  = -(dx[0]*fOut.D[0]+dx[1]*fOut.D[1]);
    qaMine /= (1.-fOut.D[2])*(1.+fOut.D[2]);

    if (qaLeft*qaMine>0) { qaLeft = qaMine; posLeft=0; posRite-=qaStep;}
    else                 { qaRite = qaMine; posRite=0; posLeft-=qaStep;}
    if (fabs(qaMine)>kBigEps)	continue;
    double dis2 = dx[0]*dx[0]+dx[1]*dx[1];
    if (fabs(qaMine) < kEps || qaMine*qaMine< kRef*kRef*dis2) break;
  }

  fOut.Update();
  return (fabs(qaMine)<kBigEps)? fLen:1e11;
}
#endif
//______________________________________________________________________________
double TRungeKutta::Dca(double x,double y, double *dcaErr) const
{
if (x || y){}
return 0;
assert(!"TRungeKutta::Dca(x,y,dcaError) n oy implemented");
}
//______________________________________________________________________________
void TRungeKutta::MakeMtx()
{
  if (fabs(fLen) < kMicron) return;
  if (!fDer3d) fDer3d = new THDer3d_t;

//  	Move from Beg to End
  THelix3d TH0(fCharge,fInp.X,fInp.P,fBInp);
  TH0.SetDerOn();
  TH0.SetEmx(fEmx3d);
  TH0.Move(fLen);
  auto *myDer = TH0.Der();
  *fDer3d = *myDer;
  THEmx3d_t Emx1 = *(TH0.Emx());

//  	Move from End to Beg and then back to End
  TH0.Set(fCharge,fOut.X,fOut.P,fBOut);
  TH0.SetDerOn(0);
  TH0.Move(-fLen);
  TH0.SetEmx(fEmx3d);
  TH0.SetDerOn();
  TH0.Move(fLen);
  THEmx3d_t &Emx2 = *(TH0.Emx());
  Emx1.Update(Emx2.TkDir()); 
  
  TCL::vlinco(Emx1,0.5,Emx2,0.5,(*fEmx3d),15);
  fEmx3d->TkDir() = Emx1.TkDir();

}
//______________________________________________________________________________
void  TRungeKutta::SetEmx(const double err[15])
{
 SetDerOn();
 if (!fEmx3d) {fEmx3d = new THEmx3d_t;}
 THelix3d::MakeTkDir(fInp.D,fBInp,fEmx3d->TkDir());
 if (err) fEmx3d->Set(err);
}
//______________________________________________________________________________
void  TRungeKutta::SetEmx(const THEmx3d_t *err)
{
 SetDerOn();
 if (!fEmx3d) {fEmx3d = new THEmx3d_t;}
 if (err) *fEmx3d = *err;
 THelix3d::MakeTkDir(fInp.D,fBInp,fEmx3d->TkDir());
}
//______________________________________________________________________________
TRungeKutta &TRungeKutta::operator=(const TRungeKutta &from)
{
  memcpy(fBeg,from.fBeg,fEnd-fBeg+1);
  if (fEmx3d) {
    fEmx3d = new THEmx3d_t;
    *fEmx3d = *from.fEmx3d;
  }
  if (fDer3d) {
    fDer3d = new THDer3d_t;
    *fDer3d = *from.fDer3d;
  }
  return *this;
}
//______________________________________________________________________________
void TRungeKutta::grkuta(double CHARGE,double STEP
                        ,const TRKutaPars_t &VECT,TRKutaPars_t &VOUT) const
{
/*
* Revision 1.1.1.1  1997/11/03 15:30:47  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:21:42  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.23  by  S.Giani
*-- Author :
      SUBROUTINE GRKUTA (CHARGE,STEP,VECT,VOUT)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *  Runge-Kutta method for tracking a particle through a magnetic *
C.    *  field. Uses Nystroem algorithm (See Handbook Nat. Bur. of     *
C.    *  Standards, procedure 25.5.20)                                 *
C.    *                                                                *
C.    *  Input parameters                                              *
C.    *       CHARGE    Particle charge                                *
C.    *       STEP      Step size                                      *
C.    *       VECT      Initial co-ords,direction cosines,momentum     *
C.    *  Output parameters                                             *
C.    *       VOUT      Output co-ords,direction cosines,momentum      *
C.    *  User routine called                                           *
C.    *       CALL GUFLD(X,F)                                          *
C.    *                                                                *
C.    *    ==>Called by : <USER>, GUSWIM                               *
C.    *       Authors    R.Brun, M.Hansroul  *********                 *
C.    *                  V.Perevoztchikov (CUT STEP implementation)    *
C.    *                                                                *
C.    *                                                                *
C.    ******************************************************************
C.
*/
      double F[4];
      double XYZT[3], XYZ[3];
      double SECXS[4],SECYS[4],SECZS[4],HXP[3];
//       EQUIVALENCE (X,XYZ(1)),(Y,XYZ(2)),(Z,XYZ(3)),
//      +            (XT,XYZT(1)),(YT,XYZT(2)),(ZT,XYZT(3))
//*
      double& X  = XYZ[0];  double& Y  = XYZ[1];  double& Z  = XYZ[2];
      double& XT = XYZT[0]; double& YT = XYZT[1]; double& ZT = XYZT[2];

      enum  {MAXIT = 1992, MAXCUT = 11};
      static const double  EC=2.9979251e-4,DLT=1e-6,DLT32=DLT/32;;
      static const double  ZERO=0, ONE=1, TWO=2, THREE=3;
      static const double  THIRD=ONE/THREE, HALF=ONE/TWO;
      static const double  PISQUA=.986960440109e+01;
//      PARAMETER      (IX=1,IY=2,IZ=3,IPX=4,IPY=5,IPZ=6)
/*.
*.    ------------------------------------------------------------------
*.
*             This constant is for units CM,GEV/C and KGAUSS
*/
      double A,B,C,AT,BT,CT;
      double dA,dB,dC,EZT;
      double PINV,TL,H,H2,H4,REST,F1,F2,F3,F4,PH,PH2,DXT,DYT,DZT ;
      double EST,CBA,RHO,TET,HNORM,RHO1,SINT,COST;
      double G1,G2,G3,G4,G5,G6,HP,ANG2;
      if (EC||ZERO||EZT){}
      fNStps = 0;
      int ITER = 0;
      int NCUT = 0;
      VOUT = VECT;
 //VP     PINV   = EC * CHARGE / VECT.Mom;
      PINV   = -VECT.Pinv;      //the opposite sign to STAR definition
      TL = 0.;
      H      = STEP;
//*
//*
 L20: REST  = STEP-TL;
      if (fabs(H) > fabs(REST)) H = REST;
      (*fGUFLD)(VOUT.X,F);

//*
//*             Start of integration
//*
      X      = VOUT.X[0];
      Y      = VOUT.X[1];
      Z      = VOUT.X[2];
      A      = VOUT.D[0];
      B      = VOUT.D[1];
      C      = VOUT.D[2];
//*
      H2     = HALF * H;
      H4     = HALF * H2;
      PH     = PINV * H;
      PH2    = HALF * PH;
      SECXS[0] = (B * F[2] - C * F[1]) * PH2;
      SECYS[0] = (C * F[0] - A * F[2]) * PH2;
      SECZS[0] = (A * F[1] - B * F[0]) * PH2;
      ANG2 = (SECXS[0]*SECXS[0] + SECYS[0]*SECYS[0] + SECZS[0]*SECZS[0]);
      if (ANG2 > PISQUA) goto L40;
      DXT    = H2 * A + H4 * SECXS[0];
      DYT    = H2 * B + H4 * SECYS[0];
      DZT    = H2 * C + H4 * SECZS[0];
      XT     = X + DXT;
      YT     = Y + DYT;
      ZT     = Z + DZT;
//*
//*              Second intermediate point
//*
      EST = fabs(DXT)+fabs(DYT)+fabs(DZT);
      if (EST > fabs(H)) goto L30;

      (*fGUFLD)(XYZT,F);
      AT     = A + SECXS[0];
      BT     = B + SECYS[0];
      CT     = C + SECZS[0];
//*
      SECXS[1] = (BT * F[2] - CT * F[1]) * PH2;
      SECYS[1] = (CT * F[0] - AT * F[2]) * PH2;
      SECZS[1] = (AT * F[1] - BT * F[0]) * PH2;
      AT     = A + SECXS[1];
      BT     = B + SECYS[1];
      CT     = C + SECZS[1];
      SECXS[2] = (BT * F[2] - CT * F[1]) * PH2;
      SECYS[2] = (CT * F[0] - AT * F[2]) * PH2;
      SECZS[2] = (AT * F[1] - BT * F[0]) * PH2;
      DXT    = H * (A + SECXS[2]);
      DYT    = H * (B + SECYS[2]);
      DZT    = H * (C + SECZS[2]);
      XT     = X + DXT;
      YT     = Y + DYT;
      ZT     = Z + DZT;
      AT     = A + TWO*SECXS[2];
      BT     = B + TWO*SECYS[2];
      CT     = C + TWO*SECZS[2];
//*
      EST = fabs(DXT)+fabs(DYT)+fabs(DZT);
      if (EST > 2.*fabs(H)) goto L30;

      (*fGUFLD)(XYZT,F);
      memcpy(fBOut,F,sizeof(fBOut));
//*
      Z      = Z + (C + (SECZS[0] + SECZS[1] + SECZS[2]) * THIRD) * H;
      Y      = Y + (B + (SECYS[0] + SECYS[1] + SECYS[2]) * THIRD) * H;
      X      = X + (A + (SECXS[0] + SECXS[1] + SECXS[2]) * THIRD) * H;
//*
      SECXS[3] = (BT*F[2] - CT*F[1])* PH2;
      SECYS[3] = (CT*F[0] - AT*F[2])* PH2;
      SECZS[3] = (AT*F[1] - BT*F[0])* PH2;
      dA = (SECXS[0]+SECXS[3]+TWO * (SECXS[1]+SECXS[2])) * THIRD;
      dB = (SECYS[0]+SECYS[3]+TWO * (SECYS[1]+SECYS[2])) * THIRD;
      dC = (SECZS[0]+SECZS[3]+TWO * (SECZS[1]+SECZS[2])) * THIRD;
      A+=dA; B+=dB; C+=dC; EZT = fabs(dA)+fabs(dB) + fabs(dC);
//*
      EST    = fabs(SECXS[0]+SECXS[3] - (SECXS[1]+SECXS[2]))
      +        fabs(SECYS[0]+SECYS[3] - (SECYS[1]+SECYS[2]))
      +        fabs(SECZS[0]+SECZS[3] - (SECZS[1]+SECZS[2]));
//*
      if (/*EST > 1e-2*EZT &&*/ EST > DLT && fabs(H) > 1.E-2) goto L30;
      ITER = ITER + 1;
      NCUT = 0;
//*               If too many iterations, go to HELIX
      if (ITER > MAXIT) goto L40;
//*;
      TL = TL + H;
      if (EST < (DLT32)) {//then;
         H = H*TWO;
      }//endif
      CBA    = ONE/ sqrt(A*A + B*B + C*C);
//       iqwe++;
//       printf("%d UUUUUUUUUUU dX = %g %g %g\n",iqwe, X-VOUT.X[0],Y-VOUT.X[1],Z-VOUT.X[2]);


      VOUT.X[0] = X;
      VOUT.X[1] = Y;
      VOUT.X[2] = Z;
      VOUT.D[0] = CBA*A;
      VOUT.D[1] = CBA*B;
      VOUT.D[2] = CBA*C;
      REST = STEP - TL;
      if (STEP < 0.) REST = -REST;

      if (REST  >  1.E-5*fabs(STEP)) goto L20;
      if (REST  >  1.E-5) goto L20; //VP
//*
      goto L999;
//*
//**              CUT STEP
 L30: NCUT = NCUT + 1;
//*               If too many cuts , go to HELIX
      if (NCUT > MAXCUT)       goto L40;
      H = H*HALF;
      goto L20;
//*
//**              ANGLE TOO BIG, USE HELIX
 L40: F1  = F[0];
      F2  = F[1];
      F3  = F[2];
      F4  = sqrt(F1*F1+F2*F2+F3*F3);
      RHO = -F4*PINV;
      TET = RHO * STEP;
      if(fabs(TET) > 1e-6) {//then;
         HNORM = ONE/F4;
         F1 = F1*HNORM;
         F2 = F2*HNORM;
         F3 = F3*HNORM;
//*
         HXP[0] = F2*VECT.D[2] - F3*VECT.D[1];
         HXP[1] = F3*VECT.D[0] - F1*VECT.D[2];
         HXP[2] = F1*VECT.D[1] - F2*VECT.D[0];

         HP = F1*VECT.D[0] + F2*VECT.D[1] + F3*VECT.D[2];
//*
         RHO1 = ONE/RHO;
         SINT = sin(TET);
         COST = TWO*pow(sin(HALF*TET),2);
//*
         G1 = SINT*RHO1;
         G2 = COST*RHO1;
         G3 = (TET-SINT) * HP*RHO1;
         G4 = -COST;
         G5 = SINT;
         G6 = COST * HP;

         VOUT.X[0] = VECT.X[0] + (G1*VECT.D[0] + G2*HXP[0] + G3*F1);
         VOUT.X[1] = VECT.X[1] + (G1*VECT.D[1] + G2*HXP[1] + G3*F2);
         VOUT.X[0] = VECT.X[2] + (G1*VECT.D[2] + G2*HXP[2] + G3*F3);

         VOUT.D[0] = VECT.D[0] + (G4*VECT.D[0] + G5*HXP[0] + G6*F1);
         VOUT.D[1] = VECT.D[1] + (G4*VECT.D[1] + G5*HXP[1] + G6*F2);
         VOUT.D[2] = VECT.D[2] + (G4*VECT.D[2] + G5*HXP[2] + G6*F3);
//*
      } else {
         VOUT.X[0] = VECT.X[0] + STEP*VECT.D[0];
         VOUT.X[1] = VECT.X[1] + STEP*VECT.D[1];
         VOUT.X[0] = VECT.X[2] + STEP*VECT.D[2];
//*
      }//endif
//*
 L999:;
 fNStps = ITER;

}
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
class TestMag: public TRKuttaMag
{
  public:
    TestMag (double mag[3]){memcpy (mMag,mag,sizeof(mMag));}
    // methods
    virtual void operator()(const double X[3], double B[3])
    { memcpy(B,mMag,sizeof(mMag));};
  protected:
    // data members
    double mMag[3];
};

//______________________________________________________________________________
void TRungeKutta::Test(int flag)
{
// Flag == 3: 3 Dca test
// Flag == 2: 2 Dca test

  assert(flag==2 || flag==3);
  double PtGev = 3.,Rho = 1./100, Hz = PtGev*Rho ;
  double ZER[3]= {0,0,0};
  double HZ[3] = {0,0,Hz};
  double XZ[3] = {100,100,100};
  double PZ[3] = {PtGev,0,PtGev*4./3};
  double HH[3],XX[3],PP[3] ;

  TVector3 vPZ(PZ); vPZ.RotateZ(gRandom->Rndm()); vPZ.GetXYZ(PZ);
  TCL::ucopy(HZ,HH,3);
  TCL::ucopy(XZ,XX,3);
  TCL::ucopy(PZ,PP,3);

  if (flag==3) {
    double r1 = gRandom->Rndm(),r2 = gRandom->Rndm();
    TVector3 vHZ(HZ);
    vHZ.RotateX(r1); vHZ.RotateY(r2); vHZ.GetXYZ(HH);

    TVector3 vXZ(XZ);
    vXZ.RotateX(r1); vXZ.RotateY(r2); vXZ.GetXYZ(XX);
    TVector3 vPZ(PZ);
    vPZ.RotateX(r1); vPZ.RotateY(r2); vPZ.GetXYZ(PP);
  }
  TestMag myMag(HH);
  double s1,s2;
  for (int charge=1;charge >=-1;charge-=2) {

    THelix3d    TH(charge,ZER,PP,HH);
    TRungeKutta T3(charge,ZER,PP,&myMag);
    if (flag==3) {
      s1 = TH.Path(XX);
      s2 = T3.Path(XX);
    } else {
      s1 = TH.Path(XX[0],XX[1]);
      s2 = T3.Path(XX[0],XX[1]);
    }
    TH.Move(s1); T3.Move(s2);
    double dif[3];
    TCL::vsub(TH.Pos(),XX,dif,3);
    double ThTest = TCL::vdot(TH.Dir(),dif,flag);
    TCL::vsub(T3.Pos(),XX,dif,3);
    double TrTest = TCL::vdot(T3.Dir(),dif,flag);
    printf ("Charge = %d ThTest,TrTest = %g/%g %g/%g\n",charge,ThTest,s1,TrTest,s2);
  }

}
//______________________________________________________________________________
class Test2Mag: public TRKuttaMag
{
  public:
    Test2Mag (double mag[3]){memcpy (mMag,mag,sizeof(mMag));}
    // methods
    virtual void operator()(const double X[3], double B[3])
    {
      double fak = 1./(1. + (X[0]*X[0]+X[1]*X[1]+X[2]*X[2]/100)*1e-5);
      TCL::vscale(mMag,fak,B,3);};
    int birth() { return 1946;}

  protected:
    // data members
    double mMag[3];
};
//______________________________________________________________________________
void TRungeKutta::Test2()
{
  double PtGev = 3,Rho = 1./50, Hz = PtGev*Rho ;
  double ZER[3]= {0,0,0};
  double HZ[3] = {0,0,Hz};
  double PZ[3] = {PtGev,0,PtGev/3*4};
  TVector3 vPZ(PZ); vPZ.RotateZ(gRandom->Rndm()); vPZ.GetXYZ(PZ);

  Test2Mag myMag(HZ);

  for (int charge=1;charge >=-1;charge-=2) {
    double myD[3]={PZ[0],PZ[1],PZ[2]};
    double myX[3]={0,0,0};
    double myH[3],s1=0;
    double step = 1.;
    int nSteps = 100./step+1e-6;
    for (int is=0;is<nSteps;is++) {
      myMag(myX,myH);
      double myRho = -charge*myH[2]/PtGev;
      THelixTrack TH(myX,myD,myRho);
      TH.Move(step);
      TCL::ucopy(TH.Pos(),myX,3);
      TCL::ucopy(TH.Dir(),myD,3);
      s1+=step;
    }

    TRungeKutta T3(charge,ZER,PZ,&myMag);
    double s2 = T3.Path(myX);
    T3.Move();
    const double *pos = T3.Pos();
    printf("Helix.X = %g %g %g\n",myX[0],myX[1],myX[2]);
    printf("RKuta.X = %g %g %g\n",pos[0],pos[1],pos[2]);
    printf ("Charge = %d S1,S2 = %g %g \n",charge,s1,s2);
  }

}
//______________________________________________________________________________
void TRungeKutta::Test3()
{
//  enum {kX=0,kY=1,kZ=2,kPx=0,kPy=1,kPz=2,kP0x=0,kP0y=1,kP0z=2};
  double PtGev = 3.,Curv = 1./100, Hz = PtGev*Curv;
  double HZ[3] = {0,0,Hz};
  double Pbeg[3],Xbeg[3]={0},Pend[3];

  double X[200],Y[200],Z[200],A[200];

  TVector3 PbegV(PtGev,0,PtGev/3*4);
  PbegV.RotateZ(gRandom->Rndm()*3.1415*2); PbegV.GetXYZ(Pbeg);
  TVector3 XbegV(Xbeg);

  double Mag[3];
  TVector3 magV(HZ);
  double r1 =gRandom->Rndm(),r2 =gRandom->Rndm();
  magV.RotateX(r1); magV.RotateY(r2); magV.GetXYZ(Mag);
  Test2Mag myMag(Mag);
  double stp = 3.1415/180/10;
  int nJk = 100;
  double myMax = -999, myMin=999;
  double Plst[3];
  for (int jk=-1;jk<nJk;jk++) {
    PbegV.GetXYZ(Pbeg);
    TRungeKutta TR0(1,Xbeg,Pbeg,&myMag);
    TR0.Move(100.);
    TCL::ucopy(TR0.Mom(),Pend,3);
    if (jk>=0) {
      A[jk] = jk*stp;
      X[jk] = (Pend[0]-Plst[0])/stp;
      Y[jk] = (Pend[1]-Plst[1])/stp;
      Z[jk] = (Pend[2]-Plst[2])/stp;
      if (myMax<X[jk]) myMax=X[jk];
      if (myMax<Y[jk]) myMax=Y[jk];
      if (myMax<Z[jk]) myMax=Z[jk];
      if (myMin>X[jk]) myMin=X[jk];
      if (myMin>Y[jk]) myMin=Y[jk];
      if (myMin>Z[jk]) myMin=Z[jk];
    }
    PbegV.RotateY(stp);
    TCL::ucopy(Pend,Plst,3);

  }
static TGraph *Gx=0,*Gy=0,*Gz=0;
static TCanvas *myCanvas=0;
  delete Gx; delete Gy; delete Gz; delete myCanvas;
  Gx = new TGraph(nJk,A,X); Gx->SetLineColor(kRed  );Gx->SetMarkerColor(kRed  );
  Gy = new TGraph(nJk,A,Y); Gy->SetLineColor(kBlue );Gy->SetMarkerColor(kBlue );
  Gz = new TGraph(nJk,A,Z); Gz->SetLineColor(kGreen);Gz->SetMarkerColor(kGreen);



  Gx->SetMaximum(myMax);    Gx->SetMinimum(myMin);
  Gy->SetMaximum(myMax);    Gy->SetMinimum(myMin);
  Gz->SetMaximum(myMax);    Gz->SetMinimum(myMin);

  myCanvas = new TCanvas("TRungeKutta__Test3","",600,800);
  Gx->Draw("AP");
  Gy->Draw("same P");
  Gz->Draw("same P");

  myCanvas->Modified();
  myCanvas->Update();
  while(!gSystem->ProcessEvents()){gSystem->Sleep(200);};

}
//______________________________________________________________________________
void TRungeKutta::TestBak()
{
//  enum {kX=0,kY=1,kZ=2,kPx=0,kPy=1,kPz=2,kP0x=0,kP0y=1,kP0z=2};
  double PtGev = 3.,Curv = 1./100, Hz = PtGev*Curv;
  TVector3 PbegV(PtGev,0,PtGev/3*4);
  PbegV.RotateZ(gRandom->Rndm()*3.1415*2);
  TVector3 XbegV(10,20,30),X1endV,P1endV;

  double L = 10.;

  TVector3 magV(0.,0.,Hz);
  double r1 =gRandom->Rndm(),r2 =gRandom->Rndm();
  magV.RotateX(r1); magV.RotateY(r2);
  PbegV.RotateX(r1); PbegV.RotateY(r2);
  Test2Mag myMag((double*)&magV[0]);
//  TestMag myMag((double*)&magV[0]);

  for (int ichar=-1; ichar<=1;  ichar +=2) {
    auto DbegV = PbegV.Unit();
    TRungeKutta TR0(ichar,(double*)&XbegV[0],(double*)&PbegV[0],&myMag);
    TR0.Print("Normal");
    TVector3 P0endV,X0endV,D0endV;
    TVector3 P1endV,X1endV,D1endV;
    TR0.SetDerOn();
    TR0.Move(L);
    TR0.Eval(0,&X0endV[0],&P0endV[0]);
    D0endV = P0endV.Unit();
    const THDer3d_t &totDer0 =*TR0.Der();

    TRungeKutta TR1(ichar,(double*)&XbegV[0],(double*)&PbegV[0],&myMag);
    TR1.SetDerOn();
    TR1.Backward();
    TR1.Print("Negativ");
    TR1.Move(-L);
    TR1.Backward();
    TR1.Eval(0,&X1endV[0],&P1endV[0]);
    D1endV = P1endV.Unit();
    const THDer3d_t &totDer1 =*TR1.Der();
    TVector3 dif = X1endV-X0endV;
    for (int i=0;i<3;i++) {
       printf("X[%d] = %g \t%g \t%g\n",i,X0endV[i],X1endV[i],dif[i]); 
    }
    dif = D1endV-D0endV;
    for (int i=0;i<3;i++) {
       printf("D[%d] = %g \t%g \t%g\n",i,D0endV[i],D1endV[i],dif[i]); 
    }
static const char* dp[5] = {"U","V","Fita","Lama","Pinv"};
    
    for (int i=0;i<5;i++) {    
    for (int j=0;j<5;j++) {    
       printf("Der[%s][%s] = %g \t%g \t%g\n",dp[i],dp[j]
             ,totDer0[i][j],totDer1[i][j]
	     ,totDer1[i][j]-totDer0[i][j]); 
    } }  
    printf("\n");
  }
}



//______________________________________________________________________________
void TRungeKutta::TestDer()
{
//  enum {kX=0,kY=1,kZ=2,kPx=0,kPy=1,kPz=2,kP0x=0,kP0y=1,kP0z=2};
  double PtGev = 3.,Curv = 1./100, Hz = PtGev*Curv;
  double tkSys[2][4][3]={{{0}}};
  TVector3 PbegV(PtGev,0,PtGev/3*4);
  PbegV.RotateZ(gRandom->Rndm()*3.1415*2);
  TVector3 XbegV(10,20,30),X1endV,P1endV;

  double L = 100.;

  TVector3 magV(0.,0.,Hz);
  double r1 =gRandom->Rndm(),r2 =gRandom->Rndm();
  magV.RotateX(r1); magV.RotateY(r2);
//  PbegV.RotateX(r1); PbegV.RotateY(r2);
  Test2Mag myMag((double*)&magV[0]);
//  TestMag myMag((double*)&magV[0]);

// XbegV.Print("XbegV");
// PbegV.Print("PbegV");
//  magV.Print("magV");
  double totDif = 0;
  for (int ichar=-1; ichar<=1;  ichar +=2) {
    double Pinv = -ichar/PbegV.Mag();
    auto DbegV = PbegV.Unit();
    TRungeKutta TR0(ichar,(double*)&XbegV[0],(double*)&PbegV[0],&myMag);
    TVector3 PendV,XendV,DendV;
    TR0.SetDerOn();
    TR0.Move(L);
    TCL::ucopy(TR0.Dir(),tkSys[1][kKT],3);
    TR0.Eval(0,&XendV[0],&PendV[0]);
    DendV = PendV.Unit();
    const THDer3d_t &totDer =*TR0.Der();
    TCL::ucopy(totDer.TkDir(0)[0],tkSys[0][0],2*4*3);


    TVectorD der(5);
static const char* dp[5] = {"U","V","Fita","Lama","Pinv"};
    for (int J=0;J<5;J++) {

      printf("\nTest d/d%s\n\n",dp[J]);
      for (int i=0;i<5;i++) {der[i] = totDer[i][J];}
      double delta = 1e-4;

      double P1inv = Pinv;
      TVector3 D1begV = DbegV;
      TVector3 X1begV = XbegV;
      if (J<2)          { X1begV += TVector3(tkSys[0][kKU+J  ])*delta;}
      else if (J<4)     { D1begV += TVector3(tkSys[0][kKU+J-2])*delta;}
      else              { P1inv  += delta                        ;}

      auto P1begV = D1begV; P1begV*=(1./fabs(P1inv));
      TRungeKutta TR1(ichar,(double*)&X1begV[0],(double*)&P1begV[0],&myMag);
      TR1.Eval(L,&X1endV[0],&P1endV[0]);
      auto D1endV = P1endV.Unit();
      TVectorD U1endV(5);
      TVector3 difX =  X1endV-XendV;
      TVector3 difD =  D1endV-DendV;
      for (int j=0;j<5;j++) {
        if (j<2)        { U1endV[j] = difX.Dot(TVector3(tkSys[1][kKU+j  ]));}
        else if (j<4)   { U1endV[j] = difD.Dot(TVector3(tkSys[1][kKU+j-2]));}
        else            { U1endV[4] = -ichar/P1endV.Mag()-Pinv         ;}
      }

      TVectorD dif = U1endV;
      dif*=1./delta;
      double  pct=0; int ipct=-1;
      for (int i=0;i<5;i++) {
        if (fabs(dif[i])<1e-2) dif[i] = 0;
        if (fabs(der[i])<1e-2) der[i] = 0;
        double bott = 0.5*(fabs(dif[i])+fabs(der[i]));
        if (bott<1e-3) continue;
        double myPct = (dif[i]-der[i])/bott;
        if (fabs(myPct)<fabs(pct)) continue;
        pct = myPct; ipct = i;
      }
      if (pct >0.2) printf("################################  [%d] = %d%% \n",ipct,int(pct*100));
      dif.Print("NUMERIC ");
      der.Print("ANALITIC");
      totDif+= (dif-der).Norm1();
    }
  } //end charge loop
  printf ("========> TotalDiff = %g\n",totDif);


}
//______________________________________________________________________________
static double myDif(TVectorD &a,TVectorD &b, int &idx)
{

  int n = a.GetNrows();
  double dif=0; idx = -1;
  for (int i=0;i<n;i++) {
    double mydif = fabs(a[i])+fabs(b[i]);
    if (mydif<1e-4) 	continue;
    mydif = fabs(a[i]-b[i])/mydif;
    if (mydif <= dif) 	continue;
    dif = mydif; idx = i;
  }
  return dif;
}
//______________________________________________________________________________
void TRungeKutta::TestDer2()
{
// Test for derivatives + changing direction

  double Ptot = 5, Pt=3;
//double curv = 1./100,Hz = Pt*curv;
  double curv = 1./30,Hz = Pt*curv;
  TVector3 XbegV = TVector3(10,20,30);
  TVector3 PbegV = TVector3(Pt,0,Pt/3*4);
  double HZ[3] = {0,0,Hz};


  double L = 100.;

  double Mag[3];
  TVector3 magV(HZ); magV.GetXYZ(Mag );
#if 1
  double r1=gRandom->Rndm(),r2=gRandom->Rndm();
  magV.RotateX(r1);  magV.RotateY(r2);  magV.GetXYZ(Mag );
  PbegV.RotateX(r1); PbegV.RotateY(r2);
  XbegV.RotateX(r1); XbegV.RotateY(r2);
#endif 
//  Test2Mag myMag((double*)&magV[0]);
  TestMag myMag((double*)&magV[0]);

// XbegV.Print("XbegV");
// PbegV.Print("PbegV");
//  magV.Print("magV");

  printf("===============PbegV:= \n"); PbegV.Print("");

  TVector3 XendV,PendV;
  const THDer3d_t *myDer = 0;
  for (int ichar=-1; ichar<=1;  ichar +=2) {
    TRungeKutta TR0(ichar,(double*)&XbegV[0],(double*)&PbegV[0],&myMag);
    TR0.SetDerOn();
    TR0.Move(L);
    TR0.Backward();			//At the end TR0 helix inverted
    myDer = TR0.Der();			//Inverted derivatives
    TR0.Eval(0,&XendV[0],&PendV[0]);	// Got X & P at the end
    auto DendV = PendV.Unit();

    TVector3 UbegV(myDer->TkDir(0)[kKU]);	//Phi vector
    TVector3 VbegV(myDer->TkDir(0)[kKV]); 	//Lam vector
    TVector3 TbegV(myDer->TkDir(0)[kKT]); 	//Along track vector

    TVector3 UendV(myDer->TkDir(1)[kKU]);	//Phi vector
    TVector3 VendV(myDer->TkDir(1)[kKV]); 	//Lam vector
    TVector3 TendV(myDer->TkDir(1)[kKT]); 	//Along track vector
    assert((DendV-TendV).Mag()<1e-3);


    TVectorD der(5);
  static const char* dp[5] = {"U","V","Fita","Lama","Pinv"};
    for (int J=0;J<5;J++) {

      printf("\nTest dFit/d%s\n\n",dp[J]);
      for (int i=0;i<5;i++) {der[i] = (*myDer)[i][J];}


      TRungeKutta TR1(ichar,(double*)&XbegV[0],(double*)&PbegV[0],&myMag);
      TR1.Backward();
      TVector3 P1begV(TR1.Mom()); 
      TVector3 X1begV(TR1.Pos());
      TkDir_t myTkDir;
      THelix3d::MakeTkDir((double*)&P1begV[0],Mag,myTkDir);
      double eps = (TVectorD(9,myTkDir[0])-TVectorD(9,myDer->TkDir(0)[0])).NormInf();
      assert(eps<1e-3);


//??      double delta = 1e-4;
      double delta = 1e-6;
      TVector3 X1endV,P1endV;
      assert(TR0.Pinv()*ichar>0);
      assert(TR1.Pinv()*ichar>0);
      switch(J) {
        case kU:{
	  X1begV += UbegV*delta;
          break;
        } 
        case kV: {
	  X1begV += VbegV*delta;
          break;
        } 
        case kPinv: {
          double Pinv1 = TR1.Pinv()+delta;         
          double Ptot1 = fabs(1./Pinv1);
	  P1begV.SetMag(Ptot1);
          break;
	}
	case kLama: {	//lamda variation (alfa)
	  P1begV+=VbegV*(Ptot*delta); P1begV.SetMag(Ptot);
          break;
        }

	case kFita: {	//Phi variation (beta)
	  P1begV+=UbegV*(Ptot*delta); P1begV.SetMag(Ptot);
          break;
        }
        default: assert(0 && "Wrong J");
      }
      TR1.Set(0,(double*)&X1begV[0],(double*)&P1begV[0]);
      assert(TR1.Pinv()*ichar>0);
      TR1.Eval(-L,&X1endV[0],&P1endV[0]);
      TVectorD dif(5);
      TVector3 difX =  X1endV-XendV;
      TVector3 difD =  (P1endV.Unit()-PendV.Unit());
      dif[kU]    = difX.Dot(UendV);
      dif[kV]    = difX.Dot(VendV);
      dif[kPinv] = TR1.Pinv()-TR0.Pinv();
      dif[kFita] = difD.Dot(UendV);
      dif[kLama] = difD.Dot(VendV);


      assert(fabs(dif.NormInf())<1.);
      dif*=1./delta;
      
      int idx=-1;
      double maxa = myDif(dif,der,idx);

      if (maxa>1e-1) printf("########################[%d]= %g\n",idx,maxa);
      der.Print("ANALITIC");
      dif.Print("NUMERIC ");
    }
  } //end charge loop
}
//______________________________________________________________________________
void TRungeKutta::TestErr(int charge)
{
  double PtGev = 3.,Curv = 1./100, Hz = PtGev*Curv;
  double HZ[3] = {0,0,Hz};
  double Pbeg[3],P1beg[3],Xbeg[3]={0},X1beg[3];
  double Pend[3],P1end[3],Xend[3]    ,X1end[3];
  double L = 300.; int nL = 1;

//              Canvas + Histograms
enum {kNCanvs=10};
static TCanvas *myCanvas[kNCanvs] = {0};
static     int  myDiv[] = {2,5,5,5,5,0};
  for (int ic = 0;myDiv[ic];ic++) {
    if (myDiv[ic]<0) continue;
    TString ts("C"); ts+=ic;
    if (!myCanvas[ic]) myCanvas[ic] = new TCanvas(ts.Data(),ts.Data(),600,800);
    myCanvas[ic]->Clear();myCanvas[ic]->Divide(1,myDiv[ic]);
  }
static TH1F *hXi2[2] = {0};
  for (int jk=0;jk<2;jk++) {
    TString ts("Xi2_"); ts += jk;
    delete hXi2[jk];
    hXi2[jk]= new TH1F(ts.Data(),ts.Data(),50,0,50);
    myCanvas[0]->cd(jk+1);  hXi2[jk]->Draw();
  }
//              Now test of DCA
static TH1F * hh[5]={0};
  for (int ih=0;ih<myDiv[1];ih++) {
static const char *tit[5]={"U","V","Fita","Lama","Pinv"};
    delete hh[ih];
    hh[ih] = new TH1F(tit[ih],tit[ih],100,0,0);
    myCanvas[1]->cd(ih+1); hh[ih]->Draw();
  }
//              Now test of TRandomVector
static TH1F * hrt[5]={0};
  for (int ih=0;ih<myDiv[1];ih++) {
static const char *tit[5]={"rU","rV","rFita","rLama","rPinv"};
    delete hrt[ih];
    hrt[ih] = new TH1F(tit[ih],tit[ih],100,-10,+10);
    myCanvas[2]->cd(ih+1); hrt[ih]->Draw();
  }
//		Now test of Corr
static TH1F * hcr[10]={0};
{
static const char *tit[]={"UV","UF","VF","UL","VL","FL","UP","VP","FP","LP"};
  for (int ih=0;ih<10;ih++) {
    delete hcr[ih];
    hcr[ih] = new TH1F(tit[ih],tit[ih],100,0,0);
    if (ih<5) myCanvas[3]->cd(ih+1  ); 
    else      myCanvas[4]->cd(ih+1-5);
    hcr[ih]->Draw();
  }}
//========================================================================

  TVector3 PbegV(PtGev,0,PtGev/3*4);
  double Ptot = PbegV.Mag();
  PbegV.RotateZ(gRandom->Rndm()*3.1415*2); PbegV.GetXYZ(Pbeg);
  TVector3 XbegV(Xbeg);

  double Mag[3]={HZ[0],HZ[1],HZ[2]};
  TVector3 magV(HZ);
  double r1 = gRandom->Rndm(),r2 = gRandom->Rndm();
   magV.RotateX(r1);  magV.RotateY(r2);  magV.GetXYZ(Mag);
  PbegV.RotateX(r1); PbegV.RotateY(r2); PbegV.GetXYZ(Pbeg);
  //  Test2Mag myMag(Mag);
  TestMag myMag(Mag);
  TVectorD dia(5);
  dia[kU]= 0.1; dia[kV]= 0.2; dia[kPinv]= 0.1/Ptot, dia[kFita]= 1./360; dia[kLama]= 2./360;
  for(int jk=0;jk<5;jk++) {dia[jk]*=dia[jk];}
  TRandomVector RV(dia);
  auto &EMX = RV.GetMtx();
  auto &val = RV.GetLam();
  dia.Print("DIA");
  val.Print("VAL");


  double Gbeg[15],GbegD[5];
  for (int i=0,li=0;i< 5;li+=++i) {
    GbegD[i] = sqrt(EMX[i][i]);
    for (int j=0;j<=i;j++) {
      Gbeg[li+j] = EMX[i][j];
  } }


  double GbegI[15];
  TCL::trsinv(Gbeg,GbegI,5);

  TRungeKutta TH0(charge,Xbeg,Pbeg,&myMag);
  TH0.SetEmx(Gbeg);

  for (int split=0;split<nL;split++) {
    TH0.Move(L/nL);
  }
  double tkDir[3][3],tkDirE[3][3];
  memcpy(tkDir[0] ,TH0.TkDir(0)[0],sizeof(tkDir ));
  memcpy(tkDirE[0],TH0.TkDir(1)[0],sizeof(tkDirE));

  TH0.Eval(0,Xend,Pend);
  auto *myEmx = TH0.Emx();
  const double *Gend = *myEmx;
  double GendD[5];
  for (int i=0,li=0;i< 5;li+=++i) {
    GendD[i] = sqrt(Gend[li+i]);
  }
  double GendI[15];
  TCL::trsinv(Gend,GendI,5);


//   auto &tkDir   = TH0.TkDir(0);
//   auto &tkDirE  = TH0.TkDir(1);
  TVector3 PendV(Pend),XendV(Xend);
  TVectorD UendV(5);
  for (int j=0;j<3;j++) { UendV[j+2]=Pend[j];}

  double myG[15]={0};
  int nIter = 1000000;
  for (int iter=0;iter<nIter;iter++) {
    TVectorD delta  = RV.Gaus();
    double chi2;
    TCL::trasat(delta.GetMatrixArray(),GbegI,&chi2,1,5);
    assert(chi2>0);
    hXi2[0]->Fill(chi2);

    for (int ih=0;ih<myDiv[1];ih++) { hrt[ih]->Fill(delta[ih]/GbegD[ih]);}
    double Pinv = -charge/PbegV.Mag(),P1inv = Pinv;
    TVector3 P1begV = PbegV,D1begV = P1begV.Unit();
    TVector3 X1begV = XbegV;
    for (int i=0;i<5;i++) {
      if (i<2)          { X1begV += TVector3(tkDir[kKU+i  ])*delta[i];}
      else if (i<4)     { D1begV += TVector3(tkDir[kKU+i-2])*delta[i];}
      else              { P1inv  += delta[kPinv];                        ;}
    }
    X1begV.GetXYZ(X1beg);
    P1begV = D1begV*(1./fabs(P1inv));
    P1begV.GetXYZ(P1beg);
    TRungeKutta TH1(charge,X1beg,P1beg,&myMag);

///???    TH1.Backward();

    double s = TH1.Path(Xend);
    TH1.Move(s);
    TH1.Eval(0,X1end,P1end);
    TVector3 X1endV(X1end),P1endV(P1end);
    TVectorD U1endV(5);
    TVector3 difX =  X1endV-XendV;
    TVector3 difD =  P1endV.Unit()-PendV.Unit();
      for (int j=0;j<5;j++) {
        if (j<2)        	{ U1endV[j] = difX.Dot(TVector3(tkDirE[kKU+j  ]));}
        else if (j<kPinv)   	{ U1endV[j] = difD.Dot(TVector3(tkDirE[kKU+j-2]));}
        else            	{ U1endV[4] = -charge/P1endV.Mag()-Pinv      ;}
      }

    for (int ih=0;ih<5;ih++) { hh[ih]->Fill(U1endV[ih]/GendD[ih]);}
    TCL::trasat(U1endV.GetMatrixArray(),GendI,&chi2,1,5);
    hXi2[1]->Fill(chi2);

  for (int i=0,li=0;i<5;li+=++i) {
    for (int j=0;j<=i;j++) {
      myG[li+j]+=U1endV[i]*U1endV[j];
  } }

  int jk=0;
  for (int i=0,li=0;i<5;li+=++i) {
    for (int j=0;j<i;j++) {
      double d = U1endV[i]*U1endV[j] - Gend[li+j];
      d/= GendD[i]*GendD[j];
      hcr[jk++]->Fill(d);
  } }


  }
  
 

  TCL::vscale(myG,1./nIter,myG,15);

  printf("Numerical vs Analitical Error matrices:\n");
  for (int i=0,li=0;i<5;li+=++i) {
    for (int j=0;j<=i;j++) {
      printf("\t%g ",myG[li+j]);
    }
    printf("\n");
    for (int j=0;j<=i;j++) {
      printf("\t%g ",Gend[li+j]);
    }
    printf("\n");
  }

  for (int i=0,li=0;i<5;li+=++i) {
    for (int j=0;j<=i;j++) {
      printf("\t%g ",Gend[li+j]/(GendD[i]*GendD[j]));
    }
    printf("\n");
    for (int j=0;j<=i;j++) {
      printf("\t%g ",myG[li+j]/ (GendD[i]*GendD[j]));
    }
    printf("\n");
  }



  for (int i=0;myCanvas[i];i++) {
    if (!myCanvas[i]) continue;
    myCanvas[i]->Modified();myCanvas[i]->Update();}

  while(!gSystem->ProcessEvents()){gSystem->Sleep(200);};
}

//______________________________________________________________________________
void TRungeKutta::TestErr2(int charge)
{
  double PtGev = 1.,Curv = 1./100, Hz = PtGev*Curv;
  double HZ[3] = {0,0,Hz};
  double Pbeg[3],P1beg[3],Xbeg[3]={0},X1beg[3];
  double Pend[3],P1end[3],Xend[3]    ,X1end[3];

//		Canvas + Histograms
enum {kNCanvs=10};
static TCanvas *myCanvas[kNCanvs] = {0};
static     int  myDiv[] = {2,5,5,5,5,0};
  for (int ic = 0;myDiv[ic];ic++) {
    if (myDiv[ic]<0) continue;
    TString ts("C"); ts+=ic;
    if (!myCanvas[ic]) myCanvas[ic] = new TCanvas(ts.Data(),ts.Data(),600,800);
    myCanvas[ic]->Clear();myCanvas[ic]->Divide(1,myDiv[ic]);
  }
static TH1F *hXi2[2] = {0};
  for (int jk=0;jk<2;jk++) {
    TString ts("Xi2_"); ts += jk;
    delete hXi2[jk];
    hXi2[jk]= new TH1F(ts.Data(),ts.Data(),50,0,0);
    myCanvas[0]->cd(jk+1);  hXi2[jk]->Draw(); 
  }
//		Now test of DCA
static TH1F * hh[5]={0};
  for (int ih=0;ih<myDiv[1];ih++) {
const char * tit[]={"U","V","Pinv","Fita","Lama",0};
    delete hh[ih];
    hh[ih] = new TH1F(tit[ih],tit[ih],100,0,0);
    myCanvas[1]->cd(ih+1); hh[ih]->Draw();
  }
//		Now test of TRandomVector
static TH1F * hrt[5]={0};
  for (int ih=0;ih<myDiv[1];ih++) {
const char * tit[]={"U0","V0","Pinv0","Fita0","Lama0",0};
    delete hrt[ih];
    hrt[ih] = new TH1F(tit[ih],tit[ih],100,0,0);
    myCanvas[2]->cd(ih+1); hrt[ih]->Draw();
  }
//		Now test of Corr
static TH1F * hcr[10]={0};
{
static const char *tit[]={"UV","UP","VP","UL","VL","PL","UF","VF","PF","LF"};
  for (int ih=0;ih<10;ih++) {
    delete hcr[ih];
    hcr[ih] = new TH1F(tit[ih],tit[ih],100,0,0);
    if (ih<5) myCanvas[3]->cd(ih+1  ); 
    else      myCanvas[4]->cd(ih+1-5);
    hcr[ih]->Draw();
  }}
//============================================================================  
    
  TVector3 PbegV(PtGev,0,PtGev*3);
  double Ptot = PbegV.Mag();
  PbegV.RotateZ(gRandom->Rndm()*3.1415*2); PbegV.GetXYZ(Pbeg);
  TVector3 XbegV(Xbeg);

  double L = 100.;

  double Mag[3]={HZ[0],HZ[1],HZ[2]};
#if 11
  TVector3 magV(HZ); 
  double r1 = gRandom->Rndm(),r2 = gRandom->Rndm(); 
   magV.RotateX(r1);  magV.RotateY(r2);  magV.GetXYZ(Mag);
  PbegV.RotateX(r1); PbegV.RotateY(r2); PbegV.GetXYZ(Pbeg);
#endif
  Test2Mag myMag(Mag);



  TVectorD dia(5);
  dia[kU]= 0.1; dia[kV]= 0.2; dia[kPinv]= 0.1/Ptot; dia[kFita]= 1./360; dia[kLama]= 2./360;
  
  TRandomVector RV(dia);
  auto &EMX = RV.GetMtx();
  auto &val = RV.GetLam();
  dia.Print("DIA");
  val.Print("VAL");


  double Gbeg[15],GbegD[5];
  for (int i=0,li=0;i< 5;li+=++i) {
    GbegD[i] = sqrt(EMX[i][i]);
    for (int j=0;j<=i;j++) {
       Gbeg[li+j] = EMX[i][j];
  } }



  
  TRungeKutta TH0(charge,Xbeg,Pbeg,&myMag);
//==================================
  THEmx3d_t *emx  = new THEmx3d_t(Gbeg);

  TRungeKutta TH0i(&TH0);
  TH0i.SetEmx(emx);
  TH0i.Backward();
  double GbegI[15];
  memcpy(Gbeg,*TH0i.Emx(),sizeof(Gbeg));
  TRandomVector RVi(5,Gbeg);
  TCL::trsinv(Gbeg,GbegI,5);  

  TH0.SetEmx(emx);
  TH0.Move(L);
  TH0.Backward();

  double tkDir[3][3],tkDirE[3][3];
  memcpy(tkDir[0] ,TH0.TkDir(0)[0],sizeof(tkDir ));
  memcpy(tkDirE[0],TH0.TkDir(1)[0],sizeof(tkDirE));

  TH0.Eval(0,Xend,Pend);
  auto *myEmx = TH0.Emx();
  const double *Gend = *myEmx;
  double GendD[5];
  for (int i=0,li=0;i< 5;li+=++i) {
    GendD[i] = sqrt(Gend[li+i]);
  } 

  double GendI[15];
  TCL::trsinv(Gend,GendI,5);  

  TVector3 PendV(Pend),XendV(Xend);
  TVectorD UendV(5);
  for (int j=0;j<3;j++) { UendV[j+2]=Pend[j];}
  
  double myG[15]={0};
  int nIter = 1000000;  
  for (int iter=0;iter<nIter;iter++) {
    TVectorD delta  = RVi.Gaus();
    double chi2;
    TCL::trasat(delta.GetMatrixArray(),GbegI,&chi2,1,5);
    assert(chi2>0);
    hXi2[0]->Fill(chi2);

    for (int ih=0;ih<myDiv[1];ih++) { hrt[ih]->Fill(delta[ih]/GbegD[ih]);}
    TVector3 P1begV(TH0i.Mom());
    TVector3 X1begV(TH0i.Pos());

    X1begV     += TVector3(tkDir[kKU])*delta[kU];
    X1begV     += TVector3(tkDir[kKV])*delta[kV];
    double Pinv = TH0i.Pinv();
    double Pinv1 = Pinv + delta[kPinv];
    double Ptot1 = fabs(1./Pinv1);
    P1begV.SetMag(Ptot1);
    P1begV += TVector3(tkDir[kKV])*(delta[kLama]*Ptot1);P1begV.SetMag(Ptot1);
    P1begV += TVector3(tkDir[kKU])*(delta[kFita]*Ptot1);P1begV.SetMag(Ptot1);

    X1begV.GetXYZ(X1beg);
    P1begV.GetXYZ(P1beg);
    TRungeKutta TH1(TH0i.Charge(),X1beg,P1beg,&myMag);
    TH1.Move(-L);
    TH1.Eval(0,X1end,P1end);
    TVector3 X1endV(X1end),P1endV(P1end);
    TVectorD U1endV(5);
    TVector3 difX =  X1endV-XendV;
    TVector3 difD =  (P1endV.Unit()-PendV.Unit());

    U1endV[kU]    = difX.Dot(TVector3(tkDirE[kKU]));
    U1endV[kV]    = difX.Dot(TVector3(tkDirE[kKV]));
    U1endV[kFita] = difD.Dot(TVector3(tkDirE[kKU]));
    U1endV[kLama] = difD.Dot(TVector3(tkDirE[kKV]));
    U1endV[kPinv] = (Pinv1-Pinv);

    for (int ih=0;ih<5;ih++) { hh[ih]->Fill(U1endV[ih]/GendD[ih]);}
    TCL::trasat(U1endV.GetMatrixArray(),GendI,&chi2,1,5);
    hXi2[1]->Fill(chi2);

  for (int i=0,li=0;i<5;li+=++i) {
    for (int j=0;j<=i;j++) {
      myG[li+j]+=U1endV[i]*U1endV[j];
  } }

  int jk=0;
  for (int i=0,li=0;i<5;li+=++i) {
    for (int j=0;j<i;j++) {
      double d = U1endV[i]*U1endV[j] - Gend[li+j];
      d/= GendD[i]*GendD[j];
      hcr[jk++]->Fill(d);
  } }
  }

  TCL::vscale(myG,1./nIter,myG,15);

  printf("Numerical vs Analitical Error matrices:\n");
  for (int i=0,li=0;i<5;li+=++i) {
    for (int j=0;j<=i;j++) {
      printf("\t%g ",myG[li+j]);
    }
    printf("\n");
    for (int j=0;j<=i;j++) {
      printf("\t%g ",Gend[li+j]);
    }
    printf("\n");
  }


  for (int i=0;myCanvas[i];i++) {
    if (!myCanvas[i]) continue;
    myCanvas[i]->Modified();myCanvas[i]->Update();}

  while(!gSystem->ProcessEvents()){gSystem->Sleep(200);}; 
}
//______________________________________________________________________________
//______________________________________________________________________________
void TRungeKutta::TestSign()
{

  double PtGev = 3.,Rho = 1./100, Hz = PtGev*Rho ;
  double ZER[3]= {0,0,0};
  double HZ[3] = {0,0,Hz};
  double PZ[3] = {PtGev,0,PtGev*4./3};

  for (int charge = -1   ;charge<=1;charge +=2  ) {
  for (double L   =-100  ;L<=100   ;L      +=200) {
  for (int ih     =-1    ;ih<=1    ;ih     +=2  ) {
    HZ[2] = fabs(HZ[2])*ih;
    TestMag myMag(HZ);
    double curv = -(HZ[2]*charge)/PtGev;

    THelixTrack th(ZER,PZ,curv);
    THelix3d    t3(charge,ZER,PZ,HZ);
    TRungeKutta tr(charge,ZER,PZ,&myMag);

    th.Move(L);
    t3.Move(L);
    tr.Move(L);

    printf("charge = %d \tL = %g , THelixTreack \tX=%g \tY=%g \tZ=%g \tDx=%g \tDy=%g \tDz=%g\n"
          , charge,L,th.Pos()[0],th.Pos()[1],th.Pos()[2]
          ,          th.Dir()[0],th.Dir()[1],th.Dir()[2]);
    printf("charge = %d \tL = %g , THelix3d     \tX=%g \tY=%g \tZ=%g \tDx=%g \tDy=%g \tDz=%g\n"
          , charge,L,t3.Pos()[0],t3.Pos()[1],t3.Pos()[2]
          ,          t3.Dir()[0],t3.Dir()[1],t3.Dir()[2]);
    printf("charge = %d \tL = %g , TRungeKutta  \tX=%g \tY=%g \tZ=%g \tDx=%g \tDy=%g \tDz=%g\n"
          , charge,L,tr.Pos()[0],tr.Pos()[1],tr.Pos()[2]
          ,          tr.Dir()[0],tr.Dir()[1],tr.Dir()[2]);
    printf("\n");
  } } }
}
#endif //0
