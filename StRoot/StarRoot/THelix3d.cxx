#if 0
#include <stdlib.h>
#include <math.h>
#include "TError.h"
#include "TArrayD.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "TSystem.h"
#include "TMath.h"
#include "TCernLib.h"
#include "TVector3.h"
#include "TVectorD.h"
#include "TRandom.h"
#include "TRandom2.h"
#include "THelixTrack.h"
#include "THelix3d.h"
#include "TRandomVector.h"
#include "StMatrixD.hh"
#include "TH1.h"
#include <cassert>
static double EmxSign(int n,const double *a); 

#if 0
//_____________________________________________________________________________
inline static void spheric(const double D[3]
                          ,double &cosL,double &sinL
			  ,double &cosP,double &sinP)
{
  sinL = D[2];
  cosL = (1.-sinL)*(1+sinL);
  cosL = (cosL<=0)? 0.:sqrt(cosL);
  if (cosL <1e-6) {//track along Z-axis
    cosP = 1; sinP = 0; cosL = 1e-6;
  } else {
    cosP = D[0]/cosL; sinP = D[1]/cosL;
  }
}
//_____________________________________________________________________________
inline static void satlit(const double &cosL,const double &sinL
                         ,const double &cosP,const double &sinP
                         ,double U[3][3])
{
  U[0][0]= cosL*cosP; U[0][1] = cosL*sinP;U[0][2]= sinL;
  U[1][0]=-     sinP; U[1][1] =       cosP; U[1][2]=0;   
  U[2][0]=-sinL*cosP; U[2][1] = -sinL*sinP; U[2][2]=cosL;   
}
#endif
//_____________________________________________________________________________
//#define dot(a,b) (a[0]*b[0]+a[1]*b[1]+a[2]*b[2])

inline static double dot(const double *a,const double *b)
{return a[0]*b[0]+a[1]*b[1]+a[2]*b[2];}


//_____________________________________________________________________________
inline static double nor(double *a)
{
  double N = dot(a,a); if (fabs(N-1)<1e-7) return N;
  if (N<=0) return N;
  N = sqrt(N);
  a[0]/=N; a[1]/=N; a[2]/=N;
  return N;
}
//_____________________________________________________________________________
inline static void lin(const double a[3],double f, const double b[3], double c[3])
{
  c[0] = a[0]+f*b[0];c[1] = a[1]+f*b[1];c[2] = a[2]+f*b[2];
}
//_____________________________________________________________________________
inline static void cop(const double a[3],double c[3])
{ c[0] = a[0]; c[1] = a[1];c[2] = a[2]; }
//_____________________________________________________________________________
inline static void cro(const double a[3],const double b[3], double c[3])
{
  c[0] = a[1]*b[2]-b[1]*a[2];
  c[1] = a[2]*b[0]-b[2]*a[0];
  c[2] = a[0]*b[1]-b[0]*a[1];
}
//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
ClassImp(THelix3d)
//_____________________________________________________________________________
// in THDer3d_t direction of track represented as 
/* 
   mTkDir[*][2]*cos(alfa)*cos(beta) +
   mTkDir[*][1]*cos(alfa)*sin(beta) +
   mTkDir[*][0]*sin(alfa)
   
Or when alfa & beta are small   
   mTkDir[*][2] +
   mTkDir[*][1]*beta +
   mTkDir[*][0]*alfa
*/   
//_____________________________________________________________________________
void TkDir_t::Backward()
{
// U   =  0.0233245  0.828218 -0.55992
// Uinv= -0.0233245 -0.828218  0.55992
// V   = -0.30932    0.538569  0.78375
// Vinv= -0.30932    0.538569  0.78375
// kKU is immediately after kKT
  TCL::vscale(mTkd[kKT],-1.,mTkd[kKT],3*2);
}
//_____________________________________________________________________________
int TkDir_t::Same(const TkDir_t &as) const
{
static const double eps = 0.01;
   for (int i=0;i<3;i++) {
     for (int j=0;j<3; j++) {
       if (fabs(mTkd[i][j]-as[i][j])>eps) return 0;
   } }
   return 1;
}
//_____________________________________________________________________________
void THDer3d_t::Clear()
{
  mLen=0;
  memset(mDer[0]     ,0,sizeof(mDer  ));
  for (int i=0;i< 5;i++) { mDer[i][i] = 1;}
  
  for (int jk=0; jk<2;jk++) { mTkDir[jk].Clear();}
}
//_____________________________________________________________________________
THDer3d_t &THDer3d_t::operator*=(const THDer3d_t &by)
{
  Mtx55D_t res;
  TCL::mxmpy(by.mDer[0],mDer[0],res[0],5,5,5);
  TCL::ucopy(res[0],mDer[0],5*5);
  TCL::ucopy(by.mTkDir[1][0],mTkDir[1][0],3*3);
  mLen+=by.mLen;
  return *this;
}
//_____________________________________________________________________________
void THDer3d_t::Backward()
{
// kU = U*dX = -1
// kV = V*dX = +1
// kT -1
// kFita = kU*dT =  1
// kLama = kV*dT = -1
// kPinv = -1
//        kU  kV  kFita  kLama kPinv
// kU      1  -1     -1      1     1
// kV     -1   1      1     -1    -1
// kFita  -1   1      1     -1    -1
// kLama   1  -1     -1      1     1
// kPinv   1  -1     -1      1     1

   mTkDir[0].Backward();
   mTkDir[1].Backward();
// U,  V, Fita, Lama, Pinv... =  0.001 0.002 0.003  0.004  0.001
// Um,Vm,FitaM, LamaM.PinvM.. = -0.001 0.002 0.003 -0.004 -0.001
// kU-,kV+,kFita+,kLama-,kPinv-

static const int idx[]= {kU,kV   , kU,kFita   
                        ,kV,kU   , kV,kLama   , kV,kPinv
			,kFita,kU, kFita,kLama, kFita,kPinv
			,kLama,kV, kLama,kFita
			,kPinv,kV, kPinv,kFita, -1,-1};

  for (int i=0;idx[i]>=0; i+=2) {mDer[idx[i]][idx[i+1]]*=-1.;}
}
//_____________________________________________________________________________
THelix3d::THelix3d(int charge,const double *xyz,const double *mom,const double *Mag)
{
//	Generalization of THelixTrack for arbitrary direction of mag field
//      V.Perevoztchikov
//
  memset(fBeg3d,0,fEnd3d-fBeg3d);
  Set(charge,xyz,mom,Mag);
}
//_____________________________________________________________________________
THelix3d::THelix3d()
{
  memset(fBeg3d,0,fEnd3d-fBeg3d);
}
//_____________________________________________________________________________
THelix3d::~THelix3d()
{
  delete fEmx3d;
  delete fDer3d;
}
//_____________________________________________________________________________
void THelix3d::Clear(const char*)
{
 memset(fBeg3d,0,fMed3d-fBeg3d);

 if (fDer3d) fDer3d->Clear();
 if (fEmx3d) fEmx3d->Clear();
}
//_____________________________________________________________________________
void THelix3d::Set(int charge,const double *xyz,const double *mom,const double *mag)
{
  if (charge) fCharge = charge;
  if (xyz) memcpy(fX3d,xyz,sizeof(fX3d));
  if (mom) memcpy(fP3d,mom,sizeof(fP3d));
  if (mag) memcpy(fH3d,mag,sizeof(fH3d[0])*3);
  Build();
}
//_____________________________________________________________________________
void THelix3d::Build()
{    
  fMom = sqrt(TCL::vdot(fP3d,fP3d,3));
  TCL::vscale(fP3d,1./fMom,fD3d,3);
  fH3d[3] = sqrt(TCL::vdot(fH3d,fH3d,3));
  MakeLocal(fH3d,fD3d,fLoc);
  ToLocal();
  fPinv = -fCharge/fMom;
  double pt = sqrt(TCL::vdot(fP,fP,2))*fMom;
  fRho = -fCharge*fH3d[3]/pt;
  THelixTrack::Build();
  if (!fEmx3d) return;
  TkDir_t tkDir;
  DoTkDir(tkDir);
  fEmx3d->Update(tkDir);
  fDer3d->Clear();
  fDer3d->SetTkDir(0,tkDir);
  fDer3d->SetTkDir(1,tkDir);
}
//_____________________________________________________________________________
THelix3d::THelix3d(const THelix3d  *from): THelixTrack(from)
{
  memcpy(fBeg3d,from->fBeg3d,fEnd3d-fBeg3d);
  fEmx3d=0; fEmx=0;
}
//_____________________________________________________________________________
THelix3d::THelix3d(const THelix3d  &from): THelixTrack(&from)
{
  memcpy(fBeg3d,from.fBeg3d,fEnd3d-fBeg3d);
  if (fEmx3d) {fEmx3d = new THEmx3d_t; *fEmx3d = *from.fEmx3d;}
  if (fDer3d) {fDer3d = new THDer3d_t; *fDer3d = *from.fDer3d;}
}
//_____________________________________________________________________________
void THelix3d::ToGlobal(const double locX[3],const double locD[3]
                       ,      double gloX[3],      double gloD[3]
		       ,      double gloP[3])const
// Convert local variables(THelixTrack) into global (THelix3d)

{
  double myX[3];
  TCL::vmatr (locX,fLoc[0],myX ,3,3);
  TCL::vadd  (myX ,fX3d   ,gloX,3  );
  TCL::vmatr (locD,fLoc[0],gloD,3,3);
  TCL::vscale(gloD,fMom   ,gloP,3  );
}
//_____________________________________________________________________________
void THelix3d::GetdDdL(double dDdL[3]) const
{
  double dDdLloc[3] = {-fP[1]*fRho*fCosL,fP[0]*fRho*fCosL,0};
  TCL::vmatr (dDdLloc,fLoc[0],dDdL,3,3);

}//_____________________________________________________________________________
void THelix3d::ToGlobal()
{
  ToGlobal(fX,fP,fX3d,fD3d,fP3d);
}
//_____________________________________________________________________________
void THelix3d::ToLocal()
{
  TCL::vzero(fX,3);
  TCL::vmatl(fLoc[0],fD3d,fP,3,3);
  memcpy(fPpre,fP,sizeof(fPpre));
  fCosL = sqrt(fabs((1.-fP[2])*(1+fP[2])));
}
//_____________________________________________________________________________
void THelix3d::Backward()
{
  TCL::vscale(fP3d   ,-1.,fP3d   ,3);
  TCL::vscale(fD3d   ,-1.,fD3d   ,3);
  fCharge = - fCharge;
  fPinv   = - fPinv;
  THelixTrack::Backward();
  if (fEmx3d) fEmx3d->Backward();
  if (fDer3d) fDer3d->Backward();
  ToLocal();
}
//_____________________________________________________________________________
double THelix3d::Move(double step,double F[5][5])
{
  if (fabs(step) <1e-11) return step;
  memcpy(fX3dPre,fX3d,sizeof(fX3dPre));
  memcpy(fP3dPre,fP3d,sizeof(fP3dPre));
  memcpy(fD3dPre,fD3d,sizeof(fD3dPre));
  if (!IsDerOn() && !F) { //No derivatives needed
    ToLocal();
    fLen = THelixTrack::Move(step);
    ToGlobal();
  } else {
    ToLocal();
    fLen = THelixTrack::Move(step);
    ToGlobal();
    MakeMtx();
    if (F) memcpy(F[0],fDer3d[0],sizeof(double)*5*5);
    if (fEmx3d) fEmx3d->Move(fDer3d); 

  }

  return fLen;
}
//_____________________________________________________________________________
double THelix3d::Eval(double step, double xyz[3], double mom[3])
{
  fLen = step;
  if (fabs(step)<1e-11) {
    if (xyz) memcpy(xyz,fX3d,sizeof(fX3d));
    if (mom) memcpy(mom,fP3d,sizeof(fP3d));
  } else {
    ToLocal();
    double myXE[3],myDE[3],myX3dE[3],myD3dE[3],myP3dE[3];
    THelixTrack::Eval(step,myXE,myDE);
    ToGlobal(myXE,myDE,myX3dE,myD3dE,myP3dE);
    if (xyz) memcpy(xyz,myX3dE,sizeof(myX3dE));
    if (mom) memcpy(mom,myP3dE,sizeof(myP3dE));
  } 
  return step;
}
//_____________________________________________________________________________
double THelix3d::Path(const double point[3],double xyz[3], double mom[3])  
{
  double myPoint[6],myXyz[3],myMom[3];
  ToLocal();
  TCL::vsub(point,fX3d,myPoint+3,3);
  TCL::vmatl(fLoc[0],myPoint+3,myPoint,3,3);
  double s = THelixTrack::Path(myPoint,myXyz,myMom);
  if (xyz)  {
    TCL::vmatr(myXyz,fLoc[0],xyz,3,3);
    TCL::vadd(xyz,fX3d,xyz,3);
  }
  if (mom) {
    TCL::vmatr(mom,fLoc[0],myMom,3,3);
    TCL::vscale(mom,fMom,mom,3);
  }
  return s;
}
//______________________________________________________________________________
double THelix3d::Dca(const double point[3],double *dcaErr) 
{
if(point || dcaErr){};
assert(!"THelix3d::Dca Not implemented");
return 0;
}
//______________________________________________________________________________
double THelix3d::Path(double x,double y) 
{
static const double kEps=1e-5,kRef=1e-4,kBigEps=1e-1;
  double keepStep = 1e11;
  THelix3d TH(this);
  const double *X = TH.Pos();
  const double *D = TH.Dir();
  double maxStep = GetPeriod()/6.28;
  maxStep/= sqrt(1. - D[2]*D[2])+1e-11;
  fLen = 0; 
  double qaLeft,posLeft=0,qaRite=0,posRite=0,qaMine,qaStep,dx[2];
  for (int iter=0; iter<=20; iter++) {
    dx[0] = x-X[0];dx[1] = y-X[1]; 
    qaMine = dx[0]*D[0] + dx[1]*D[1];
    qaMine /=(1.-D[2])*(1.+D[2]);
    if (fabs(qaMine) < kEps || fabs(qaMine) < kRef*fabs(fLen)) goto END;;
    if (!iter)                  {qaLeft = qaMine; qaStep = qaMine;        }
    else if (qaMine*qaLeft>0)   {qaLeft = qaMine; qaStep*= 2     ;        }
    else                        {qaRite = qaMine; posLeft=-qaStep; break ;}
    if (fabs(qaStep)>maxStep) qaStep = (qaStep<0)? -maxStep:maxStep;
    TH.Move(qaStep) ;
    fLen += qaStep;
//    assert(fabs(fLen)<4*maxStep);
    if (fabs(fLen)>4*maxStep) return 1e11;
  }
  assert(qaRite);
  for (int iter=0; iter<=20; iter++) {
    double denom = qaRite-qaLeft;
    if (iter&1 || fabs(denom) < 1e-4) { //Half step
      qaStep = 0.5*(posRite+posLeft);
    } else {				//Linear approach
      qaStep = -(qaLeft*posRite-qaRite*posLeft)/denom;
    }
    TH.Move(qaStep) ;
    fLen += qaStep;
    dx[0] = x-X[0];dx[1] = y-X[1]; 
    qaMine = dx[0]*D[0]+dx[1]*D[1];
    qaMine /=(1.-D[2])*(1.+D[2]);
    if (fabs(qaMine) < kEps || fabs(qaMine) < kRef*fabs(fLen)) goto END;;
    if (qaLeft*qaMine>0) { qaLeft = qaMine; posLeft=0; posRite-=qaStep;}
    else                 { qaRite = qaMine; posRite=0; posLeft-=qaStep;}
    assert(fabs(posRite-posLeft)<=keepStep);
    keepStep= fabs(posRite-posLeft);
  }

END:  return (fabs(qaMine)<kBigEps)? fLen:1e11;
 }
//______________________________________________________________________________
double THelix3d::Dca(double x,double y, double *dcaErr)  
{
if (x || y){}
return 0;
assert(!"THelix3d::Dca(x,y,dcaError) n oy implemented");
}

//static  double mydPdP0[3][3];
//static  double mydXdP0[3][3];

//______________________________________________________________________________
void THelix3d::MakeMtx()
{
static const double Zero[3]= {0,0,1}; //Start of coordinate in local sys
  if (!fDer3d) fDer3d = new THDer3d_t;
  auto &der = *fDer3d;   
  der.Clear();
  TkDir_t &TkDir  = der.mTkDir[0];
  TkDir_t &TkDirE = der.mTkDir[1];

  assert(fLen!=0);
  double Dhlx[5][5],to3d[5][5],to3di[5][5],tmp[5*5];
  THelixTrack::MakeMtx(fLen,Dhlx);
  THelixTrack preHlx(Zero,fPpre,fRho);
  ConvertErrs(                  &preHlx, fH3d[3],0,   0,to3di);
  ConvertErrs((const THelixTrack *)this, fH3d[3],0,to3d,    0);

  TCL::mxmpy(to3d[0],Dhlx[0],tmp,5,5,5);
  TCL::mxmpy(tmp,to3di[0],der[0],5,5,5);

  MakeTkDir(fD3dPre,fH3d,TkDir );
  MakeTkDir(fD3d   ,fH3d,TkDirE);


  der.mLen = fLen;
}

//______________________________________________________________________________
THEmx3d_t *THelix3d::SetEmx(THEmx3d_t *emx)
{
  SetDerOn();
  if (!fEmx3d) 	{ fEmx3d = new THEmx3d_t;}
  else    	{ fEmx3d->Clear()	;}
  if (!fDer3d) 	{ fDer3d = new THDer3d_t;}
  else 		{ fDer3d->Clear()	;}
  if (emx) { *fEmx3d = *emx;}; 
  TkDir_t &tkDir = fEmx3d->TkDir();
  DoTkDir(tkDir); 
  fDer3d->Clear();
  fDer3d->SetTkDir(0,tkDir);
  fDer3d->SetTkDir(1,tkDir);
  return fEmx3d;
}
//______________________________________________________________________________
THEmx3d_t *THelix3d::SetEmx(const double G[15])
{
  SetDerOn();
  if (!fEmx3d) 	{ fEmx3d = new THEmx3d_t;}
  else    	{ fEmx3d->Clear()	;}
  fEmx3d->Set(G);
  if (!fDer3d) 	{ fDer3d = new THDer3d_t;}
  else 		{ fDer3d->Clear()	;}
  TkDir_t &tkDir = fEmx3d->TkDir();
  DoTkDir(tkDir); 
  fDer3d->Clear();
  fDer3d->SetTkDir(0,tkDir);
  fDer3d->SetTkDir(1,tkDir);
  return fEmx3d;
}
//______________________________________________________________________________
void THelix3d::MakeTkDir( const double T[3],const double H[3],TkDir_t &tkDir)
{
// H mag field direction
// T track direction
// Zaxis along T,Yaxis is normal to H and T, X axis (X*H) >=0 

 cop(T,tkDir[kKT]);		//Zaxis along T
 nor(tkDir[kKT]);
 cop(H,tkDir[kKV]);		
 double N = nor(tkDir[kKV]);
 if (N<1e-11) {
   tkDir[kKV][0]=0; tkDir[kKV][1]=0; tkDir[kKV][2]=1; N=1;
 }
 double HT = dot(tkDir[kKV],tkDir[kKT]);
 lin(tkDir[kKV],-HT,tkDir[kKT],tkDir[kKV]);		
 N = nor(tkDir[kKV]);				//Y axis = H - (H*T)*T,then norm
 assert(N>1e-11);
 cro(tkDir[kKV],tkDir[kKT],tkDir[kKU]);	//Yaxis normal to X,Z 
 nor(tkDir[kKU]);

#if 1
 for (int i=0;i< 3;i++) {
 for (int k=0;k<=i;k++) {
   double dot = TCL::vdot(tkDir[i],tkDir[k],3);
   if (i==k) dot--;
   assert(fabs(dot)<1e-3);
 }}

 for (int i=0;i<3;i++) {
   int j=(i+1)%3;int k=(j+1)%3;
   double qwe =(TVector3(tkDir[i]).Cross(TVector3(tkDir[j]))).Dot(TVector3(tkDir[k]));
   assert(fabs(qwe-1)<1e-3);
 }

#endif
}
//______________________________________________________________________________
void THelix3d::DoTkDir(TkDir_t &tkDir)
{
   const double *t =fD3d;
   MakeTkDir(t,fH3d,tkDir);
   GetdDdL(tkDir[kKdDdL]);
}
//______________________________________________________________________________
void THelix3d::MakeLocal( const double H[3],const double T[3],double tkDir[3][3])
{
// H mag field direction
// T track direction
// Zaxis along T,Yaxis is normal to H and T, X axis (X*H) >=0 

 cop(H,tkDir[2]);		//Zaxis along Z
 nor(  tkDir[2]);
 cop(T,tkDir[1]);		
 nor(  tkDir[1]);
 double HT = dot(tkDir[1],tkDir[2]);
 lin(tkDir[1],-HT,tkDir[2],tkDir[1]);		
 nor(tkDir[1]);				//Y axis = H - (H*T)*T,then norm
 cro(tkDir[1],tkDir[2],tkDir[0]);	//Xaxis normal to Y,Z 
 nor(tkDir[0]);
}
//_____________________________________________________________________________
void THEmx3d_t::Clear()
{
   mLen=0;mTimes[0]=0;mTimes[1]=0;
   memset(&mUU,0,sizeof(mUU)*15);
   for (int i=0,li=0;i< 5;li+=++i) 	{ (&mUU)[li+i] = 1;}
   mTkDir.Clear();
}
//______________________________________________________________________________
void  THEmx3d_t::Set(double const err[15],TkDir_t *tkDir)  
{
  TCL::ucopy(err,&mUU,15);
assert(tkDir || fabs(mTkDir[kKU][0])+fabs(mTkDir[kKU][1])+fabs(mTkDir[kKU][2])>0.1);
  if (!tkDir) return;
  TCL::ucopy((*tkDir)[0],mTkDir[0],3*3);
}  
//______________________________________________________________________________
void  THEmx3d_t::Set(double eUU,double eUV,double eVV)  
{
  mUU=eUU;mUV=eUV;mVV=eVV;
assert(fabs(mTkDir[kKU][0])+fabs(mTkDir[kKU][1])+fabs(mTkDir[kKU][2])>0.1);
}  
//______________________________________________________________________________
void THEmx3d_t::Add(double Theta2,double Orth2,double PinvRr)
{
  mTimes[1]++;
  mFF+= Theta2; 		
  mLL+= Theta2;
  mUU+= Orth2;
  mVV+= Orth2;
  mPP+= PinvRr;
}
//______________________________________________________________________________
void THEmx3d_t::Move(const THDer3d_t *der)
{
  assert(mUU>0);
  assert(mTkDir.Same(der->TkDir(0)));
  mTimes[0]++;
  mLen +=der->Len();
  double oErr[15];
  memcpy(oErr,*this,sizeof(oErr));
  TCL::trasat((*der)[0],oErr,*this,5,5); 
  TCL::ucopy(der->TkDir(1)[0],mTkDir[0],3*3);
}
//_____________________________________________________________________________
void THEmx3d_t::Backward()
{
//        kU  kV  kFita  kLama kPinv
// kU      1  -1     -1      1     1
// kV     -1   1      1     -1    -1
// kFita  -1   1      1     -1    -1
// kLama   1  -1     -1      1     1
// kPinv   1  -1     -1      1     1
// kU-,kV+,kFita+,kLama-,kPinv-
   mTkDir.Backward();

   mUV*=-1; mUF*=-1;
   mVL*=-1; mVP*=-1;
   mFL*=-1; mFP*=-1;
}
//_____________________________________________________________________________
void THEmx3d_t::Update(const TkDir_t &tkDir)
{
//============================================================      
//  U0*u0     +  V0*v0     +  T0*t0 = U1*u1 + V1*v1
// (U0*T1)*u0 + (V0*T1)*v0 + (T0*T1)*t0 = 0
// )/
// t0 = -((U0*T1)*u0 + (V0*T1)*v0)/(T0*T1)
// 
// u1 = (U0*U1)*u0 + (V0*U1)*v0 - (T0*U1)/(T0*T1)*((U0*T1)*u0 + (V0*T1)*v0)
// v1 = (U0*V1)*u0 + (V0*V1)*v0 - (T0*V1)/(T0*T1)*((U0*T1)*u0 + (V0*T1)*v0)
// 
// 
// du1/du0 = (U0*U1) - (T0*U1)/(T0*T1) *(U0*T1)
// du1/dv0 = (V0*U1) - (T0*U1)/(T0*T1) *(V0*T1)
// dv1/du0 = (U0*V1) - (T0*V1)/(T0*T1) *(U0*T1)
// dv1/dv0 = (V0*V1) - (T0*V1)/(T0*T1) *(V0*T1)
// 
// 
// 
// u0 = (U1*U0)*u1 + (V1*U0)*v1
// v0 = (U1*V0)*u1 + (V1*V0)*v1
// 
// det = (U1*U0)*(V1*V0)-(V1*U0)*(U1*V0)
// 
//       ( (V1*V0)  -(V1*U0))
// inv = (                 )/det
//       (-(U1*V0)   (U1*U0))
//       
// ============================================================      
// 
// vector = U0*fita0 + V0*lama0 + T0 = U1*fita1 + V1*lama1 + T1
// 
// fita1  = (U1*U0)*fita0 + (U1*V0)*lama0 +(T0*U1)
// lama1  = (V1*U0)*fita0 + (V1*V0)*lama0 +(T0*V1)
// 
// dFita1_dFita0 = (U1*U0);
// dFita1_dLama0 = (U1*V0);
// dLama1_dFita0 = (V1*U0);
// dLama1_dLama0 = (V1*V0);
// ============================================================      



  double dots[2][2];
  enum {kU,kV,kFita,kLama,kPinv};
//   for (int i=0;i<3;i++) {  
//   for (int j=0;j<3;j++) {
//     dots[i][j] = dot(tkDir[i],mTkDir[j]);
//   } }
  for (int i=0;i<=1;i++) {  
  for (int j=0;j<=1;j++) {
    dots[i][j] = dot(tkDir[i+kKU],mTkDir[j+kKU]);
  } }
  double det = dots[0][0]*dots[1][1]-dots[0][1]*dots[1][0];

  double du1_du0 =  dots[1][1]/det;
  double du1_dv0 = -dots[1][0]/det;
  double dv1_du0 = -dots[0][1]/det;
  double dv1_dv0 =  dots[0][0]/det;

  double dFita1_dFita0 = dots[0][0];
  double dFita1_dLama0 = dots[0][1];
  double dLama1_dFita0 = dots[1][0];
  double dLama1_dLama0 = dots[1][1];

  double T[5][5]={{0}};  
  T[kU][kU] 	  = du1_du0;
  T[kU][kV] 	  = du1_dv0;
  T[kV][kU] 	  = dv1_du0;
  T[kV][kV] 	  = dv1_dv0;
  T[kFita][kFita] = dFita1_dFita0;
  T[kFita][kLama] = dFita1_dLama0;
  T[kLama][kFita] = dLama1_dFita0;
  T[kLama][kLama] = dLama1_dLama0;
  T[kPinv][kPinv] = 1;
  
  double preRR[5*(5+1)/2];
  memcpy(preRR,&mUU,sizeof(preRR));
  for (int i=0,li=0;i< 5;li+=++i) {
    assert(preRR[li+i]>0);
  }
  TCL::trasat(T[0],preRR,&mUU,5,5);
  assert(mPP>0);
  memcpy(mTkDir[0],tkDir[0],sizeof(mTkDir));
  
}
//______________________________________________________________________________
double THEmx3d_t::Sign() const
{
  const double *E = &mUU;
  return EmxSign(5,E);
}
//______________________________________________________________________________
double THEmx3d_t::Trace() const {return mUU+mVV+mFF+mLL+mPP;}
//______________________________________________________________________________
void THEmx3d_t::Print(const char *tit) const
{
static const char *N="UVXFLP";
  if (!tit) tit = "";
  printf("THEmx3d_t::::Print(%s) ==\n",tit);
  const double *e = &mUU;
  for (int i=0,li=0;i< 5;li+=++i) {
    printf("%c ",N[i]);
    for (int j=0;j<=i;j++) {
    printf("%g\t",e[li+j]);} 
    printf("\n");
  }
}
//____________________________________________________________
double mySign(const double *a,int n)
{
   enum {kMaxDim=5,kMaxSize=(kMaxDim*(kMaxDim+1))/2 };
   double ans=3e33;
   double *aa = (double *)a;
   double save = aa[0]; if (!save) aa[0] = 1;
   double B[kMaxSize];
   double *myb = B;
   if (n>kMaxDim) myb = new double[(n*(n+1))/2];
   double *b = myb;
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
            if (sum<=0.) goto RETN;
            dc = sqrt(sum);
            b[kpiv] = dc;
            if (r__ > 0.)  r__ = (double)1. / dc;
         }
         kpiv += j;
      }

   } while  (i__ < n);

RETN: aa[0]=save; 
   if (myb!=B) delete [] myb;
   return ans;
} /* trchlu_ */
//______________________________________________________________________________
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
void THelix3d::Test()
{
  double PtGev = 1.,Rho = 1./100, Hz = PtGev*Rho ;
  double ZER[3]= {0,0,0};
  double HZ[3] = {0,0,Hz};
  double XZ[3] = {100,100,100};
  double HH[3],XX[3],PP[3],PZ[3] ;
  
  TVector3 vHZ(HZ);
  vHZ.RotateX(1.); vHZ.RotateY(1.); vHZ.GetXYZ(HH);

  TVector3 vXZ(XZ);
  vXZ.RotateX(1.); vXZ.RotateY(1.); vXZ.GetXYZ(XX);

  TVector3 vPZ(PtGev,0,4./3*PtGev);
  /*vPZ.RotateZ(1.);*/ vPZ.GetXYZ(PZ);
  vPZ.RotateX(1.); vPZ.RotateY(1.); vPZ.GetXYZ(PP);

  for (int charge=1;charge >=-1;charge-=2) {

    THelixTrack TH(ZER,PZ,-Rho*charge);
    double s1 = TH.Path(XZ);
    THelix3d    T3(charge,ZER,PP,HH);
    double s2 = T3.Path(XX);
    printf ("Charge = %d S1,S2 = %g %g \n",charge,s1,s2);
  }

}
//______________________________________________________________________________
void THelix3d::Test2()
{
printf("THelix3d::Test2() Empty\n");
}
//______________________________________________________________________________
void THelix3d::TestDer2()
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

// XbegV.Print("XbegV");
// PbegV.Print("PbegV");
//  magV.Print("magV");

  printf("===============PbegV:= \n"); PbegV.Print("");

  TVector3 XendV,PendV;
  const THDer3d_t *myDer = 0;
  for (int ichar=-1; ichar<=1;  ichar +=2) {
    THelix3d TH0(ichar,(double*)&XbegV[0],(double*)&PbegV[0],Mag);
    TH0.SetDerOn();
    TH0.Move(L);
    TH0.Backward();			//At the end TH0 helix inverted
    myDer = TH0.Der();			//Inverted derivatives
    TH0.Eval(0,&XendV[0],&PendV[0]);	// Got X & P at the end
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


      THelix3d TH1(ichar,(double*)&XbegV[0],(double*)&PbegV[0],Mag);
      TH1.Backward();
      TVector3 P1begV(TH1.Mom()); 
      TVector3 X1begV(TH1.Pos());
      TkDir_t myTkDir;
      THelix3d::MakeTkDir((double*)&P1begV[0],Mag,myTkDir);
      double eps = (TVectorD(9,myTkDir[0])-TVectorD(9,myDer->TkDir(0)[0])).NormInf();
      assert(eps<1e-3);


//??      double delta = 1e-4;
      double delta = 1e-6;
      TVector3 X1endV,P1endV;
      assert(TH0.Pinv()*ichar>0);
      assert(TH1.Pinv()*ichar>0);
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
          double Pinv1 = TH1.Pinv()+delta;         
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
      TH1.Set(0,(double*)&X1begV[0],(double*)&P1begV[0],0);
      assert(TH1.Pinv()*ichar>0);

      TH1.Move(-L);

      TH1.Eval(0,&X1endV[0],&P1endV[0]);
      auto D1endV = P1endV.Unit();
      double dL = -(X1endV-XendV).Dot(PendV)/(PendV.Dot(D1endV));
      TH1.Move(dL);
      TH1.Eval(0,&X1endV[0],&P1endV[0]);

      TVectorD dif(5);
      TVector3 difX =  X1endV-XendV;
      TVector3 difD =  (P1endV.Unit()-PendV.Unit());
      dif[kU]    = difX.Dot(UendV);
      dif[kV]    = difX.Dot(VendV);
      dif[kPinv] = TH1.Pinv()-TH0.Pinv();
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
void THelix3d::TestDer()
{
// Test for derivatives + changing direction

  double Pt=1;
  double curv = 1./30,Hz = Pt*curv;
  TVector3 XbegV = TVector3(0,0,0);
  TVector3 PbegV = TVector3(Pt,0,Pt/3*4);
  double Ptot = PbegV.Mag();
  double cosBeg = Pt/Ptot;

  double HZ[3] = {0,0,Hz};


  double L = 100.;
  L = M_PI/curv/cosBeg;



  double Mag[3];
  TVector3 magV(HZ); magV.GetXYZ(Mag );
#if 1
  double r1=gRandom->Rndm(),r2=gRandom->Rndm();
  magV.RotateX(r1);  magV.RotateY(r2);  magV.GetXYZ(Mag );
  PbegV.RotateX(r1); PbegV.RotateY(r2);
  XbegV.RotateX(r1); XbegV.RotateY(r2);
#endif 


  printf("===============PbegV:= \n"); PbegV.Print("");

  TVector3 XendV,PendV;
  const THDer3d_t *myDer = 0;
  for (int ichar=-1; ichar<=1;  ichar +=2) {
    THelix3d TH0(ichar,(double*)&XbegV[0],(double*)&PbegV[0],Mag);
    TH0.SetDerOn();
    TH0.Move(L);
 //??   TH0.Backward();			//At the end TH0 helix inverted
    myDer = TH0.Der();			//Inverted derivatives
    TH0.Eval(0,&XendV[0],&PendV[0]);	// Got X & P at the end
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

      printf("\nTest dFit/d%s\n",dp[J]);
      printf("==============\n");
      for (int i=0;i<5;i++) {der[i] = (*myDer)[i][J];}


      THelix3d TH1(ichar,(double*)&XbegV[0],(double*)&PbegV[0],Mag);
//??      TH1.Backward();
      TVector3 P1begV(TH1.Mom()); 
      TVector3 X1begV(TH1.Pos());
      TkDir_t myTkDir;
      THelix3d::MakeTkDir((double*)&P1begV[0],Mag,myTkDir);
      double eps = (TVectorD(9,myTkDir[0])-TVectorD(9,myDer->TkDir(0)[0])).NormInf();
      assert(eps<1e-3);


      double delta = 1e-6;
      TVector3 X1endV,P1endV;
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
          double Pinv1 = TH1.Pinv()+delta;         
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
      TH1.Set(0,(double*)&X1begV[0],(double*)&P1begV[0],0);
      TH1.Move(L);
      TH1.Eval(0,&X1endV[0],&P1endV[0]);
      auto D1endV = P1endV.Unit();
      double dL = -(X1endV-XendV).Dot(PendV)/(PendV.Dot(D1endV));
      TH1.Move(dL);
      TH1.Eval(0,&X1endV[0],&P1endV[0]);

      TVectorD dif(5);
      TVector3 difX =  X1endV-XendV;
      TVector3 difD =  (P1endV.Unit()-PendV.Unit());
      dif[kU]    = difX.Dot(UendV);
      dif[kV]    = difX.Dot(VendV);
      dif[kPinv] = TH1.Pinv()-TH0.Pinv();
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
void THelix3d::TestErr(int charge)
{
  double PtGev = 3.,Curv = 1./100, Hz = PtGev*Curv;
  double HZ[3] = {0,0,Hz};
  double Pbeg[3],P1beg[3],Xbeg[3]={0},X1beg[3];
  double Pend[3],P1end[3],Xend[3]    ,X1end[3];

  double L = 300.;

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
const char * tit[]={"U","V","Fita","Lama","Pinv",0};
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
static const char *tit[]={"UV","UF","VF","UL","VL","FL","UP","VP","FP","LP"};
  for (int ih=0;ih<10;ih++) {
    delete hcr[ih];
    hcr[ih] = new TH1F(tit[ih],tit[ih],100,0,0);
    if (ih<5) myCanvas[3]->cd(ih+1  ); 
    else      myCanvas[4]->cd(ih+1-5);
    hcr[ih]->Draw();
  }}
  
    
  TVector3 PbegV(PtGev,0,PtGev*3/4);
  double Ptot = PbegV.Mag();
  PbegV.RotateZ(gRandom->Rndm()*3.1415*2); PbegV.GetXYZ(Pbeg);
  TVector3 XbegV(Xbeg);

  double Mag[3]={HZ[0],HZ[1],HZ[2]};
#if 11
  TVector3 magV(HZ); 
  double r1 = gRandom->Rndm(),r2 = gRandom->Rndm(); 
   magV.RotateX(r1);  magV.RotateY(r2);  magV.GetXYZ(Mag);
  PbegV.RotateX(r1); PbegV.RotateY(r2); PbegV.GetXYZ(Pbeg);
#endif
  TVectorD dia(5);
  dia[kU]= 0.1; dia[kV]= 0.2; dia[kPinv]= 0.1/Ptot; dia[kFita]= 1./360; dia[kLama]= 2./360;
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
  assert( mySign(Gbeg,5)>0);


  double GbegI[15];
  TCL::trsinv(Gbeg,GbegI,5);  
  assert( mySign(GbegI,5)>0);
  
  THelix3d TH0(charge,Xbeg,Pbeg,Mag);
  THEmx3d_t *emx = new THEmx3d_t(Gbeg);
  TH0.SetEmx(emx);
  TH0.Move(L);
  TH0.Eval(0,Xend,Pend);
  auto *myEmx = TH0.Emx();
  const double *Gend = *myEmx;
  assert(mySign(Gend,5)>0);
  double GendD[5];
  for (int i=0,li=0;i< 5;li+=++i) {
    GendD[i] = sqrt(Gend[li+i]);
  } 
  double GendI[15];
  TCL::trsinv(Gend,GendI,5);  

  assert(mySign(GendI,5)>0);

  auto &tkDir   = TH0.TkDir(0);
  auto &tkDirE  = TH0.TkDir(1);
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
    TVector3 P1begV = PbegV;
    TVector3 X1begV = XbegV;

    X1begV     += TVector3(tkDir[kKU])*delta[kU];
    X1begV     += TVector3(tkDir[kKV])*delta[kV];
    double Pinv = -charge/Ptot;
    double Pinv1 = Pinv + delta[kPinv];
    double Ptot1 = fabs(1./Pinv1);
    P1begV.SetMag(Ptot1);
    P1begV += TVector3(tkDir[kKV])*(delta[kLama]*Ptot1);P1begV.SetMag(Ptot1);
    P1begV += TVector3(tkDir[kKU])*(delta[kFita]*Ptot1);P1begV.SetMag(Ptot1);

    X1begV.GetXYZ(X1beg);
    P1begV.GetXYZ(P1beg);
    THelix3d TH1(charge,X1beg,P1beg,Mag);
    double s = TH1.Path(Xend);
    TH1.Move(s);
    TH1.Eval(0,X1end,P1end);
    TVector3 X1endV(X1end),P1endV(P1end);
    TVectorD U1endV(5);
    TVector3 difX =  X1endV-XendV;
    TVector3 difD =  (P1endV.Unit()-PendV.Unit());

    U1endV[kU]    = difX.Dot(TVector3(tkDirE[kKU]));
    U1endV[kV]    = difX.Dot(TVector3(tkDirE[kKV]));
    U1endV[kPinv] = (Pinv1-Pinv);
    U1endV[kLama] = difD.Dot(TVector3(tkDirE[kKV]));
    U1endV[kFita] = difD.Dot(TVector3(tkDirE[kKU]));

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
  }

  for (int i=0;myCanvas[i];i++) {
    if (!myCanvas[i]) continue;
    myCanvas[i]->Modified();myCanvas[i]->Update();}

  while(!gSystem->ProcessEvents()){gSystem->Sleep(200);}; 
}
//______________________________________________________________________________
void THelix3d::TestErr2(int charge)
{
  double PtGev = 1.,Curv = 1./30, Hz = PtGev*Curv;
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
  assert( mySign(Gbeg,5)>0);


  
  THelix3d TH0(charge,Xbeg,Pbeg,Mag);
//==================================
  THEmx3d_t *emx  = new THEmx3d_t(Gbeg);

  THelix3d TH0i(&TH0);
  TH0i.SetEmx(emx);
  TH0i.Backward();
  double GbegI[15];
  memcpy(Gbeg,*TH0i.Emx(),sizeof(Gbeg));
  TRandomVector RVi(5,Gbeg);
  TCL::trsinv(Gbeg,GbegI,5);  
  assert( mySign(GbegI,5)>0);

  TH0.SetEmx(emx);
  TH0.Move(L);
  TH0.Backward();

  double tkDir[3][3],tkDirE[3][3];
  memcpy(tkDir[0] ,TH0.TkDir(0)[0],sizeof(tkDir ));
  memcpy(tkDirE[0],TH0.TkDir(1)[0],sizeof(tkDirE));

  TH0.Eval(0,Xend,Pend);
  auto *myEmx = TH0.Emx();
  const double *Gend = *myEmx;
  assert(mySign(Gend,5)>0);
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
    THelix3d TH1(TH0i.Charge(),X1beg,P1beg,Mag);
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
void THelix3d::TestErr3(int charge)
{
//  double PtGev = 1.,Curv = 1./100, Hz = PtGev*Curv;
  double PtGev = 1.,Curv = 1./30, Hz = PtGev*Curv;
  double HZ[3] = {0,0,Hz};
  double Pbeg[3],P1beg[3],Xbeg[3]={0},X1beg[3];
  double Pend[3],P1end[3],Xend[3]    ,X1end[3];
  double myTkRot = 0.1;
  
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
const char * tit[]={"U","V","Fita","Lama","Pinv",0};
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
static const char *tit[]={"UV","UF","VF","UL","VL","FL","UP","VP","FP","LP"};
  for (int ih=0;ih<10;ih++) {
    delete hcr[ih];
    hcr[ih] = new TH1F(tit[ih],tit[ih],100,0,0);
    if (ih<5) myCanvas[3]->cd(ih+1  ); 
    else      myCanvas[4]->cd(ih+1-5);
    hcr[ih]->Draw();
  }}
  
    
  TVector3 PbegV(PtGev,0,PtGev*3);
  double Ptot = PbegV.Mag();
  PbegV.RotateZ(gRandom->Rndm()*3.1415*2); PbegV.GetXYZ(Pbeg);
  TVector3 XbegV(Xbeg);

  double Mag[3]={HZ[0],HZ[1],HZ[2]};
#if 11
  TVector3 magV(HZ); 
  double r1 = gRandom->Rndm(),r2 = gRandom->Rndm(); 
   magV.RotateX(r1);  magV.RotateY(r2);  magV.GetXYZ(Mag);
  PbegV.RotateX(r1); PbegV.RotateY(r2); PbegV.GetXYZ(Pbeg);
#endif
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
  assert( mySign(Gbeg,5)>0);


  double GbegI[15];
  TCL::trsinv(Gbeg,GbegI,5);  
  assert( mySign(GbegI,5)>0);
  
//	initial state
  THelix3d TH0(charge,Xbeg,Pbeg,Mag);
  TH0.SetEmx(Gbeg);
  THEmx3d_t emx = *TH0.Emx();
  TkDir_t &tkDir = emx.TkDir();

//	now forget Pbeg, it was only temporary. Choose another Pbeg but close to
//	previous. But errors and TkDir will belong to new Pbeg. For this case
//      tkDir[kkT] != new Pbeg



  TH0.Eval(0,Xbeg,Pbeg);
  PbegV.RotateX(myTkRot);
  PbegV.RotateY(myTkRot);
  PbegV.GetXYZ(Pbeg);
  TH0.Set(0,0,Pbeg);
  TkDir_t tkDirE   = TH0.TkDir(0);
//		Pend == Pbeg
  TVector3 PendV(Pbeg),XendV(Xbeg);
  PendV.GetXYZ(Pend); XendV.GetXYZ(Xend);
  


  auto *emxE = TH0.Emx();
  const double *Gend = *emxE;


  assert(mySign(Gend,5)>0);
  double GendD[5];
  for (int i=0,li=0;i< 5;li+=++i) {
    GendD[i] = sqrt(Gend[li+i]);
  } 
  double GendI[15];
  TCL::trsinv(Gend,GendI,5);  

  assert(mySign(GendI,5)>0);
  TVectorD UendV(5);

  for (int j=0;j<3;j++) { UendV[j+2]=Pend[j];}
  
  double myG[15]={0};
  int nIter = 1000000;  
  for (int iter=0;iter<nIter;iter++) {
    TVectorD delta  = RV.Gaus();
    double chi2;
    TCL::trasat(delta.GetMatrixArray(),GbegI,&chi2,1,5);
    assert(chi2>0);
    assert(chi2<100);
    hXi2[0]->Fill(chi2);

    for (int ih=0;ih<myDiv[1];ih++) { hrt[ih]->Fill(delta[ih]/GbegD[ih]);}
    TVector3 P1begV = PbegV;
    TVector3 D1begV = P1begV.Unit();
    TVector3 X1begV = XbegV;
    double cosL = D1begV.Dot(tkDir.V());
    double cosLF= D1begV.Dot(tkDir.T());

    cosL=1;cosLF=1;
    
    X1begV     += TVector3(tkDir.U())*delta[kU];
    X1begV     += TVector3(tkDir.V())*delta[kV];
    double Pinv = -charge/Ptot;
    double Pinv1 = Pinv + delta[kPinv];
    double Ptot1 = fabs(1./Pinv1);
    P1begV.SetMag(Ptot1);
    P1begV += TVector3(tkDir[kKV])*(delta[kLama]*Ptot1*cosL );P1begV.SetMag(Ptot1);
    P1begV += TVector3(tkDir[kKU])*(delta[kFita]*Ptot1*cosLF);P1begV.SetMag(Ptot1);

    X1begV.GetXYZ(X1beg);
    P1begV.GetXYZ(P1beg);
    THelix3d TH1(charge,X1beg,P1beg,Mag);
    double aa =      (X1begV-XbegV).Dot(tkDirE.T());
    double bb = P1begV.Unit().Dot(tkDirE.T());
    double tau = -aa/bb;
    assert(fabs(tau)<1);
    TH1.Move(tau);
    TH1.Eval(0,X1end,P1end);
    TVector3 X1endV(X1end),P1endV(P1end);
    TVectorD U1endV(5);
    TVector3 difX =  X1endV-XendV;
    TVector3 difD =  (P1endV.Unit()-PendV.Unit());

    U1endV[kU]    = difX.Dot(tkDirE[kKU]);
    U1endV[kV]    = difX.Dot(tkDirE[kKV]);
    U1endV[kPinv] = (Pinv1-Pinv);
    U1endV[kLama] = difD.Dot(tkDirE[kKV]);
    U1endV[kFita] = difD.Dot(tkDirE[kKU]);

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
  }

  for (int i=0;myCanvas[i];i++) {
    if (!myCanvas[i]) continue;
    myCanvas[i]->Modified();myCanvas[i]->Update();}

  while(!gSystem->ProcessEvents()){gSystem->Sleep(200);}; 
}
//______________________________________________________________________________
void THelix3d::ConvertErrs(const THelixTrack *he, const double Bzp
                          , double G[15],double D[5][5],double Di[5][5])
{
//		in THelixTrack:
//  Let Dx,Dy,Dz direction of track
//  dH:  along  vector (-Dy,Dx,0)
//  dZ:  along Zaxis
//  dA: delta azimuth angle (in X,Y plane; 
//  dC: delta of curvature;  
//  dL = dLambda, angle between track and X,Y plane
//   B: direction of mag field and it is along Z
//   T = (cosLam*cosPhi,coaLam*sinPhi,sinLam) == track dir
//   H = (-sinPhi,cosPhi,0)
//   Z = (      0,     0,1)
//   C = curvature
//   THelixTrack space point modification = H*h + Z*z
//   
// //		in THelix3d:
//   V: vector orthogonal T and in plane (T,B) in our case B==Z
// 
//      V ~= Z-(Z*T)*T = Z - sinLam*T
//      V ~= (-sinLam*coaLam*cosPhi,-sinLam*coaLam*sinPhi,1-sinLam**2)
//      V ~= (-sinLam*cosPhi,-sinLam*sinPhi,cosLam)*cosLam
// After normalization:
// 
// 
//      V  = (-sinLam*cosPhi,-sinLam*sinPhi,cosLam)
//      U  = (-       sinPhi,        cosPhi,     0)  
// 
//    We see that H == U
//
//   THelixTrack: space point modification = H*h + Z*z 
//   THelix3d:    space point modification = U*u + V*v 
// 
// 
//       U*h + Z*z +T*t = U*u + V*v
//       t is moving along track from crossing with THelixTrack plane
//       orthogonal (Tx,Ty,0) 
//       to THelix3d plane orthogonal (Tx,Ty,Tz) 
//       Mult by T
//       (T*Z)z + t = 0
//       t = - (T*Z)*z
// 
//       U*h + Z*z -T*(T*Z)*z = U*u + V*v
//       U*h +  (Z-T*(T*Z))*z = U*u + V*v
// 
//       From the above (Z-T*(T*Z)) == V*cosLam
//       U*h +  V*cosLam*z = U*u + V*v
// 
// Hence h == u and cosLam*z = v
// 
// du/dh = 1
// dv/dz = cosLam
// 


// ======================================================================
//   Now angles:
//
//       U*h + Z*z +T*t = U*u + V*v
//   T = Ex*cosLam*cosPhi+Ey*cosLam*sinPhi+Ez*sinLam
//   
//   dT = (-Ex*cosLam*sinPhi+Ey*cosLam*cosPhi           )*dPhi
//      + (-Ex*sinLam*cosPhi-Ey*sinLam*sinPhi +Ez*CosLam)*dLam
// 
//   dT = (-Ex*sinPhi+Ey*cosPhi)*cosLam             *dPhi
//      + ((-Ex*cosPhi-Ey*sinPhi)*sinLam +Ez*CosLam)*dLam
//      =         U*fita +V*Lama
// 
//   dT = U*cosLam*dPhi+ V*dLam
//   fita = cosLam*dPhi
//   lama = dLam


//     dPhi = dPhi0 - (T*Z)*z*cosLam*Rho
//     dPhi = dPhi0 - sinLam*cosLam*Rho*z
//
//      dT = U*cosLam*(dPhi-(T*Z)*z*cosLam*Rho) + V*dLam
// 	   = U*fita       + V*Lama
// 
//   dFita/dPhi =   cosLam 
//   dFita/dz   = - sinLam*cosLam*cosLam*Rho
//   dLamadLam = 1
// 
// 
//   	Now Pinv
//   Pinv = -fCharge/fMom;
//   Rho = -fCharge*B[2]/pt;
//   -fCharge/pt = Rho/B[2]
//   -fCharge/(pt/cosLam) = Rho/B[2]*cosLam
//   -fCharge/(P) = Rho/B[2]*cosLam
//    Pinv = Rho*cosLam/B[2]
// 
//     dPinv/dRho =  cosLam/B[2] 
//     dPinv/dLam =-sinLam*Rho/B[2]
//
//=======================================================================
//		In total
//=======================================================================
// du    = dH
// dv    = cosLam*dz
// dFita = cosLam*dPhi - sinLam*cosLam*cosLam*Rho*dz
// dLama = dLam 
// dPinv =  cosLam/B[2]*dRho-sinLam*Rho/B[2]*dLam
//=======================================================================
//   du/dh      = 1
//   dv/dz      = cosLam
//   dFita/dPhi = cosLam 
//   dFita/dz   = - sinLam*cosLam *cosLam*Rho
//   dLamadLam  = 1
//   dPinv/dRho = cosLam/B[2] 
//   dPinv/dLam =-sinLam*Rho/B[2]
// 
//   dH = du
//   dPhi  = dFita/cosLam + sinLam*Rho*dv
//   dRho = dPinv/cosLam*B[2] +tanLam*Rho*dLama
//   dz = dv/cosLam
//   dLam = dLama
//
// ======================================================================
static const double kMinBz = 0.001494399 * 1e-3; 
  const double *T  = he->Dir();
  double Rho = he->GetRho();
  double sinLam = T[2], cosLam = sqrt((1-sinLam)*(1+sinLam));
//double cosPhi = T[0]/cosLam, sinPhi = T[1]/cosLam;


  double Bz = (fabs(Bzp) < kMinBz)? kMinBz:Bzp;
  double dUdH       = 1;
  double dVdZ       = cosLam;
  double dFita_dPhi = cosLam;
  double dFita_dZ   = -sinLam*cosLam*cosLam*Rho;
  double dLama_dLam = 1;
  double dPinv_dRho = cosLam/Bz;
  double dPinv_dLam = -Rho*sinLam/Bz;


  double Mtx[5][5] ={
  //             H         Phi       Curv        Z        Lam
  //----------------------------------------------------------
  /*U*/ 	{dUdH,       0,         0,       0,         0},
  /*V*/ 	{0, 	     0,         0,    dVdZ,         0},
  /*Fita*/	{0, dFita_dPhi,         0,dFita_dZ,         0},
  /*Lama*/	{0, 	     0,         0,       0,dLama_dLam},
  /*Pinv*/	{0,          0,dPinv_dRho,       0,dPinv_dLam}};
  //-----------------------------------------------------------
  if ( D) memcpy(D[0],Mtx[0],sizeof(Mtx));

  if (Di) {
    double dHdU = 1;
    double dPhi_dFita = 1./cosLam;
    double dPhi_dV = sinLam*Rho;
    double dRho_dPinv = Bz/cosLam;
    double dRho_dLama = sinLam/cosLam*Rho;
    double dZdV = 1./cosLam;
    double dLam_dLama = 1;
    double Xtm[5][5] ={

    //             U           V             Fita        Lama        Pinv
    //----------------------------------------------------------------------
    /*H*/ 	{dHdU,       0,               0,          0,          0},
    /*Phi*/ 	{0,    dPhi_dV,      dPhi_dFita,          0,          0},
    /*Curv*/	{0,          0,               0, dRho_dLama, dRho_dPinv},
    /*Z*/		{0, 	     dZdV,            0,          0,          0},
    /*Lam*/	{0,          0,               0, dLam_dLama,          0}};
    //----------------------------------------------------------------------  

    double qwe[5][5];
    TCL::mxmpy(Mtx[0],Xtm[0],qwe[0],5,5,5);
    for (int i=0;i<5;i++) {
    for (int j=0;j<5;j++) {
      double ttt = qwe[i][j]; if (i==j) ttt--;
      assert(fabs(ttt)<1e-8);
    } }
    memcpy(Di[0],Xtm[0],sizeof(Mtx));
  }

  if (!G) return;

  double hemx[15];
  TCL::ucopy(*he->Emx(),hemx,15);
  TCL::trasat(Mtx[0],hemx,G,5,5);
  for (int i=0,li=0;i<5;li+=++i) { 
    assert(hemx[li+i]>0);
    assert(   G[li+i]>0);
  }
}  
//______________________________________________________________________________
void THelix3d::TestConvertErrs()
{
  double PtGev = 1.,Curv = 1./100, Mag[3] = {0,0,PtGev*Curv};
  double Pbeg[3],Xbeg[3]={0};
    
  TVector3 PbegV(PtGev,0,PtGev/3*4);
  int icharge = (-Curv*PtGev/Mag[2]<0)? -1:1;
#if 0
  PbegV.RotateZ(gRandom->Rndm()*3.1415);
#endif
  PbegV.GetXYZ(Pbeg);
  TVector3 XbegV(Xbeg);

  double L = 100.;
  TVectorD dia(5);
  dia[0]= 0.1; dia[1]= 3./360; dia[2]= Curv*0.01; dia[3]= 0.2; dia[4]= 2./360;
  for (int i=0;i<5;i++) {dia[i]*=dia[i];}
  
  TRandomVector RV(dia);
  auto &EMX = RV.GetMtx();
  auto &val = RV.GetLam();
  dia.Print("DIA");
  val.Print("VAL");


  double Gbeg[15],Gbeg1[15],Gend[15];
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i;j++) {
      Gbeg[li+j] = EMX[i][j];
  } }
  
  THelixTrack TH0(     Xbeg,Pbeg,Curv);
  THelix3d    TH1(icharge  ,Xbeg,Pbeg,Mag );
  assert (fabs(TH1.GetRho()-Curv)<1e-5);
  TH0.SetEmx(Gbeg); 

  ConvertErrs(&TH0,Mag[2],Gbeg1);

  TVectorD Gbeg0V(15,Gbeg  );  Gbeg0V.Print("Gbeg ");
  TVectorD Gbeg1V(15,Gbeg1 );  Gbeg1V.Print("Gbeg1");

  auto *emx = TH1.SetEmx(Gbeg1);
  emx->Print("Converted1");
  
  TH0.Move(L);
  TH1.Move(L);
  TVector3 vX0(TH0.Pos());
  TVector3 vX1(TH1.Pos());
  double eps = (vX1-vX0).Mag();
  assert(eps<1e-6);
  TVector3 vD0(TH0.Dir());
  TVector3 vD1(TH1.Dir());
  eps = (vD1-vD0).Mag();
  assert(eps<1e-6);

  ConvertErrs(&TH0,Mag[2],Gend);
//const double *Gend0 = *TH0.Emx();   
  const double *Gend1 = *TH1.Emx();   
  TVectorD Gend0V(15,Gend  );  Gend0V.Print("Gend ");
  TVectorD Gend1V(15,Gend1 );  Gend1V.Print("Gend1");

  double GendD[5],Gend1D[5];
  const char *tit = "UVFLP";
  for (int i=0,li=0;i< 5;li+=++i) {
    GendD [i] = sqrt(Gend [li+i]);
    Gend1D[i] = sqrt(Gend1[li+i]);
    double dif = 2*(GendD[i]-Gend1D[i])/(GendD[i]+Gend1D[i]);
    printf("[%c][%c] %g %g dif = %g\n",tit[i],tit[i],Gend [li+i],Gend1[li+i],dif);
   for (int j=0;j<i;j++) {
     double cor0 = Gend [li+j]/(GendD [i]*GendD [j]);
     double cor1 = Gend1[li+j]/(Gend1D[i]*Gend1D[j]);
     dif = (cor0-cor1);
     printf("[%c][%c] %g %g dif = %g\n",tit[i],tit[j],cor0,cor1,dif);
  }}

}
//______________________________________________________________________________
void THelix3d::TestConvertDers()
{
// Test for derivatives + changing direction
enum {kkH,kkPhi,kkCur,kkZ,kkLam};

  double Pt=1;
  double Curv = 1./100,Hz = Pt*Curv;
  TVector3 XbegV = TVector3(0,0,0);
//??  TVector3 PbegV = TVector3(Pt,0,Pt/3*4);
  TVector3 PbegV = TVector3(Pt,0,0.01);
  TVector3 PbegV1(PbegV);
  TVector3 DbegV = PbegV.Unit();
  double HZ[3] = {0,0,Hz};
  int icharge = (-Curv*Pt/HZ[2]<0)? -1:1;

  double L = 100.,dL;

  double Dhlx[5][5],Dh3d[5][5],ConvEnd[5][5],ConvBeg[5][5];
  double Der1[5][5],NumDer1[5][5]; //dH3d/dHlx
  double Der2[5][5],NumDer2[5][5]; //dH3d/dHlx
  double NumDer3[5][5];  

  THelixTrack THbeg((double*)&XbegV[0],(double*)&DbegV[0],Curv);
  double CosBeg = THbeg.GetCos();
  printf(" ==================== CosBeg = %g  ==========================\n",CosBeg);

//??  L = 0.5*CosBeg/Curv*M_PI; ////????


  THelixTrack THend((double*)&XbegV[0],(double*)&DbegV[0],Curv);
  THend.Move(L,Dhlx);
  double CosEnd = THend.GetCos();
  TVector3 XendV(THend.Pos());
  TVector3 DendV(THend.Dir());

  THelix3d T3beg(icharge,(double*)&XbegV[0],(double*)&PbegV[0],HZ);
  THelix3d T3end(icharge,(double*)&XbegV[0],(double*)&PbegV[0],HZ);
  double Pinv = T3beg.Pinv();
  T3end.SetDerOn();
  T3end.Move(L,Dh3d);

//  		Conv = d(3d)/d(Hlx)
  T3beg.ConvertErrs(&THbeg,HZ[2],0,ConvBeg);
  T3end.ConvertErrs(&THend,HZ[2],0,ConvEnd);

  TCL::mxmpy(ConvEnd[0],Dhlx[0],Der1[0],5,5,5);
  TCL::mxmpy(Dh3d[0],ConvBeg[0],Der2[0],5,5,5);
    auto myDer = T3end.Der();		//derivatives

    TVector3 HbegV(myDer->TkDir(0)[kKU]);	//H vector
    TVector3 UbegV(myDer->TkDir(0)[kKU]);	//U vector
    TVector3 VbegV(myDer->TkDir(0)[kKV]);	//V vector
    TVector3 ZbegV(0.,0.,1.); 			//zVECTOR
    TVector3 TbegV(myDer->TkDir(0)[kKT]); 	//Along track vector

    TVector3 UendV(myDer->TkDir(1)[kKU]);	//U Phi vector
    TVector3 VendV(myDer->TkDir(1)[kKV]); 	//V Lam vector
    TVector3 TendV(myDer->TkDir(1)[kKT]); 	//Along track vector
    
    TVectorD der(5);
    for (int J=0;J<5;J++) {
      TVector3 X1begV(XbegV),D1begV(DbegV);
      double Curv1 = Curv;
      double delta = 1e-4;
      switch(J) {
        case kkH:{
	  X1begV += HbegV*delta;
          break;
        } 
	case kkPhi: {	//Phi variation (beta)
	  D1begV+=HbegV*(delta*CosBeg); D1begV.SetMag(1.);
          break;
        }
        case kkCur: {
          Curv1+=delta;         
          break;
	}
        case kkZ: {
	  X1begV += ZbegV*delta;
          break;
        } 
	case kkLam: {	//lamda variation (alfa)
	  D1begV+=VbegV*delta; D1begV.SetMag(1.);
          break;
        }

        default: assert(0 && "Wrong J");
      }
//		Now separate THelx3d derivative
      TVector3 X2begV(XbegV),D2begV(DbegV),P2begV;
      double P2invBeg=Pinv;
      double delta2 = delta;
      switch(J) {
        case kU:{
	  X2begV += UbegV*delta2;
          break;
        } 
	case kFita: {	//Fita variation (beta)
	  D2begV+=UbegV*(delta2); D2begV.SetMag(1.);
          
          break;
        }
        case kPinv: {
          P2invBeg+= delta2;
          break;
	}
        case kV: {
	  X2begV += VbegV*delta2;
          break;
        } 
	case kLama: {	//lama variation 
          delta2 = 3.14/180;
	  D2begV+=VbegV*delta2; D1begV.SetMag(1.);
          break;
        }

        default: assert(0 && "Wrong J");
      }



//		By THelixTrack
      THelixTrack TH1((double*)&X1begV[0],(double*)&D1begV[0],Curv1);
      double CosBeg1 = TH1.GetCos();
      TH1.Move(L);
      TVector3 X1endV(TH1.Pos());
      TVector3 D1endV(TH1.Dir());
      dL = -(X1endV-XendV).Dot(DendV)/(DendV.Dot(D1endV));
      TH1.Move(dL);
      double CosEnd1 = TH1.GetCos();
      X1endV = TVector3(TH1.Pos());
      D1endV = TVector3(TH1.Dir());


      TVectorD dif(5);
      TVector3 difX =  X1endV-XendV;
      TVector3 difD =  (D1endV-DendV);
      dif[kU]    = difX.Dot(UendV);
      dif[kV]    = difX.Dot(VendV);
      dif[kPinv] = -icharge*(Curv1*CosEnd1-Curv*CosEnd)/Hz;
      dif[kFita] = difD.Dot(UendV);
      dif[kLama] = difD.Dot(VendV);
      assert(fabs(dif.NormInf())<1.);
      dif*=1./delta;
      for (int i=0;i<5;i++) { NumDer1[i][J] = dif[i];}

//		By THelix3d
      double P1tot1 = Hz/(Curv1*CosBeg1);
      auto P1begV = D1begV*=P1tot1;
      THelix3d T31(icharge,(double*)&X1begV[0],(double*)&P1begV[0],HZ);
      T31.Move(L);
      TVector3 P1endV;
      T31.Eval(0,&X1endV[0],&P1endV[0]);
      D1endV = P1endV.Unit();
      dL = -(X1endV-XendV).Dot(DendV)/(DendV.Dot(D1endV));
      TH1.Move(dL);
      X1endV = TVector3(T31.Pos());
      D1endV = TVector3(T31.Dir());
      double PinvEnd = T31.Pinv();
      difX =  X1endV-XendV;
      difD =  (D1endV-DendV);
      dif[kU]    = difX.Dot(UendV);
      dif[kV]    = difX.Dot(VendV);
      dif[kPinv] = PinvEnd-Pinv;
      dif[kFita] = difD.Dot(UendV);
      dif[kLama] = difD.Dot(VendV);
      assert(fabs(dif.NormInf())<1.);
      dif*=1./delta;
      for (int i=0;i<5;i++) { NumDer2[i][J] = dif[i];}


//		By THelixTrack but 3d derivative
      double CosBeg2 = sqrt((1.-D2begV.CosTheta())*(1.+D2begV.CosTheta()));
      double Curv2beg = -icharge*P2invBeg/CosBeg2*Hz;
#if 1
      THelixTrack TH2((double*)&X2begV[0],(double*)&D2begV[0],Curv2beg);
#endif
#if 0
      P2begV = D2begV*(1/fabs(P2invBeg));
      THelix3d TH2(icharge,(double*)&X2begV[0],(double*)&P2begV[0],HZ);
#endif

      TH2.Move(L);



      double CosEnd2 = TH2.GetCos();
      double CurvEnd2 = TH2.GetRho();
      TVector3 X2endV(TH2.Pos());
      TVector3 D2endV(TH2.Dir());
      difX =  X2endV-XendV;
      difD =  (D2endV-DendV);
      dif[kU]    = difX.Dot(UendV);
      dif[kV]    = difX.Dot(VendV);
      dif[kPinv] = -icharge*(CurvEnd2*CosEnd2-Curv*CosEnd)/Hz;
      dif[kFita] = difD.Dot(UendV);
      dif[kLama] = difD.Dot(VendV);
//???      assert(fabs(dif.NormInf())<1.);
      dif*=1./delta2;
      for (int i=0;i<5;i++) { NumDer3[i][J] = dif[i];}



    }

static const char* LabHlx[]={"H  ","Phi","Cur","Z  ","Lam"};
static const char* LabH3d[]={"U  ","V  ","Fit","Ama","Pin"};
  for (int ihlx=0;ihlx<5;ihlx++) {
  for (int ih3d=0;ih3d<5;ih3d++) {
    printf("[%s][%s] = %g == %g == %g == %g\n",LabH3d[ih3d],LabHlx[ihlx]
                                  ,Der1[ih3d][ihlx]
                                  ,Der2[ih3d][ihlx]
                                  ,NumDer1[ih3d][ihlx]
                                  ,NumDer2[ih3d][ihlx]);
  }}
printf("\n\n");
  for (int ih3d=0;ih3d<5;ih3d++) {
  for (int jh3d=0;jh3d<5;jh3d++) {
    printf("[%s][%s] = %g == %g \n",LabH3d[ih3d],LabH3d[jh3d]
                                  ,Dh3d[ih3d][jh3d]
                                  ,NumDer3[ih3d][jh3d]);
  }}

}
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
