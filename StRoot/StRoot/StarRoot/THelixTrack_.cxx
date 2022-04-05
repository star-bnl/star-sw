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
#include "TRandomVector.h"
#include "StMatrixD.hh"
#include "TComplex.h"
#include "TH1.h"
#include <cassert>
#include "THelixTrack_.h"

// Complex numbers
const TComplex Im(0,1);
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

//_____________________________________________________________________________
inline static double dot(const TComplex &a,const TComplex &b)
{return a.Re()*b.Re()+a.Im()*b.Im();}
//_____________________________________________________________________________
inline static double dot(const double *a,const double *b)
{return a[0]*b[0]+a[1]*b[1]+a[2]*b[2];}
//_____________________________________________________________________________
inline static TComplex expOne(TComplex x)
{
  double a = TComplex::Abs(x);
  if (a<0.01) {
    return 1.+x*((1/2.) + x*((1/6.)+ x*(1/24.)));
  } else {
    return (TComplex::Exp(x)-1.)/x;
  }
}
//_____________________________________________________________________________
inline static TComplex expOneD(TComplex x)
{
  double a = TComplex::Abs(x);
  if (a<0.01) {
    return (1/2. + x*((1/6.)+ x*((1/24.)+x*(1/120.))));
  } else {
    return (TComplex::Exp(x)-1.-x)/(x*x);
  }
}

//______________________________________________________________________________
void TCEmx_t_::Set(const double *err)  	
{ if (err) {memcpy(this,err,sizeof(*this));} else {Clear();}}

//______________________________________________________________________________
void TCEmx_t_::Move(const double F[3][3])
{
  assert(mHH);
  double oErr[6];
  memcpy(oErr,Arr(),sizeof(oErr));
  TCL::trasat(F[0],oErr,Arr(),3,3); 
}
//______________________________________________________________________________
void TCEmx_t_::Backward()
{
  mHA*=-1; mAC*=-1; 
}
//______________________________________________________________________________
double TCEmx_t_::Sign() const
{
  const double *E = &mHH;
  double dia[3];
  for (int i=0,li=0;i<3;li+=++i) {
    dia[i] = E[li+i];
    if (dia[i]<0) return -(i+1)*11;
    for (int j=0;j<=i;j++) {
      double dis = dia[i]*dia[j]-E[li+j]*E[li+j];
      if (dis<0) return -(i+1+10*(j+1));
  } }
  return 0;

}

//______________________________________________________________________________
void THEmx_t_::Set(const double *errxy,const double *errz)
{
  Clear();
  memcpy(&mHH,errxy,sizeof(mHH)*6);
  mZZ = errz[0]; mZL =errz[1]; mLL =errz[2];
}
//______________________________________________________________________________
void THEmx_t_::Set(const double *err)  	
{ if (err) {memcpy(this,err,sizeof(*this));} else {Clear();}}
//_____________________________________________________________________________
void THEmx_t_::Backward()
{
  mHA*=-1; mAC*=-1; mHZ*=-1; mCZ*=-1; mAL*=-1; mZL*=-1;
}
//______________________________________________________________________________
void THEmx_t_::Move(const double F[5][5])
{
  assert(mHH);
  double oErr[15];
  memcpy(oErr,Arr(),sizeof(oErr));
  TCL::trasat(F[0],oErr,Arr(),5,5); 
}
//______________________________________________________________________________
void THEmx_t_::Print(const char *tit) const
{
static const char *N="HACZL";
  if (!tit) tit = "";
  printf("THEmx_t_::::Print(%s) ==\n",tit);
  const double *e = &mHH;
  for (int i=0,li=0;i< 5;li+=++i) {
    printf("%c ",N[i]);
    for (int j=0;j<=i;j++) {
    printf("%g\t",e[li+j]);} 
    printf("\n");
  }
}
//______________________________________________________________________________
double THEmx_t_::Sign() const
{
  const double *E = &mHH;
  double dia[5];
  for (int i=0,li=0;i<5;li+=++i) {
    dia[i] = E[li+i];
    if (dia[i]<0) return -(i+1)*11;
    for (int j=0;j<=i;j++) {
      double dis = dia[i]*dia[j]-E[li+j]*E[li+j];
      if (dis<0) return -(i+1+10*(j+1));
  } }
  return 0;

}

//______________________________________________________________________________
const double Zero = 1.e-6;
static TComplex sgCX1,sgCX2,sgCD1,sgCD2,sgImTet,sgCOne,sgCf1;
static  int  SqEqu(double *, double *);
#if 0
//_____________________________________________________________________________
static int myEqu(double *s, int na, double *b,int nb)
{
  StMatrixD mtx(na,na);
  double *m = &mtx(1,1);
  TCL::trupck(s,m,na);
  size_t ierr=0;
  mtx.invert(ierr);
  if (ierr) return ierr;
  for (int ib=0;ib<nb;ib++) {
    TCL::vmatl(m,b+ib*na,s,na,na);
    memcpy(b+ib*na,s,na*sizeof(*b));
  }
  TCL::trpck(m,s,na);
  return 0;  
}
//_____________________________________________________________________________
static void Eigen2(const double err[3], double lam[2], double eig[2][2])
{

  double spur = err[0]+err[2];
  double det  = err[0]*err[2]-err[1]*err[1];
  double dis  = spur*spur-4*det;
  if (dis<0) dis = 0;
  dis = sqrt(dis);
  lam[0] = 0.5*(spur+dis);
  lam[1] = 0.5*(spur-dis);
  if (!eig) return;
  eig[0][0] = 1; eig[0][1]=0;
  if (dis>1e-6*spur) {// eigenvalues are different
    if (fabs(err[0]-lam[0])>fabs(err[2]-lam[0])) {
     eig[0][1] = 1; eig[0][0]= -err[1]/(err[0]-lam[0]);
    } else {
     eig[0][0] = 1; eig[0][1]= -err[1]/(err[2]-lam[0]);
    }
    double tmp = sqrt(eig[0][0]*eig[0][0]+eig[0][1]*eig[0][1]);
    eig[0][0]/=tmp; eig[0][1]/=tmp;
  }
  eig[1][0]=-eig[0][1];  eig[1][1]= eig[0][0];
}
//_____________________________________________________________________________
static TComplex MyFactor(double rho,double drho,double s)
{
// Integral exp(i*Phi)*dL where Phi = rho*L + 0.5*drho*L**2
// Let it is equal  exp(i*Phi)*A(L) + const
// then dA/dL + i*(rho+drho*L)*A = 1
// Solve this equation for Taylor representation of A(L)
// static int Iter=0;
  TComplex arr[3],add;
  TComplex Sum; //
  Sum = 0.0;
  arr[0] = 1.; arr[1] = 0.;
  drho = drho/rho;
  double ss = s;
  for (int j=2;1;j++) {
    arr[2] = -TComplex(0,1)*rho*(arr[1]+drho*arr[0])/double(j);
    ss *=s; add = ss*arr[2]; Sum += add;
    if (1e-12*Sum.Rho2() > add.Rho2()) break;
//    printf(" Iter=%d %d %g\n",Iter++,j-1,TComplex::Abs(add));
    arr[0]=arr[1]; arr[1]=arr[2]; 
  }
  return Sum;
}
#endif //0

//ClassImp(THelixTrack_)
//_____________________________________________________________________________
THelixTrack_::THelixTrack_(const double *xyz,const double *dir,double rho)
{
//	Made from GEANT3 ghelix by V.Perevoztchikov
//
//    ******************************************************************
//    *                                                                *
//    *  Performs the tracking of one step in a magnetic field         *
//    *  The trajectory is assumed to be a helix in a constant field   *
//    *  taken at the mid point of the step.                           *
//    *  Parameters:                                                   *
//    *   input                                                        *
//    *     STEP =arc length of the step asked                         *
//    *     VECT =input vector (position,direction cos and momentum)   *
//    *     CHARGE=  electric charge of the particle                   *
//    *   output                                                       *
//    *     VOUT = same as VECT after completion of the step           *
//    *                                                                *
//    *    ==>Called by : <USER>, GUSWIM                               *
//    *       Author    M.Hansroul  *********                          *
//    *       Modified  S.Egli, S.V.Levonian                           *
//    *       Modified  V.Perevoztchikov
//    *                                                                *
//    ******************************************************************
//
  fEmx=0;
  Set(xyz,dir,rho);
}
//_____________________________________________________________________________
THelixTrack_ &THelixTrack_::operator=(const THelixTrack_ &from)
{
  memcpy(fBeg,from.fBeg,fEnd-fBeg);
  fEmx=0;
  if (from.fEmx) SetEmx(from.fEmx->Arr());
  return *this;
}
//_____________________________________________________________________________
THelixTrack_::THelixTrack_(const THelixTrack_ &from)
{
  *this = from;
  fEmx=0;
  if (from.fEmx) SetEmx(from.fEmx->Arr());
}
//_____________________________________________________________________________
THelixTrack_::THelixTrack_(const THelixTrack_ *fr)
{
  fEmx=0;
  Set(fr->fX,fr->fP,fr->fRho);
}
//_____________________________________________________________________________
THelixTrack_::~THelixTrack_()
{ delete fEmx;fEmx=0;}
//_____________________________________________________________________________
THelixTrack_::THelixTrack_()
{
  memset(fBeg,0,fEnd-fBeg);
}
//_____________________________________________________________________________
void THelixTrack_::Set(const double *xyz,const double *dir,double rho)
{
  fX[0] = xyz[0]; fX[1] = xyz[1]; fX[2] = xyz[2];
  fP[0] = dir[0]; fP[1] = dir[1]; fP[2] = dir[2];
  fRho = rho; 
  Build();
}
//_____________________________________________________________________________
void THelixTrack_::SetEmx(const double*  err2xy,const double*  err2sz)
{
  if (!fEmx) fEmx = new THEmx_t_;
  fEmx->Set(err2xy,err2sz);
}
//_____________________________________________________________________________
void THelixTrack_::SetEmx(const double*  err)
{
  if (!fEmx) fEmx = new THEmx_t_;
  fEmx->Set(err);
}
//_____________________________________________________________________________
void THelixTrack_::StiEmx(double err[21]) const
{
enum {kXX
     ,kYX,kYY                       
     ,kZX,kZY,kZZ                 
     ,kEX,kEY,kEZ,kEE           
     ,kPX,kPY,kPZ,kPE,kPP     
     ,kTX,kTY,kTZ,kTE,kTP,kTT
     ,kLN
     };
   memset(err,0,sizeof(err[0])*kLN);
   double cosCA = fP[0]/fCosL;
   err[kYY] = fEmx->mHH/(cosCA*cosCA);
   err[kZY] = fEmx->mHZ/(cosCA);
   err[kZZ] = fEmx->mZZ;
   err[kEY] = fEmx->mHA/cosCA;
   err[kEZ] = fEmx->mAZ;
   err[kEE] = fEmx->mAA;
   err[kPY] = fEmx->mHC/cosCA;
   err[kPZ] = fEmx->mCZ;
   err[kPE] = fEmx->mAC;
   err[kPP] = fEmx->mCC;
   err[kTY] = fEmx->mHL/(cosCA*fCosL*fCosL);
   err[kTZ] = fEmx->mZL/(      fCosL*fCosL);
   err[kTE] = fEmx->mAL/(      fCosL*fCosL);
   err[kTP] = fEmx->mCL/(      fCosL*fCosL);
   err[kTT] = fEmx->mLL/(      fCosL*fCosL*fCosL*fCosL);
}
//_____________________________________________________________________________
void THelixTrack_::Backward()
{

  double d[3];
  for (int i=0;i<3;i++) { d[i]=-fP[i];}
  Set(fX,d,-fRho); 
  if(fEmx) fEmx->Backward();
}
//_____________________________________________________________________________
void THelixTrack_::GetSpot(const double axis[3][3],double emx[3]) const
{
/// THelixTrack_::GetSpot(double axis[3][3],emx[3]) const
/// axis[0,1]  - vectors in plane. 
/// axis[2]    - normal vector of plane
/// emx[3] error matrix of coordinates according vectors in plane.

//   transformation matrix from "helix" coordinate to global
   double my[3][3] = {{-fP[1]/fCosL, 0,fP[0]}
                     ,{ fP[0]/fCosL, 0,fP[1]}
                     ,{           0, 1,fP[2]}};

   double T[3][3],tmp[3][3],g[6],t[2][2];
   TCL::mxmpy (axis[0],my[0],T[0],3,3,3);
//   	now account that matrix axis may be non orthogonal
   TCL::traat(axis[0],g,3,3);
   if (fabs(g[0]-1)+fabs(g[1])+fabs(g[2]-1)
      +fabs(g[3])+fabs(g[4])+fabs(g[5]-1)>1e-10) {//non orthogonal case
     TCL::trsinv(g,g,3);
     memcpy(tmp[0],T[0],sizeof(T));
     TCL::trsa  (g,tmp[0],T[0],3,3);
   }
   TCL::vlinco(T[0],1.,T[2],-T[0][2]/T[2][2],t[0],2);
   TCL::vlinco(T[1],1.,T[2],-T[1][2]/T[2][2],t[1],2);
   double myerr[3]={fEmx->mHH,fEmx->mHZ,fEmx->mZZ};
   TCL::trasat(t[0],myerr,emx,2,2);
   return;
}
//_____________________________________________________________________________
void THelixTrack_::Build()
{

  double tmp;
    
  tmp = fP[0]*fP[0]+ fP[1]*fP[1]+ fP[2]*fP[2];
  if (fabs(tmp-1.) > 1.e-12) {
    tmp = ::sqrt(tmp); fP[0] /=tmp; fP[1] /=tmp; fP[2] /=tmp; }
    
  fCosL = ::sqrt(fabs((1.-fP[2])*(1.+fP[2])));
}
//______________________________________________________________________________
void THelixTrack_::MakeMtx(double step,double F[5][5])
{
//  H,A,C,Z,L
  enum {kH=0,kA,kC,kZ,kL};

  double S = step*fCosL;
  memset(F[0],0,sizeof(F[0][0])*5*5);

  F[kH][kH]   = sgCf1.Re()+1.;
  double dSdH = sgCf1.Im();

  F[kH][kA]   = S*sgCOne.Re();
  double dSdA = S*sgCOne.Im();

  TComplex llCOneD = S*S*expOneD(-sgImTet);
  F[kH][kC]   = llCOneD.Re();
  double dSdC = llCOneD.Im();

  F[kA][kH] =  -dSdH*fRho;
  F[kA][kA] = 1-dSdA*fRho;
  F[kA][kC] = S+dSdC*fRho;
  F[kC][kC] = 1;

  double tanL = fP[2]/fCosL;

  F[kZ][kH] = -dSdH*tanL;
  F[kZ][kA] = -dSdA*tanL;
  F[kZ][kC] =  dSdC*tanL;
  F[kZ][kZ] = 1;
  F[kZ][kL] = S/(fCosL*fCosL);
  F[kL][kL] = 1;
}
//_____________________________________________________________________________
double THelixTrack_::Move(double step) 
{
  double F[5][5];
  Eval(step,fX,fP,&fRho);
  if (fEmx && fEmx->mHH>0) {
    MakeMtx(step,F);
    fEmx->Move(F);
  } 
  return step;
}
//_____________________________________________________________________________
double THelixTrack_::Move(double step,double F[5][5]) 
{
  double xyz[3],dir[3],rho;
  Eval(step,xyz,dir,&rho);
  Set(xyz,dir,rho);
  MakeMtx(step,F);
  if (fEmx && fEmx->mHH>0) fEmx->Move(F); 
  return step;
}

//_____________________________________________________________________________
double THelixTrack_::Path(double stmax,const  double *surf, int nsurf,
                         double *xyz, double *dir, int nearest) const
{
  int i;
  double s[10]={0,0,0,0,0,0,0,0,0,0},tmp=0;
  memcpy(s,surf,nsurf*sizeof(surf[0]));
  
  for(i=1;i<nsurf;i++) if (fabs(s[i])>tmp) tmp = fabs(s[i]);
  if(fabs(tmp-1.)>0.1) {for(i=0;i<nsurf;i++) s[i]/=tmp;}
  double stmin = (nearest)? -stmax:0;
//  if (!s[3] && !s[6] && !s[8] && !s[9] && fabs(s[4]-s[5])<1.e-12) 
//         return PathHZ(s,nsurf,xyz,dir,nearest);
//   else  return Path(stmin,stmax,s,nsurf,xyz,dir,nearest);
  return Path(stmin,stmax,s,nsurf,xyz,dir,nearest);
}


//_____________________________________________________________________________
double THelixTrack_::Path(double stmin,double stmax, const double *s, int nsurf,
                         double *xyz, double *dir, int nearest) const
{
  int ix,jx,nx,ip,jp;
  double poly[4][3],tri[3],sol[2],cos1t,f1,f2,step,ss;
  const double *sp[4][4] = {{s+0,s+1,s+2,s+3}, {s+1,s+4,s+7,s+9}, 
                            {s+2,s+7,s+5,s+8}, {s+3,s+9,s+8,s+6}}; 

  double myMax = 1./(fabs(fRho*fCosL)+1.e-10);
  THelixTrack_ th(fX,fP,fRho);
  cos1t = 0.5*fRho*fCosL;
  double totStep=0;
  while (2005) {
    double hXp[3]={-th.fP[1],th.fP[0],0};
    poly[0][0]=1.;poly[0][1]=0.;poly[0][2]=0.;
    tri[0]=tri[1]=tri[2]=0;
    for(ix=1;ix<4;ix++) {
      poly[ix][0] =th.fX  [ix-1]; 
      poly[ix][1] =th.fP  [ix-1]; 
      poly[ix][2] =hXp[ix-1]*cos1t;
    }

    nx = (nsurf<=4) ? 1:4;
    for(ix=0;ix<nx;ix++) {
      for(jx=ix;jx<4;jx++) {  
	ss = *sp[ix][jx]; if(!ss) 	continue;
	for (ip=0;ip<3;ip++) {
          f1 = poly[ix][ip]; if(!f1) 	continue;
          f1 *=ss;
          for (jp=0;jp+ip<3;jp++) {
            f2 = poly[jx][jp]; if(!f2) 	continue;
            tri[ip+jp] += f1*f2;
    } } } }

    int nsol = SqEqu(tri,sol);
    step = 1.e+12;
    if (nsol<0) 	return step;

    if (nearest && nsol>1) {
      if(fabs(sol[0])>fabs(sol[1])) sol[0]=sol[1];
      nsol = 1;
    }
    if (nsol) step = sol[0];
    if (step < stmin && nsol > 1) step = sol[1];
    if (step < stmin || step > stmax) 	{
      nsol = 0; 
      if (step>0) {step = stmax; stmin+=myMax/2;}
      else        {step = stmin; stmax-=myMax/2;}}

    if (!nsol && fabs(step) < 0.1*myMax) return 1.e+12;
    if (fabs(step)>myMax) {step = (step<0)? -myMax:myMax; nsol=0;}

    double x[3],d[3];
    th.Eval(step,x,d);
    if (nsol) {//test it
      ss = s[0]+s[1]*x[0]+s[2]*x[1]+s[3]*x[2];
      if (nsurf > 4) ss += s[4]*x[0]*x[0]+s[5]*x[1]*x[1]+s[6]*x[2]*x[2];
      if (nsurf > 7) ss += s[7]*x[0]*x[1]+s[8]*x[1]*x[2]+s[9]*x[2]*x[0];
      if (fabs(ss)<1.e-7) {
	if (xyz) memcpy(xyz,x,sizeof(*xyz)*3);
	if (dir) memcpy(dir,d,sizeof(*dir)*3);
	return totStep+step;
    } }

    stmax -=step; stmin -=step;
    if (stmin>=stmax) return 1.e+12;
    totStep+=step;
    th.Move(step);
  }

}

//_____________________________________________________________________________
double THelixTrack_::PathHZ(const double *su, int nsurf, 
                           double *xyz, double *dir,int nearest) const
{
   double tri[3] = {0,0,0};
   double f0,fc,fs,R,tet,tet0,tet1,tet2,costet,su45=0,fcs;
   

   R = 1./fRho/fCosL;
//		X
   f0 = fX[0] - fP[1]*R;
   fc = fP[1]*R;
   fs = fP[0]*R;

   tri[0] = su[0] + su[1]*f0;
   tri[1] = su[1]*fc;
   tri[2] = su[1]*fs;
   if (nsurf >4) {
     su45 = 0.5*(su[4]+su[5]);
     fcs  = fc*fc + fs*fs;
     tri[0] += su45*f0*f0 + su45*fcs; 
     tri[1] += su45*2*f0*fc;
     tri[2] += su45*2*f0*fs;
   }
//		Y
   f0 =  fX[1] + fP[0]*R;
   fc = -fP[0]*R;
   fs =  fP[1]*R;

   tri[0] += su[2]*f0;
   tri[1] += su[2]*fc;
   tri[2] += su[2]*fs;

   if (nsurf >4) {
     tri[1] += su45*2*f0*fc;
     tri[2] += su45*2*f0*fs;
   }
   costet = -tri[0]/::sqrt(tri[1]*tri[1]+tri[2]*tri[2]);
   if(fabs(costet)>1.) return 1.e+12;
   tet0 = atan2(tri[2],tri[1]);
   tet  = acos(costet);
   tet1 =  tet + tet0;
   tet2 = -tet + tet0;

   if (tet1 > 2*M_PI) tet1 -= 2*M_PI;
   if (tet2 > 2*M_PI) tet2 -= 2*M_PI;
   if (nearest) { 	//Select the neares solution
     if (fabs(tet1)>fabs(tet1-2*M_PI)) tet1 -=2*M_PI;
     if (fabs(tet1)>fabs(tet1+2*M_PI)) tet1 +=2*M_PI;
     if (fabs(tet2)>fabs(tet2-2*M_PI)) tet2 -=2*M_PI;
     if (fabs(tet2)>fabs(tet2+2*M_PI)) tet2 +=2*M_PI;
     if (fabs(tet1)>fabs(tet2)       ) tet1  =tet2;
     return Eval(tet1*R,xyz,dir);
   } else {		//forward seqrch 
     double s1 = tet1*R;
     if (s1<=0) s1 += 2*M_PI*fabs(R);
     double s2 = tet2*R;
     if (s2<=0) s2 += 2*M_PI*fabs(R);
     if (s1>s2) s1=s2;
     return Eval(s1,xyz,dir);
   }

}
//_____________________________________________________________________________
double THelixTrack_::Path(const THelixTrack_ &th,double *path2) const
{
static const double kMinAng = 0.1,kDeltaL=1e-4;

   THelixTrack_ thMe(this),thHe(&th);
   double Rho1=thMe.GetRho()*thMe.GetCos();
   double Rho2=thHe.GetRho()*thHe.GetCos();

   for (int ix=0;ix<3;ix++) 
   {if (fabs(thMe.Pos()[ix]-thHe.Pos()[ix])>100)return 3e33;}
   double sMe=0,sHe=0;
   int conv = 0;
   for (int iter=0;iter<20; iter++) {
     TVector3 P1(thMe.Pos());
     TVector3 P2(thHe.Pos());
     TVector3 D1(thMe.Dir());
     TVector3 D2(thHe.Dir());
     TVector3 dP =  P1-P2;

//double eps1 = dP.Dot(D1);
//double eps2 = dP.Dot(D2);
//printf("%d - eps1 = %g eps2 = %g\n",iter,eps1,eps2);
     TVector3 Dm = D1-D2;
     TVector3 Dp = D1+D2;
     double dPDm = dP.Dot(Dm);
     double dPDp = dP.Dot(Dp);
     double DDm = Dm.Mag2();
     double DDp = Dp.Mag2();
     if (DDm<1e-10) return 3e33;
     double t1 = -(dPDm)/DDm;
     double t2 = -(dPDp)/DDp;
     double F = 1;
     double s1 = t1+t2;
     double s2 = t1-t2;
     if (fabs(s1*Rho1*F) > kMinAng) F = kMinAng/fabs(s1*Rho1);
     if (fabs(s2*Rho2*F) > kMinAng) F = kMinAng/fabs(s2*Rho2);
     if (F<1) {s1*=F; s2*=F;}
     thMe.Move(s1); sMe+=s1;
     thHe.Move(s2); sHe+=s2;
     if (F<1) continue;
     if (fabs(s1)>kDeltaL) continue;
     if (fabs(s2)>kDeltaL) continue;
     conv = 1; break;
   }
   if (!conv)  return 3e33;
   if(path2) *path2=sHe;
   return sMe;
}
//_____________________________________________________________________________
double THelixTrack_::PathX(const THelixTrack_ &th,double *s2, double *dst, double *xyz) const
{
  double ss1,ss2,dd,ss1Best,ss2Best,ddBest=1e33;
  double xx[9];
  int jkBest=-1;
  for (int jk=0;jk<4;jk++) {
    THelixTrack_ th1(this),th2(&th);
    if (jk&1) th1.Backward();
    if (jk&2) th2.Backward();
    ss1 = th1.Path(th2,&ss2);
    if (ss1>=1e33) continue;
    if (ss2>=1e33) continue;
    th1.Eval(ss1,xx+0);
    th2.Eval(ss2,xx+3);
    TCL::vsub(xx,xx+3,xx+6,3);
    dd = TCL::vdot(xx+6,xx+6,3);
    if (dd > ddBest) continue;
    ddBest = dd; jkBest=jk; ss1Best = ss1; ss2Best = ss2;
    if (xyz) TCL::ucopy(xx,xyz,6);
  }
  if (jkBest<0) { if(s2) *s2=3e33; return 3e33; }
  if (jkBest&1) ss1Best = -ss1Best;
  if (jkBest&2) ss2Best = -ss2Best;
  if (s2 ) *s2  = ss2Best;
  if (dst) *dst = ddBest;
  return ss1Best;
}
//_____________________________________________________________________________
double THelixTrack_::Path(double x,double y) const
{
   TCircle_ ht(fX,fP,fRho);
   double ar[2]={x,y};
   return ht.Path(ar)/fCosL;
}
//_____________________________________________________________________________
double THelixTrack_::Path(const double *point,double *xyz, double *dir) const
{

    static int nCount=0; nCount++;
    TComplex cpnt(point[0]-fX[0],point[1]-fX[1]);
    TComplex cdir(fP[0],fP[1]); cdir /=TComplex::Abs(cdir);
    double step[3]={0,0,0};
//		Z estimated step 

    int zStep=0;
    if (fabs(fP[2]) > 0.01){ //Z approximation
      zStep = 1;
      step[1] = (point[2]-fX[2])/fP[2];
    }
//angle approximation
//		R estimated step
    {
      cpnt /= cdir;
      if (fabs(cpnt.Re()*fRho) < 0.01) {
        step[2]=cpnt.Re();
      } else {
        double rho = fRho;
        for (int i=0;i<2;i++) {
          TComplex ctst = (1.+TComplex(0,1)*rho*cpnt);
	  ctst /=TComplex::Abs(ctst);
	  ctst = TComplex::Log(ctst);
	  step[2]= ctst.Im()/rho;
        }
      }
      step[2]/=fCosL;
    }

    if (zStep) {
      double p = GetPeriod();
      int nperd = (int)((step[1]-step[2])/p);
      if (step[2]+nperd*p>step[1]) nperd--;
      if (fabs(step[2]-step[1]+(nperd+0)*p)
         >fabs(step[2]-step[1]+(nperd+1)*p)) nperd++;
      step[2]+=(nperd)*p;
    }
    step[0] = step[2];

    double ds = step[1]-step[2];
    if (zStep && fabs(ds)>1.e-5) {
      double dz = ds*fP[2];
      step[0] += dz*dz/ds;
    }


    double xnear[6],ss=0;  double* pnear=xnear+3;
//		iterations
    double dstep = 1.e+10,oldStep=dstep,dztep;
    double lMax = step[0]+0.25*GetPeriod();
    double lMin = step[0]-0.25*GetPeriod();

    if (zStep) {
      lMax = (step[1]>step[2])? step[1]:step[2];
      lMin = (step[1]>step[2])? step[2]:step[1];}
    int iter=99,icut=1;
    THelixTrack_ local(this);
    local.Move(step[0]);
    lMax-=step[0];lMin-=step[0];
    local.Eval(0.,xnear,pnear);
    for (; iter; iter--)
    { 
      double diff = (icut)? lMax-lMin: fabs(dstep);
      if (diff < 0.1) {
        if (diff < 1.e-6) 			break;
        double tmpxy = fabs(point[0]-xnear[0])+fabs(point[1]-xnear[1]);
        double tmpz  = fabs(point[2]-xnear[2]);
        if (fabs(fCosL)   *diff <tmpxy*1e-6
	  &&fabs(pnear[2])*diff <tmpz *1e-6) 	break;
        if (tmpxy+tmpz < 1.e-6) 		break;
      }
      
      local.Eval(ss,xnear,pnear);
      dstep = 0; icut = 0;
      for (int i=0;i<3;i++) {dstep += pnear[i]*(point[i]-xnear[i]);}
      if(dstep<0) {
        lMax = ss; dztep = -0.5*(lMax-lMin);
	if (dstep<dztep || fabs(dstep)>0.7*oldStep) {icut=1;dstep = dztep;}
      } else {
        lMin = ss; dztep =  0.5*(lMax-lMin);
	if (dstep>dztep || fabs(dstep)>0.7*oldStep) {icut=1;dstep = dztep;}
      }
      ss += dstep; 
      oldStep=fabs(dstep);
    }
//    printf("ITERS=%d dSTEP=%g \n",iter,dstep);
    if (!iter){ printf("*** Problem in THElixTrack::Path(vtx) ***\n");
                printf("double vtx[3]={%g,%g,%g};",point[0],point[1],point[2]);
                Print();}
    assert(iter);
    step[0]+=ss;
    return (xyz) ? Eval(step[0],xyz,dir) : step[0];
}
//_____________________________________________________________________________
double THelixTrack_::Dca(const double *point,double *dcaErr) const
{
   double x[3],T[3][3],emx[3];
   double s = Path(point,x,T[2]);
   for (int i=0;i<3;i++) {T[0][i]=point[i]-x[i];}
   double dca = sqrt(T[0][0]*T[0][0]+T[0][1]*T[0][1]+T[0][2]*T[0][2]);
   if (!dcaErr) return dca;

   for (int i=0;i<3;i++) {T[0][i]/=dca;}
   T[1][0]=T[0][1]*T[2][2]-T[2][1]*T[0][2];
   T[1][1]=T[0][2]*T[2][0]-T[2][2]*T[0][0];
   T[1][2]=T[0][0]*T[2][1]-T[2][0]*T[0][1];
   
   THelixTrack_ th(*this);
   th.Move(s);
   th.GetSpot(T,emx);
   *dcaErr=emx[0];
   return dca;
}
//_____________________________________________________________________________
double THelixTrack_::Dca(double x,double y,double *dcaErr) const
{
  double dir[3]={fP[0],fP[1],0};
  THelixTrack_ hlx(fX,dir,fRho);
  if (fEmx) hlx.SetEmx(fEmx->Arr());
  double vtx[3]={x,y,fX[2]};
  return hlx.Dca(vtx,dcaErr);
}


//_____________________________________________________________________________
double THelixTrack_::Dca(const double point[3]
                       ,double &dcaXY,double &dcaZ,double dcaEmx[3],int kind) const
/// Full 3d dca evaluation
/// point[3] - x,y,z of vertex
/// dcaXY - dca in xy plane
/// dcaZ  - dca in Z direction
/// dcaEmx[3] - err(dcaXY*dcaXY),err(dcaXY*dcaZ),err(dcaZ*dcaZ)
/// kind - 3=3d dca,2=2d dca
/// return distance to dca point
{
   double dif[3];
   double s = 0;
   assert(kind==2 || kind==3);
   if (kind==3) s = Path(point);
   else         s = Path(point[0],point[1]);

   THelixTrack_ th(*this);
   th.Move(s);
   const double *x=th.Pos();
   const double *d=th.Dir();

   for (int i=0;i<3;i++) {dif[i]=x[i]-point[i];}
   double nor = th.GetCos();
   double T[3][3]={{-d[1]/nor, d[0]/nor,    0}
                  ,{        0,        0,    1}
		  ,{ d[0]/nor, d[1]/nor,    0}};

   dcaXY = T[0][0]*dif[0]+T[0][1]*dif[1];
   dcaZ  = dif[2];
   if (!dcaEmx) return s;
   THEmx_t_ *emx =th.Emx();
   dcaEmx[0] = emx->mHH;
   dcaEmx[1] = 0;
//	cos(Lambda) **4 to account that we are in the nearest point
   dcaEmx[2] = emx->mZZ*pow(th.GetCos(),4);
   return s;
}


//_____________________________________________________________________________
double THelixTrack_::GetPeriod() const
{
   double per = (fabs(fRho) > 1.e-10) ? fabs(2.*M_PI/fRho):1.e+10;
   return per/fCosL;
}
//______________________________________________________________________________
void THelixTrack_::Rot(double angle)
{
  Rot(cos(angle),sin(angle));
}
//______________________________________________________________________________
void THelixTrack_::Rot(double cosa,double sina)
{
  TComplex CX(fX[0],fX[1]),CP(fP[0],fP[1]);
  TComplex A (cosa,sina);
  CX *=A; fX[0] = CX.Re(); fX[1]=CX.Im();
  CP *=A; fP[0] = CP.Re(); fP[1]=CP.Im();
}
//_____________________________________________________________________________
void THelixTrack_::Print(Option_t *) const
{
  printf("\n THelixTrack_::this = %p\n",(void*)this);
  printf(" THelixTrack_::fX[3] = { %f , %f ,%f }\n",fX[0],fX[1],fX[2]);
  printf(" THelixTrack_::fP[3] = { %f , %f ,%f }\n",fP[0],fP[1],fP[2]);
  printf(" THelixTrack_::fRho  =   %f \n\n",fRho);

  printf("double xyz[3] = {%g,%g,%g};\n" ,fX[0],fX[1],fX[2]); 
  printf("double dir[3] = {%g,%g,%g};\n" ,fP[0],fP[1],fP[2]); 
  printf("double Rho = %g;\n" ,fRho); 
  printf("THelixTrack_ *ht = new THelixTrack_(xyz,dir,Rho);\n");
  
}
//______________________________________________________________________________
void THelixTrack_::TestMtx() 
{
  enum {kH=0,kA,kC,kZ,kL};
const static char* T="HACZL";
  double Dir[4][3],X[4][3]={{0}},Rho[2],step,F[5][5],Del,Dif,Fi[5][5];
  double maxEps = 0;  
  int nErr=0;
  int iR = 10+ gRandom->Rndm()*100;
  int iAlf=30+ gRandom->Rndm()*100;
  int iLam=10+ gRandom->Rndm()*60;
  step = gRandom->Rndm()*6*iR;
iLam=80; 				//******* Tested for big lambda
    Rho[0] = 1./iR;
    double alf = iAlf/180.*M_PI;
    double lam = iLam/180.*M_PI;
    Dir[0][0] = cos(lam)*cos(iAlf/180.*M_PI);
    Dir[0][1] = cos(lam)*sin(iAlf/180.*M_PI);
    Dir[0][2] = sin(lam);
    THelixTrack_ tc(X[0],Dir[0],Rho[0]);
    tc.Eval(step,X[1],Dir[1]);
    tc.Move(step,F);
    memcpy(Fi[0],F[0],sizeof(F));
    tc.InvertMtx(Fi);
    TMatrixD one = TMatrixD(5,5,F[0])*TMatrixD(5,5,Fi[0]);
//    one.Print();
    
//    printf("TestMtx: Angle=%d Lam=%d \tRad=%d Step=%d \n",iAlf,iLam,iR,int(step));

    for (int iHAR=0;iHAR<5;iHAR++) {
      memcpy(X[2]  ,X[0]  ,sizeof(X[0][0])  *3);
      memcpy(Dir[2],Dir[0],sizeof(Dir[0][0])*3);
      Del = 0;
      Rho[1]=Rho[0];
      switch (iHAR) {
	case kH: { 
	  Del = 0.001*iR;
          X[2][0] += -Dir[0][1]*Del/cos(lam);
          X[2][1] +=  Dir[0][0]*Del/cos(lam);
          break;}
	  
	case kA: {
	  Del = M_PI/180*0.01;
          Dir[2][0] = cos(lam)*cos(alf+Del);
          Dir[2][1] = cos(lam)*sin(alf+Del);
          Dir[2][2] = sin(lam);
          break;}

	case kC: {
          Del = Rho[0]*0.005;
          Rho[1] = Rho[0]+Del;
          break;}
	case kZ: {
          Del = 0.02;
          X[2][2] += Del;
          break;}
	case kL: {
          Del = M_PI/180*0.1;
          Dir[2][0] = cos(lam+Del)*cos(alf);
          Dir[2][1] = cos(lam+Del)*sin(alf);
          Dir[2][2] = sin(lam+Del);
          break;}
        }//end switch
      
        THelixTrack_ tcc(X[2],Dir[2],Rho[1]);
        tcc.Move(step);
	double myStep = tcc.Path(X[1][0],X[1][1]);
        tcc.Eval(myStep,X[3],Dir[3]);

        for (int jHAR=0;jHAR<5; jHAR++) {
          if (jHAR==kC) continue;
          if (jHAR==kL) continue;
	  switch(jHAR) {
	  case kH: {
	    Dif = (X[3][0]-X[1][0])*(-Dir[1][1])
	        + (X[3][1]-X[1][1])*( Dir[1][0]);
            Dif/=cos(lam);
            break;}
	  case kA: {
	    Dif = atan2(Dir[3][1],Dir[3][0])
	         -atan2(Dir[1][1],Dir[1][0]); 
            if (Dif>  M_PI) Dif-=2*M_PI;
            if (Dif< -M_PI) Dif+=2*M_PI;
            break;}
	  case kZ: {
	    Dif = X[3][2]-X[1][2];
            break;}
          }
          double est = Dif/Del;
	  double eps = fabs(est-F[jHAR][iHAR])*2
	             /(fabs(est)+fabs(F[jHAR][iHAR]+1e-9));
          if (eps>maxEps) maxEps=eps;
          if (eps > 1e-2) nErr++;
          printf(" m%c%c \tDer= %g \tEst= %g \teps= %g\n",
	         T[jHAR],T[iHAR],F[jHAR][iHAR],est,eps);
	
    } }  
    printf("TestMtx: %d errors maxEps=%g\n",nErr,maxEps);

}
//_____________________________________________________________________________
int SqEqu(double *cba, double *sol)
{
//	
//	made from fortran routine GVPSQR (Geant320)
/*
************************************************************************
*                                                                      *
*     SUBROUTINE GVPSQR (CBA,SOL,NSOL)             870924  VP          *
*                                                                      *
*       SOLVE  QUADRATIC  EQUATION                                     *
*                                                                      *
*   ARGUMENTS:                                                         *
*       CBA     Array of coeff's A0 + A1*x + A2*x**2                   *
*       SOL     Solutions                                              *
*       NSOL    Number of solutions :                                  *
*               if zero - SOL[0]= extremum                             *
*               if -ve  - No solution at all                           *
*                                                                      *
************************************************************************
*/
  const double zero2=1.e-12;
  double swap,a,b,c,amx,dis,bdis;
  int nsol;
/*--------------------------------------------------------------------*/

  a = cba[2]; b = cba[1]*0.5; c = cba[0];
  if (b < 0.) {a = -a; b = -b; c = -c;}
  amx = fabs(a); if (amx<b) amx = b; if (amx<fabs(c)) amx = fabs(c);
  if (amx <= 0.) return -1;
  a = a/amx; b = b/amx; c = c/amx;

  dis = b*b - a*c;
  nsol = 1;
  if (fabs(dis) <= zero2)  dis = 0;
  if (dis < 0.) { nsol = 0; dis  = 0.;}

  dis = ::sqrt(dis); bdis = b + dis;
  if (fabs(c) > 1.e+10*bdis)	return -1;
  sol[0] = 0.;
  if (fabs(bdis) <= 0.)      	return nsol;
  sol[0] = (-c/bdis);		
  if (dis <= 0.)            	return nsol;
  if (bdis >= 1.e+10*fabs(a))   return nsol;    
  nsol   = 2; sol[1] = (-bdis/a);
  if (sol[0] > sol[1]) { swap = sol[0]; sol[0] = sol[1]; sol[1] = swap;}
  return nsol;
}
//_____________________________________________________________________________
double THelixTrack_::Eval(double step, double *xyz, double *dir,double *rho) const
{
  if (rho) *rho = fRho;
  if (!step) {
    if (xyz) memcpy(xyz,fX,sizeof(fX));
    if (dir) memcpy(dir,fP,sizeof(fP));
    return 0.;
  }

  double ztep = step*fCosL;
  double teta = ztep*fRho;

  sgCX1   = TComplex(fX[0]  ,fX[1]);
  sgCD1   = TComplex(fP[0],fP[1])/fCosL;
  sgImTet = TComplex(0,teta);
  sgCOne  = expOne(sgImTet);			//(exp(I*Rho*L)-1)/(I*Rho*L)
  sgCf1   = sgImTet*sgCOne;
  sgCD2   = sgCD1*sgCf1+sgCD1; 			// exp(I*Fi0+I*Rho*L)
  sgCX2   = sgCD1*sgCOne*ztep;			// exp(I*Fi0)*(exp(I*Rho*L)-1)/(I*Rho)

  if (xyz) {
    xyz[0] = sgCX2.Re()+sgCX1.Re();
    xyz[1] = sgCX2.Im()+sgCX1.Im();
    xyz[2] = fX[2]+fP[2]*step;
  }
  if (dir) {    
    sgCD2/= TComplex::Abs(sgCD2);
    dir[0] = sgCD2.Re()*fCosL;
    dir[1] = sgCD2.Im()*fCosL;
    dir[2] = fP[2];
  }
  return step;
}
//_____________________________________________________________________________
void THelixTrack_::Fill(TCircle_ &circ) const
{
  circ.fX[0]=fX[0];
  circ.fX[1]=fX[1];
  circ.fD[0]=fP[0]/fCosL;
  circ.fD[1]=fP[1]/fCosL;
  circ.fRho=fRho;
  if (fEmx) circ.SetEmx(fEmx->Arr());
}
//_____________________________________________________________________________
void THelixTrack_::InvertMtx(double F[5][5])
{
static const int minus[][2] = {{0,1},{1,0},{1,2},{3,0},{3,2},{3,4},{-1,0}};
  for (int i=0;minus[i][0]>=0;i++) {
    F[minus[i][0]][minus[i][1]] = -F[minus[i][0]][minus[i][1]];
  }
}


//_____________________________________________________________________________
//______________________________________________________________________________
void THelixTrack_::TestErr()
{
#if 0
enum {kH,kPhi,kCurv,kZ,kLam};
  double PtGev = 1.,Curv = 1./100;
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
const char * tit[]={"H","Phi","Curv","Z","Lam",0};
    delete hh[ih];
    hh[ih] = new TH1F(tit[ih],tit[ih],100,0,0);
    myCanvas[1]->cd(ih+1); hh[ih]->Draw();
  }
//		Now test of TRandomVector
static TH1F * hrt[5]={0};
  for (int ih=0;ih<myDiv[1];ih++) {
const char * tit[]={"H0","Phi0","Curv0","Z0","Lam0",0};
    delete hrt[ih];
    hrt[ih] = new TH1F(tit[ih],tit[ih],100,0,0);
    myCanvas[2]->cd(ih+1); hrt[ih]->Draw();
  }
//		Now test of Corr
static TH1F * hcr[10]={0};
{
static const char *tit[]={"HP","HC","PC","HZ","PZ","CZ","HL","PL","CL","ZL"};
  for (int ih=0;ih<10;ih++) {
    delete hcr[ih];
    hcr[ih] = new TH1F(tit[ih],tit[ih],100,0,0);
    if (ih<5) myCanvas[3]->cd(ih+1  ); 
    else      myCanvas[4]->cd(ih+1-5);
    hcr[ih]->Draw();
  }}
  
    
  TVector3 PbegV(PtGev,0,PtGev*3);
  PbegV.SetMag(1.);
  PbegV.RotateZ(gRandom->Rndm()*3.14); PbegV.GetXYZ(Pbeg);
  TVector3 XbegV(Xbeg);

  double L = 300.;

  TVectorD dia(5);
  dia[0]= 0.1; dia[1]= 3./360; dia[2]= Curv*0.1; dia[3]= 0.2; dia[4]= 6./360;
  for (int i=0;i<5;i++) {dia[i]*=dia[i];}
  
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
  
  THelixTrack_ TH0(Xbeg,Pbeg,Curv);

  const auto *d = TH0.Dir();
  double cosL = TH0.GetCos();
  double tkDir[3][3] = {{-d[1]/cosL,d[0]/cosL,0},{0,0,1},{d[0],d[1],d[2]}};

  TH0.SetEmx(Gbeg);
  TH0.Move(L);
  TH0.Eval(0,Xend,Pend);

  d = TH0.Dir();
  double tkDirE[3][3] = {{-d[1]/cosL,d[0]/cosL,0},{0,0,1},{d[0],d[1],d[2]}};

  auto *myEmx = TH0.Emx();
  const double *Gend = myEmx->Arr();
  double GendD[5];
  for (int i=0,li=0;i< 5;li+=++i) {
    GendD[i] = sqrt(Gend[li+i]);
  } 
  double GendI[15];
  TCL::trsinv(Gend,GendI,5);  

  TVector3 PendV(Pend),XendV(Xend);  
  double myG[15]={0};
  int nIter = 100000;  
  for (int iter=0;iter<nIter;iter++) {
    TVectorD delta  = RV.Gaus();
    double chi2;
    TCL::trasat(delta.GetMatrixArray(),GbegI,&chi2,1,5);
    assert(chi2>0);
    hXi2[0]->Fill(chi2);

    for (int ih=0;ih<myDiv[1];ih++) { hrt[ih]->Fill(delta[ih]/GbegD[ih]);}
    TVector3 P1begV = PbegV;
    TVector3 X1begV = XbegV;

//     X1begV     += TVector3(tkDir[0])*delta[kH];
    X1begV[0]  += -Pbeg[1]/cosL*delta[kH];
    X1begV[1]  +=  Pbeg[0]/cosL*delta[kH];
    X1begV[2]  +=  delta[kZ];
    double Curv1 = Curv + delta[kCurv];
    double Phi =  P1begV.Phi();    Phi+= delta[kPhi];  P1begV.SetPhi  (Phi);
    double The =  P1begV.Theta();  The-= delta[kLam];  P1begV.SetTheta(The);
    X1begV.GetXYZ(X1beg);
    P1begV.GetXYZ(P1beg);
    THelixTrack_ TH1(X1beg,P1beg,Curv1);
    double s = TH1.Path(Xend[0],Xend[1]);
    TH1.Move(s);
    TH1.Eval(0,X1end,P1end);

    TVector3 X1endV(X1end),P1endV(P1end);
    TVectorD UendV(5);
    TVector3 difX =  X1endV-XendV;

    UendV[kH]    = difX.Dot(TVector3(tkDirE[0]));
    UendV[kZ]    = difX[2];
    UendV[kCurv] = (Curv1-Curv);
    UendV[kLam] = -(P1endV.Theta()-PendV.Theta());
    UendV[kPhi] =  (P1endV.Phi()  -PendV.Phi()  );
    for (int i: {kPhi,kLam}) {    
      if (UendV[i]<-M_PI) UendV[i]+= M_PI*2;
      if (UendV[i]> M_PI) UendV[i]-= M_PI*2;
      assert(fabs(UendV[i])<1);
    }

    for (int ih=0;ih<5;ih++) { hh[ih]->Fill(UendV[ih]/GendD[ih]);}
    TCL::trasat(UendV.GetMatrixArray(),GendI,&chi2,1,5);
    hXi2[1]->Fill(chi2);

  for (int i=0,li=0;i<5;li+=++i) {
    for (int j=0;j<=i;j++) {
      myG[li+j]+=UendV[i]*UendV[j];
  } }

  int jk=0;
  for (int i=0,li=0;i<5;li+=++i) {
    for (int j=0;j<i;j++) {
      double d = UendV[i]*UendV[j] - Gend[li+j];
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
#endif
}

//______________________________________________________________________________
void THelixTrack_::Show(double len, const THelixTrack_ *other) const
{
static TCanvas *myCanvas = 0;
  int kolor[2]={kRed,kBlue};

  TGraph  *ciGraph[2][2] = {{0}};
  TGraph  *ptGraph[2][2] = {{0}};
  TGraph  *szGraph[2]    = {0};
  
  double  x[100],y[100],z[100],l[100],xyz[3];
  double  X[4],Y[4],Z[4],L[4];
  const THelixTrack_ *th[]={this,other};
  int nH = (other)? 2:1;
  for (int ih=0;ih<nH;ih++) {
    double rho = fabs(th[ih]->GetRho());
    double step = 0.01*(1./(rho+1e-10));
   
    if (step>fabs(len)*0.10) step=fabs(len)*0.1;
    if (step<fabs(len)*0.01) step=fabs(len)*0.01;


    int nPts = (int)(fabs(len)/step);
    step = fabs(len)/nPts;
    if (len<0) {len = -len; step = -step;}
    for (int ipt=0; ipt<nPts; ipt++) {
      double s = ipt*step;
      th[ih]->Eval(s,xyz); 
      l[ipt]=s; x[ipt]=xyz[0]; y[ipt]=xyz[1], z[ipt]=xyz[2];
    }
    ciGraph[ih][0]  = new TGraph(nPts  , x, y);
    ciGraph[ih][1]  = new TGraph(nPts  , l, z);
    ciGraph[ih][0]->SetLineColor(kolor[ih]);
    ciGraph[ih][1]->SetLineColor(kolor[ih]);
    ptGraph[ih][0]  = new TGraph(   1  , x, y);
    ptGraph[ih][1]  = new TGraph(   1  , l, z);
    ptGraph[ih][0]->SetMarkerColor(kolor[ih]);
    ptGraph[ih][1]->SetMarkerColor(kolor[ih]);

    X[ih*2+0]=x[0]; X[ih*2+1]=x[nPts-1];
    Y[ih*2+0]=y[0]; Y[ih*2+1]=y[nPts-1];
    Z[ih*2+0]=z[0]; Z[ih*2+1]=z[nPts-1];
    L[ih*2+0]=l[0]; L[ih*2+1]=l[nPts-1];
  }

  szGraph[0]  = new TGraph(nH*2  , X, Y);
  szGraph[1]  = new TGraph(nH*2  , L, Z);
// 
  myCanvas = new TCanvas("THelixTrack_Show","",600,800);
  myCanvas->Divide(1,2);
  for (int ipad=0;ipad<2;ipad++) {
    myCanvas->cd(ipad+1); 
    szGraph[ipad]->Draw("AP");
    for (int ih = 0;ih<nH;ih++) {
      ptGraph[ih][ipad]->Draw("same *");
      ciGraph[ih][ipad]->Draw("same CP");
  } }


  myCanvas->Modified();
  myCanvas->Update();
  while(!gSystem->ProcessEvents()){gSystem->Sleep(200);}; 

}
//_____________________________________________________________________________
void THelixTrack_::Test1()
{
double surf[4] = {-11.32212856152224, 0.50109792630239824, -0.86539108263698283, 0.00078459561521909921};
double xyz[3] = {-0.0206564,-0.0153429,0.0285582};
double dir[3] = {0.450295,-0.596426,-0.664463};
double Rho = 0.00678696;
THelixTrack_ TH(xyz,dir,Rho);

double s = TH.Path(100000,surf,4);
printf("s=%g = 15.3589\n",s);
}
//_____________________________________________________________________________
void THelixTrack_::Test2()
{
double diff[3];

double xyz[3] = {-60.0301,1.51445,-1.57283};
double dir[3] = {-0.849461,0.526419,0.0360391};
double Rho = 0.00363571;
THelixTrack_ ht(xyz,dir,Rho);

double MyHit[3]= { -177.673, 41.305, 2.90798};
double MyClo[3];

printf("%s= %g %g %g\n","MyHit",MyHit[0],MyHit[1],MyHit[2]);
double s = ht.Path(MyHit,MyClo);
ht.Eval(s,MyClo);
TCL::vsub(MyClo,MyHit,diff,3);
double MyDist = sqrt(TCL::vdot(diff,diff,3));
printf("%s= %g %g %g\n","MyClo ",MyClo[0],MyClo[1],MyClo[2]);
printf("MustBe= -177.661 41.4145 2.94559\n");

printf("%s= %g %g %g\n","MyDif ",diff[0],diff[1],diff[2]);
printf("MustBe= 0.0122709 0.109539 0.0376077\n");
printf("%s=%g\n","MyS   ",s);
printf("MustBe=125.375\n");
printf("%s= %g\n","MyDist",MyDist);
printf("MustBe= 0.116463\n");
}
//_____________________________________________________________________________
void THelixTrack_::Test3()
{
double xyz[3] = {100,200,300};
double dir[3] = {-0.224845,-0.491295,-0.841471};
double Rho = 0.02;
double sur[8]={-120,1,0,0,0,0,0};
THelixTrack_ *ht = new THelixTrack_(xyz,dir,Rho);
double newX[3],newD[3];
ht->Backward();
double s = ht->Path(1000.,sur,4,newX,newD);
printf("Result: s=%g newX=(%g %g %g) newD=(%g %g %g)\n"
      ,s,newX[0],newX[1],newX[2],newD[0],newD[1],newD[2]);
      
printf("MustBe: s=56.1931 newX=(120 222.222 347.285) newD=(0.464979 0.275174 0.841471)\n\n");

sur[6]=1e-6;
       s = ht->Path(1000.,sur,7,newX,newD);
printf("Result: s=%g newX=(%g %g %g) newD=(%g %g %g)\n"
      ,s,newX[0],newX[1],newX[2],newD[0],newD[1],newD[2]);
printf("MustBe: s=55.9338 newX=(119.88 222.151 347.067) newD=(0.464206 0.276476 0.841471)\n\n");
}
//_____________________________________________________________________________
void THelixTrack_::Test4()
{
double surf[7] = {-100, 0, 0, 0, 1,1,0};
double xyz[3] = {-0.0206564,-0.0153429,0.0285582};
double dir[3] = {0.450295,-0.596426,-0.664463};
double Rho = 0.00678696;
double xnew[3];
THelixTrack_ TH(xyz,dir,Rho);

double s = TH.Path(100000,surf,7,xnew);
double dif = xnew[0]*xnew[0]+xnew[1]*xnew[1]-100;
printf("s=%g dif=%g\n",s,dif);

}
//_____________________________________________________________________________
void THelixTrack_::Test5()
{
  double pars[4][2][7] = {
   {{0,0,0, 1,1,1, -0.001},{0,0, 0, -1,1,-1,0.002}},
   {{0,0,1, 1,1,1, -0.001},{0,0,-1, -1,1,-1,0.002}},
   {{0,0,1, 1,1,1, -0.001},{0,0,-1,  1,1,-1,0.002}},
  };
  for (int ip=0;ip<3;ip++) {
    THelixTrack_ th1(pars[ip][0],pars[ip][0]+3,pars[ip][0][6]); 
    THelixTrack_ th2(pars[ip][1],pars[ip][1]+3,pars[ip][1][8]); 
    th1.Move(-50);
    th2.Move(-50);
    double s2;
    double s1 = th1.Path(th2,&s2);
    th1.Move(s1);
    th2.Move(s2);
    double diff[3];
    TCL::vsub(th1.Pos(),th2.Pos(),diff,3);
    double dist = sqrt(TCL::vdot(diff,diff,3));
    printf("s1=%g s2=%g dist=%g\n",s1,s2,dist);

  }  
}
//______________________________________________________________________________
void THelixTrack_::TestDer() 
{
  enum {kH=0,kA,kC,kZ,kL};
  const char* Enum[]={"H  ","Phi","Rho","Z  ","Lam"};
  double rho,rho1,step,F[5][5],F1[5][5];
  int iR = 100+ gRandom->Rndm()*100;
  int iAlf=30+ gRandom->Rndm()*100;
  int iLam=10+ gRandom->Rndm()*60;
  rho = 1./iR;
  double alf = iAlf/180.*M_PI,alf1 = alf;
  double lam = iLam/180.*M_PI,lam1 = lam;
  step = 100;

lam=0;///???
step = iR*M_PI * 1.0 / cos(lam);


  printf("TestDer: Angle=%d Lam=%d \tRad=%d Step=%d \n",iAlf,iLam,iR,int(step));

  TVector3 Xbeg(0.,0.,0.),Xend;
  TVector3 Dbeg(cos(lam)*cos(alf),cos(lam)*sin(alf),sin(lam)),Dend;
  TVector3 Hbeg(-sin(alf),cos(alf),0);

  TVector3 Xbeg1,Xend1;
  TVector3 Dbeg1,Dend1;

  THelixTrack_ hlx((double*)&Xbeg[0],(double*)&Dbeg[0],rho);
  THelixTrack_ hlx1(hlx);
  hlx1.Move(step,F);
  hlx1.Eval(0,(double*)&Xend[0],(double*)&Dend[0]);
  TVector3 Hend(-Dend[1],Dend[0],0); Hend.SetMag(1.);
  double Aend = atan2(Dend[1],Dend[0]);
  double Lend = asin( Dend[2]);

  for (int JK=0;JK<5;JK++) {
//    double delta = 1e-5;
    double delta = 1e-8;
    Xbeg1 = Xbeg; Dbeg1 = Dbeg;
    alf1  = alf;  lam1  = lam;
    rho1 = rho;
    switch (JK) {
      case kH:
      Xbeg1 += Hbeg*delta;
      break;

      case kA:
      alf1 +=delta ;
      break;
      case kC:
      rho1 += delta;
      break;

      case kZ:
      Xbeg1[2]+=delta;
      break;

      case kL:
      lam1+=delta;
      break;
    }
    Dbeg1 = TVector3(cos(lam1)*cos(alf1),cos(lam1)*sin(alf1),sin(lam1));
    THelixTrack_ hlxM((double*)&Xbeg1[0],(double*)&Dbeg1[0],rho1);
    hlxM.Move(step);
    double dL = hlxM.Path(Xend[0],Xend[1]);
    hlxM.Move(dL); 
    hlxM.Eval(0,(double*)&Xend1[0],(double*)&Dend1[0]);
    double Aend1 = atan2(Dend1[1],Dend1[0]);
    double Lend1 = asin( Dend1[2]);
    F1[kH][JK] = (Xend1-Xend).Dot(Hend);
    F1[kA][JK] = Aend1-Aend; 
    if (F1[kA][JK]<-M_PI*2) F1[kA][JK]+=M_PI*2;
    if (F1[kA][JK]> M_PI*2) F1[kA][JK]-=M_PI*2;
    F1[kC][JK] = rho1-rho;
    F1[kZ][JK] = Xend1[2]-Xend[2];
    F1[kL][JK] = Lend1-Lend;
    if (F1[kL][JK]<-M_PI*2) F1[kL][JK]+=M_PI*2;
    if (F1[kL][JK]> M_PI*2) F1[kL][JK]-=M_PI*2;
    for(int i=0;i<5;i++) {F1[i][JK]/=delta;}
  }
  for (int j=0;j<5;j++) {
  for (int i=0;i<5;i++) {
    printf("[%s][%s] Ana=%g Num=%g Dif = %g\n"
          ,Enum[i],Enum[j]
	  ,F[i][j],F1[i][j],(F1[i][j]-F[i][j]));
  } }

}
//______________________________________________________________________________
void THelixTrack_::TestTwoHlx() 
{
   TVector3 dif(0.1,0.,0.);
   double rnd = gRandom->Rndm();
   dif.RotateX(rnd);
   rnd = gRandom->Rndm();
   dif.RotateY(rnd);
   rnd = gRandom->Rndm();
   dif.RotateZ(rnd);
   TVector3 D1 = dif.Orthogonal();
   rnd = gRandom->Rndm();
   D1.Rotate(rnd,dif);
   TVector3 D2 = dif.Orthogonal();
   rnd = gRandom->Rndm();
   D2.Rotate(rnd,dif);
   double pos[3]={0};
   double &d1 = D1[0];
   double R1=20,R2=100;
   double shift1 = R1*gRandom->Rndm()*0.1; if (shift1>33) shift1=33;
   double shift2 = R2*gRandom->Rndm()*0.1; if (shift2>33) shift2=33;
   THelixTrack_ th1(pos,&d1,1./R1);
   double &p2 = dif[0]; 
   double &d2 = D2[0]; 
   THelixTrack_ th2(&p2,&d2,1./R2);

   {
     TVector3 P1(th1.Pos()),P2(th2.Pos());
//     TVector3 dP = (P1-P2).Unit();
     TVector3 dP = (P1-P2);
     TVector3 D1(th1.Dir());
     TVector3 D2(th2.Dir());
     double eps1 = dP.Dot(D1);
     double eps2 = dP.Dot(D2);
     printf("TestTwoHlx: Eps1 = %g Eps2 = %g\n",eps1,eps2);
   }



   th1.Move(shift1); th2.Move(shift2);
   double s1=0,s2=0;
   s1 = th1.Path(th2,&s2);
   th1.Move(s1);
   th2.Move(s2);

   {
     
     TVector3 P1(th1.Pos()),P2(th2.Pos());
     TVector3 dP = (P1-P2);
     double dist = dP.Mag();
     dP = dP.Unit();
     TVector3 D1(th1.Dir());
     TVector3 D2(th2.Dir());
     double eps1 = dP.Dot(D1);
     double eps2 = dP.Dot(D2);

     printf("TestTwoHlx: Eps1 = %g Eps2 = %g dist = %g\n",eps1,eps2,dist);

     printf("TestTwoHlx: s1=%g(%g),s2 = %g(%g)\n",s1,shift1,s2,shift2);
   }
   
}
//______________________________________________________________________________
void THelixTrack_::TestBak()
{

  double X0[3]={0,0,0},D0[3]={1,2,3},Rho0=1./100,L=100;
  double X1[3],D1[3],Rho1;
  THelixTrack_ th(X0,D0,Rho0);
  TCL::ucopy(th.Dir(),D0,3);

  th.Move(L);
  th.Backward();
  th.Move(L);
  Rho1 = th.GetRho();
  th.Backward();
  TCL::ucopy(th.Pos(),X1,3);
  TCL::ucopy(th.Dir(),D1,3);
  
  printf("Rho1=%g Xbak = 0 = %g %g %g\n",Rho1,X1[0],X1[1],X1[2]);
}    





//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
//ClassImp(TCircle_)
//______________________________________________________________________________
TCircle_::TCircle_()
{
  Set(0,0,0.);
  fEmx = 0;
}
//______________________________________________________________________________
TCircle_::~TCircle_()
{delete fEmx;fEmx=0;}

//______________________________________________________________________________
TCircle_::TCircle_(const double *x,const double *d,double rho)
{
  Set(x,d,rho);
  fEmx = 0;
}
//______________________________________________________________________________
void TCircle_::Set(const double *x,const double *d,double rho)
{
  fX[0]=0; fX[1]=0; fD[0]=0; fD[1]=0;
  if (x) {fX[0]=x[0];fX[1]=x[1];}
  if (d) {
    fD[0]=d[0];fD[1]=d[1];
    double n = sqrt(fD[0]*fD[0]+fD[1]*fD[1]);
    fD[0]/=n; fD[1]/=n;
  }
  fRho = rho;
}
//______________________________________________________________________________
TCircle_::TCircle_(const TCircle_& fr)
{
  fEmx = 0;
  *this = fr;
}
//______________________________________________________________________________
TCircle_::TCircle_(const TCircle_* fr)
{
  fEmx = 0;
  Set(fr->fX,fr->fD,fr->fRho);
}
//______________________________________________________________________________
TCircle_ &TCircle_::operator=(const TCircle_& fr)
{
  Set(fr.fX,fr.fD,fr.fRho);
  if (fr.fEmx) SetEmx(fr.fEmx->Arr());
  return *this;
}

//______________________________________________________________________________
void TCircle_::Clear(const char *)   
{
 if (fEmx) fEmx->Clear();
 memset(fX,0,(char*)(&fEmx)-(char*)fX);
}


//______________________________________________________________________________
void TCircle_::SetEmx(const double *err)   
{ 
  if(!fEmx) fEmx = new TCEmx_t_;
  fEmx->Set(err);
}
//______________________________________________________________________________
void TCircle_::Nor(double *norVec) const  
{
// direction from center of circle

  norVec[0] =  fD[1];    norVec[1] = -fD[0];
  if (fRho>=0) return;
  norVec[0] = -norVec[0];norVec[1] = -norVec[1];
}
//______________________________________________________________________________
void TCircle_::GetCenter(double cent[3]) const  
{
  double R;
  if (fabs(fRho) > 1e-10) { R = 1./fRho              ;}
  else                    { R =( fRho>0) ? 1e10:-1e10;} 
  cent[0] = fX[0]-fD[1]*R;
  cent[1] = fX[1]+fD[0]*R;
}
//______________________________________________________________________________
void TCircle_::Print(const char* txt) const
{
  if (!txt) txt="";
  printf("TCircle_(%s): x,y=%g %g dir=%g %g curv=%g\n",txt,fX[0],fX[1],fD[0],fD[1],fRho);
  if (!fEmx) return;
  printf("Errs: %g\n"          ,fEmx->mHH); 
  printf("    : %g \t%g\n"     ,fEmx->mHA,fEmx->mAA); 
  printf("    : %g \t%g \t%g\n",fEmx->mHC,fEmx->mAC,fEmx->mCC); 
}
//______________________________________________________________________________
double TCircle_::Path(const double pnt[2]) const
{
  TComplex CX1(pnt[0]-fX[0],pnt[1]-fX[1]);
  TComplex CP(fD[0],fD[1]);
  TComplex CXP = TComplex(0,1)*CX1/CP;
  TComplex CXPRho = CXP*fRho;
  double s;
  if (TComplex::Abs(CXPRho)>0.001) {
    s = TComplex::Log(1.+CXPRho).Im()/fRho;
  } else {
    s = (CXP*(1.-CXPRho*(0.5-CXPRho*(1/3.-CXPRho*0.25)))).Im();
  }
//   Check
//   double x[2],d[2];
//   Eval(s,x,d) ;
//   assert(fabs((pnt[0]-x[0])*d[0]+(pnt[1]-x[1])*d[1])<1e-6);
   return s;
}
//______________________________________________________________________________
double TCircle_::Path(const double *pnt,const double *exy) const
{
  double x[2],d[2];
  double s = Path(pnt);
  if (!exy || exy[0]<=0) return s;
  Eval(s,x,d);
  double k = (x[0]-pnt[0])*(d[1]) + (x[1]-pnt[1])*(-d[0]);
  double t =((d[1]*d[1]-d[0]*d[0])*exy[1]-d[1]*d[0]*(exy[2]-exy[0]))
           /( d[0]*d[0]*exy[2] -2*d[1]*d[0]*exy[1]+d[1]*d[1]*exy[0]);

  return s+k*t;
}
//_____________________________________________________________________________
double TCircle_::Path(const TCircle_ &hlx,double *S2) const
{
  double s,s1=3e33,s2=3e33;
  const static TComplex Im(0.,1.);
  const TCircle_ *one = this;
  const TCircle_ *two = &hlx;
  if (fabs(fRho) > fabs(hlx.fRho)) { one=two; two=this; }
  double Rho1 = one->Rho();
  double Rho2 = two->Rho();
  int kase = 0;
  if (fabs(Rho1) > 1e-4) kase+=1;
  if (fabs(Rho2) > 1e-4) kase+=2;
  
  int nSol = 0;
  TComplex CX1(one->Pos()[0],one->Pos()[1]);
  TComplex CX2(two->Pos()[0],two->Pos()[1]);
  TComplex CP1(one->Dir()[0],one->Dir()[1]);
  TComplex CP2(two->Dir()[0],two->Dir()[1]);
  TComplex CdX = CX2-CX1;
  double L[2];
  switch(kase) {
    case 2:;
    case 3: {
      if (kase==3) {
	TComplex A = CP1/CP2*(Rho2/Rho1);
	TComplex B = 1.-CdX/CP2*(Im*Rho2) - CP1/CP2*(Rho2/Rho1);
	double a = A.Rho();
	double b = B.Rho();
	double alfa = A.Theta();
	double beta = B.Theta();
	double myCos = (1.-(a*a+b*b))/(2.*a*b);
	double myAng = 0;
             if (myCos>= 1.)	{nSol=1; myAng = 0.		;}
	else if (myCos<=-1.) 	{nSol=1; myAng = M_PI		;}
	else      		{nSol=2; myAng = acos(myCos)	;}
	s  = ( myAng -(alfa-beta))/Rho1;
        if (s<0) s+= 2.*M_PI/fabs(Rho1);
        s1 = s;
	if (nSol==2) {
	  s =(-myAng -(alfa-beta))/Rho1;
          if (s< 0) s+= 2.*M_PI/fabs(Rho1);
          if (s<s1) s1 = s;
	}
      } else {
	TComplex A = CP1/CP2*(Im*Rho2);
	TComplex B = 1.-CdX/CP2*(Im*Rho2);
	double cba[3]={B.Rho2()-1., 2*(A.Re()*B.Re()+A.Im()*B.Im()), A.Rho2()};
	nSol = SqEqu(cba, L);
	if (nSol< 0) break;
	if (nSol==0) nSol=1;
        if (L[0]>0) s1=L[0];
        if (nSol>1 && L[1]>0 && L[1] <s1) s1 = L[1];
      }

      break;
    }// end case 3
    case 0: {
      if (fabs((CdX/CP1).Im())>fabs((CP2/CP1).Im())*1e6) break;
      nSol = 1;
      s =  (CdX/CP2).Im()/(CP1/CP2).Im();
      if (s<0) break;
      s1 = s;
      break;   
    }//end case 0
    default: assert(0);
  }
  if (nSol) {
    TCircle_ tc1(*one),tc2(*two);
    double xy[2];
    tc1.Eval(s1,xy);   
    s = tc2.Path(xy);
    if (s<0 && kase) s += 2.*M_PI/fabs(Rho2);
    if (s>0 ) s2 = s;
  }


  if (one != this) {s=s1;s1=s2;s2=s;}
  if (S2) *S2=s2;
  return s1;
}
//_____________________________________________________________________________
void TCircle_::Test4() 
{
  double pars[4][2][5] = {
   {{0,0,1,0.,-0.001},{0,0.0,1,0,0.002}},
   {{0,0,1,0.,-0.001},{0,0.1,1,0,0.002}},
   {{0,0,1,0.,-0.001},{0,0.0,1,1,1e-8 }},
   {{0,0,1,0.,-1e-8 },{0,0.0,1,1,1e-8 }}};
  for (int ip=0;ip<4;ip++) {
    TCircle_ tc1(pars[ip][0],pars[ip][0]+2,pars[ip][0][4]); 
    TCircle_ tc2(pars[ip][1],pars[ip][1]+2,pars[ip][1][4]); 
    tc1.Move(-20);
    tc2.Move(-20);
    double s2;
    double s1 = tc1.Path(tc2,&s2);
    printf("s1=%g s2=%g \n",s1,s2);
  }  
}   
//______________________________________________________________________________
double TCircle_::Eval(double step,double *X, double *D) const
{
  
  sgCX1		=TComplex(fX[0],fX[1]);
  sgCD1		=TComplex(fD[0],fD[1]);		//  exp(I*Fi0)
  sgImTet	=TComplex(0,step*fRho);		//  I*Rho*L
  sgCOne        =expOne(sgImTet);		// (Exp(I*Rho*L)-1)/(I*Rho*L)
  sgCf1 	=sgImTet*sgCOne;		// (Exp(I*Rho*L)-1)
  
  sgCD2 = sgCD1*sgCf1+sgCD1; 			// exp(I*Fi0+I*Rho*L)
  sgCX2 = sgCD1*sgCOne*step;			// exp(I*Fi0)*(exp(I*Rho*L)-1)/(I*Rho)
  if (X) {
    X[0] = sgCX2.Re()+sgCX1.Re();
    X[1] = sgCX2.Im()+sgCX1.Im();
  }
  if (D) {
    sgCD2/= TComplex::Abs(sgCD2);
    D[0] = sgCD2.Re();
    D[1] = sgCD2.Im();
  }
  return step;
}
//______________________________________________________________________________
double TCircle_::Move(double step)
{
  Eval(step,fX,fD);
  if (fEmx && fEmx->mHH>0) MoveErrs(step);
  if (fabs(fD[0])>1) fD[0]=(fD[0]<0)? -1:1;
  if (fabs(fD[1])>1) fD[1]=(fD[1]<0)? -1:1;
  return step;
}
//______________________________________________________________________________
void TCircle_::MakeMtx(double S,double F[3][3])
{
  enum {kH=0,kA,kC};
  memset(F[0],0,sizeof(F[0][0])*3*3);
  F[kH][kH]   = sgCf1.Re()+1.;
  double dSdH = sgCf1.Im();

  F[kH][kA]   = S*sgCOne.Re();
  double dSdA = S*sgCOne.Im();
  TComplex llCOneD = S*S*expOneD(-sgImTet);
  F[kH][kC]   = llCOneD.Re();
  double dSdC = llCOneD.Im();

  F[kA][kH] =  -dSdH*fRho;
  F[kA][kA] = 1-dSdA*fRho;
  F[kA][kC] = S+dSdC*fRho;
  F[kC][kC] = 1;
}
//______________________________________________________________________________
void TCircle_::MoveErrs(double s)
{
  double F[3][3];
  MakeMtx(s,F);
  fEmx->Move(F);
}
//______________________________________________________________________________
void TCircle_::Rot(double angle)
{
  Rot(cos(angle),sin(angle));
}
//______________________________________________________________________________
void TCircle_::Rot(double cosa,double sina)
{
  TComplex CX(fX[0],fX[1]),CP(fD[0],fD[1]);
  TComplex A (cosa,sina);
  CX *=A; fX[0] = CX.Re(); fX[1]=CX.Im();
  CP *=A; CP/=TComplex::Abs(CP);
  fD[0] = CP.Re(); fD[1]=CP.Im();
}
//______________________________________________________________________________
void TCircle_::Backward()
{
  fRho=-fRho;fD[0]=-fD[0];fD[1]=-fD[1];
  if(fEmx) fEmx->Backward();
}

//______________________________________________________________________________
void TCircle_::Test2() 
{
// double xyz[4][3]= {{-39.530250549316406, -165.19537353515625, 184.05630493164062}
//                   ,{-37.718906402587891, -167.19537353515625, 186.41175842285156}
// 		  ,{-35.468486785888672, -169.19537353515625, 189.05546569824219}
//                   ,{-33.657142639160156, -171.19537353515625, 191.347900390625}};
// double x[4],y[4];
// for (int i=0;i<4;i++) { x[i]=xyz[i][0];  y[i]=xyz[i][1]; }
// 
// 
// 
// TCircle_ TC;
// double qa0 = TC.Approx(4,xyz[0],3);
// double qa1 = TC.Resid (4,xyz[0],3);
// printf("Approx qa0 = %g qa1=%g\n",qa0,qa1);
// TC.Print();
// 

}
//______________________________________________________________________________
void TCircle_::Test3() 
{
// enum {nPnts=4};
// double xyz[nPnts][3] = 
// {{80.815544128417969, 159.77731323242188, 129.11553955078125}
// ,{82.239913940429688, 161.25840759277344, 131.24034118652344}
// ,{84.462181091308594, 162.28025817871094, 133.59538269042969}
// ,{86.321846008300781, 163.51133728027344, 135.19621276855469}};
// 
// double err[nPnts][4] = 
// {{0.0010703595155359307, 0.00061836299089800776, 0.00035723771589107141,0.0032088035791992191}
// ,{0.0010505530116463389, 0.00060692047199979574, 0.00035062719848397145,0.0031350950603759769}
// ,{0.0010286003088986414, 0.00059423806134026682, 0.00034330037672605356,0.0030533996126220703}
// ,{0.0010136781863030494, 0.00058561716272119912, 0.00033831985920934062,0.0029978674575439454}};
// 
// 
// double res;
// TCircle_ circ;
// res=circ.Approx(nPnts,xyz[0],3);
// printf("res = %g \n",res);
// circ.Print();
// res=circ.Resid (nPnts,xyz[0],3);
// printf("res = %g \n",res);
// circ.Print();
// 
// circ.Show(nPnts,xyz[0],3);
// res = circ.Fit(nPnts,xyz[0],3,err[0],4);
// printf("res = %g \n",res);
// circ.Print();
// circ.Show(nPnts,xyz[0],3);

}
//______________________________________________________________________________
void TCircle_::TestMtx() 
{
  double Dir[8],X[8]={0},Rho[2],step,F[3][3],Del[3],Dif[3]={0};
  double maxEps = 0;  
  int nErr=0,tally=0;
  for (int iR = 1010;iR>-1010;iR-=20) {
    Rho[0] = 1./iR;
    int iAlf = 360*gRandom->Rndm();
    Del[0] = 0.001;
    Del[1] = M_PI/180*0.01;
    Del[2] = 1e-4+ Rho[0]*0.001;
    for (int iStep=10;iStep<=350;iStep+=10){  
      Dir[0] = cos(iAlf/180.*M_PI);
      Dir[1] = sin(iAlf/180.*M_PI);
      TCircle_ tc(X,Dir,Rho[0]);
      step = iStep/180.*M_PI*abs(iR);
      tc.Eval(step,X+2,Dir+2);
      tc.MakeMtx(step,F);

      for (int iHAR=0;iHAR<3;iHAR++) {
        double minFak = 1e-4;
        for (double fak=1;fak>minFak;fak/=2) {//loop faktor/2

        memcpy(X  +4,X  ,sizeof(X[0]  )*2);
        memcpy(Dir+4,Dir,sizeof(Dir[0])*2);
        Rho[1]=Rho[0];
        switch (iHAR) {
	case 0: { 
          X[4+0] = X[0]-Dir[1]*Del[0]*fak;
          X[4+1] = X[1]+Dir[0]*Del[0]*fak;
          break;}
	  
	case 1: {
          Dir[4+0] = cos(iAlf/180.*M_PI+Del[1]*fak);
          Dir[4+1] = sin(iAlf/180.*M_PI+Del[1]*fak);
          break;}

	case 2: {
          Rho[1] = Rho[0]+Del[2]*fak;
          break;}
        }//end switch
      
        TCircle_ tcc(X+4,Dir+4,Rho[1]);
        tcc.Move(step);
        double myStep = tcc.Path(X+2);
        tcc.Move(myStep);
        tcc.Eval(0,X+6,Dir+6);
        double dX[2];
        TCL::vsub(X+6,X+2,dX,2);
        
        myStep = -TCL::vdot(dX,Dir+2,2)/TCL::vdot(Dir+6,Dir+2,2);
        tcc.Eval(myStep,X+6,Dir+6);

        for (int jHAR=0;jHAR<2; jHAR++) {
	  switch(jHAR) {
	  case 0: {
	    Dif[0] = (X[6+0]-X[2+0])*(-Dir[2+1])
	           + (X[6+1]-X[2+1])*( Dir[2+0]);
            break;}
	  case 1: {
	    Dif[1] = (atan2(Dir[6+1],Dir[6+0])
	           -  atan2(Dir[2+1],Dir[2+0])); 
            if (Dif[1]>  M_PI) Dif[1]-=2*M_PI;
            if (Dif[1]< -M_PI) Dif[1]+=2*M_PI;
            break;}
          }
          tally++;
          double mat = F[jHAR][iHAR];
//          if (fabs(Dif[jHAR]) > 100*Del[jHAR] && fak > minFak*2) continue;
//           if (fabs(Dif[jHAR])     < 1e-3*Del[jHAR]) Dif[jHAR]=0;
//           if (fabs(mat*Del[iHAR]) < 1e-3*Del[jHAR]) mat=0;

          double est = Dif[jHAR]/(Del[iHAR]*fak);
          double min = Del[jHAR]/Del[iHAR]*0.001;
          

          if (jHAR==iHAR) {
             if (fabs(est) < 1e-4) est = 0;
             if (fabs(mat) < 1e-4) mat = 0;
          }

	  double eps =(fabs(est-mat)*2)
	             /(fabs(est)+fabs(mat)+min);
          if (eps < 1e-2) 	{
            if (eps>maxEps) maxEps=eps;
 	    break;
          }
          if (fak > minFak*2) 	continue;
          nErr++;
          if (eps>maxEps) maxEps=eps;
          printf("%6d Mtx[%d][%d] \t%g \t%g \tAngle=%d \tRad=%d \tLen=%g\n",
	         tally,jHAR,iHAR,F[jHAR][iHAR],est,
		 iAlf,iR,myStep);
	
    } } } } } 
    printf("TestMtx: %d errors maxEps=%g\n",nErr,maxEps);

}


//______________________________________________________________________________
//______________________________________________________________________________
TCircleFitter_::TCircleFitter_()
{
  Clear();
  SetEmx();
}
//______________________________________________________________________________
void TCircleFitter_::Clear(const char*)
{
   fArr.Reset();
   memset(fBeg,0,fEnd-fBeg+1);
   TCircle_::Clear();
}
//______________________________________________________________________________
TCircleFitterAux_* TCircleFitter_::GetAux(int i) const
{
  return (TCircleFitterAux_*)(fArr.GetArray()+i*TCircleFitterAux_::dSize());
}
//______________________________________________________________________________
const double* TCircleFitter_::GetX(int i) const
{
  return &(fAux[i].x);
}
//______________________________________________________________________________
double* TCircleFitter_::GetX(int i) 
{
  return &(fAux[i].x);
}
//______________________________________________________________________________
void  TCircleFitter_::Add(double x,double y,const double *errs) 
{
  fNuse =++fN;
  int n = fN*TCircleFitterAux_::dSize();
  if (fArr.GetSize()<n) {fArr.Set(n*2);fAux=0;}
  if (!fAux) fAux = GetAux(0);
  TCircleFitterAux_ *aux = fAux+fN-1;
  aux->x = x; aux->y=y; aux->exy[0]=1; aux->exy[2]=1; aux->ezz=1;aux->wt=0;
  if (errs) AddErr(errs);
}
//______________________________________________________________________________
void  TCircleFitter_::Add(double x,double y,double z) 
{
  fNuse =++fN;
  int n = fN*TCircleFitterAux_::dSize();
  if (fArr.GetSize()<n) {fArr.Set(n*2);fAux=0;}
  if (!fAux) fAux = GetAux(0);
  TCircleFitterAux_ *aux = fAux+fN-1;
  aux->x = x; aux->y=y; aux->z=z;
  aux->exy[0]=1; aux->exy[1]=0; aux->exy[2]=1;aux->ezz=1;aux->wt=0;
}
//______________________________________________________________________________
void  TCircleFitter_::AddErr(const double *errs,double ezz) 
{
  TCircleFitterAux_ *aux = fAux+fN-1;
  assert(ezz>=0);
  assert(errs[0]>=0);
  assert(errs[2]>=0);
  double spur = errs[0]+errs[2];
  assert(spur>0);
  double *e = aux->exy;
  memcpy(e,errs,sizeof(aux->exy));

  if (e[0]<0 && e[0]>-1e-5*spur) {e[0]=0;e[1]=0;}
  if (e[2]<0 && e[2]>-1e-5*spur) {e[2]=0;e[1]=0;}
  assert(e[1]*e[1]<=1.01*e[0]*e[2]);

  aux->wt = 0;
  aux->ezz = ezz;
}
//______________________________________________________________________________
void  TCircleFitter_::AddErr(double errhh,double ezz) 
{
  TCircleFitterAux_ *aux = fAux+fN-1;
  assert(ezz>=0);
  assert(errhh>0);
  aux->wt = 1./errhh;
  aux->exy[0]= -1;
  aux->ezz = ezz;
}
//______________________________________________________________________________
void  TCircleFitter_::AddZ(double z,double ez) 
{
// Must be called immediatelly after Add(...)
  fAux[fN-1].z  =z;
  fAux[fN-1].ezz=ez;
}
//______________________________________________________________________________
double TCircleFitter_::Fit() 
{
static const int nAVERs = &fRr-&fXx;
static int nCall=0; nCall++;
    double xx, yy, xx2, yy2;
    double f, g, h, p, q, t, g0, g02, d=0;
    double xroot, ff, fp;
    double dx, dy, xnom,wt,hord,tmp,radius2,radiuc2;
    fKase = fCase;
    if (fNuse < 3) return 3e33;
    TCircleFitterAux_ *aux = GetAux(0);
    dx = aux[fN-1].x - aux[0].x;
    dy = aux[fN-1].y - aux[0].y;
    hord = sqrt(dx*dx+dy*dy);
    fCos = dx/hord;
    fSin = dy/hord;
    fNor[0] = -fSin,fNor[1] = fCos;
    const double *exy=0;
    for (int iter=0;iter<2;iter++) {// one or two iters
      fWtot = 0;
      memset(&fXgravity,0,sizeof(double)*(nAVERs+2));
      for (int i=0; i<fN; i++) {//Loop over points
        if (aux[i].wt<0) continue;
        int kase = iter;
	if (aux[i].wt    >0) 	kase+=2;	//weight defined
        if (aux[i].exy[0]>0)	kase+=4;	//error matrix defined
        switch (kase) {
          case 0: wt = 1; break;
          case 2: ; case 3: wt = aux[i].wt; break;
          case 5: ; case 7: {
            fNor[0] = fXCenter - aux[i].x;
            fNor[1] = fYCenter - aux[i].y;
            tmp = sqrt(fNor[0]*fNor[0]+fNor[1]*fNor[1]);
            fNor[0]/=tmp; fNor[1]/=tmp; }
          case 4:; case 6: {
            exy = aux[i].exy;
            wt = (fNor[0]*fNor[0]*exy[0]
		 +fNor[0]*fNor[1]*exy[1]*2
		 +fNor[1]*fNor[1]*exy[2]);
            if (wt<1e-8) wt = 1e-8;
            wt = 1/wt; break;}
          default: assert(0);
	}//end switch
        aux[i].wt = wt;
        fWtot += wt;
	fXgravity += aux[i].x *wt;
	fYgravity += aux[i].y *wt;
      }//End Loop over points
      fXgravity /= fWtot;
      fYgravity /= fWtot;

      for (int i=0; i<fN; i++) {
	  dx  = aux[i].x-fXgravity;
	  dy  = aux[i].y-fYgravity;
	  xx  =  dx*fCos + dy*fSin;
	  yy  = -dx*fSin + dy*fCos;
	  xx2 = xx*xx;
	  yy2 = yy*yy;
          wt  = aux[i].wt;
	  fXx    += xx2 		*wt;
	  fYy    += yy2 		*wt;
	  fXy    += xx*yy 		*wt;
	  fXrr   += xx*(xx2+yy2) 	*wt;
	  fYrr   += yy*(xx2+yy2) 	*wt;
	  fRrrr += (xx2+yy2)*(xx2+yy2) 	*wt;
      }
      double *dd = &fXx;
      for (int i=0;i<nAVERs;i++) {dd[i]/=fWtot;}
      fRr = fXx+fYy;

      if (fNuse <= 3 && !fKase) fKase=1;
      if (!fKase) fKase =(fYy < fXx *(0.5)*(0.5)/210)? 1:2;
SWIT: switch(fKase) {
        case 1:	{	//Try 1st method

//  		Variables v0=1, v1=2*x, v2 =-rr == -(x*x+y*y)
//  		Orthogonal functions of these variables:
//  		Nor0 = fPol[0]
//  		Nor1 = fPol[1]+ v1*fPol[2]
//  		Nor2 = fPol[3]+ v1*fPol[4]+ v2*fPol[5] 
    
	  double myCof[3];    
	  fPol[0] = 1;
	  fPol[1] = 0;    fPol[2] = 1./(2*sqrt(fXx));
	  fPol[3] = fRr;  fPol[4] = fXrr/(2*fXx);   fPol[5] = 1.;
	  double tmp = sqrt(fPol[3]*fPol[3]
                	   +fPol[4]*fPol[4]*(4*fXx  )
	        	   +fPol[5]*fPol[5]*(fRrrr  )
                	   +fPol[3]*fPol[5]*(-fRr   ) *2
                	   +fPol[4]*fPol[5]*(-2*fXrr) *2);
	  fPol[3]/=tmp;fPol[4]/=tmp;fPol[5]/=tmp;
	  myCof[0] =   0;
	  myCof[1] = - (fPol[2]*(4*fXy));
	  myCof[2] = - (fPol[4]*(4*fXy) + fPol[5]*(-2*fYrr));
	  fC = myCof[0]*fPol[0]+myCof[1]*fPol[1]+myCof[2]*fPol[3];
	  fA =                  myCof[1]*fPol[2]+myCof[2]*fPol[4];
	  fB =                                   myCof[2]*fPol[5];
          fYd = (fabs(fB)>1e-6) ? 1./fB : 1e6;
          fXd = fA*fYd;
        }// end case 1
        break;

        case 2:	{	//Try 2nd method(Ososkov/Chernov)

	  f = (3.*fXx+fYy);
	  g = (fXx+3.*fYy);
	  h = 2*fXy;
	  p = fXrr;
	  q = fYrr;
	  t = fRrrr;
	  g0 = (fXx+fYy);
	  g02 = g0*g0;
	  fA = -4.0;
	  fB = (f*g-t-h*h)/g02;
	  fC = (t*(f+g)-2.*(p*p+q*q))/(g02*g0);
	  d = (t*(h*h-f*g)+2.*(p*p*g+q*q*f)-4.*p*q*h)/(g02*g02);
	  xroot = 1.0;
	  for (int i=0; i<5; i++) {
	      ff = (((xroot+fA)*xroot+fB)*xroot+fC)*xroot+d;
	      fp = ((4.*xroot+3.*fA)*xroot+2.*fB)*xroot+fC;
	      xroot -= ff/fp;
	  }
	  fG1 = xroot*g0;
	  xnom = (g-fG1)*(f-fG1)-h*h;

//	  assert(xnom>3e-33);
          if (xnom<1e-20) { fKase=1; goto SWIT;}

	  fXd = ( p*(g-fG1)-q*h      )/xnom;
	  fYd = (-p*h      +q*(f-fG1))/xnom;
        }//end case 2
        break;
        
	default: assert(0);
      } //end switch
      fXCenter = fXd*fCos-fYd*fSin + fXgravity;
      fYCenter = fXd*fSin+fYd*fCos + fYgravity;
      if (!exy)	break;
    }// end iters
    
//	Update TCircle_
    switch (fKase) {    
      case 1:  {//Big R approx
	fCorrR = sqrt(1+fA*fA+fC*fB );
	fCorrB = sqrt(1+fA*fA     );
	fRho = fabs(fB)/fCorrR;
	int sgB = (fB<0)? -1:1;
	fNy = sgB/sqrt(1+fA*fA);
	fNx = fA*fNy;
	fH = -fC*sgB/(fCorrR+fCorrB);
	fChi2 = (4*fA*fXy +4*fYy- 2*fB*fYrr)/4;
	fChi2 /= (fCorrR*fCorrR);
    fX[0] = fNx*fH; fX[1] = fNy*fH;
// 	let we are moving left --> right
//    	remember to change sign of correlation related to H if fRho<0
    fD[0] = fNy; fD[1] =-fNx;  
//
    Rot(fCos,fSin);
    fX[0] +=  fXgravity;
    fX[1] +=  fYgravity;
      } 
      break;
      case 2:  {//Ososkov
	radiuc2  = fXd*fXd+fYd*fYd;
	radius2  = radiuc2+fG1;
	fR  = ::sqrt(radius2);
	fRd = ::sqrt(radiuc2);
	fRho  = 1./fR;
	fH = -fG1/(fR+fRd);
	if (fRd>1e-6) { fNx = fXd/fRd; fNy = fYd/fRd;}
        else          { fNx = 0;       fNy = 1;  fRd = 0; }
	fChi2 = (fG1-fRr)/2;
    fX[0] = fNx*fH; fX[1] = fNy*fH;
// 	let we are moving left --> right
//    	remember to change sign of correlation related to H if fRho<0
    fD[0] = fNy; fD[1] =-fNx;  
//
    Rot(fCos,fSin);
    fX[0] +=  fXgravity;
    fX[1] +=  fYgravity;





      }
      break;
      default: assert(0);
    }
    fNdf = fNuse-3;
    if (fNdf>0) fChi2 *= fWtot/fNdf;
    tmp = fD[0]*(aux[0].x-fX[0])+fD[1]*(aux[0].y-fX[1]);
//	remember to change corrs related to rho and h
    fBack = 0;
    if (tmp>0) {fD[0]*=-1;fD[1]*=-1;fRho*=-1;fBack=1;}
    return fChi2;
}
//______________________________________________________________________________
void TCircleFitter_::MakeErrs() 
{
   fEmx->Clear();
   double F[3][3]; memset(F[0],0,sizeof(F));
   double myFact = 1.;
   switch (fKase) {
     case 1: { //For BigYC  fit
       fCov[0] = fPol[2]*fPol[2]+ fPol[4]*fPol[4];
       fCov[1] = fPol[4]*fPol[5];
       fCov[2] = fPol[5]*fPol[5];
       fCov[3] = fPol[1]*fPol[2]+ fPol[3]*fPol[4];
       fCov[4] = fPol[3]*fPol[5];
       fCov[5] = fPol[0]*fPol[0]+ fPol[1]*fPol[1]+fPol[3]*fPol[3];
       for (int i=0;i<6;i++) {fCov[i]*=4;}
       int sgB = (fB<0)? -1:1;
       double corrRB = fCorrR+fCorrB;
       double corrR3 = fCorrR*fCorrR*fCorrR;
 //      fH = -c*sgB/(fCorrR+fCorrB);

       F[0][0] =      sgB*fA*fC/(corrRB*fCorrB*fCorrR);		//dH/da
       F[0][1] =  0.5*sgB*fC*fC/(corrRB*corrRB*fCorrR);		//dH/db
       F[0][2] =  0.5*sgB*fC*fB/(corrRB*corrRB*fCorrR)		//dH/dc
               -      sgB      /(corrRB              );
       F[1][0] =  -1/(fCorrB*fCorrB);				//dFi/da
       F[2][0] =  -   sgB*fA*fB/(corrR3);			//d(aRho)/da
       F[2][1] =  -0.5*sgB*fC*fB/(corrR3)+sgB/fCorrR;		//d(aRho)/db
       F[2][2] =  -0.5*sgB*fB*fB/(corrR3);			//d(aRho)/dc
       myFact  = (fCorrR*fCorrR);
       break;
     }
     case 2:    { //For Ososkov/Chernov fit

       double aRho = fabs(fRho),aRho2 = aRho*aRho,aRho3 = aRho*aRho2;
       for (int i=0,li=0;i< 3;li+=++i) {
         for (int j=0   ;j<=i;j++    ) {
           fCov[li+j] = d2F(i,j)*0.5;  }}
       TCL::trsinv(fCov,fCov ,3);
#if 1

       double dRho = -fH/(fRd*fR);
       dRho = 1/fRd-1/fR;
       F[0][0] =  fXd*dRho;		// -dH /dXd
       F[0][1] =  fYd*dRho;		// -dH /dYd
       F[0][2] = -0.5*aRho;		// -dH /dG1
       F[1][0] = -fNy*aRho;		// dFi/dXd
       F[1][1] =  fNx*aRho;		// dFi/dYd
       F[1][2] = 0;			// dFi/dG1
       F[2][0] = -fXd*aRho3;		// dRho/dXd 
       F[2][1] = -fYd*aRho3;		// dRho/dYd 
       F[2][2] = -0.5*aRho3;		// dRho/dG1 
#endif
#if 0
       fXCenter = fXd*fCos-fYd*fSin + fXgravity;
       fYCenter = fXd*fSin+fYd*fCos + fYgravity;
       fNx = fXd*fCos-fYd*fSin; 
       fNy = fXd*fSin+fYd*fCos; 
       double nor = sqrt(fNx*fNx+fNy*fNy);
       fNx/=nor; fNy/=nor;
//     |fXCenter -fNx*fR|
//     |fYCenter -fNy*fR|
//     H = fXCenter*fNx+fYCenter*fNy - fR
       fX[0] = fXCenter -fNx*fR;
       fX[1] = fYCenter -fNy*fR;
       fD[0] = fNy;
       fD[1] =-fNx;


       F[0][0] = ( fNx*fCos+fNy*fSin) -fXd*aRho;	// dH /dXd
       F[0][1] = (-fNx*fSin+fNy*fCos) -fYd*aRho;	// dH /dYd
       F[0][2] =                      -0.5*aRho;	// dH /dG1
       F[1][0] = (-fNy*fCos + fNx*fSin)*aRho;		// dFi/dXd
       F[1][1] = ( fNy*fSin + fNx*fCos)*aRho;		// dFi/dYd
       F[1][2] = 0;					// dFi/dG1
       F[2][0] = -fXd*aRho3;				// dRho/dXd 
       F[2][1] = -fYd*aRho3;				// dRho/dYd 
       F[2][2] = -0.5*aRho3;				// dRho/dG1 

#endif

       break;
     }
     default: assert(0);
   } // end switch
   TCL::vscale(fCov,myFact/fWtot,fCov,6);
   TCL::trasat(F[0],fCov,fEmx->Arr(),3,3); 
   if (fBack) {fEmx->mHA*=-1;fEmx->mAC*=-1;}
}
//______________________________________________________________________________
double TCircleFitter_::EvalChi2() 
{
  if (!fNuse) return 0;
  TCircle_ M(this);
  double sum = 0,wtot=0,wt;
  TCircleFitterAux_ *aux = GetAux(0);
  const double *p = M.Pos();
  for (int i=0;i<fN;i++) {
    if (aux[i].wt<0) continue;
    double s = M.Path(&(aux[i].x));
    M.Move(s);
    wt = aux[i].wt;
    sum+= (pow(p[0]-aux[i].x,2)+pow(p[1]-aux[i].y,2))*wt;
    wtot +=wt;
  }
  if (fNdf) sum /= fNdf;
  fChi2 = sum;
  return sum;
}
//_____________________________________________________________________________
double TCircleFitter_::FixAt(const double vals[5],int flag) 
{
///  void TCircleFitter_::FixAt(const double vals[4],double curv,int flag) 
///  fix circle at specific x,y;Psi;Curv
///  vals[0,1]	- x,y
///  vals[2]    - reserved for Z, not used here
///  vals[3]   	- Psi
///  vals[4]	- Curvature
///  flag	- +1=xy fix,+2=Psi fix,+4 =curv fix

   assert(fEmx);
   assert(flag);
   double g[6]={1,0,1,0,0,1},c[6]={1,0,1,0,0,1}
         ,e[6],adj[3]={0},amda[3],dlt[2];
   int sel[3] ={!!(flag&1), !!(flag&2), !!(flag&4)};
   int nFix=0;
   if (sel[0]) {  	// h corr
     nFix++;
     dlt[0] = vals[0]-fX[0]; dlt[1] = vals[1]-fX[1];
     adj[0] = -dlt[0]*fD[1]+dlt[1]*fD[0];
   }
   if (sel[1]) {	// angle corr
     nFix++;
     adj[1] = vals[3]-atan2(fD[1],fD[0]);
     if (adj[1]< -M_PI) adj[1] += 2*M_PI;
     if (adj[1]>  M_PI) adj[1] -= 2*M_PI;
   }
   if (sel[2]) {	//curv corr
     nFix++;
     adj[2] = vals[4]-fRho;
   }
//	calculate add to Chisq
   for (int i=0,li=0;i< 3;li+=++i) {
     for (int j=0   ;j<=i;j++    ) {
       if (!(sel[i]&sel[j])) continue;
       c[li+j] = (*fEmx)[li+j];     } }
   double addXi2=0;
   TCL::trsinv(c        ,c   ,3  );
   TCL::trasat(adj,c,&addXi2,1,3); 

   TCL::trsinv(fEmx->Arr(),e,3);
   for (int i=0,li=0;i< 3;li+=++i) {
     for (int j=0   ;j<=i;j++    ) {
       if (!(sel[i]|sel[j])) continue;
       e[li+j] = (i==j);
       if (!(sel[i]&sel[j])) continue;
       g[li+j] = (*fEmx)[li+j];
   } }
   TCL::trsinv(g        ,g   ,3  );
   TCL::trsa  (g   ,adj ,amda,3,1);
   TCL::trsa  (fEmx->Arr(),amda,adj   ,3,1);
   TCL::trsinv(e          ,fEmx->Arr(),3  );

   for (int i=0,li=0;i< 3;li+=++i) {if (sel[i]) (*fEmx)[li+i]=0;}
//     	update x,y
   fX[0] += -adj[0]*fD[1];
   fX[1] +=  adj[0]*fD[0];
//  	update direction
//    double S = adj[1]*(1-adj[1]*adj[1]/6);
//    double C = 1-adj[1]*adj[1]/2;
//   double S = sin(adj[1]);
   double S = sin(adj[1]);
   double C = cos(adj[1]);
   double d0 = fD[0];
   fD[0] = d0*C-fD[1]*S;
   fD[1] = d0*S+fD[1]*C;
//  	update curvature
   fRho += adj[2];
   fNdf+=nFix;
   fChi2 += (addXi2-fChi2*nFix)/fNdf;
   return fChi2;
}
//_____________________________________________________________________________
void TCircleFitter_::Skip(int idx) 
{
  fAux[idx].exy[0] = -1;
  SetNdf(fNdf-1);	//compensate increasing it inside FixAt(double*)
}
//_____________________________________________________________________________
void TCircleFitter_::SetNdf(int ndf) 
{
  fChi2*=fNdf; if (ndf) fChi2/=ndf; fNdf=ndf;
}
//______________________________________________________________________________
void TCircleFitter_::Print(const char* txt) const
{
  if (!txt) txt="";
  printf("TCircleFitter_::NPoints = %d method=%d",fN,fKase);
  if (fChi2) printf(" Chi2 = %g",fChi2);
  printf("\n");
  TCircle_::Print();

  int iP = (strchr(txt,'P') || strchr(txt,'p'));
  int iE = (strchr(txt,'E') || strchr(txt,'e'));
  int iF = (strchr(txt,'F') || strchr(txt,'f'));
  int iZ = (strchr(txt,'Z') || strchr(txt,'z'));if(iZ){};
  TCircleFitterAux_ *aux = GetAux(0);
  if (iP) { //Print points
    for (int ip=0;ip<fN;ip++) {
      printf("%3d - X: %g\tY:%g \tZ:%g",ip,aux[ip].x,aux[ip].y,aux[ip].z);
      if (iE)  
      printf(" \tExy: %g %g %g \tEz:%g "
            ,aux[ip].exy[0],aux[ip].exy[1],aux[ip].exy[2],aux[ip].ezz);
      printf("\n");
  }}
  if (iF) { //Print fit
    TCircle_ circ(this);
    const double *xy = GetX(0);
    double ds=circ.Path(xy);
    circ.Move(ds);
    double s=0;
    for (int i=0;i<fN;i++) {
      xy = GetX(i);
      ds = circ.Path(xy);
      s+=ds;
      circ.Move(ds);
      if (fabs( s)<1e-6) s=0;
      if (fabs(ds)<1e-6)ds=0;
      printf("%3d - S=%g(%g) \tX: %g=%g \tY:%g=%g \tdirX=%g dirY=%g\n"
          ,i,s,ds
          ,xy[0],circ.Pos()[0]
          ,xy[1],circ.Pos()[1]
          ,circ.Dir()[0],circ.Dir()[1]);
  }}

}
//______________________________________________________________________________
void TCircleFitter_::Test(int iTest) 
{
// iTest  fitterCase + negativeRadius*10 + backwardOrderFitPoints*100+1000*noShift
  int myKase = (iTest/1   )%10;
  int negRad = (iTest/10  )%10;
  int bakPts = (iTest/100 )%10;
  int noShft = (iTest/1000)%10;

//  enum {nPts=20};
  enum {nPts=5};
  double e[4],x[5],dir[2];    
  double aShift[6];
  aShift[0]=-acos(0.25);
  aShift[1]=-acos(0.50);
  aShift[2]= 0;
  aShift[3]= acos(0.25);
  aShift[5]= acos(0.50);
  memset(aShift,0,sizeof(aShift));
  double xCenter[2];
  double RERR = 0.01;
TRandom ran;

static TCanvas* myCanvas[9]={0};
static TH1F *hh[10]={0};
static const char *hNams[]={"pH","pA","pC","Xi2","dErr"};
static const double lims[][2]={{-5  ,5},{-5   ,5   },{-5  ,5}
                              ,{ 0  ,5},{-0.05,0.05}};
const int nPads = sizeof(hNams)/sizeof(void*);

  if(!myCanvas[0])  myCanvas[0]=new TCanvas("TCircleFitter_Test0","",600,800);
  myCanvas[0]->Clear();
  myCanvas[0]->Divide(1,nPads);

  for (int i=0;i<nPads;i++) { 
    delete hh[i]; hh[i]= new TH1F(hNams[i],hNams[i],100,lims[i][0],lims[i][1]);
    myCanvas[0]->cd(i+1); hh[i]->Draw();
  }

static TH1F *hc[10]={0};
static const char *cNams[]={"HHpull","HApull","AApull","HCpull","ACpull","CCpull"};
static const int cPads=sizeof(cNams)/sizeof(char*);
  if(!myCanvas[1])  myCanvas[1]=new TCanvas("TCircleFitter_TestCorr1","",600,800);
  myCanvas[1]->Clear();
  myCanvas[1]->Divide(1,cPads);

  for (int i=0;i<cPads;i++) { 
    delete hc[i]; hc[i]= new TH1F(cNams[i],cNams[i],100,-15,15);
    myCanvas[1]->cd(i+1); hc[i]->Draw();
  }

static TH1F *hl[10]={0};
static const char *lNams[]={"XP","YP","GP","XY","XG","YG"};
static const int lPads=sizeof(lNams)/sizeof(char*);
  if(!myCanvas[2])  myCanvas[2]=new TCanvas("TCircleFitter_TestCorr2","",600,800);
  myCanvas[2]->Clear();
  myCanvas[2]->Divide(1,lPads);

  for (int i=0;i<lPads;i++) { 
    delete hl[i]; hl[i]= new TH1F(lNams[i],lNams[i],100,-5,5);
    myCanvas[2]->cd(i+1); hl[i]->Draw();
  }
//================================================================================

  int nFit = 0,isgn;
  static uint seed=0;


  for (int ir = 50; ir <= 50; ir +=5) 		{//loop over radius
    double aR = ir;
    double len = 0.3*aR;
    for (double ang0 = 0; ang0 <= 0; ang0+=0.05)	{//loop over ang 
	double R = (negRad)? -aR:aR;
	double dang = len/R/(nPts-1);
        if (bakPts) dang*=-1;
	double C0 = cos(ang0);
	double S0 = sin(ang0);

        double myX[2]={0,0},myD[2]={C0,S0};
        xCenter[0] = myX[0]-myD[1]*R;
        xCenter[1] = myX[1]+myD[0]*R;
        double corr[6]={0},korr[6]={0};
        seed++;
        for (int iter=0;iter<2;iter++) {
static const int nEv = 100000;
          ran.SetSeed(seed);
          for (int ev=0;ev<nEv;ev++) {
            TCircleFitter_ helx;
            TCircle_ idex(myX,myD,1/R);;
	    for (int is=0;is<nPts;is++) {	//loop over points
              double ang = ang0 + dang*is;
              double S = sin(ang),C = cos(ang);
              double eR = ran.Gaus(0,RERR);
              double shift = aShift[is%5];
              shift=0;
              double SS = sin(ang+shift);
              double CC = cos(ang+shift);
              e[0] = pow(RERR*SS,2);
              e[1] =-pow(RERR   ,2)*CC*SS;
              e[2] = pow(RERR*CC,2);

              x[0] = myX[0] + (R)*(S-S0);
              x[1] = myX[1] - (R)*(C-C0);
              x[0]+= -SS*eR; 
              x[1]+=  CC*eR; 
//            helx.Add (x[0],x[1],e);
              helx.Add (x[0],x[1]);
              helx.AddErr (RERR*RERR);
	    }		//end points
            helx.SetCase(myKase); //VP
	    double Xi2 = helx.Fit();
            double Xj2 = helx.EvalChi2();
            assert(fabs(Xi2-Xj2) < 1e-2*(Xi2+Xj2+0.01));

            assert (!myKase || helx.GetCase()==myKase);

	    nFit++;
	    helx.MakeErrs();
            if ((isgn=helx.Emx()->Sign())<0) {
	      ::Warning("Test1","Negative errmtx %d",isgn);continue;}

            if (helx.Rho()*idex.Rho() < 0) idex.Backward();

            double myShift = (aR*M_PI/180)*360*gRandom->Rndm();
            if (noShft) myShift=0;
	    helx.Move(myShift);
	    idex.Move(myShift);
            if ((isgn=helx.Emx()->Sign())<0) {
	      ::Warning("Test2","Negative errmtx %d",isgn);continue;}
            double s = idex.Path(helx.Pos());
	    idex.Eval(s,x,dir);

	    double dd[10];
	    double dx = helx.Pos()[0]-x[0];
	    double dy = helx.Pos()[1]-x[1];
	    dd[0] = -dx*dir[1]+dy*dir[0];
	    dd[1] = atan2(helx.Dir()[1],helx.Dir()[0])-atan2(dir[1],dir[0]);
	    if (dd[1]> M_PI) dd[1]-=2*M_PI;
	    if (dd[1]<-M_PI) dd[1]+=2*M_PI;
	    dd[2] = helx.Rho()-idex.Rho();

            if (!iter) {
              for (int i=0,li=0;i< 3;li+=++i) {
                for (int j=0;j<=i;j++) {
                corr[li+j]+= (*(helx.Emx()))[li+j];
                korr[li+j]+= dd[i]*dd[j];
              } }
              continue;
            }

	    dd[3] = dd[0]/sqrt(corr[0]);
	    dd[4] = dd[1]/sqrt(corr[2]);
	    dd[5] = dd[2]/sqrt(corr[5]);
            dd[6] = Xi2;
            dd[7] = sqrt(helx.Emx()->mHH)-RERR;

	    for (int ih=0;ih<nPads;ih++) { hh[ih]->Fill(dd[ih+3]);}


            double cc[6]={0},dia[3];
            for (int i=0,li=0;i< 3;li+=++i) {
              dia[i] = corr[li+i];
              for (int j=0;j<=i;j++) {
                cc[li+j] = (dd[i]*dd[j]-corr[li+j])/sqrt(dia[i]*dia[j]);
              } 
            }

	    for (int ih=0;ih<cPads;ih++) { hc[ih]->Fill(cc[ih]);}
  //=====================================================================================
            double myCenter[3];
	    myCenter[0]=xCenter[0]-helx.fXgravity;
	    myCenter[1]=xCenter[1]-helx.fYgravity;
            double xx = myCenter[0];
	    myCenter[0]  =  xx*helx.fCos + myCenter[1]*helx.fSin;
	    myCenter[1]  = -xx*helx.fSin + myCenter[1]*helx.fCos;
            myCenter[2]  = R*R - (pow(myCenter[0],2)+pow(myCenter[1],2));
            for (int j=0;j<3;j++) {dd[j]=(&(helx.fXd))[j]-myCenter[j];}
            dd[3+0] = dd[0]/sqrt(helx.fCov[0]);
            dd[3+1] = dd[1]/sqrt(helx.fCov[2]);
            dd[3+2] = dd[2]/sqrt(helx.fCov[5]);
	    for (int ih=0;ih<3;ih++) { hl[ih]->Fill(dd[ih+3]);}
	    cc[0] = (dd[3+0]*dd[3+1]-helx.fCov[1])/sqrt(helx.fCov[0]*helx.fCov[2]);
	    cc[1] = (dd[3+0]*dd[3+2]-helx.fCov[3])/sqrt(helx.fCov[0]*helx.fCov[5]);
	    cc[2] = (dd[3+1]*dd[3+2]-helx.fCov[4])/sqrt(helx.fCov[2]*helx.fCov[5]);
	    for (int ih=0;ih<3;ih++) { hl[ih+3]->Fill(cc[ih]);}
          }	//end of events
          if (iter) break;
          TCL::vscale(corr,1./nEv,corr,6);
          TCL::vscale(korr,1./nEv,korr,6);
          double dia[3];
          for (int i=0,li=0;i< 3;li+=++i) {
            dia[i]=sqrt(corr[li+i]);
            int n = 0;
            for (int j=0;j<=i;j++) {n+=printf("%15g",corr[li+j]/(dia[i]*dia[j]));}
            n = 45-n;
	    for (int j=0;j< n;j++) {printf(" ");}
            for (int j=0;j<=i;j++) {n+=printf("%15g",korr[li+j]/(dia[i]*dia[j]));}
            printf("\n");
          }
          
         
	}	//end of iters
    }		//end ang0
  } 		// curv

  if (e[0]) {};
  for (int i=0;myCanvas[i];i++) {
    myCanvas[i]->Modified();myCanvas[i]->Update();}

  while(!gSystem->ProcessEvents()){gSystem->Sleep(200);}; 

}

//______________________________________________________________________________
void TCircleFitter_::TestCorr(int kase) 
{
// 1=fit case    1 alowed
// 2=fit case    2 alowed
// 4=fit +ive curv alowed
// 8=fit -ive curv alowed
//16=fit -ive curv alowed

  if (!(kase&3 ))kase+=1+2;
  if (!(kase&12))kase+=4+8;
  enum {nPts=20};
  double e[4],x[3],ex[3];    
  double aShift[6];
  aShift[0]=-acos(0.25);
  aShift[1]=-acos(0.50);
  aShift[2]= 0;
  aShift[3]= acos(0.25);
  aShift[5]= acos(0.50);
  double RERR = 0.001;
TRandom ran;
static TCanvas* myCanvas=0;
static TH1F *hh[6]={0,0,0,0,0,0};
static const char *hNams[]={"HA","HA-","HC","HC-","AC","AC-",0};
  if(!myCanvas)  myCanvas=new TCanvas("TCircleFitter_TestCorr","",600,800);
  myCanvas->Clear();
  myCanvas->Divide(1,6);

  for (int i=0;i<6;i++) { 
    delete hh[i]; hh[i]= new TH1F(hNams[i],hNams[i],100,-1,1);
    myCanvas->cd(i+1); hh[i]->Draw();
  }

  int nFit = 0;
  for (int ir = 50; ir <= 1000; ir +=5) 		{//loop over radius
    double aR = ir;
    double len = 100; if (len>aR*3) len = aR*3;
    for (double ang0 = -3; ang0 < 3.1; ang0+=0.05)	{//loop over ang 
      for (int sgn = -1; sgn<=1; sgn+=2)    		{//loop over signes of curv
if ((sgn>0) && !(kase&4)) continue;
if ((sgn<0) && !(kase&8)) continue;
	double R = sgn*aR;
	double dang = len/R/nPts;
	double C0 = cos(ang0);
	double S0 = sin(ang0);
        TCircleFitter_ helx;
	for (int is=0;is<nPts;is++) {	//loop over points
          double ang = ang0 + dang*is;
          double S = sin(ang),C = cos(ang);
          double eR = ran.Gaus(0,RERR)*sgn;
          double shift = aShift[is%5];
          double SS = sin(ang+shift);
          double CC = cos(ang+shift);
          e[0] = pow(RERR*SS,2);
          e[1] =-pow(RERR   ,2)*CC*SS;
          e[2] = pow(RERR*CC,2);

          x[0] = 100 + (R)*(S-S0);
          x[1] = 200 - (R)*(C-C0);
          ex[0]= x[0]-SS*eR; 
          ex[1]= x[1]+CC*eR; 
          helx.Add (ex[0],ex[1],e);
	}		//end points
	helx.Fit();
if (!(helx.GetCase()&kase)) continue;
	nFit++;
	helx.MakeErrs();
        int iFix = 0;
        if (kase&16) iFix +=1;
        if (kase&32) iFix +=4;
	if (iFix) {
	  double vals[5];
	  TCL::ucopy(x,vals,3);
	  vals[3]=0;
	  vals[4]=1./R;
	  helx.FixAt(vals,iFix);
	}
	x[0] = 100 ;
	x[1] = 200 ;
	double s = helx.Path(x);
        assert(s<0);
	assert(fabs(s) < len);
	helx.Move(s);
	double dd[6],hf[6];
	double dx = helx.Pos()[0]-x[0];
	double dy = helx.Pos()[1]-x[1];
        const TCEmx_t_ *emx = helx.Emx();
	dd[0] = -dx*S0+dy*C0;
	dd[1] = atan2(helx.Dir()[1],helx.Dir()[0])-ang0;
	if (dd[1]> M_PI) dd[1]-=2*M_PI;
	if (dd[1]<-M_PI) dd[1]+=2*M_PI;
	dd[2] = helx.Rho()-1./R;
        hf[0] = (dd[0]*dd[1])	*1e1/(RERR*RERR);
        hf[1] = (emx->mHA)	*1e1/(RERR*RERR);
        hf[2] = dd[0]*dd[2]	*1e3/(RERR*RERR);
        hf[3] = (emx->mHC)	*1e3/(RERR*RERR);
        hf[4] = dd[1]*dd[2]	*1e4/(RERR*RERR);
        hf[5] = (emx->mAC)	*1e4/(RERR*RERR);

        
	for (int ih=0;ih<6;ih++) { hh[ih]->Fill(hf[ih]);}
    } 		//end sign
  }		//end ang0
  } 		// curv
  myCanvas->Modified();
  myCanvas->Update();
  while(!gSystem->ProcessEvents()){gSystem->Sleep(200);}; 

}
//______________________________________________________________________________
void TCircle_::Show(int nPts,const double *Pts,int pstep) 
{
static TCanvas *myCanvas = 0;
static TGraph  *ptGraph  = 0;
static TGraph  *ciGraph  = 0;
  double x[100],y[100];
  if (nPts>100) nPts=100;
  for (int i=0;i<nPts;i++) { x[i]=Pts[i*pstep+0];  y[i]=Pts[i*pstep+1]; }


  if(!myCanvas) myCanvas = new TCanvas("TCircle_Show","",600,800);
  myCanvas->Clear();
  delete ptGraph; delete ciGraph;

  ptGraph  = new TGraph(nPts  , x, y);
  ptGraph->SetMarkerColor(kRed);
  ptGraph->Draw("A*");

  TCircle_ tc(this);
  double xy[2];
  xy[0] = x[0];
  xy[1] = y[0];
  double s = tc.Path(xy);
  tc.Move(s);
  xy[0] = x[nPts-1];
  xy[1] = y[nPts-1];
  s = tc.Path(xy);
  if (s<0) { tc.Backward(); s = tc.Path(xy);}
  double ds = s/99;
  for (int i=0;i<100;i++) {x[i]=tc.Pos()[0];y[i]=tc.Pos()[1];tc.Move(ds);}
  
  ciGraph  = new TGraph(100  , x, y);
  ciGraph->Draw("Same CP");
  myCanvas->Modified();
  myCanvas->Update();
  while(!gSystem->ProcessEvents()){gSystem->Sleep(200);}; 

}
//______________________________________________________________________________
THelixFitter_::THelixFitter_():fPoli1Fitter(1)
{
  Clear();
  SetEmx();
}
//______________________________________________________________________________
THelixFitter_::~THelixFitter_()
{;}
//______________________________________________________________________________
void THelixFitter_::Clear(const char*)
{
  fCircleFitter.Clear();
  fPoli1Fitter.Clear();
  fPoli1Fitter.SetCoefs(1);
  fChi2=0;
}
//______________________________________________________________________________
void THelixFitter_::Print(const char*) const
{
  THelixTrack_::Print();
  fCircleFitter.Print();
  fPoli1Fitter.Print();
}
//______________________________________________________________________________
void THelixFitter_::Add (double x,double y,double z) 
{
  fCircleFitter.Add(x,y,z);
}  
//______________________________________________________________________________
void THelixFitter_::AddErr(const double *err2xy,double err2z) 
{  
  fCircleFitter.AddErr(err2xy,err2z);
}  
//______________________________________________________________________________
void THelixFitter_::AddErr(double errhh,double err2z) 
{  
  fCircleFitter.AddErr(errhh,err2z);
}  
//______________________________________________________________________________
double THelixFitter_::Fit()
{
  TCircleFitterAux_* myAux= GetAux(0);
  int nDat = Size();
  double Xi2xy = fCircleFitter.Fit();
  if (Xi2xy>1e11) return Xi2xy;
  int ndfXY = fCircleFitter.Ndf();
  TCircle_ circ(fCircleFitter);
  const double *xy=0;
  xy = &(myAux[nDat-1].x);
  double z1 = xy[2];
  double s1 = circ.Path(xy);

  xy = &(myAux[0].x);
  double z0 = xy[2];
  double s  = circ.Path(xy);
//	estimation of tan(dip) to correct z errs
  double tanDip = (z1-z0)/(s1-s);

  circ.Move(s);
//  set lengths
  for (int iDat=0;iDat<nDat;iDat++) {
    TCircleFitterAux_* aux = myAux+iDat;
    xy = &(aux->x);
    double ds = circ.Path(xy,aux->exy);
    circ.Move(ds); s+=ds;
//		correct errors
    double corErr = 0;;
    if (aux->exy[0]>0) {
      const double *dc = circ.Dir();
      corErr = tanDip*tanDip*
               (dc[0]*dc[0]*aux->exy[0]
               +dc[1]*dc[1]*aux->exy[2]
               +dc[0]*dc[1]*aux->exy[1]*2);
    }
    fPoli1Fitter.Add(s,aux->z,aux->ezz+corErr);
  }
  double Xi2z = fPoli1Fitter.Fit();
//	Now set THelixTrack_
  int ndfSz = fPoli1Fitter.Ndf();
  Update(1);
  int ndf = ndfSz+ndfXY;
  fChi2 = Xi2xy*ndfXY+Xi2z*ndfSz;
  if (ndf) fChi2/=ndf;
  return fChi2;
   
}  
//_____________________________________________________________________________
double THelixFitter_::FixAt(const double val[5],int flag) 
{
  double xx[3],s;
  memcpy(xx,fX,sizeof(xx));
  int move = (flag&1); 
  if (move) {
    s = fCircleFitter.Path(val);
    fCircleFitter.Move(s);
    fPoli1Fitter.Move(s);
  }
  double Xi2c = fCircleFitter.FixAt(val,flag);
  if (flag&1)   fPoli1Fitter.FixAt(0.,val[2]);
//  Update(1+2);
  if (move) {
    s = fCircleFitter.Path(xx);
    fCircleFitter.Move(s);
    fPoli1Fitter.Move(s);
  }
  Update(1+2);
//  double Xi2c = fCircleFitter.EvalChi2();
  double Xi2z = fPoli1Fitter.Chi2();
  int ndfc = fCircleFitter.Ndf();
  int ndfz = fPoli1Fitter.Ndf();
  
  int ndf = ndfc+ndfz;
  fChi2 = Xi2c*ndfc+Xi2z*ndfz;
  if (ndf) fChi2/=ndf;
  return fChi2;
}
//_____________________________________________________________________________
void THelixFitter_::Skip(int idx) 
{
  fCircleFitter.Skip(idx);
  fPoli1Fitter.Skip(idx);
  int ndfc = fCircleFitter.Ndf();
  int ndfz = fPoli1Fitter.Ndf();
  int ndf = ndfc+ndfz;
  fChi2 = fCircleFitter.Chi2()*ndfc+fPoli1Fitter.Chi2()*ndfz;
  if (ndf) fChi2/=ndf;
}
//______________________________________________________________________________
void THelixFitter_::Update(int kase)
{
  if(kase&1) {
    const double *pol = fPoli1Fitter.Coe();
    fCosL = 1./sqrt(pol[1]*pol[1]+1);
    double *haslet = fCircleFitter.Pos();
    fX[0] = haslet[0];
    fX[1] = haslet[1];
    fX[2] = pol[0];
    fP[0] = haslet[2]*fCosL;
    fP[1] = haslet[3]*fCosL;
    fP[2] = pol[1]*fCosL;
    fRho  = haslet[4];
  }
  if(kase&2) {
    double emx[3];
    emx[0] = fPoli1Fitter.Emx()[0];
    emx[1] = fPoli1Fitter.Emx()[1]*fCosL*fCosL;
    emx[2] = fPoli1Fitter.Emx()[2]*fCosL*fCosL*fCosL*fCosL;
    fEmx->Set(fCircleFitter.Emx()->Arr(),emx);
  }
}
//______________________________________________________________________________
void THelixFitter_::MakeErrs()
{
  fCircleFitter.MakeErrs();
  fPoli1Fitter.MakeErrs();
  Update(2);
}
//______________________________________________________________________________
double THelixFitter_::EvalChi2() 
{
  double Xi2c = fCircleFitter.EvalChi2();
  double Xi2z = fPoli1Fitter.EvalChi2();
  fChi2 = Xi2c*fCircleFitter.Ndf()+Xi2z*fPoli1Fitter.Ndf();
  fChi2/=(fCircleFitter.Ndf()+fPoli1Fitter.Ndf()+1e-10);
  return fChi2;
}
//______________________________________________________________________________
void THelixFitter_::Test(int kase)
{
// 1=fit case    1 alowed
// 2=fit case    2 alowed
// 4=fit +ive curv alowed
// 8=fit -ive curv alowed
// 16=fix last point 
// 32=fix curvature 
// 64=fix angle (not implemented in test) 
//128=show each track 
  if (!(kase&3 ))kase+=1+2;
  if (!(kase&12))kase+=4+8;
//  enum {nPts=20,nHH=8};
  enum {nPts=5,nHH=8};
  double e[4],x[10],xe[10];    
  double aShift[6];
  aShift[0]=-acos(0.25);
  aShift[1]=-acos(0.50);
  aShift[2]= 0;
  aShift[3]= acos(0.25);
  aShift[5]= acos(0.50);
  double RERR = 0.1;
  double ZERR = 0.1;
TRandom ran;
static TCanvas* myCanvas[9]={0};
static TH1F *hh[nHH]={0};
static const char *hNams[]={"pH","pA","pC","pZ","pD","Xi2","Xi2E","Xi2d",0};
  if(!myCanvas[0])  myCanvas[0]=new TCanvas("THelixFitter_TestC1","",600,800);
  myCanvas[0]->Clear();
  myCanvas[0]->Divide(1,nHH);

  for (int i=0;i<nHH;i++) { 
    double low = (i>=5)? 0:-5;
    double upp = 5;
    delete hh[i]; hh[i]= new TH1F(hNams[i],hNams[i],100,low,upp);
    myCanvas[0]->cd(i+1); hh[i]->Draw();
  }

//		Init Second histo group 
static TH1F *h2h[4]={0,0,0,0};
static const char *h2Nams[]={"targYY","targZZ","targYZ","calcYZ",0};
  int n2h=4;
  if(!myCanvas[1])  myCanvas[1]=new TCanvas("THelixFitter_TestC2","",600,800);
  myCanvas[1]->Clear();
  myCanvas[1]->Divide(1,n2h);
  for (int i=0;i<n2h;i++) { 
    delete h2h[i]; h2h[i]= new TH1F(h2Nams[i],h2Nams[i],100,-5,5);
    myCanvas[1]->cd(i+1); h2h[i]->Draw();
  }
//		End Init Second histo group 

//		Init 3rd histo group 
static TH1F *h3h[4]={0,0,0,0};
static const char *h3Nams[]={"dcaXY","dcaXYNor","dcaZ","dcaZNor",0};
  int n3h=4;
  if(!myCanvas[2])  myCanvas[2]=new TCanvas("THelixFitter_TestC3","",600,800);
  myCanvas[2]->Clear();
  myCanvas[2]->Divide(1,n3h);
  for (int i=0;i<n3h;i++) { 
    delete h3h[i]; h3h[i]= new TH1F(h3Nams[i],h3Nams[i],100,-5,5);
    myCanvas[2]->cd(i+1); h3h[i]->Draw();
  }
//		End Init 3rd histo group 


  double spotSurf[4]= {-100,1,0,0};
  double spotAxis[3][3]= {{0,1,0},{0,0,1},{1,0,0}};


  int nFit = 0,isgn;
for (double idip=-1;idip<=1;idip+=0.2){
  double dip = idip;
//  dip = 0;
  double cosDip = cos(dip);
  double sinDip = sin(dip);
  double tanDip = tan(dip); if(tanDip){};
  for (int ir = 30; ir <= 100; ir +=20) 		{//loop over radius
    double aR = ir;
    double len = 100; if (len>aR*3) len = aR*3;
    for (double ang00 = -3; ang00 < 3.1; ang00+=0.2)	{//loop over ang 
      double ang0 = ang00;
//      ang0 = 0;
      for (int sgn = -1; sgn<=1; sgn+=2)    		{//loop over signes of curv
if(sgn>0 && !(kase&4)) continue; 
if(sgn<0 && !(kase&8)) continue; 

	double R = sgn*aR;
	double dang = len/R/nPts;
	double C0 = cos(ang0);
	double S0 = sin(ang0);
        THelixFitter_ helx;

        double trakPars[7]={100,200,300,C0*cosDip,S0*cosDip,sinDip,1/R};
        THelixTrack_ trak(trakPars+0,trakPars+3,trakPars[6]);

	for (int is=0;is<nPts;is++) {	//loop over points
          double ang = ang0 + dang*is;
          double S = sin(ang),C = cos(ang);
          double eR = ran.Gaus(0,RERR)*sgn;
          double eZ = ran.Gaus(0,ZERR);
          double shift = aShift[is%5];
          double SS = sin(ang+shift);
          double CC = cos(ang+shift);
          e[0] = pow(RERR*SS,2);
          e[1] =-pow(RERR   ,2)*CC*SS;
          e[2] = pow(RERR*CC,2);
          e[3] = pow(ZERR,2);
          x[0] = 100 + (R)*(S-S0);
          x[1] = 200 - (R)*(C-C0);
          double len = (R)*(ang-ang0);
          x[2] = 300 + len*tan(dip);
          xe[0]= x[0]-SS*eR; 
          xe[1]= x[1]+CC*eR; 
          xe[2]= x[2]+eZ; 
          helx.Add (xe[0],xe[1],xe[2]);
          helx.AddErr(e,e[3]);
	}		//end points
	double Xi2 =helx.Fit();
if(!(kase&helx.GetCase())) continue; 

	helx.MakeErrs();
        if ((isgn=helx.Emx()->Sign())<0) {
	  ::Warning("Test1","Negative errmtx %d",isgn);continue;}
	nFit++;
if (kase&16) Xi2=helx.FixAt(x);


if (kase&32) {
	double vals[5];
	TCL::ucopy(x,vals,3);vals[3]=0;vals[4]=1./R;
	Xi2=helx.FixAt(vals,4);
	}
if (kase&128) helx.Show();
	double Xi2E =helx.EvalChi2();

        trak.Move(0.3*len/cosDip);
	memcpy(x,trak.Pos(),sizeof(x));
        ang0 = atan2(trak.Dir()[1],trak.Dir()[0]);
//	double s = helx.Path(x[0],x[1]);
	double s = helx.Path(x);
//      assert(s<0);
//	assert(fabs(s) < len*1.1);

        double pos[3],dir[3],rho;
	helx.Move(s);
          if ((isgn=helx.Emx()->Sign())<0) {
	  ::Warning("Test2","Negative errmtx %d",isgn);continue;}
        THEmx_t_ *emx = helx.Emx();
        helx.Eval   (0.,pos,dir,&rho);
	double psi = atan2(dir[1],dir[0]);
	double sinPsi = sin(psi);
	double cosPsi = cos(psi);
	double tanPsi = sinPsi/cosPsi; if(tanPsi){};
	double dd[10],hf[10];
	double dx = x[0]-pos[0];
	double dy = x[1]-pos[1];
	dd[0] = -dx*sinPsi+dy*cosPsi;
	hf[0] = dd[0]/sqrt(emx->mHH+1e-20);
	dd[2] = psi-ang0;
	if (dd[2]> M_PI) dd[2]-=2*M_PI;
	if (dd[2]<-M_PI) dd[2]+=2*M_PI;
	hf[1] = dd[2]/sqrt(emx->mAA+1e-20);
	dd[4] = rho-1./R;
	hf[2] = dd[4]/sqrt(emx->mCC+1e-20);
        dd[6] = (helx.Pos()[2]-x[2])/pow(helx.GetCos(),2);
        hf[3] = dd[6]/sqrt(emx->mZZ+1e-20);
        dd[8] = asin(dir[2])-dip;
	if (dd[8]> M_PI) dd[8]-=2*M_PI;
	if (dd[8]<-M_PI) dd[8]+=2*M_PI;
        hf[4] = dd[8]/(sqrt(emx->mLL));
        hf[5] = Xi2;
        hf[6] = Xi2E;
        hf[7] = Xi2E-Xi2+1;
	for (int ih=0;ih<nHH;ih++) { hh[ih]->Fill(hf[ih]);}

//		Fill 2nd histo group
        double xIde[3],pIde[3],xFit[3],pFit[3],eSpot[3],hfil,sIde,sFit;
//        if(fabs(dip)>1) continue;
        int closePoint=0;
        spotSurf[0] = -110;
  
        { spotSurf[0] = -x[0]; closePoint=2006;}
        sIde = trak.Path(200.,spotSurf,4, xIde,pIde,closePoint);if (sIde){};
 
        if (fabs(spotSurf[0]+TCL::vdot(xIde,spotSurf+1,3))>0.001) {
          printf("***Wrong point found**\n");
          trak.Print();
          assert(0);
	}
        sFit = helx.Path(200.,spotSurf,4, xFit,pFit,closePoint);
        if (sFit>=1000 ) continue;
        if (fabs(pIde[0]-pFit[0])>0.1) continue;
        helx.Move(sFit);
        emx = helx.Emx();
        helx.GetSpot(spotAxis,eSpot);
        hfil = (xFit[1]-xIde[1]); hfil/= sqrt(eSpot[0]); 
        h2h[0]->Fill(hfil);
        hfil = (xFit[2]-xIde[2]); hfil/= sqrt(eSpot[2]); 
        h2h[1]->Fill(hfil);
        hfil = (xFit[1]-xIde[1])*(xFit[2]-xIde[2]);
        h2h[2]->Fill(hfil*100);
        h2h[3]->Fill(hfil/sqrt(eSpot[0]*eSpot[2]));
//        h2h[3]->Fill(eSpot[1]*100);
//		End 2nd histo group

//		Fill 3rd histo group
        double dcaXY,dcaZ,dcaEmx[3];
        double sDca = helx.Dca(trakPars,dcaXY,dcaZ,dcaEmx);
        if (fabs(sDca)<1000) {
          h3h[0]->Fill(dcaXY);
          h3h[1]->Fill(dcaXY/sqrt(dcaEmx[0]));
          h3h[2]->Fill(dcaZ );
          h3h[3]->Fill(dcaZ /sqrt(dcaEmx[2]));
        }
//		End 3rd histo group

    } 		//end sign
  }		//end ang0
  } 		// curv
}		// dip
  for (int ih=0;myCanvas[ih];ih++) {
    myCanvas[ih]->Modified();
    myCanvas[ih]->Update();
  }
  while(!gSystem->ProcessEvents()){gSystem->Sleep(200);}; 
}
//______________________________________________________________________________
void THelixFitter_::Show() const
{
static TCanvas *myCanvas = 0;
static TGraph  *ptGraph[2]  = {0,0};
static TGraph  *ciGraph[2]  = {0,0};
  double  x[100],y[100],z[100],l[100]
        , X[100],Y[100],Z[100];
  int nPts = Size();
  if (nPts>100) nPts=100;
  TCircleFitterAux_* aux=GetAux(0);
  THelixTrack_ tc(this);
  double s = tc.Path(aux[0].x,aux[0].y); tc.Move(s);
  s = tc.Path(aux[nPts-1].x,aux[nPts-1].y);
  if (s<0) { tc.Backward();}
  l[0]=0;
  double ds=0;
  for (int i=0;i<nPts;i++) {
    if (i) {ds = tc.Path(aux[i].x,aux[i].y);tc.Move(ds);l[i]=l[i-1]+ds;}
    x[i]=aux[i].x;   y[i]=aux[i].y;   z[i]=aux[i].z;
    X[i]=tc.Pos()[0];Y[i]=tc.Pos()[1];Z[i]=tc.Pos()[2];
  }


  if(!myCanvas) myCanvas = new TCanvas("THelixFitter_Show","",600,800);
  myCanvas->Clear();
  myCanvas->Divide(1,2);

  delete ptGraph[0]; delete ciGraph[0];
  ptGraph[0]  = new TGraph(nPts  , x, y);
  ptGraph[0]->SetMarkerColor(kRed);
  myCanvas->cd(1); ptGraph[0]->Draw("A*");
  delete ptGraph[1]; delete ciGraph[1];
  ptGraph[1]  = new TGraph(nPts  , l, z);
  ptGraph[1]->SetMarkerColor(kRed);
  myCanvas->cd(2); ptGraph[1]->Draw("A*");
  
  ciGraph[0]  = new TGraph(nPts  , X, Y);
  myCanvas->cd(1); ciGraph[0]->Draw("Same CP");
  ciGraph[1]  = new TGraph(nPts  , l, Z);
  myCanvas->cd(2); ciGraph[1]->Draw("Same CP");

  myCanvas->Modified();
  myCanvas->Update();
  while(!gSystem->ProcessEvents()){gSystem->Sleep(200);}; 

}
//______________________________________________________________________________
double TCircleFitter_::f() 
{ //f*Rho2 = 4*F
return 4*((fG1-fRr)/2)*fR*fR;
}
//______________________________________________________________________________
double TCircleFitter_::F() 
{ //f*Rho2 = 4*F
return (fG1-fRr)/2;
}
//______________________________________________________________________________
double TCircleFitter_::df(int i)
{
   switch (i) {
     case 0: return -4*(fXrr -2*fXx*fXd - 2*fXy*fYd);
     case 1: return -4*(fYrr -2*fXy*fXd - 2*fYy*fYd);
     case 2: return  2*(fG1 - fRr);
     default: assert(0);
   }
   assert(0);
return 0;
}   
//______________________________________________________________________________
double TCircleFitter_::d2f(int i,int j)
{   
//   d2f/dA/dA = 4*XX
//   d2f/dA/dB = 4*XY ;d2f/dB/dB = 4*YY
//   d2f/dA/dG = 0    ;d2f/dB/dG = 0    ;d2f/dG/dG = 1
  assert(j<=i);
  int ij = i+10*j;
  switch(ij) {
    case  0: return 8*fXx;
    case 01: return 8*fXy;
    case 11: return 8*fYy;
    case 02:; case 12: return 0;
    case 22: return 2;
    default: printf ("Kase=%d\n",ij); assert(0);
    assert(0);
  }
  return 0;
}  
//______________________________________________________________________________
double TCircleFitter_::Rho2 () { return fRho*fRho;}
//______________________________________________________________________________
//______________________________________________________________________________
double TCircleFitter_::dRho2(int i)
{
  double ans =fRho*fRho;
  ans *= -ans;  
  switch (i) {
    case 0: return 2*ans*fXd;	//-2*Rh2**2*x
    case 1: return 2*ans*fYd;   //-2*Rh2**2*y
    case 2: return ans;		//-  Rh2**2
  }
  assert(0);   
  return 0;
}
//______________________________________________________________________________
double TCircleFitter_::d2Rho2(int i,int j)
{
//   d2(Rho2)/dA/dA = 8*(Rho6)*A*A-2*(Rho4);	
//   d2(Rho2)/dA/dB = 8*(Rho6)*A*B 	;d2(Rho2)/dB/dB = 8*(Rho6)*B*B -2*(Rho4)	
//   d2(Rho2)/dA/dG = 4*(Rho6)*A	;d2(Rho2)/dB/dG = 4*(Rho6)*B; 	        d2(Rho2)/dG/dG = 2*(Rho6);	
  if (j>i) { int jj = j; j = i; i = jj;}
  int ij = i+10*j;
  double rho2 = fRho*fRho,rho4 = rho2*rho2, rho6 = rho4*rho2;
  switch(ij) {
    case  0: return 8*(rho6)*fXd*fXd-2*(rho4);;
    case 01: return 8*(rho6)*fXd*fYd;
    case 11: return 8*(rho6)*fYd*fYd-2*(rho4);
    case 02: return 4*(rho6)*fXd;
    case 12: return 4*(rho6)*fYd;
    case 22: return 2*(rho6);
    default: printf ("Kase=%d\n",ij); assert(0);
  }
}
//______________________________________________________________________________
double TCircleFitter_::dF(int i)
{
//		 1./4*(df(P,i)*Rho2(P)+f(P)*dRho2(P,i));}	

double ans =  1./4*(df(i)*Rho2() + f()*dRho2(i));
return ans;
}		    
//______________________________________________________________________________
double TCircleFitter_::d2F(int i,int j)
{
//		 1./4*(df(P,i)*Rho2(P)+f(P)*dRho2(P,i));}	

double ans =  1./4*(d2f(i,j)*Rho2()  +df(j)*dRho2(i)
                   +df (i)  *dRho2(j)+f() *d2Rho2(i,j));
return ans;
}		    

//______________________________________________________________________________
/***************************************************************************
 *
 * $Id: THelixTrack_.cxx,v 1.2 2020/03/08 20:11:08 perev Exp $
 *
 * Author: Victor Perev, Mar 2006
 * Rewritten Thomas version. Error hangling added
 * Author: Thomas Ullrich, Dec 1999
 ***************************************************************************
 *
 * Description:
 *
 * Fast fitting routine using a iterational linear regression 
 * method (ILRM). Reference: N.Chernov, G.A.Ososkov, Computer  
 * Physics Communication 33 (1984) 329-333.                   
 *
 ***************************************************************************
 *
 * $Log: THelixTrack_.cxx,v $
 * Revision 1.2  2020/03/08 20:11:08  perev
 * _added
 *
 * Revision 1.1  2020/03/01 22:51:43  perev
 * NEW temorary THelixTrack_
 *
 * Revision 1.19  2019/09/23 19:09:52  perev
 * TestErr fix
 *
 * Revision 1.17  2019/01/23 00:07:38  perev
 * More detailed TestDer2() added
 *
 * Revision 1.15  2017/11/22 23:35:15  perev
 * Cleanup
 *
 * Revision 1.78  2017/06/27 23:45:39  perev
 * Coverity
 *
 * Revision 1.77  2015/05/21 23:37:01  perev
 * CheckCpp fixes. No real bugs
 *
 * Revision 1.76  2015/04/28 20:36:37  perev
 * Crossing of two helices rewritten
 *
 * Revision 1.75  2014/06/02 18:28:22  perev
 * Chec XX and YY for non zero error matrix
 *
 * Revision 1.74  2013/06/10 15:50:10  perev
 * fabs(eigen) + TComplex &x added
 *
 * Revision 1.72  2013/05/16 20:04:09  perev
 * Init all variables to 0
 *
 * Revision 1.71  2013/05/02 02:00:12  perev
 * Defence aginst strait track along X
 *
 * Revision 1.70  2013/05/01 17:33:34  perev
 * method TCirleFitter::Show added
 *
 * Revision 1.69  2013/05/01 15:58:52  perev
 * Some pre fit analisys improved to avoid crashes for Stv
 *
 * Revision 1.66  2013/04/23 01:47:16  perev
 * add Show() ++ defence against abnormal cases
 *
 * Revision 1.65  2013/04/20 03:37:11  perev
 * Reorganization to account non standard cases
 *
 * Revision 1.64  2013/04/17 03:01:32  perev
 * Special case xy1st ~= xyLst
 *
 * Revision 1.63  2013/04/17 02:12:20  perev
 * More accurate fast track estimation 2
 *
 * Revision 1.62  2013/04/16 18:54:20  perev
 * More accurate fast track estimation
 *
 * Revision 1.61  2013/02/20 02:01:44  perev
 * Cleanup
 *
 * Revision 1.9  2017/04/07 14:53:05  perev
 * rename sysCoo==>tkCoo & make it static member
 *
 * Revision 1.2  2015/04/28 18:47:27  perev
 * THelix & Thelix crossing improved. Iterations along directions
 *
 * Revision 1.54  2011/07/19 19:29:19  perev
 * set hh & zz errors
 *
 * Revision 1.53  2011/04/01 20:10:32  perev
 * +Check for 0 array
 *
 * Revision 1.52  2010/12/07 16:59:27  perev
 * Cleanup
 *
 * Revision 1.51  2010/12/07 16:50:32  perev
 * THelixTrack_::Path(x,y) TCircle_ inside
 *
 * Revision 1.50  2010/10/31 23:36:35  perev
 * TestDer() Test deiivates added
 *
 * Revision 1.49  2010/10/14 17:45:49  perev
 * Inversion of derivative matrix added
 *
 * Revision 1.48  2010/07/16 20:31:38  perev
 * Put back some ctr(this) to ctr(*this)
 *
 * Revision 1.47  2010/07/15 18:08:43  perev
 * TestMtx added
 *
 * Revision 1.46  2010/06/01 20:54:54  perev
 * Correlation HZ accounted now
 *
 * Revision 1.45  2010/04/23 22:51:27  perev
 * Method Move with derivatives adde
 *
 * Revision 1.44  2009/11/09 19:58:58  perev
 * FitZ removed everywhere
 *
 * Revision 1.43  2009/09/07 04:32:50  fine
 * workaround for the bug #1628
 *
 * Revision 1.42  2009/08/28 16:38:55  fine
 * fix the compilation issues under SL5_64_bits  gcc 4.3.2
 *
 * Revision 1.41  2009/08/24 23:40:33  perev
 * operator=() added
 *
 * Revision 1.40  2009/08/22 00:11:59  perev
 * Full error matrix + derivatives matrix
 *
 * Revision 1.39  2009/07/18 00:12:56  perev
 * method PatX(helx,,,) added
 *
 * Revision 1.38  2009/07/01 21:48:39  perev
 * Fix -tive errors & remove obsolete
 *
 * Revision 1.37  2009/04/06 17:51:32  perev
 * Replace assert(wt>0) by error condition
 *
 * Revision 1.36  2008/10/29 19:36:25  perev
 * flag 2d and 3d dca added
 *
 * Revision 1.35  2007/12/20 00:47:27  perev
 * WarnOff
 *
 * Revision 1.34  2007/12/18 23:11:05  perev
 * Distance to helix & circle added
 *
 * Revision 1.33  2007/10/24 22:43:24  perev
 * Implementation was forgotten. Thanx Adam
 *
 * Revision 1.32  2007/09/10 02:05:37  perev
 * Misstype fixed
 *
 * Revision 1.31  2007/07/13 18:17:10  perev
 * remove member fMax from THelixTrack_
 *
 * Revision 1.30  2007/07/12 00:22:29  perev
 * TCircleFitter_::Fit case 1 if case 2 failed
 *
 * Revision 1.29  2007/06/25 19:26:40  perev
 * Cleanup
 *
 * Revision 1.28  2007/04/26 04:20:18  perev
 * Some improvements
 *
 * Revision 1.27  2007/03/21 17:41:32  fisyak
 * replace complex by TComplex
 *
 * Revision 1.26  2007/01/26 19:56:24  perev
 * tune up
 *
 * Revision 1.25  2006/08/10 04:09:50  perev
 * Test cleanup
 *
 * Revision 1.23  2006/06/28 18:39:07  perev
 * cos(dip)**4 added to Dca(...) to account z err in the nearest point
 *
 * Revision 1.22  2006/06/26 19:09:21  perev
 * DcaXY & DcaZ with errors added
 *
 * Revision 1.21  2006/06/09 19:53:51  perev
 * double Dca(double x,double y,double *dcaErr=0) added
 *
 * Revision 1.2  2003/09/02 17:59:34  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  1999/12/21 16:28:48  ullrich
 * Initial Revision
 *
 **************************************************************************/
 
