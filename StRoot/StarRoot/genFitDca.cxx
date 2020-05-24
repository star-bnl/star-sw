/***************************************************************************
 *
 * $Id: genFitDca.cxx,v 1.1 2020/05/23 23:25:06 perev Exp $
 *
 * Author: Victor Perevoztchikov, 2020
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: genFitDca.cxx,v $
 * Revision 1.1  2020/05/23 23:25:06  perev
 * GenFit errors conversion into Dca
 *
 *
 *
 **************************************************************************/
#include <assert.h>
#include "genFitDca.h"
#include "Riostream.h"
#include "TCernLib.h"    
#include "TVector3.h"    
#include "TVectorD.h" 
   
double *Arr(TVector3 &v) {return (double*)&v[0];}

double EmxSign(int n,const double *a);
double DOT2(const TVector3 A,const TVector3 B) {return A[0]*B[0]+A[1]*B[1];}



//________________________________________________________________________________
StDcaGenFit::StDcaGenFit()
{
    memset(mBeg,0,mEnd-mBeg+1);
}

//________________________________________________________________________________
StDcaGenFit::~StDcaGenFit() {/* noop */}

//________________________________________________________________________________
TVector3 StDcaGenFit::origin() const
{
    double x = -mImp*sin(mPsi);
    double y =  mImp*cos(mPsi);
    return TVector3(x,y,mZ);
}

//________________________________________________________________________________
TVector3 StDcaGenFit::momentum() const
{
    double ptt = pt();
    double x   = ptt*cos(mPsi);
    double y   = ptt*sin(mPsi);
    double z   = ptt*mTan;
    return TVector3(x,y,z);
}

//________________________________________________________________________________
void StDcaGenFit::set(const float pars[kNMinPars],const float errs[kNMinErrs])
{
    if (pars) memcpy(&mImp   ,pars,sizeof(float)*kNMinPars );
    if (errs) memcpy(&mImpImp,errs,sizeof(float)*kNMinErrs);
}
//________________________________________________________________________________
void StDcaGenFit::set(const double pars[kNMinPars],const double errs[kNMinErrs])
{
  if (pars) TCL::ucopy(pars, &mImp, 6);
  if (errs) TCL::ucopy(errs, &mImpImp, kNMinErrs);
}

//________________________________________________________________________________
ostream&  operator<<(ostream& os, const StDcaGenFit& dca) {
  const Float_t *errMx =  dca.errMatrix();
  return os << Form("Dca: imp %7.2f +/-%7.2f, Z:%7.2f +/-%7.2f, psi:%7.2f +/-%7.2f, pT/q:%7.2f +/-%6.1f%%, TanL:%8.3f +/-%8.3f",
		    dca.impact(),    (errMx[0] >= 0)  ? sqrt(errMx[0]) : -13,
		    dca.z(),         (errMx[2] >= 0)  ? sqrt(errMx[2]) : -13,
		    dca.psi(),       (errMx[5] >= 0)  ? sqrt(errMx[5]) : -13,
		    dca.charge()*dca.pt(),    (errMx[9] >= 0 && dca.pt() > 0)  ? 100*sqrt(errMx[9])*dca.pt() : -13,
		    dca.tanDip(),    (errMx[14] >= 0) ? sqrt(errMx[14]): -13);
}
//________________________________________________________________________________
void   StDcaGenFit::Print(const char *) const {cout << *this << endl;}
//________________________________________________________________________________
GFGlob::GFGlob()
{
  TCL::vzero(mH,3+3+3*3);
}
//________________________________________________________________________________
GFitPars::GFitPars()
{
  memset((char*)&mqPinv,0,(char*)&mSig+sizeof(mSig)-(char*)&mqPinv);
}
//________________________________________________________________________________
GFitErrs::GFitErrs()
{
  memset((char*)&qPqP,0,(char*)&VV+sizeof(VV)-(char*)&qPqP);
}
//________________________________________________________________________________
void GFull::SetMag(const double h[3])
{ memcpy(mGlob.mH,h,sizeof(mGlob.mH));}

//________________________________________________________________________________
void GFull::SetGlob(const double pos[3],const double uvn[3][3]) 
{ 
  memcpy(mGlob.mPos,      pos,sizeof(mGlob.mPos));
  memcpy(mGlob.mUVN[0],uvn[0],sizeof(mGlob.mUVN));
  for (int i=0;i<3;i++) {
  for (int k=0;k<3;k++) {
    double tmp = TCL::vdot(mGlob.mUVN[i],mGlob.mUVN[k],3);
    if (i==k) tmp-=1;
    assert(fabs(tmp)<1e-6);
  }}
}
//________________________________________________________________________________
void GFull::SetPars(double qpinv,double uc,double vc,double u,double v,int sig) 
{ 
  mPars.SetPars(qpinv,uc,vc,u,v,sig);
  MakeTrak();  
}
//________________________________________________________________________________
void GFitPars::SetPars(double qpinv,double uc,double vc,double u,double v,int sig) 
{ 
  mqPinv = qpinv;
  mUc  = uc;
  mVc  = vc;
  mNc  = 1-(mUc*mUc+mVc*mVc); mNc = (mNc<0)? 0:sqrt(mNc)*sig;
  mU   = u;
  mV   = v;
  mN   = 0;
  mSig = sig;
}    

//________________________________________________________________________________
void GFull::SetPars(const double pars[kNMinPars] ,int sig) 
{ 
  SetPars( pars[0],pars[1],pars[2],pars[3],pars[4],sig);
}
//________________________________________________________________________________
TVectorD GFull::GetPars(int* iSig) const
{
  TVectorD v(kNMinPars);
  v[0] = mPars.mqPinv ;
  v[1] = mPars.mUc  ;
  v[2] = mPars.mVc  ;
  v[3] = mPars.mU   ;
  v[4] = mPars.mV   ;
  if (iSig) *iSig = mPars.mSig;
  return v;
}
//________________________________________________________________________________
void GFull::SetPars(int icharge,const TVector3 pos,const TVector3 mom)
{
  TVector3 XlocV = pos-TVector3(mGlob.mPos);
  double pars[kNMinPars];
  pars[0] = icharge/mom.Mag();  
  pars[1] = mom.Unit().Dot(TVector3(mGlob.mUVN[0]));
  pars[2] = mom.Unit().Dot(TVector3(mGlob.mUVN[1]));
  double tmp = mom.Unit().Dot(TVector3(mGlob.mUVN[2]));
  int iSig = (tmp<0) ? -1:1;
  pars[3] = XlocV.Dot(TVector3(mGlob.mUVN[0]));
  pars[4] = XlocV.Dot(TVector3(mGlob.mUVN[1]));
  tmp = XlocV.Dot(TVector3(mGlob.mUVN[2]));
  assert(fabs(tmp) <1e-5);
  SetPars(pars,iSig);


}
//________________________________________________________________________________
void GFull::SetBigPars(int icharge,const TVector3 Pos,const TVector3 Mom)
{
  mBigPars[kqPtInv] = icharge/Mom.Mag()/CosL();  
  double tau = -DOT2(Pos,Mom)/DOT2(Mom,Mom);
  TVector3 pos = Pos + tau*Mom;
  
  for (int i=0;i<3;i++) {
    mBigPars[kDirX+i] = Mom.Unit()[i];
    mBigPars[kPosX+i] = pos[i];
  }
}
//________________________________________________________________________________
void GFull::MakeTrak()
{
  mqPinv = mPars.mqPinv;
  double uc[3];
  TCL::ucopy(&mPars.mUc,uc,2);
  uc[2] = 1.-(uc[0]*uc[0]+uc[1]*uc[1]);
  assert(uc[2]>-1e-6);
  if (uc[2]<1e-6) uc[2] = 0.;
  uc[2] = sqrt(uc[2])*mPars.mSig;

  mDir = TVector3(mGlob.mUVN[0])*uc[0];
  mDir+= TVector3(mGlob.mUVN[1])*uc[1];
  mDir+= TVector3(mGlob.mUVN[2])*uc[2];
  assert(fabs(mDir.Mag()-1)<1e-5);

  mPos=  TVector3(mGlob.mPos);
  mPos+= TVector3(mGlob.mUVN[0])*mPars.mU;
  mPos+= TVector3(mGlob.mUVN[1])*mPars.mV;
  mPos+= TVector3(mGlob.mUVN[2])*mPars.mN;

}
//________________________________________________________________________________
void GFull::SetErrs(const double emx[kNMinErrs])
{
  double *e = (mErrs);
//		Copy error matrix
{
double sig=EmxSign(kNMinPars,emx);
assert(sig>0);
}
  int n=0;  
  for (int i=0,li=0;i< kNMaxPars;li+=++i) {
    if (i == kNc) continue;
    for (int j = 0;j<=i;j++) {
      if (j == kNc) continue;
      e[li+j] = emx[n++]; 
    }
  }
//		Extend error matrix

  double Nc = mPars.mNc;
  if (Nc<0.01) Nc = 0.01;
  GFitErrs &E = mErrs;
  double Uc = mPars.mUc,Vc = mPars.mVc;
  E.qPNc =-(E.qPUc*Uc + E.qPVc*Vc)/Nc;
  E.UcNc =-(E.UcUc*Uc + E.UcVc*Vc)/Nc;
  E.VcNc =-(E.UcVc*Uc + E.VcVc*Vc)/Nc;
  E.NcNc = (E.UcUc*Uc*Uc + E.VcVc*Vc*Vc + 2*E.UcVc*Uc*Vc)/(Nc*Nc);
  E.NcU  =-(E.UcU*Uc  + E.VcU*Vc )/Nc;
  E.NcV  =-(E.UcV*Uc  + E.VcV*Vc )/Nc;

{
double sig=EmxSign(kNMaxPars,e);
assert(fabs(sig)<1e-6);
}
}
//________________________________________________________________________________
TVector3 GFull::Pos() const
{
  double xx[3];
  TCL::vmatr(&mPars.mU,mGlob.mUVN[0],xx,3,3);
  TCL::vadd(xx,mGlob.mPos,xx,3);
  return TVector3(xx);
}
//________________________________________________________________________________
TVectorD GFull::BigVal() const 
{
  TVectorD v(kNBigPars);
  v[kqPtInv] = Pti();
  for (int i=0;i<3;i++) {
    v[kDirX+i] = Dir()[i];
    v[kPosX+i] = Pos()[i];
  }
  return v;
}

//________________________________________________________________________________
double GFull::BigDer(int ib,int iu)
{
  TVectorD A(kNBigPars),B(kNBigPars),C(kNBigPars);  
  double *U = &(mPars.mqPinv);
  double delta = fabs(U[iu])*1e-2;
  if (delta<1e-6) delta = 1e-6;
  A = BigVal();
  U[iu] += delta;
  B = BigVal();
  U[iu] -= delta;
  C = (B-A); C*=(1./delta);

  double DdX = (A[kDirX]*C[kPosX]+A[kDirY]*C[kPosY]);
  double XdD = (A[kPosX]*C[kDirX]+A[kPosY]*C[kDirY]);
  double DD  = (A[kDirX]*A[kDirX]+A[kDirY]*A[kDirY]);
  double tau = -(DdX+XdD)/DD;
  C[kPosX] += A[kDirX]*tau;
  C[kPosY] += A[kDirY]*tau;
  C[kPosZ] += A[kDirZ]*tau;


  return C[ib];
}

//________________________________________________________________________________
TVector3 GFull::Dir() const
{
  double xx[3];
  TCL::vmatr(&mPars.mUc,mGlob.mUVN[0],xx,3,3);
  return TVector3(xx);

}
//________________________________________________________________________________
double GFull::SinL() const
{
  return Dir().CosTheta();
}
//________________________________________________________________________________
double GFull::CosL() const
{
  return Dir().Unit().Perp();
}
//________________________________________________________________________________
double GFull::TanL()  const
{
  return Dir()[2]/Dir().Perp();
}
//________________________________________________________________________________
double GFull::Lam() const
{
  return atan2(Dir()[2],Dir().Perp());
}
//________________________________________________________________________________
double GFull::Psi() const
{
  return Dir().Phi();
}
//________________________________________________________________________________
double GFull::Pti() const
{
  return mPars.mqPinv/CosL();
}
//________________________________________________________________________________
double GFull::Pinv() const
{
  return mPars.mqPinv;
}
//________________________________________________________________________________
double GFull::Imp() const
{
  return -Pos()[0]*sin(Psi()) + Pos()[1]*cos(Psi());
}
//________________________________________________________________________________
void GFull::FillDcaPars(StDcaGenFit &dca)
{
  TVector3 dir = Dir(),pos = Pos();
  double tau = -DOT2(pos,dir)/DOT2(dir,dir);
  pos += tau*dir;
  dca.mImp = (pos[0])*(-sin(Psi())) + (pos[1])*(cos(Psi()));
  dca.mZ   = pos[2];
  dca.mPsi = Psi();
  dca.mPti = Pti();
  dca.mTan = TanL();

}

//________________________________________________________________________________
void GFull::FillDcaErrs(StDcaGenFit &dca)
{

// D - 2d(xy) direction
// X - 2d(xy) position
// 
// D * (X+t*D) = 0		// Dca condition
// 
// dD*X + D*(dX + dt*D) = 0
// dt = -(dD*X + D*dX)/(D*D)
// D
// X +t*D
// 
// dD
// dX - D*(dD*X + D*dX)/(D*D)
// 
// PtInv = Pinv/CosL = Pinv/sqrt(Dx*Dx+Dy*Dy) = Pinv/sqrt(1-Dz*Dz)
// dPtinv = dPinv/cosL +Pinv/(cosL**2)*(dDz*Dz)
//
// D = U*uc
// Dx = U[0][0]*uc + U[1][0]*vc+ U[2][[0]*nc
// Dy = U[0][1]*uc + U[1][1]*vc+ U[2][[1]*nc
// Dz = U[0][2]*uc + U[1][2]*vc+ U[2][[2]*nc
//
// X = PosX + U[0][0]*u + U[1][0]*v + U[2][[0]*0
// Y = PosY + U[0][1]*u + U[1][1]*v + U[2][[1]*0
// Z = PosZ + U[0][2]*u + U[1][2]*v + U[2][[2]*0
//
// iU = 0-2 {DirX,DirY,DirZ}
// iU = 3-4 {PosX,PosY,PosZ} v    }
//
#define dDir_du(iX,iU) ((iU >2          )? 0:mGlob.mUVN[iU  ][iX])
#define dPos_du(iX,iU) ((iU <3 || (iU>4))? 0:mGlob.mUVN[iU-3][iX])
double bigDers[kNBigPars][kNMaxPars] = {{0}};		// derivs 
double impDers[kNMinPars][kNBigPars] = {{0}};

TVector3 dir = Dir(),pos = Pos();
double cosl = CosL(),     cos2l = cosl*cosl;
double cosp = cos(Psi()), sinp  = sin(Psi());
double pinv  = Pinv();


// 		PtInv = Pinv/CosL = Pinv/sqrt(Dx*Dx+Dy*Dy) = Pinv/sqrt(1-Dz*Dz)
// 		dPtinv = dPinv/cosL +Pinv/(cosL**2)*(dDz*Dz)
//		1/Pt = 1/(P*cosL) = 1/P 
bigDers[kqPtInv][kqPinv] = 1./cosl;
{
  double dCosIdX[3],x = dir[0],y = dir[1],z = dir[2],r2 = x*x+y*y,z2 = z*z,r=sqrt(r2),r4 = r2*r2;
  dCosIdX[0] = -r*z2*x/r4; dCosIdX[1] = -r*z2*y/r4; dCosIdX[2] = r*r2*z/r4;
  for (int iu=0;iu<3;iu++) {
    double sum = 0;
    for (int ix=0;ix<3;ix++) { sum += dCosIdX[ix]*dDir_du(ix,iu);} 
    bigDers[kqPtInv][kUc+iu] = pinv*sum;
  }
}
//		dDir
for   (int ix=0;ix<3;ix++) {
  for (int iu=0;iu<3;iu++) {bigDers[kDirX+ix][kUc+iu]=dDir_du(ix,iu);}}

// 		dX - D*(dD*X + D*dX)/(D*D)
//		dX - D*((dDx*X + dDy*Y) + (Dx*dX + Dy*dY)(cos2l)
//		dZ


for   (int ix=0;ix<3;ix++) {
  for (int iu=0;iu<5;iu++) {
    bigDers[kPosX+ix][kUc+iu] = dPos_du(ix,iu) - dir[ix]/cos2l*(
                             (pos[0]*dDir_du(0,iu) + pos[1]*dDir_du(1,iu))+
                             (dir[0]*dPos_du(0,iu) + dir[1]*dPos_du(1,iu)));}};
#if 0
{
for (int ix = 0; ix<kNBigPars;ix++) {
for (int iu = 0; iu<kNMaxPars;iu++) {
  printf("UUU(%d,%d) %g == %g\n",ix,iu,bigDers[ix][iu],BigDer(ix,iu));
}}
assert(0);
}
#endif
//		gen fit dca errs
 TCL::trasat(bigDers[0],mErrs,mBigErrs,kNBigPars,kNMaxPars);
{
double tmp = EmxSign(kNMaxPars,mBigErrs);
assert(fabs(tmp)<1e-6);
}
//		Fill myDer
  impDers[kPsi][kDirX] = - dir[1]/cos2l;
  impDers[kPsi][kDirY] =   dir[0]/cos2l;

//   dca.mImp = (pos[0]+dX)*(-sin(Psi())) + (pos[1]+dY)*(cos(Psi()));
//   dca.mZ   = pos[2]+dZ;
//   dca.mPsi = Psi();
//   dca.mPti = Pti();
//   dca.mTan = TanL();

// impact = -Pos()[0]*sin(Psi()) + Pos()[1]*cos(Psi());
impDers[kImp][kPosX] =-sinp;
impDers[kImp][kPosY] = cosp;
impDers[kImp][kDirX] = (-pos[0]*cosp-pos[1]*sinp)*impDers[kPsi][kDirX];
impDers[kImp][kDirY] = (-pos[0]*cosp-pos[1]*sinp)*impDers[kPsi][kDirY];

impDers[kPti][kqPtInv] = 1;

impDers[kTan][kDirZ] = 1./(cos2l*cosl);
impDers[kZ  ][kPosZ] = 1.;

double tmpErrs[kNMinErrs],tmpPars[kNMinPars];
TCL::trasat(impDers[0],mBigErrs,tmpErrs,kNMinPars,kNBigPars);
{
double tmp = EmxSign(kNMinPars,tmpErrs);
assert(tmp>0);
}

tmpPars[kImp] = Imp();
tmpPars[kZ  ] = Pos()[2];
tmpPars[kPsi] = Psi();
tmpPars[kPti] = Pti();
tmpPars[kTan] = TanL();
dca.set(tmpPars,tmpErrs);
}
//________________________________________________________________________________
//____________________________________________________________
double EmxSign(int n,const double *a) 
{
   long double ans=3e33;
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
   long double sum;


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
//_____________________________________________________________________________
#if 1
#include "TSystem.h"
#include "TVectorD.h"
#include "TCanvas.h"
#include "TH1F.h"
#include "TRandomVector.h"
//TVectorD TestToDca(const TVectorD &delta, const TVector3 &Pos,const TVector3 &Tk
//                  ,const double UVN[3][3]);

//______________________________________________________________________________
void GFull::TestConvertErrs()
{

enum {kNCanvs=10};
static TCanvas*    myCanvas[20] = {0};
static TH1F*       hh[100] = {0};
static int Mh[10] = {0,21,21+15,-9999};
{
static const char* kom[] = {
"6_GFI","4_GFI","5_GFI","6_GFI",
"6_IMP","4_IMP","5_IMP",
"6_BIG","4_BIG","5_BIG","6_BIG","7_BIG",
0};

static const char* tit[] = {
"qPqP",
"qPUc","_Uc_",
"qPVc","UcVc","_Vc_",
"qPNc","UcNc","VcNc","_Nc_",
"qPU"," UcU"," VcU" ,"NcU" ,"_U_",
"qPV"," UcV"," VcV" ,"NcV" ,"UV","_V_",

"_Ip_",
"ZIp",  "_Z_",
"PsiIp","PsiZ","_Psi_",
"PtiIp","PtiZ","PtiPsi","_Pti_",
"TanIp","TanZ","TanPsi","TanPti","_Tan_",

"_Pti_ ",
"PtiDirX","_DirX_",
"PtiDirY","DirXDirY","_DirY_",
"PtiDirZ","DirXDirZ","DirYDirZ","_DirZ_",
"PtiPosX","DirXPosX","DirYPosX","DirZPosX","_PosX_",
"PtiPosY","DirXPosY","DirYPosY","DirZPosY","PosXPosY","_PosY_",
"PtiPosZ","DirXPosZ","DirYPosZ","DirZPosZ","PosXPosZ","PosYPosZ","_PosZ_",
 0};

  int ic=-1,nPad=0,jPad=99;
  for (int ih=0;tit[ih];ih++) {
    if (jPad>=nPad) { ic++; jPad=0; nPad = kom[ic][0]-'0';
      TString ts(kom[ic]+2); ts+=(ic);   	
      if (!myCanvas[ic]) myCanvas[ic] = new TCanvas(ts.Data(),ts.Data(),600,800);
      myCanvas[ic]->Clear();myCanvas[ic]->Divide(1,nPad);
    }
    TString ts(tit[ih]); delete hh[ih];
    hh[ih] = new TH1F(ts.Data(),ts.Data(),100,-5,5);
    myCanvas[ic]->cd(++jPad); hh[ih]->Draw();
  }
};

//		Now test of Corr
//==========================================================================  
  double PtGev = 1.,Curv = 1./100, Mag[3] = {0,0,PtGev*Curv};
  double Pbeg[3]={PtGev,0,4./3*PtGev};
  TVector3 PbegV(Pbeg),MidV(1.,1.,1.);MidV.SetMag(1.);
//		Prepare UVN

  double UVN[3][3]={{0}};
  for (int ix=0;ix<3;ix++) { UVN[ix][ix]=1.;}
  double ang = PbegV.Angle(MidV);
  TVector3 Axi = MidV.Cross(PbegV); Axi.SetMag(1.);
  MidV.Rotate(ang,Axi);
  if (MidV.Dot(PbegV)<0) {
    MidV.Rotate(-ang,Axi);
    Axi *= -1.;
    MidV.Rotate(ang,Axi);
  }  
  assert(fabs(MidV.Dot(PbegV.Unit())-1)<1e-6);
  for (int i=0;i<3;i++) {  
    TVector3 uvn(UVN[i]);
    uvn.Rotate(ang,Axi);
    uvn.GetXYZ(UVN[i]); 
  }
//		Prepare position
  TVector3 PosV(gRandom->Rndm()+1,gRandom->Rndm()+1,gRandom->Rndm()+2);

  int icharge = (-Curv*PtGev/Mag[2]<0)? -1:1;
  PbegV.RotateZ(gRandom->Rndm()*3.1415);
  PbegV.GetXYZ(Pbeg);
  TVector3 XbegV(-PbegV[1],PbegV[0],0);
  XbegV.SetMag(gRandom->Rndm()+1);
  XbegV[2] = gRandom->Rndm();
assert(fabs(XbegV[0]*PbegV[0]+XbegV[1]*PbegV[1])<1e-5);


  TVectorD dia(kNMinPars);
  dia[0]= 0.1*PtGev; dia[1]= 0.1; dia[2]= 0.2, dia[3]= 0.3, dia[4]=0.4;
  dia*=0.1;
  for (int i=0;i<kNMinPars;i++) {dia[i]*=dia[i];}
  
  TRandomVector RV(dia);
  auto &EMX = RV.GetMtx();
  auto &val = RV.GetLam();
  dia.Print("DIA");
  val.Print("VAL");


  double gfiErrs[kNMinErrs];
  for (int i=0,li=0;i< kNMinPars;li+=++i) {
    for (int j=0;j<=i;j++) {
      gfiErrs[li+j] = EMX[i][j];
  } }

  double sig = EmxSign(kNMinPars,gfiErrs);
  assert(sig>0);  
  PosV = XbegV;		////PosV == XbegV now
  GFull gf,gf1;
  gf.SetGlob(Arr(PosV),UVN);
  gf.SetPars(icharge,PosV,PbegV);  gf.SetBigPars(icharge,PosV,PbegV);
  gf.SetErrs(gfiErrs);

  TVectorD xPars = TVectorD(kNMaxPars,gf.XtdPars());
  const double *xErrs = gf.XtdErrs();

  StDcaGenFit dca,dca1;
  gf.FillDcaPars(dca);
  gf.FillDcaErrs(dca);

  gf.SetBigPars(icharge,PosV,PbegV);
  const TVectorD &bigPars = gf.GetBigPars();
  const double *bigErrs = gf.GetBigErrs();

  gf1 = gf;
  TVectorD dcaPars(TVectorF(kNMinPars,dca.params()   )); 
  TVectorD dcaErrs(TVectorF(kNMinErrs,dca.errMatrix())); 

  int nIter=90000;
  for (int iter=0;iter<nIter;iter++) {
    TVectorD delta = RV.Gaus();
//    delta = TestToDca(delta,gf.Dir(),UVN);
    TVectorD pars1 = delta;
    int iSig=0;
    pars1 += gf.GetPars(&iSig);
    gf1.SetPars((double*)&pars1[0],iSig);
    gf1.FillDcaPars(dca1);


    TVectorD dcaPars1(TVectorF(kNMinPars,dca1.params()));     
    TVectorD xPars1  (kNMaxPars,gf1.XtdPars());

    gf1.SetBigPars(icharge,gf1.Pos(),gf1.Mom());
    const TVectorD &bigPars1 = gf1.GetBigPars();

//		Deltas
    TVectorD xDif   = xPars1  -xPars;
    TVectorD bigDif = bigPars1-bigPars;
    TVectorD dcaDif = dcaPars1-dcaPars;




    double dia[10];     
//		xDif
    for (int i=0,li=0;i< kNMaxPars;li+=++i) {
      dia[i] = sqrt(xErrs[li+i]);
      hh[Mh[0] +li+i]->Fill(xDif[i]/ dia[i]);
      for (int j = 0;j<i;j++) {
        hh[Mh[0] + li+j]->Fill((xDif[i]*xDif[j]-xErrs[li+j])/(dia[i]*dia[j]));
    } }

//		dcaDif
    for (int i=0,li=0;i< kNMinPars;li+=++i) {
      dia[i] = sqrt(dcaErrs[li+i]);
      hh[Mh[1] +li+i]->Fill(dcaDif[i]/ dia[i]);
      for (int j = 0;j<i;j++) {
        hh[Mh[1] + li+j]->Fill((dcaDif[i]*dcaDif[j]-dcaErrs[li+j])/(dia[i]*dia[j]));
    } }

//		bigDif
    for (int i=0,li=0;i< kNBigPars;li+=++i) {
      dia[i] = sqrt(bigErrs[li+i]);
      hh[Mh[2] +li+i]->Fill(bigDif[i]/ dia[i]);
      for (int j = 0;j<i;j++) {
        hh[Mh[2] + li+j]->Fill((bigDif[i]*bigDif[j]-bigErrs[li+j])/(dia[i]*dia[j]));
    } }

  }
  for (int ih=0;hh[ih];ih++) {
     const char *name = hh[ih]->GetName();
     int ent = hh[ih]->GetEntries();
     double ave = hh[ih]->GetMean();
     double rms = hh[ih]->GetRMS();
     printf("%d - %s(%d) \tAve = %g \tRms = %g\n",ih,name,ent,ave,rms);
  } 
 
 
  for (int i=0;myCanvas[i];i++) {
    myCanvas[i]->Modified();myCanvas[i]->Update();}

//

  while(!gSystem->ProcessEvents()){gSystem->Sleep(200);}; 

}

#endif //EndTest
