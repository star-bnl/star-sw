#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "TMath.h"
#include "TMatrixD.h"
#include "TVectorD.h"
#include "TCernLib.h"
#include "StvFitter.h"
#include "StvUtil/StvNodePars.h"
#include "StvHit.h"
#include "StvConst.h"
#include "StvUtil/StvDebug.h"
#include "StvUtil/StvHitErrCalculator.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"

StvFitter *StvFitter::mgFitter=0;
#define VDOT(a,b)   ( a[0]*b[0]+a[1]*b[1]+a[2]*b[2])
#define DIST(a,b)   ((a[0]-b[0])*(a[0]-b[0])+(a[1]-b[1])*(a[1]-b[1])+(a[2]-b[2])*(a[2]-b[2]))
#define DDOT(a,b,c) ((a[0]-b[0])*c[0]+(a[1]-b[1])*c[1]+(a[2]-b[2])*c[2])
#define VADD(a,b)   { a[0]+=b[0];a[1]+=b[1];a[2]+=b[2];}

enum {kDeltaFactor = 9};
static const double kMinCos = 0.5;	// minimal cos allowed

static const double kXtraBigXi2 = 9e9;

static inline double MyXi2(const double G[3],double dA,double dB)  
{
  double Gdet = G[0]*G[2]-G[1]*G[1];
  if (Gdet < 1e-11) return kXtraBigXi2;
  double Xi2 =  (G[2]*dA*dA-2*G[1]*dA*dB+G[0]*dB*dB)/Gdet;
  if (Xi2 > kXtraBigXi2) Xi2 = kXtraBigXi2;
  return Xi2;
}
double JoinTwoT(int nP1,const double *P1,const double *E1
               ,int nP2,const double *P2,const double *E2
	               ,      double *PJ,      double *EJ);
//______________________________________________________________________________
double JoinTwo(int nP1,const double *P1,const double *E1
              ,int nP2,const double *P2,const double *E2
	               ,     double *PJ,      double *EJ)
{
static int nCall = 0;  nCall++;
StvDebug::Break(nCall);
  assert(nP1<=nP2);
  int nE1 = nP1*(nP1+1)/2;
  int nE2 = nP2*(nP2+1)/2;
  TArrayD ard(nE2*5+nP2*3);
  double *a = ard.GetArray();  
  double *E1i = a;
  double *E2i = (a+=nE2);
  double *EJi = (a+=nE2);
  double *EJm = (a+=nE2);
  double *ERi = (a+=nE2);
  
  double *Pdif= (a+=nE2);
  double *P1J = (a+=nP2);
  double *P2J = (a+=nP2);

  TCL::trsinv(E1,E1i,nP1);		//E1i = 1/E1
  TCL::trsinv(E2,E2i,nP2);		//E2i = 1/E2
  TCL::vadd (E1i,E2i,EJi ,nE2);		//EJi = E1i+E2i
  TCL::trsinv(EJi,EJm,nP2);		//EJ = 1/EJi
  if (P2) TCL::vsub (P1 ,P2 ,Pdif,nP1);	//Pdif = P1-P2
  else    TCL::ucopy(P1     ,Pdif,nP1);
  TCL::trqsq(E1i,EJm,ERi ,nP1);		//ERi = E1i*EJm*E1i
  TCL::vsub (E1i,ERi,ERi ,nE1);         //ERi = E1i- E1i*EJi*E1i == 1/(E1+E2)
  double chi2;
  TCL::trasat(Pdif,ERi,&chi2,1,nP1); 

  if (!PJ)      return chi2;
  if (chi2>=kXtraBigXi2) return chi2;
  TCL::ucopy(EJm,EJ,nE2);		//EJ = 1/EJi
  
  TCL::trsa(E1i,P1,P1J,nP1,1);		//P1J = E1i*P1
  TCL::trsa(E2i,P2,P2J,nP2,1);		//P2J = E2i*P2
  TCL::vadd(P2J,P1J,P2J,nP1);		//P2J = P1J+P2J
  TCL::trsa(EJ,P2J,PJ,nP2,1);		//PJ  = EJ*P3J


  return chi2;
}
//______________________________________________________________________________
double JoinTwoT(int nP1,const double *P1,const double *E1
               ,int nP2,const double *P2,const double *E2
	               ,      double *PJ,      double *EJ)
{
///  Fit track(P2) & errors E2 with track or hit (P1) & errors E1
///  nP1 size of array P1. E1 errors packed as low triangle
///  nP2 size of array P2. E2 errors packed as low triangle
///  PJ output parameters size nP2. EJ according packed errors

  assert(nP1<=nP2);
//  int nE1 = nP1*(nP1+1)/2;
//  int nE2 = nP2*(nP2+1)/2;
  TMatrixD  E1i(nP2,nP2 ),E2i(nP2,nP2);
  for (int i=0,li=0;i< nP1;li+=++i) {
    for (int j=0;j<=i; j++) {E1i[i][j]=E1[li+j]; E1i[j][i]=E1[li+j];}}

  for (int i=0,li=0;i< nP2;li+=++i) {
    for (int j=0;j<=i; j++) {E2i[i][j]=E2[li+j]; E2i[j][i]=E2[li+j];}}

  for (int i=nP1;i<nP2;i++) {E1i[i][i]=1;};
  E1i.Invert();
  for (int i=nP1;i<nP2;i++) {E1i[i][i]=0;};
  E2i.Invert();
  TMatrixD  EJi = E1i+E2i;
  TMatrixD  EJm = EJi; EJm.Invert();
  TVectorD P1v(nP2); TCL::ucopy(P1,P1v.GetMatrixArray(),nP1);
  TVectorD P2v(nP2,P2);
  
  TVectorD PJv = EJm*((E1i*P1v) + (E2i*P2v));
  double chi2 = (PJv-P1v)*(E1i*(PJv-P1v))+(PJv-P2v)*(E2i*(PJv-P2v));
  if (!PJ) return chi2;
  TCL::ucopy(PJv.GetMatrixArray(),PJ,nP2);
  TCL::trpck(EJm.GetMatrixArray(),EJ,nP2);
  return chi2;


}
//______________________________________________________________________________
double JoinVtx(int nP1,const double *P1,const double *E1
              ,int nP2,const double *P2,const double *E2
	               ,     double *PJ,      double *EJ)
{
///  Fit track(P2) & errors E2 with vertex (P1) & errors E1
///  Track must exactly pass thru the vertex. 
///  Vertex errors added afterwards
///  nP1 size of array P1. E1 errors packed as low triangle
///  nP2 size of array P2. E2 errors packed as low triangle
///  PJ output parameters size nP2. EJ according packed errors

  assert(nP1<nP2);
  int nPBig = nP1+nP2;

  TVectorD P1t(nP1,P1),P2t(nP2,P2);
  TVectorD D = (P1t-P2t.GetSub(0,nP1-1));
  TMatrixDSym smaMx(nP1);
  TCL::trupck(E2,smaMx.GetMatrixArray(),nP1);
  smaMx.Invert();
  double Xi2 = D*(smaMx*D);
  if (!PJ) return Xi2;

  TMatrixDSym E2t(nP2);
  TCL::trupck(E2,E2t.GetMatrixArray(),nP2);
  E2t.Invert();

  TVectorD bigB(nPBig),bigP(nPBig);
  bigB.SetSub(0,E2t*P2t);
  bigB.SetSub(nP2,P1t);

  TMatrixDSym bigMx(nPBig);
  bigMx.SetSub(0,E2t);
  for (int i=nP2,j=0;i< nPBig;++i,j++) {bigMx[i][j]=1;bigMx[j][i]=1;}
  bigMx.Invert();
  bigP = bigMx*bigB;
  assert(fabs(bigP[0]-P1[0])<1e-5);
  assert(fabs(bigP[1]-P1[1])<1e-5);

//		set vertex errors (not add because in bigMx they are zeros
  TMatrixDSym E1t(nP1);
  TCL::trupck(E1,E1t.GetMatrixArray(),nP1);
  bigMx.SetSub(0,E1t);

//		To output  
  TCL::ucopy(bigP.GetMatrixArray(),PJ,nP2);
  TCL::trpck(bigMx.GetSub(0,nP2-1,0,nP2-1).GetMatrixArray(),EJ,nP2);
  return Xi2;


}



//______________________________________________________________________________
StvFitter::StvFitter(const char *name):TNamed(name,"")
{
  memset(mBeg,0,mEnd-mBeg+1);
  assert(!mgFitter);
  mgFitter = this;
}
//______________________________________________________________________________
void StvFitter::Set(const StvNodePars *inPars, const StvFitErrs *inErrs
                   ,      StvNodePars *otPars,       StvFitErrs *otErrs)
{
  memset(mBeg,'Z',mEnd-mBeg+1);
  mKase = 0;		// track + hit case
  mInPars = inPars; mInErrs = inErrs;
  mOtPars = otPars; mOtErrs = otErrs;
  mJnPars =      0; mJnErrs =      0;
}
//______________________________________________________________________________
void StvFitter::Set(const StvNodePars *inPars, const StvFitErrs *inErrs
                   ,const StvNodePars *jnPars, const StvFitErrs *jnErrs
                   ,      StvNodePars *otPars,       StvFitErrs *otErrs)
{
  mDelta *= kDeltaFactor;
  mKase = 1;		// join left & rite part of track
  mInPars = inPars; mInErrs = inErrs;
  mOtPars = otPars; mOtErrs = otErrs;
  mJnPars = jnPars; mJnErrs = jnErrs;
  mDelta  = mInPars->delta();  mDelta *= kDeltaFactor;

}
//______________________________________________________________________________
void StvFitter::Prep()
{
  mDelta  = mInPars->delta();  mDelta *= kDeltaFactor;
  mHit   = 0; mHitPlane = 0;
  double myTan = mInPars->_tanl;
  mCos2L = 1./(1+myTan*myTan);
  mCosL = sqrt(mCos2L);
  mSinL = myTan*mCosL;
  mCosP = mInPars->_cosCA;
  mSinP = mInPars->_sinCA;

  mTkPars = *mInPars;
//		Track Frame
  mDcaFrame[0][0] =  mCosL*mCosP;
  mDcaFrame[0][1] =  mCosL*mSinP;
  mDcaFrame[0][2] =  mSinL;

  mDcaFrame[1][0] = -mSinP;
  mDcaFrame[1][1] =  mCosP;
  mDcaFrame[1][2] =  0;

  mDcaFrame[2][0] = -mSinL*mCosP;
  mDcaFrame[2][1] = -mSinL*mSinP;
  mDcaFrame[2][2] =  mCosL;

}
//______________________________________________________________________________
double StvFitter::Xi2(const StvHit *hit)
{
  if (mHit == hit) return mXi2;
  mFailed = 0;
  mHit = hit;
  const float *errMtx=mHit->errMtx();
  if (errMtx) mKase=2; 		//Hit is a vertex

  mHitPlane = mHit->detector();

//	restore old parameters for nhits>1  
  mTkPars._x = mInPars->_x; mTkPars._y = mInPars->_y; mTkPars._z = mInPars->_z;

//		Hit position
  const float *hP = mHit->x();
  assert(fabs(hP[2])>1e-5);

//		Track direction
  double *tD = mDcaFrame[0];
//		Start track position
  double *tP = &mTkPars._x;


//		Distance to DCA along track in xy
//mDeltaL = DDOT(hP,tP,tD);  
//		DCA track position
  switch (mKase) {
    case 0: {
      mHitErrCalc = (StvHitErrCalculator*)mHitPlane->GetHitErrCalc();
      assert(mHitErrCalc);
      mHitErrCalc->SetTrack(tD);
      const StHitPlane *hp = hit->detector(); 
      const Mtx33F_t &hD = hp->GetDir(hit->x());
      int ans = mHitErrCalc->CalcDcaErrs(hit->x(),hD,mHitErrs);
      if (ans) {mXi2 = 1e11; return mXi2;}
    }; break;

    case 1: assert(0 && "Wrong case 1");

    case 2: {
      double d[6]={errMtx[0],errMtx[1],errMtx[2]
                  ,errMtx[3],errMtx[4],errMtx[5]};
      TCL::trasat(mDcaFrame[1],d,mHitErrs,2,3); }
  }
  assert(mHitErrs[0]>0);
  assert(mHitErrs[2]>0);
  assert(mHitErrs[2]*mHitErrs[0]>mHitErrs[1]*mHitErrs[1]);

//		Hit position wrt track 
  double dca[3] = {hP[0]-tP[0],hP[1]-tP[1],hP[2]-tP[2]};

  mDcaT=VDOT(mDcaFrame[0],dca);
  mDcaP=VDOT(mDcaFrame[1],dca);
  mDcaL=VDOT(mDcaFrame[2],dca);
//		small account non zero distance to hit along track
  double dS = mDcaT*mCosL;
  mDcaP-= 0.5*mTkPars._curv*dS*dS;

  double G[3] = {mInErrs->mHH,mInErrs->mHZ,mInErrs->mZZ};
  if (mKase==0) {for (int j=0;j<3;j++) {G[j]+=mHitErrs[j];}}

//  (BB*dX*dX-2*BA*dX*dY+AAdY*dY)/det 
  mXi2 = MyXi2(G,mDcaP,mDcaL);
  return mXi2 ; 
}  
//______________________________________________________________________________
double StvFitter::Xi2()
{
  mFailed = 0;
  double inErr = mInErrs->mHH+mInErrs->mZZ;
  double jnErr = mJnErrs->mHH+mJnErrs->mZZ;
  if (jnErr>inErr) {//Not good order
    const StvNodePars *swp = mInPars; mInPars=mJnPars; mJnPars=swp;
    const StvFitErrs  *swe = mInErrs; mInErrs=mJnErrs; mJnErrs=swe;
  }
  double qwe = (1 - mInPars->_tanl * mJnPars->_tanl);
  if (fabs(qwe)<1e-2) {mFailed=1; mXi2 = 1e11;return mXi2;}

  StvFitPars F   = (*mInPars-*mJnPars);
  double     Zero[5]= {0};
  mXi2 = JoinTwo(5,F.Arr()    ,mInErrs->Arr()
                ,5,Zero       ,mJnErrs->Arr()
		,mQQPars.Arr(),mQQErrs.Arr());
  return mXi2;
}  
//______________________________________________________________________________
int StvFitter::Update()
{
static int nCall=0; nCall++;
StvDebug::Break(nCall);
  if(mFailed>0) return mFailed;
  mFailed = 0;
  switch (mKase) {
    case 0: mFailed = Hpdate(); break;		//Hit+Track
    case 1: mFailed = Jpdate();	break; 		//Track join
    case 2: mFailed = Vpdate();	break;		//Vertex+track
  }

  double fak = 1.;
  for (int i=0;i<5;i++) {
    double f = fabs(mQQPars[i])/mDelta[i];
    if (fak<f) fak=f;
  }
  if (fak>1.) { mFailed = -1; TCL::vscale(mQQPars,1./fak,mQQPars,5);}

  *mOtPars+= mQQPars;
   mOtErrs->SetHz(mOtPars->_hz);
  return mFailed;
}
//______________________________________________________________________________
int StvFitter::Hpdate()
{
///		this is Update for track+hit fit
		
  mTkErrs = *mInErrs;


//		New Z ortogonal to X (track direction)
  StvFitPars myHitPars(mDcaP, mDcaL );
  StvFitErrs myHitErrs(mHitErrs[0],mHitErrs[1],mHitErrs[2]);
  StvFitPars myTrkPars;

  double myXi2 = JoinTwo(2,myHitPars.Arr(),myHitErrs.Arr()
                        ,5,myTrkPars.Arr(),mTkErrs.Arr()
		        ,  mQQPars.Arr(),mOtErrs->Arr());
  assert(fabs(myXi2-mXi2)<1e-1*(myXi2+mXi2+1));
  *mOtPars = mTkPars;

  return mFailed;
}  
//______________________________________________________________________________
int StvFitter::Vpdate()
{
///		this is Update for track+vertex fit
static int nCall=0; nCall++;
  mTkErrs = *mInErrs;

//		New Z ortogonal to X (track direction)
  StvFitPars myHitPars(mDcaP, mDcaL );
  StvFitPars myTrkPars;

  double myXi2 = JoinVtx(2,myHitPars.Arr(),mHitErrs
                        ,5,myTrkPars.Arr(),mTkErrs.Arr()
		        ,  mQQPars.Arr(),mOtErrs->Arr());
  if (myXi2){}
  *mOtPars = mTkPars;
  for (int i=0;i<3;i++) {mOtErrs->Arr()[i]+=mHitErrs[i];}
  return 0;
}  
//______________________________________________________________________________
int StvFitter::Jpdate()
{
///		this is Update for sub track+sub track fit (join)
  *mOtPars = *mJnPars; 
  *mOtErrs =  mQQErrs;   
  return mFailed;
}
//______________________________________________________________________________
double StvFitter::TooBig(StvFitPars &fp, int *mask) const
{
  double fakt = 0;
  int msk=0;
  for (int i=0;i<5;i++) { 
    double f = fabs(fp[i])/mDelta[i];
    if (fakt>f) continue;
    fakt=f; msk|= 1<<i;
  }
  if (mask) *mask = msk;
  return fakt;
}
//______________________________________________________________________________
void StvFitter::Test()
{
  double A[5]={1,2,3,4,5};
  double AA[15]=
  {1
  ,0,2
  ,0,0,3
  ,0,0,0,4
  ,0,0,0,0,5};
  double DA[5]={1,2,3,4,5};
  
  double B[5]={6,7,8,9,10};
  double BB[15]=
  {6
  ,0,7
  ,0,0,8
  ,0,0,0,9
  ,0,0,0,0,10};
  double DB[5]={6,7,8,9,10};
  
  double C[5],DC[5],CC[15],myC[5];
  
  for (int i=0;i<5;i++) {
    C[i] = (A[i]/DA[i]+ B[i]/DB[i])/(1/DA[i]+ 1/DB[i]);
    DC[i] = 1/(1/DA[i]+ 1/DB[i]);
    printf("%d C=%g (%g)\n",i,C[i],DC[i]);
  }
  
  
  JoinTwo(5,A,AA,5,B,BB,myC,CC);
  printf("Result =");
  for (int i=0;i<5;i++) {printf(" %g",myC[i]);}
  printf("\nError matrix =\n");
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i; j++) { printf("%g\t",CC[li+j]);}
    printf("\n");
  }


 BB[5]=1e3;BB[9]=1e3;BB[14]=1e3;

  printf("  JoinTwo(5,A,AA,5,B,BB,myC,CC)\n");
  JoinTwo(5,A,AA,5,B,BB,myC,CC);
  printf("Result =");
  for (int i=0;i<5;i++) {printf(" %g",myC[i]);}
  printf("\nError matrix =\n");
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i; j++) { printf("%g\t",CC[li+j]);}
    printf("\n");
  }
  printf("  JoinTwo(2,B,BB,5,A,AA,myC,CC)\n");
  JoinTwo(2,B,BB,5,A,AA,myC,CC);
  printf("Result =");
  for (int i=0;i<5;i++) {printf(" %g",myC[i]);}
  printf("\nError matrix =\n");
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i; j++) { printf("%g\t",CC[li+j]);}
    printf("\n");
  }

}
