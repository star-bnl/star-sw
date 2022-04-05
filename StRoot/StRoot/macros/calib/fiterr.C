//#ifndef __CINT__
#include <stdio.h>
#include <iostream>
#include <fstream>
#include "TSystem.h"
#include "TMath.h"
#include "TBenchmark.h"
#include "TFile.h"
#include "TTree.h"
#include "TH1.h"
#include "TF1.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "TCernLib.h"
#include "TMatrixD.h"
#include "TRandom.h"
#include "TVectorD.h"
#include "TVector2.h"
#include "TVector3.h"
#include "StarRoot/TTreeIter.h"
#include "TTable.h"
#include "TInterpreter.h"
#include "TPolinom.h"
#include "THelixTrack.h"
#include <vector>
//#endif
#define SQ(X) ((X)*(X))
#define PW2(X) ((X)*(X))
#define PW3(X) ((X)*(X)*(X))

enum {kNOBAGR=0,kNDETS=4,kMINHITS=50};
static const double WINDOW_NSTD=3;

static const double kAGREE=1e-7,kSMALL=1e-9,kBIG=1.,kDAMPR=0.1;
static const int    MinErr[4][2] = {{200,200},{200,200},{30,500},{20,20}};
static char  gTimeStamp[16]={0};
class MyPull;
class HitPars_t;
int fiterr(const char *opt);
double     Fit(HitPars_t &pout); 
void AveRes(); 
double AveErr(); 
int  kind(double xl);
void DbInit();
void DbDflt();
void DbEnd();
void Init(HitPars_t &hitp);
void HInit();
void HFit();
void HEnd();
void CheckErr();
void FillPulls(int befAft);
double newPull(int iYZ,const MyPull& myRes,const HitPars_t &pars);
TBranch *tb=0;
TFile *pulFile =0;
TTree *pulTree=0;
TH1F *hz=0;
TH1F *hx=0;
TH1F    *hh[100];
TCanvas *C[10];

class myTCL {
public:
static double vmaxa (const double   *a, int na);
static double vmaxa (const TVectorD &a);
static void   vfill (double *a,double f,int na);
static void   mxmlrt(const TMatrixD &A,const TMatrixD &B,TMatrixD &X);   
static void   mxmlrtS(const TMatrixD &A,const TMatrixD &B,TMatrixD &X);   
static void   mxmlrtS(const double *A,const double *B,double *X,int nra,int nca);  
static void   eigen2(const double err[3], double lam[2], double eig[2][2]);
static double simpson(const double *F,double A,double B,int NP);
static double vasum(const double *a, int na);
static double vasum(const TVectorD &a);
static int SqProgSimple(      TVectorD &x
                       ,const TVectorD &g,const TMatrixD &G 
		       ,const TVectorD &Min		   
		       ,const TVectorD &Max,int iAkt);
};


class HitAccr {
public:
int    iDet;
double A[2][3];
double Pull[2];
double PredErr[2];
};


class MyPull {
public:
float x_g() const { return grf*cos(gpf);}
float y_g() const { return grf*sin(gpf);}
float z_g() const { return gzf         ;}

float xhg() const { return xyz[0]*cos(ang) - xyz[1]*sin(ang);}
float yhg() const { return xyz[0]*sin(ang) + xyz[1]*cos(ang);}
float zhg() const { return xyz[2]                           ;}
public:
int   trk;
float xyz[3];
float psi;
float dip;
float pt;
float ypul;
float zpul;
float yfit;
float zfit;
float pye;	//pull y errvmaxa
float pze;	//pull z err
float uyy;	//untouched y err
float uzz;	//untouched z err
float fye;	//fited y err
float fze;	//fited z err
float hye;	//hited y err
float hze;	//hited z err
float curv;	//curva
float dens;
float grf;	//  Rxy of Fit  in global Sti frame
float gpf;	//  Phi of Fit  in global Sti frame
float gzf;	//  Z   of Fit  in global Sti frame
float ang;	//  rotation angle
};

class HitPars_t {
public:
HitPars_t();
HitPars_t(const HitPars_t &fr);
const double &Err(int idx) const 	{return mDrr[idx];}
      double &Err(int idx) 		{return mDrr[idx];}
double  Err(         int iYZ,const HitAccr &accr) const;
double  Err(int iDet,int iYZ,const double A[3]) const;
const double &operator[](int i) const;
      double &operator[](int i);
HitPars_t &operator= (const HitPars_t &fr);
HitPars_t &operator* (double f);
HitPars_t &operator+=(const double *f);
HitPars_t &operator= (double f);
HitPars_t &operator+=(const TVectorD &add);
int  NPars() const 			{return mNPars    ;}
int  Len(int iDet,int iYZ=0) const      {return mLen[iDet][iYZ];}
int  Lim(int i) const ;     
const double &Min(int i) const 		{return mMin[i];}     
      double &Min(int i) 		{return mMin[i];}     
const double &Max(int i) const 		{return mMax[i];}     
      double &Max(int i) 		{return mMax[i];}     

const double &Min(int iDet,int iYZ,int i) const {return mMin[IPar(iDet,iYZ)+i];}     
const double &Max(int iDet,int iYZ,int i) const {return mMax[IPar(iDet,iYZ)+i];}     
      double &Min(int iDet,int iYZ,int i)       {return mMin[IPar(iDet,iYZ)+i];}     
      double &Max(int iDet,int iYZ,int i)       {return mMax[IPar(iDet,iYZ)+i];}     

int  IPar(int iDet,int iYZ,int *npars=0) const;
void Set(int iDet,int iYZ,int nini,const double *ini);
void Limit();
double Deriv(int npt, const MyPull *pt,TVectorD &Di,TMatrixD &Dij) const;
double DERIV(int npt, const MyPull *pt,TVectorD &Di,TMatrixD &Dij,int maxTrk=9999999);
int    Test(int npt, const MyPull *pt) 	const;
void   Print(const HitPars_t *init=0) 	const;
double Diff (const HitPars_t &init) 	const;
static void Prep(int npt, const MyPull *pt,TVectorD &Y,TVectorD &Z
                ,TVectorD &S,TVectorD &cos2Psi);
static int Test();
static void HitCond(const MyPull& myRes,HitAccr &acc);
static void Show(int npt,const MyPull *pt);
static double Dens(double rxy,int ntk); 
static double Err(const double Pars[3],int nPars,const double A[3]); 
static void   myDers( double fake, double wy, double wz, double dens
                    , double g[2][3],double G[2][3][3]);
static double myFake( double fake, double wy, double wz, double dens
                    , double Cd[3],double Cdd[3][3]);
public:
int mNDets;
int mNPars;
int mNTrks;
int mLen[kNDETS][2];
double *mPars[kNDETS][2]; //mPars[iDet][iYZ][3]
double *mErrs[kNDETS][2]; //mErrs[iDet][iYZ][3]
double  mDat[1+kNDETS*2*3];
double  mDrr[1+kNDETS*2*3];
double  mMin[1+kNDETS*2*3];
double  mMax[1+kNDETS*2*3];

private:
static void myTrans( double fake,double wy, double wz, double dens, TMatrixD *mx);
};
HitPars_t operator+(const HitPars_t &a,const double    *add);
HitPars_t operator+(const HitPars_t &a,const TVectorD &add);




//______________________________________________________________________________
class Poli2 {
public:
enum {kXMAX=200};
Poli2(int npw=1);
Poli2(int npw,int n,const double *X,const double *Y,const double *W);
void Init();
void Clear();
void Add(double x, double y, double w);
double Xi2() const  			{return fXi2;}
double Xi2(int ipt) const; 		
double EvalXi2() const; 		
int    Pw()  const 			{return fPw ;}
int    NPt() const 			{return fN  ;}
double Fun( double x ) const;
double dXi2dW( int ipt ) const;
double d2Xi2d2W( int ipt,int jpt ) const;
double Deriv( TVectorD &Di, TMatrixD &Dij ) const;
void   TestIt() const;
void   MyTest() const;
static void Test();
private:
int fPw;
char   fBeg[1];
int    fN;
double fX[kXMAX];
double fY[kXMAX];
double fW[kXMAX];
double fXi2;
double fX0,fY0;
char   fEnd[1];
TVectorD fP;
TVectorD fB;
TMatrixD fA;
TMatrixD fAi;
}; 



#if !defined(__MAKECINT__)





std::vector<MyPull> MyVect;
  double aveRes[4][6],aveTrk[4][2][3];
  int    numRes[4];
  double pSTI[8][3] =      {{0.000421985  ,0.00124327 ,0.0257111  }
                      	   ,{0.000402954   ,0.00346896 ,0.0377259 }
                           ,{0.            ,0.0009366  ,0.0004967 }
                           ,{0.00018648    ,0.00507244 ,0.002654  }
                           ,{0.0030*0.0030 ,0.0        ,0.0       }
                           ,{0.0700*0.0700 ,0.0        ,0.0       }
                           ,{0.0080*0.0080 ,0.0        ,0.0       }
                           ,{0.0080*0.0080 ,0.0        ,0.0       }};
HitPars_t HitErr;

static const char *DETS[]={"OutY","OutZ","InnY","InnZ"
                          ,"SsdY","SsdZ","SvtY","SvtZ"};
static const char *DETZ[]={"Out" ,"Inn","Ssd","Svt"};
  int FitOk[4]={0,0,0,0};
static char dbFile[4][100] = {
"StarDb/Calibrations/tracker/tpcOuterHitError.20050101.235959.C",  
"StarDb/Calibrations/tracker/tpcInnerHitError.20050101.235959.C",  
"StarDb/Calibrations/tracker/ssdHitError.20050101.235959.C"     , 
"StarDb/Calibrations/tracker/svtHitError.20050101.235959.C"    };

static TTable *dbTab[4];
void myBreak(int kase) { 
  static int myKase=-1946; 
  if (kase != myKase) return;
  printf("BOT OHO\n");
}

int fiterr(const char *opt)
{
  if (!opt) opt = "H";
  int optH = strstr(opt,"H")!=0;
  int optU = strstr(opt,"U")!=0;
  int optT = strstr(opt,"T")!=0;
  memcpy(gTimeStamp,strstr(opt,"20"),15);
  
  DbInit();
  if (optH) HInit();
  TTreeIter th(pulTree);
  th.AddFile("pulls.root");
//  th.ls();
//  return;
  const int &run = th("mRun");
  const int &evt = th("mEvt");
//const int   *&mNTrks   = th("mNTrks[2]");
  const int   &mTrksG    = th("mTrksG");
  const int   &nGlobs    = th("mHitsG");
  const float *&mCurv    = th("mHitsG.mCurv");
  const float *&mPt      = th("mHitsG.mPt");
  const short *&mTrackNumber    = th("mHitsG.mTrackNumber");
  const float *&mNormalRefAngle = th("mHitsG.mNormalRefAngle");
  const UChar_t *&nTpcHits = th("mHitsG.nTpcHits");
  const UChar_t *&nSsdHits = th("mHitsG.nSsdHits");
  const UChar_t *&nSvtHits = th("mHitsG.nSvtHits");
  const float *&lYPul = th("mHitsG.lYPul");
  const float *&lZPul = th("mHitsG.lZPul");
  const float *&lXHit = th("mHitsG.lXHit");
  const float *&lYHit = th("mHitsG.lYHit");
  const float *&lZHit = th("mHitsG.lZHit");
  const float *&lYFit = th("mHitsG.lYFit");
  const float *&lZFit = th("mHitsG.lZFit");
  const float *&lPsi  = th("mHitsG.lPsi");
  const float *&lDip  = th("mHitsG.lDip");
  const float *&lYFitErr = th("mHitsG.lYFitErr");
  const float *&lZFitErr = th("mHitsG.lZFitErr");
  const float *&lYHitErr = th("mHitsG.lYHitErr");
  const float *&lZHitErr = th("mHitsG.lZHitErr");
  const float *&lYPulErr = th("mHitsG.lYPulErr");
  const float *&lZPulErr = th("mHitsG.lZPulErr");
  const float *&gRFit    = th("mHitsG.gRFit");
  const float *&gPFit    = th("mHitsG.gPFit");
  const float *&gZFit    = th("mHitsG.gZFit");

  printf("*lYPul = %p\n",lYPul);
  printf("&run=%p &evt=%p\n",&run,&evt);
  MyPull mp;   
  MyVect.clear();
  int nTpc=0,nPre=0,iTk=-1,iTkSkip=-1;
  while (th.Next()) 
  {
//  printf("serial = %d run=%d evt=%d nGlo = %d\n",n,run,evt,nGlobs);n++;
  float rxy=0;
  for (int ih=0;ih<nGlobs;ih++) { 
    if (mTrackNumber[ih]==iTkSkip) 		continue;
    float rxy00 = rxy; rxy=lXHit[ih];
    if (mTrackNumber[ih]!=iTk || rxy<rxy00) {
      iTk = mTrackNumber[ih];
      iTkSkip = iTk;
      if (nTpcHits[ih]    <25)   		continue;
      if (fabs(mCurv[ih])>1./200) 		continue;
      if (fabs(mPt[ih]/cos(lDip[ih]))<0.5) 	continue;
      int nSS = nSsdHits[ih]+nSvtHits[ih];
      if (nSS && nSS<2) 			continue;
      if (!nSS && nPre && nTpc>100 && nTpc>3*nPre) 	continue;
      nTpc += (nSS==0);
      nPre += (nSS!=0);
      iTkSkip = -1;
    }

    memset(&mp,0,sizeof(mp));
    mp.trk = mTrackNumber[ih];
    mp.pye = lYPulErr[ih];
    mp.pze = lZPulErr[ih];
    float uyy = (lYPulErr[ih]-lYHitErr[ih])*(lYHitErr[ih]+lYPulErr[ih]);
//    printf ("yy = %g %g %g \n",yy,lYHitErr[ih],lYPulErr[ih]);
//    if (uyy <1e-8     ) continue;
    float uzz = (lZPulErr[ih]-lZHitErr[ih])*(lZHitErr[ih]+lZPulErr[ih]);
//    printf ("zz = %g\n",zz);
//    if (uzz <1e-8     ) continue;
    mp.uyy = uyy;
    mp.uzz = uzz;
    mp.fye = lYFitErr[ih];
    mp.fze = lZFitErr[ih];
    mp.hye = lYHitErr[ih];
    mp.hze = lZHitErr[ih];
    mp.xyz[0] = lXHit[ih];
    if (mp.xyz[0]<4) continue;
    mp.xyz[1] = lYHit[ih];
    mp.xyz[2] = lZHit[ih];
    mp.psi    = lPsi[ih] ;
    mp.dip    = lDip[ih] ;
    mp.ypul =   lYPul[ih];
//    if (fabs(mp.ypul)<1e-8) continue;
    mp.zpul =   lZPul[ih];
//    if (fabs(mp.zpul)<1e-8) continue;
    mp.yfit =   lYFit[ih];
    mp.zfit =   lZFit[ih];
    mp.dens =   HitPars_t::Dens(mp.xyz[0],mTrksG);
    mp.grf  =   gRFit[ih];
    mp.gpf  =   gPFit[ih];
    mp.gzf  =   gZFit[ih];
    mp.curv =   mCurv[ih];
    mp.ang  =   mNormalRefAngle[ih];

    MyVect.push_back(mp);

  }
//  printf("lYPul[0]=%g\n",lYPul[0]);
  }
  printf("fiterr:: %d hits accepted\n\n",MyVect.size());
  printf("fiterr:: %d Tpc and %d Svt/Ssd tracks accepted\n\n",nTpc,nPre);
  AveRes();
  HFit();
  DbDflt();
  Init(HitErr);
  if (optH) CheckErr();
  if (optH) FillPulls(0);
  if (optT) return HitErr.Test(MyVect.size(),&(MyVect[0]));

  double maxPct = Fit(HitErr);
  maxPct= AveErr();
  if (optH) FillPulls(1);
  if (optU) DbEnd();
  if (optH) HEnd();
//return (maxPct<10) ? 99:0;
  return (maxPct< 3) ? 99:0;
}
//______________________________________________________________________________
Poli2::Poli2(int npw):fP(npw+1),fB(npw+1),fA(npw+1,npw+1),fAi(npw+1,npw+1)
{
assert(0);
  fPw = npw;
  Clear();  
  
}
//______________________________________________________________________________
void Poli2::Clear()
{
  memset(fBeg,0,fEnd-fBeg+1);  
}
//______________________________________________________________________________
Poli2::Poli2(int npw,int N,const double *X,const double *Y,const double *W)
:fP(npw+1),fB(npw+1),fA(npw+1,npw+1),fAi(npw+1,npw+1)
{
assert(0);
  fPw = npw;
  Clear();  
  fN = N;
  int nb = N*sizeof(double);
  memcpy(fX,X,nb);
  memcpy(fY,Y,nb);
  memcpy(fW,W,nb);
}
//______________________________________________________________________________
void Poli2::Add(double x, double y, double w)
{ 
  fX[fN]=x; fY[fN]=y, fW[fN]=w; fN++;
  assert(fN<=kXMAX);
}
//______________________________________________________________________________
void Poli2::Init()
{ 
  double Wt=0,Wx=0,Wy=0;
  for (int i=0;i<fN;i++) { Wt += fW[i]; Wx += fW[i]*fX[i];Wy += fW[i]*fY[i];}
  fX0 = Wx/Wt; fY0 = Wy/Wt;
  for (int i=0;i<fN;i++) { fX[i]-=fX0; fY[i]-=fY0;}

  double A[3][3]={{0}},B[3]={0};
  double x[3]={1};
  fXi2=0;
  for (int i=0;i<fN;i++) {
    double w = fW[i];
    x[1] = fX[i]; x[2]=x[1]*x[1];
    double y = fY[i],y2=y*y;
    for (int j=0;j<=fPw;j++) { B[j]    +=w*x[j]*y;
    for (int k=0;k<=j  ;k++) { A[j][k] +=w*x[j]*x[k];}}
    fXi2 +=w*y2;
  }
  for (int j=0;j<=fPw;j++){ fB[j] = B[j];
  for (int k=0;k<=j  ;k++){ fA(j,k) = A[j][k]; fA(k,j)=A[j][k];}}

  double det=0;
  fAi=fA;fAi.InvertFast(&det);
  fP=fAi*fB;
  fXi2 -= (fB*fP);

}
//______________________________________________________________________________
double Poli2::Fun( double x ) const
{
  x -= fX0;
  double xx[3]={1,x,x*x};
  TVectorD XX(fPw+1,xx);
  return fP*XX + fY0;
}
//______________________________________________________________________________
double Poli2::Xi2(int ipt) const
{
   double dy = Fun(fX[ipt]+fX0) - (fY[ipt]+fY0);
   return dy*dy*fW[ipt];
}
//______________________________________________________________________________
double Poli2::EvalXi2() const
{ 
  double sum=0;
  for (int ipt=0;ipt<fN;ipt++) {sum+=Xi2(ipt);}
  return sum;
}
//______________________________________________________________________________
double Poli2::dXi2dW( int ipt ) const
{
//   Xi2 = B*Ai*B
//  dXi2 = 2*dB*Ai*B - B*Ai*dA*Ai*B
//  dXi2 = 2*dB*Ai*B - P*dA*P

   double der = fY[ipt]*fY[ipt];
   TVectorD dB(fPw+1);
   TMatrixD dA(fPw+1,fPw+1);
   double x[3]={1,fX[ipt],fX[ipt]*fX[ipt]};
   for (int j=0;j<=fPw;j++) { dB[j] = x[j]*fY[ipt];   
   for (int k=0;k<=fPw;k++) { dA(j,k) = x[j]*x[k]; }}  
   der -= (2*(dB*(fAi*fB))-(fP*(dA*fP)));
   return der;
}
//______________________________________________________________________________
//______________________________________________________________________________
double Poli2::d2Xi2d2W( int ipt,int jpt ) const
{
//  P = Ai*B
//  dXi2/dWi = 2*diB*Ai*B - B*Ai*diA*Ai*B
//  d2Xi2/dWi/dWj = -2*diB*Ai*djA*Ai*B
//                  +2*diB*Ai*djB
//                  -2*djB*Ai*diA*Ai*B
//                  +2*B*Ai*djA*Ai*diA*Ai*B
//  d2Xi2/dWi/dWj = -2*diB*Ai*djA*P
//                  +2*diB*Ai*djB
//                  -2*djB*Ai*diA*P
//                  +2*P*djA*Ai*diA*P

   double xi[3]={1,fX[ipt],fX[ipt]*fX[ipt]};
   double xj[3]={1,fX[jpt],fX[jpt]*fX[jpt]};

   TVectorD diB(fPw+1)      ,djB(fPw+1);
   TMatrixD diA(fPw+1,fPw+1),djA(fPw+1,fPw+1);
   for (int j=0;j<=fPw;j++) { diB[j]=fY[ipt]*xi[j];   djB[j]=fY[jpt]*xj[j];
   for (int k=0;k<=fPw;k++) { diA(j,k) = xi[j]*xi[k]; djA(j,k) = xj[j]*xj[k];}}  
   double der = -2*(-((fAi*diB)*(djA*fP))
                    +(diB*(fAi*djB))
		    -((fAi*djB)*(diA*fP))
                    +(fP*(djA*(fAi*(diA*fP)))));
   return der;
}
//______________________________________________________________________________
double Poli2::Deriv( TVectorD &Di, TMatrixD &Dij ) const
{
  Di.ResizeTo(fN);
  Dij.ResizeTo(fN,fN);
  TVectorD diB(fPw+1)      ,djB(fPw+1);
  TMatrixD diA(fPw+1,fPw+1),djA(fPw+1,fPw+1);
  for (int ipt=0;ipt<fN;ipt++) {
    double der = fY[ipt]*fY[ipt];
    double xi[3]={1,fX[ipt],fX[ipt]*fX[ipt]};
    for (int k=0;k<=fPw;k++) { diB[k]   = fY[ipt]*xi[k];
    for (int l=0;l<=fPw;l++) { diA(k,l) = xi[k]*xi[l];}}  
    der -= (2*(diB*(fAi*fB))-(fP*(diA*fP)));
    Di[ipt] = der;
    for (int jpt=0;jpt<=ipt;jpt++) {
      double xj[3]={1,fX[jpt],fX[jpt]*fX[jpt]};
      for (int k=0;k<=fPw;k++) { djB[k]   = fY[jpt]*xj[k];
      for (int l=0;l<=fPw;l++) { djA(k,l) = xj[k]*xj[l];}}  
      der = -2*(-((fAi*diB)*(djA*fP))
                +(diB*(fAi*djB))
	        -((fAi*djB)*(diA*fP))
                +(fP*(djA*(fAi*(diA*fP)))));
      Dij[ipt][jpt]=der;
      Dij[jpt][ipt]=der;
    } 
  }
  return Xi2();
}

//______________________________________________________________________________
void Poli2::MyTest() const
{
  printf("Print B\n");
  fB.Print();
  printf("Print A\n");
  fA.Print();
  printf("Print Ai\n");
  fAi.Print();
  printf("Print A*Ai\n");
  TMatrixD tst(fPw+1,fPw+1);
  tst = fA*fAi;
  tst.Print();
}
//______________________________________________________________________________
void Poli2::Test()
{
  int npw=2;
  double X[20],Y[20],W[20];
  for (int i=0;i<20;i++) {
    X[i]=i;
    Y[i]= 3+X[i]*(.02+.03*X[i]);
    W[i]= 1-0.01*i;
    Y[i]+=gRandom->Gaus(0,sqrt(1./W[i]));
  }
  Poli2 pp(npw,20,X,Y,W);
  pp.Init(); 
//  pp.MyTest();
  double Xi2 = pp.Xi2();
  double Xi2Eva = pp.Xi2();
  printf ("Xi2 = %g == %g\n",Xi2,Xi2Eva);
  for (int i=0; i<20; i++) {
    printf(" %g %g : %g\n",X[i],Y[i],pp.Fun(X[i]));
  }
// check d/dW
  printf("\n check d/dWi\n");
  double delta = 1e-3;
  for (int k=0;k<20;k++) {
    Poli2 ppp(npw);
    for (int i=0;i<20;i++) {
      double w = W[i];
      if (i==k) w+=delta;
      ppp.Add(X[i],Y[i],w);
    }
    ppp.Init();
    double ana = pp.dXi2dW(k);
    double num = (ppp.Xi2()-pp.Xi2())/delta;
    double dif = 2*(num-ana)/(fabs(num+ana)+3e-33);
    if (fabs(dif)<0.01) continue;
    printf ("dXi2dW(%2d) \tana=%g \tnum = %g \tdif=%g\n",k,ana,num,dif);
  }
// check d2/dWi/DWj
  printf("\n check d2/dWi/DWj\n");
  for (int i=0;i<20;i++) {
    double sav = W[i]; W[i]=sav+delta;
    Poli2 ppp(npw,20,X,Y,W);
    W[i]=sav;
    ppp.Init();
    for (int j=0;j<20;j++) {
      double ana = pp.d2Xi2d2W(i,j);
      double num = (ppp.dXi2dW(j)-pp.dXi2dW(j))/delta;
      double dif = 2*(num-ana)/(fabs(num+ana)+3e-33);
      if (fabs(dif)<0.01) continue;
      printf ("d2Xi2d2W(%2d,%2d) \tana=%g \tnum = %g \tdif=%g\n",i,j,ana,num,dif);
  } }
  TVectorD g;
  TMatrixD gg;
  pp.Deriv(g,gg);
  for (int i=0;i<20;i++) {
    double tst = pp.dXi2dW(i)-g[i];
    if (fabs(tst)>1e-10) printf("g[%d] %g **************\n",i,tst);
    for (int j=0;j<20;j++) {
      double tst = pp.d2Xi2d2W(i,j)-gg[i][j];
      if (fabs(tst)>1e-10) printf("gg[%d][%d] %g **************\n",i,j,tst);
  } }


}
//______________________________________________________________________________
void Poli2::TestIt() const
{
  TVectorD g,gT;
  TMatrixD G,GT;
  Poli2 pp(*this);
  pp.Init(); 
  double Xi2 = pp.Deriv(g,G);

  double delta = 1e-3;
  for (int k=0;k<fN;k++) {
    double sav = pp.fW[k];
    pp.fW[k]=sav+delta;
    pp.Init(); 
    double Xi2T = pp.Deriv(gT,GT);
    pp.fW[k]=sav;

    double ana = 0.5*(g[k]+gT[k]);
    double num = (Xi2T-Xi2)/delta;
    double dif = 2*(num-ana)/(fabs(num+ana)+1e-10);
    if (fabs(dif)>0.01)
    printf ("\ndXi2dW(%2d) \tana=%g \tnum = %g \tdif=%g\n\n",k,ana,num,dif);
// check d2/dWi/DWj
    for (int i=0;i<fN;i++) {
      ana = 0.5*(GT[k][i]+G[k][i]);
      num = (gT[i]-g[i])/delta;
      dif = 2*(num-ana)/(fabs(num+ana)+1e-10);
      if (fabs(dif)>0.01)
      printf ("d2Xi2dW2(%2d,%2d) \tana=%g \tnum = %g \tdif=%g\n",k,i,ana,num,dif);
  } }


}
//______________________________________________________________________________
void poli2()
{
Poli2::Test();
}
//______________________________________________________________________________
void Init(HitPars_t &hiterr)
{
  hiterr[0] = 0.0;
  for (int iDet=0;iDet<kNDETS;iDet++) {
    if (numRes[iDet]<kMINHITS) continue;
    int n = (iDet<=1)? 3:1;
    hiterr.Set(iDet,0,n,pSTI[iDet*2+0]);
    hiterr.Set(iDet,1,n,pSTI[iDet*2+1]);
    hiterr.Min(iDet,0,0) = pow(MinErr[iDet][0]*1.e-4,2);
    hiterr.Min(iDet,1,0) = pow(MinErr[iDet][1]*1.e-4,2);
  }
}
//______________________________________________________________________________
//______________________________________________________________________________
class FitState_t : public HitPars_t { 
  public: 
  FitState_t();
  FitState_t(HitPars_t &hp);
  const HitPars_t &Pars() const { return (const HitPars_t &)(*this);} 
        HitPars_t &Pars()       { return (      HitPars_t &)(*this);} 
  int    Saddle() const {return neg<0;}
  int    LimX(int i) const; 
  double Der() const {return der;}
  double Fcn() const {return fcn;}
  void   Deriv(const std::vector<MyPull> &MyVect);
  int    MaxStp(TVectorD &add,int mode) const;
  void   MakeErrs();
  int operator<(const FitState_t &other) const;
FitState_t &operator=(const FitState_t &other);
static int FixWeak( TVectorD &g, TMatrixD &G); 
  public: 
  TVectorD g;
  TMatrixD G;
  char   myBeg[1];
  int    npt;
  int    ider;
  double fak;
  double fcn;
  double der;
  double neg;
  char   myEnd[1];
}; 
//______________________________________________________________________________
FitState_t::FitState_t() 
{
  memset(myBeg,0,myEnd-myBeg+1);
  der = 1e99;fcn = 1e99;neg =-1e99;
}
//______________________________________________________________________________
FitState_t::FitState_t(HitPars_t &hp) :HitPars_t(hp)
{
  memset(myBeg,0,myEnd-myBeg+1);
  der = 1e99;fcn = 1e99;neg =-1e99;
}
//______________________________________________________________________________
void FitState_t::Deriv(const std::vector<MyPull> &MyVect)
{
  npt = MyVect.size();
  fcn = DERIV(npt,&(MyVect[0]),g,G);
  fcn/=npt;
  der = -1; ider =-1; neg = 0;
  for (int i=0;i<mNPars;i++)  {
    if (G[i][i]<neg) neg =G[i][i];
    if(LimX(i))	continue;
    if (fabs(g[i])>der) { ider = i; der = fabs(g[i]);}
  }
  der/=npt;
  double myMax=myTCL::vmaxa(G.GetMatrixArray(),G.GetNoElements());
  fak = pow(2.,-int(log(myMax)/log(2.)));
  G*=fak; g*=fak; 
  if (neg>=0) { //Check positiveness
    for (int i = 0; i<mNPars && neg>0; i++){
      for (int j = 0; j<i && neg>0; j++) 	{
        if (pow(G[i][j],2) >= G[i][i]*G[j][j]) neg = -1;
    } }  
  }
}


//______________________________________________________________________________
int FitState_t::operator<(const FitState_t &old) const
{
//if (Pos()<old.Pos()) return 1;
  double delta = (Fcn()-old.Fcn())/Fcn();

  if (Der()<0        ) 				return 0;
  if (Fcn()<old.Fcn()) 				return 1;
  if (Der()<old.Der() 
  &&  Fcn()-old.Fcn()-0.1*fabs(old.Fcn())<0)	return 1;
  return 0;
}
//______________________________________________________________________________
FitState_t &FitState_t::operator=(const FitState_t &other) 
{
  memcpy(myBeg,other.myBeg,myEnd-myBeg+1);
  g.ResizeTo(other.g);
  G.ResizeTo(other.G);
  g = other.g;
  G = other.G;
  HitPars_t::operator=(other);
  return *this;
}
//______________________________________________________________________________
int FitState_t::MaxStp(TVectorD &add,int mode) const
{ 
  int lim=0;
  double fak=1;
  for (int i=0;i<mNPars;i++) {
    double maxStp = (0.01+mDat[i])*0.3;
    if (maxStp>fak*fabs(add[i])) continue;
    lim = lim*100+i+1;
    if (!mode) { add[i] = (add[i]<0)? -maxStp:maxStp;}
    else       { fak = maxStp/fabs(add[i]);}
  }
  if (fak<1) add*=fak;
  return lim;
}
//______________________________________________________________________________
void FitState_t::MakeErrs() 
{
//	Evaluate errors
  TMatrixD E(G); 
  for (int i=0;i<mNPars;i++) {
    if (!Lim(i)) continue;
    for (int j=0;j<mNPars;j++) {E(i,j) = 0; E(j,i) = 0;}; E(i,i) = 1; 
  }
  double det=12345; E.InvertFast(&det);
  E*=fak;
  for (int i=0;i<mNPars;i++) {Err(i) = sqrt(E(i,i));}
}
//______________________________________________________________________________
int FitState_t::LimX(int i) const 
{
  int lim = Lim(i);
  if (!lim) return 0;
  if (lim<0 && g[i]<0) return 0;
  if (lim>0 && g[i]>0) return 0;
  return lim;
}
//______________________________________________________________________________
int FitState_t::FixWeak( TVectorD &g, TMatrixD &G) 
{
  int n = g.GetNrows();
  int nfix = 0;
  double ave = myTCL::vasum(g)/n;
  for (int i=0;i<n;i++) {
    if (fabs(g[i])>=ave) continue;
    nfix++; g[i]=0;
    for (int j=0;j<n;j++) {G[i][j]=0; G[j][i]=0;}
    G[i][i]=1;
  }
  return nfix;
}
//______________________________________________________________________________
double Fit(HitPars_t &pout)
{
  enum {kMAXITER=10000};
static int const MAXCUT[2]={4,10};
static int kount=0;

  int ifail = 99,nPars,iAkt=0,iter,nCut,lim=0,con=0;
  nPars       = pout.NPars();
  HitPars_t init(pout);
  init.Limit();
  
  TVectorD add(nPars),g(nPars),ge(nPars),Gg(nPars);
  FitState_t Best(init),Now(init);
  double dif,difFcn=0,difDer;

  static int idebug = 1; 

  iAkt=0;nCut=0;

  for (iter=0;iter<kMAXITER;iter++) {// Iterations
      kount++;
      Now.Deriv(MyVect);
      difFcn = Now.fcn-Best.fcn; 
      difDer = Now.der-Best.der; 
      if (idebug) {
        printf("Fit(%3d) \tFcn = %g(%g) \tDer(%2d)=%g(%g) \tLim=%d \tCon=%d\tAkt=%d Cut=%d Sad=%d\n"
              ,iter,Now.fcn,difFcn,Now.ider,Now.der,difDer,lim,con,iAkt,nCut,Now.Saddle());
      }
      if (Now.Der()<0) {ifail=1; break;
      } else if (Now.Der() < kAGREE) {
        Best=Now; ifail=0;  break;

      } else if (Now < Best ) {//Accept
        nCut = 0; Best = Now; iAkt = 0;

      } else if (nCut>MAXCUT[iAkt]) {//Cut step failed
        if (iAkt==0) Now=Best;
        Best=Now; nCut=0; iAkt=1-iAkt;

      } else {
        nCut++;
        add*= 0.5;
        Now.Pars() = Best.Pars()+add;
        continue;
      }
	    
//		SOLVING

//		Sometimes try grad method randomly
static int rnd=0; rnd++;
      if (!(rnd&3))  iAkt=1;

//      if (Now.Saddle() && iter&1)iAkt=1;
      TVectorD P0(nPars,&Best[0]);
      for (int jAkt=iAkt;jAkt<2;jAkt++) {
        iAkt = jAkt;
        TVectorD P1(P0);
        con = myTCL::SqProgSimple(P1,Now.g,Now.G 
		                ,TVectorD(nPars,&Now.Min(0))		   
		                ,TVectorD(nPars,&Now.Max(0)),jAkt);
        if (con<0)  	continue;
        add = P1-P0;
        double along = -(add*Now.g);
        if (along <0) 	continue;
        lim=Best.MaxStp(add,1);
        Now.Pars() = Best.Pars()+add;
        break;
      } 
  }// End Iters

  if (ifail==0 || ifail==99) Best.MakeErrs();
  pout=Best.Pars();

  dif = pout.Diff(init);
  printf("\nFit: Iter=%d Fcn=%g Der=%g Dif=%6.3g%% Fail=%d\n"
        ,iter,Best.fcn,Best.der,dif,ifail);
  pout.Print(&init);
  return (ifail)? 1e10:dif;
 
}

//______________________________________________________________________________
void AveRes() 
{
  memset(aveRes[0]   ,0,sizeof(aveRes));
  memset(aveTrk[0][0],0,sizeof(aveTrk));
  memset(numRes   ,0,sizeof(numRes));
  for (int jhit=0; jhit<(int)MyVect.size(); jhit++) {
     MyPull &myRes = MyVect[jhit];
     int jdx = kind(myRes.xyz[0]);
     aveRes[jdx][0]+= myRes.ypul*myRes.ypul;
     aveRes[jdx][1]+= myRes.zpul*myRes.zpul;
     aveRes[jdx][2]+= myRes.uyy;
     aveRes[jdx][3]+= myRes.uzz;
     aveRes[jdx][4]+= pow(myRes.xyz[1]-myRes.yfit,2);
     aveRes[jdx][5]+= pow(myRes.xyz[2]-myRes.zfit,2);
     if (hh[jdx*2+0+30]) hh[jdx*2+0+30]->Fill(myRes.ypul);
     if (hh[jdx*2+1+30]) hh[jdx*2+1+30]->Fill(myRes.zpul);
     numRes[jdx]++;
     HitAccr ha;
     HitPars_t::HitCond(myRes,ha);
     TCL::vadd(aveTrk[jdx][0],ha.A[0],aveTrk[jdx][0],3);
     TCL::vadd(aveTrk[jdx][1],ha.A[1],aveTrk[jdx][1],3);

  }
//		CleanUp
  int ihit=0;
  for (int jhit=0; jhit<(int)MyVect.size(); jhit++) {
     MyPull &myRes = MyVect[jhit];
     int jdx = kind(myRes.xyz[0]);
     if (numRes[jdx]<kMINHITS) continue;
     aveRes[jdx][0]+= myRes.ypul*myRes.ypul;
     if (ihit!=jhit) MyVect[ihit]=MyVect[jhit];
     ihit++;
  }
  MyVect.resize(ihit);

  for(int jdx=0;jdx<4;jdx++) {
    if (numRes[jdx]<kMINHITS) continue;
    double f = 1./numRes[jdx];
    TCL::vscale(aveRes[jdx],f,aveRes[jdx],6);
    TCL::vscale(aveTrk[jdx][0],f,aveTrk[jdx][0],6);
    double hitYErr,hitZErr;
    TCL::vsub(aveRes[jdx]+0,aveRes[jdx]+2,aveRes[jdx]+2,2);
    hitYErr = aveRes[jdx][2];
    hitYErr = (hitYErr<0)? -sqrt(-hitYErr):sqrt(hitYErr);
    hitZErr = aveRes[jdx][3];
    hitZErr = (hitZErr<0)? -sqrt(-hitZErr):sqrt(hitZErr);
    printf("AveRes:: N=%5d \taveRes[%s][yz]=%g %g \tavePul[yz]=%g %g \thitErr(yz)=%g %g\n\n"
      ,numRes[jdx],DETZ[jdx]
      ,sqrt(aveRes[jdx][4]),sqrt(aveRes[jdx][5])
      ,sqrt(aveRes[jdx][0]),sqrt(aveRes[jdx][1])
      ,hitYErr,hitZErr);
    


  }
}
//______________________________________________________________________________
int kind(double x) 
{
static const double radds[5]={120,25,16,0,0};
  for(int jdx=0;1;jdx++) {if (x>radds[jdx]) return jdx;}
}
//___________________________________myRes___________________________________________
void HInit()
{
  memset(hh,0,sizeof(hh));
  memset(C,0,sizeof(C));
//  Pulls before and after
static const char *NamPuls[]={
"YOut.bef","YOut.aft","ZOut.bef","ZOut.aft",
"YInn.bef","YInn.aft","ZInn.bef","ZInn.aft",
"YSsdBef","YSsdAft","ZSsdBef","ZSsdAft",
"YSvtBef","YSvtAft","ZSvtBef","ZSvtAft"};  
static const char *NamRes[]={
"YOut.res","ZOut.res",
"YInn.res","ZInn.res",
"YSsd.res","ZSsd.res",
"YSvt.res","ZSvt.res"};  

  for (int ih=0;ih<16;ih++) {
    hh[ih] = new TH1F(NamPuls[ih],NamPuls[ih],100,-3,3);}
  C[0] = new TCanvas("C0","",600,800);
  C[0]->Divide(1,8);
  C[1] = new TCanvas("C1","",600,800);
  C[1]->Divide(1,8);
  for (int ih=0;ih<8;ih++) {C[0]->cd(ih+1);hh[ih+0]->Draw();}
  for (int ih=0;ih<8;ih++) {C[1]->cd(ih+1);hh[ih+8]->Draw();}

  hh[20]= new TH1F("ChekErr","ChekErr",100,-100.,100.);   
  hh[21]= new TH1F("ChekPul","ChekPul",100,-100.,100.);   
  C[2]  = new TCanvas("C2","",600,800);
  C[2]->Divide(1,2);
  for (int ih=0;ih<2;ih++) {C[2]->cd(ih+1);hh[ih+20]->Draw();}
//  hh[20]->Draw();

  for (int ih=0;ih<8;ih++) {
    hh[ih+30] = new TH1F(NamRes[ih],NamRes[ih],100,-0.3,0.3);}
  C[3] = new TCanvas("C3","",600,800);
  C[3]->Divide(1,8);
  for (int ih=0;ih<8;ih++) {C[3]->cd(ih+1);hh[ih+30]->Draw();}

}

//______________________________________________________________________________
void FillPulls(int befAft)
{
  MyPull myRes;
  for (int jhit=0; jhit<(int)MyVect.size(); jhit++) {
     myRes = MyVect[jhit];
     int jdx = kind(myRes.xyz[0]);
     if (befAft==0) {
      hh[jdx*4+0]->Fill(myRes.ypul/(myRes.pye));
      hh[jdx*4+2]->Fill(myRes.zpul/(myRes.pze));
     } else {
     hh[jdx*4+1]->Fill(newPull(0,myRes,HitErr));
     hh[jdx*4+3]->Fill(newPull(1,myRes,HitErr));
     }
  }
}  
//______________________________________________________________________________
double newPull(int iYZ,const MyPull& myRes,const HitPars_t &pars)
{
  HitAccr accr;
  HitPars_t::HitCond(myRes,accr);
  double S = pars.Err(iYZ,accr)+accr.PredErr[iYZ];
  if (S<1e-6) S=1e-6;
  return accr.Pull[iYZ]/sqrt(S);
}

//______________________________________________________________________________
void DbInit()
{
 gSystem->Load("libStDb_Tables.so");
 TString command;
 TTable *newdat = 0;
 for (int idb=0;idb<4;idb++) {
   memcpy(strstr(dbFile[idb],"20"),gTimeStamp,15);
   int ready=0;
   if (!gSystem->AccessPathName(dbFile[idb])) {//file exists
     command = ".L "; command += dbFile[idb];
     gInterpreter->ProcessLine(command);
     newdat = (TTable *) gInterpreter->Calc("CreateTable()");
     command.ReplaceAll(".L ",".U "); 
     gInterpreter->ProcessLine(command);
     ready = 2006;
   } else {
    newdat = (TTable *)gInterpreter->Calc("new St_HitError(\"someHitError\",1)"); 
    newdat->SetUsedRows(1);
   }
   assert(newdat);
   dbTab[idb] = newdat;
 }
}
//______________________________________________________________________________
void DbDflt()
{
//	Set initial values if db file was non existing

  for (int idb=0;idb<4;idb++) 
  {
    if (numRes[idb]<kMINHITS) continue;
    double *d = (double *)dbTab[idb]->GetArray();
    if (d[0]<=0) {d[0]=aveRes[idb][0];d[3]=aveRes[idb][1];}
    TCL::ucopy(d+0,pSTI[idb*2+0],3);
    TCL::ucopy(d+3,pSTI[idb*2+1],3);
  }
}
//______________________________________________________________________________
void DbEnd()
{
  double par[2][3];
  for (int idb=0;idb<kNDETS;idb++) 
  {
    memset(par,0,sizeof(par));
    if (!HitErr.mPars[idb][0])	continue;
    if (!HitErr.Len(idb))	continue;
    TString ts(dbFile[idb]);
    ts +=".BAK";
    gSystem->Rename(dbFile[idb],ts);
    int ny = HitErr.Len(idb,0);
    TCL::ucopy(HitErr.mPars[idb][0],par[0],ny);
    int nz = HitErr.Len(idb,1);
    TCL::ucopy(HitErr.mPars[idb][1],par[1],nz);

    TCL::ucopy(par[0],(double*)dbTab[idb]->GetArray()+0,3+3);

    std::ofstream ofs(dbFile[idb]);
    dbTab[idb]->SavePrimitive(ofs);
  }
}

//______________________________________________________________________________
void HEnd()
{
 for (int ic=0;ic<(int)(sizeof(C)/sizeof(void*));ic++) {
   TCanvas *cc = C[ic]; if (!cc) continue;
   cc->Modified();cc->Update();
 }
 while(!gSystem->ProcessEvents()){};
}

//______________________________________________________________________________
void CheckErr()
{
  for (int jhit=0; jhit<(int)MyVect.size(); jhit++) {
     MyPull &myRes = MyVect[jhit];
     int jdx = kind(myRes.xyz[0]); if(jdx){};
     if (jdx != 2) continue;
     float pye = myRes.pye;
     float hye = myRes.hye;

     float ypul = myRes.ypul;
     if (fabs(ypul/pye)<2) {
       double tmp = (ypul*ypul-pye*pye)/(hye*hye);
       hh[20]->Fill(tmp);
     }
     hh[21]->Fill(pye/hye);
  }
}  
//______________________________________________________________________________
double AveErr()
{
// idx=0 outer resY
// idx=1 outer resZ
// idx=2 inner resY
// idx=3 inner resZ
// idx=4 Ssd   resY
// idx=5 Ssd   resZ
// idx=4 Svt   resY
// idx=5 Svt   resZ
  double pctMax=0;
  for (int iDet=0;iDet<kNDETS;iDet++) { 
    for (int iYZ=0;iYZ<2;iYZ++) {
      int idx = iYZ + 2*iDet;
      int nPars = HitErr.Len(iDet,iYZ);
      double sum=0,sum2=0,sig[3]={0};
      int nTot=0;
      for (int jhit=0; jhit<(int)MyVect.size(); jhit++) {
	 MyPull &myRes = MyVect[jhit];
	 int jdx = kind(myRes.xyz[0]);
	 if (jdx != iDet) continue;
	 nTot++;
	 HitAccr accr;
	 HitPars_t::HitCond(myRes,accr);
	 double S = HitErr.Err(iYZ,accr);
	 double delta = pow((&myRes.ypul)[iYZ],2);
	 double P0 = delta/((&myRes.uyy)[iYZ]+pow((&myRes.hye)[iYZ],2));
	 double P1 = delta/((&myRes.uyy)[iYZ]+HitPars_t::Err(pSTI[idx],nPars,accr.A[iYZ]));
	 double P2 = delta/((&myRes.uyy)[iYZ]+S);
	 sum +=S; sum2 += S*S; sig[0]+=P0; sig[1]+=P1;sig[2]+=P2;
      }
      if (!nTot) continue;
      sum/=nTot; sum2/=nTot; sum2-=sum*sum; if (sum2<0) sum2=0; sum2=sqrt(sum2);
      sum = sqrt(sum); sum2 /= 2*sum;
      for (int i=0;i<3;i++) {sig[i]=sqrt(sig[i]/nTot);}

      int isig = int(10000*sum +0.5);
      int esig = int(10000*sum2+0.5);
      double newErr = sum;
      double oldErr = sqrt(HitPars_t::Err(pSTI[idx],nPars,aveTrk[iDet][iYZ]));
      double pct = 200*fabs(newErr-oldErr)/(newErr+oldErr);
      if (pct>pctMax) pctMax=pct;
      int iold = int(10000*oldErr +0.5);
//       printf("AveErr(%s): <Err.old> = %4d <Err.new> %4d(+-%4dmu) <Sig> =%g %g %g\t//evts=%d\n"
//             ,DETS[idx],iold,isig,esig,sig[0],sig[1],sig[2],nTot);
      printf("AveErr(%s): <Err.old> = %4d <Err.new> %4d(+-%4dmu) Dif=%4.1g%%\t// evts=%d\n"
            ,DETS[idx],iold,isig,esig,pct,nTot);
  } } 
  printf("AveErr(): maxPct = %4.1g%%\n\n", pctMax );


  return pctMax;
}    
//______________________________________________________________________________
void HFit()
{
   if (!hh[30+4]) return;
}
//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
HitPars_t::HitPars_t ()
{
   memset(this,0,sizeof(HitPars_t));
   mNPars=1;
   int n = sizeof(mMin)/sizeof(*mMin);
   myTCL::vfill(mMin,kSMALL,n);
   myTCL::vfill(mMax,kBIG  ,n);
   mMax[0] = 3;
   mDat[0] = 0.1;
}
//_____________________________________________________________________________
HitPars_t::HitPars_t (const HitPars_t &fr)
{
  *this = fr;
}
//_____________________________________________________________________________
HitPars_t &HitPars_t::operator=(const HitPars_t &fr)
{
  memcpy(this,&fr,sizeof(HitPars_t));
  int inc = (char*)mDat-(char*)fr.mDat;
  int n   = sizeof(mPars)/sizeof(mPars[0][0]);
  char **p = (char**)mPars[0];
  for (int i=0;i<n;i++) {if (p[i]) p[i] += inc;}
  assert(mDat <=mPars[0][0]);
  return *this;
}
//_____________________________________________________________________________
const double &HitPars_t::operator[](int i) const 
{ return mDat[i];}

//_____________________________________________________________________________
      double &HitPars_t::operator[](int i) 
{ return mDat[i];}
//_____________________________________________________________________________
HitPars_t &HitPars_t::operator*(double f)  
{ for (int i=0;i<mNPars;i++) { (*this)[i]*=f;} return *this; }
//_____________________________________________________________________________
HitPars_t &HitPars_t::operator+=(const double *f)
{ for (int i=0;i<mNPars;i++) {(*this)[i]+=f[i];} return *this;}
//_____________________________________________________________________________
HitPars_t &HitPars_t::operator=(double f)
{ for (int i=0;i<mNPars;i++) {(*this)[i]=f;} return *this;}
//_____________________________________________________________________________
HitPars_t operator+(const HitPars_t &a,const double *add)
{
  HitPars_t tmp(a); tmp+=add; return tmp;
}
//_____________________________________________________________________________
HitPars_t operator+(const HitPars_t &a,const TVectorD &add)
{
  HitPars_t tmp(a); tmp+=add; return tmp;
}
//_____________________________________________________________________________
int HitPars_t::IPar(int iDet,int iYZ, int *npars) const
{ 
  if (npars) *npars=Len(iDet,iYZ);
  int ans = mPars[iDet][iYZ]-mDat;
  assert(ans>=0 && ans < 100);
  return ans;
}
//_____________________________________________________________________________
int HitPars_t::Lim(int i) const
{ 
  int lim = 0;
  if (mDat[i] <= mMin[i]*1.1) lim = -1;
  if (mDat[i] >= mMax[i]*0.9) lim =  1;
  return lim;
}
//_____________________________________________________________________________
double HitPars_t::Err(int iDet,int iYZ,const double A[3]) const 
{
 return Err(mPars[iDet][iYZ],Len(iDet,iYZ),A);
}
//_____________________________________________________________________________
double HitPars_t::Err(int iYZ,const HitAccr &accr) const 
{
 return Err(accr.iDet,iYZ,accr.A[iYZ]);
}
//_____________________________________________________________________________
HitPars_t &HitPars_t::operator+=(const TVectorD &add)
{ 
  for (int i=0;i<mNPars;i++) {
    mDat[i]+=add[i];
    if (mDat[i]<mMin[i]) mDat[i]=mMin[i];
    if (mDat[i]>mMax[i]) mDat[i]=mMax[i];
  }
  return *this;
}
//_____________________________________________________________________________
void HitPars_t::Set(int iDet,int iYZ,int nini,const double *ini)
{
  mLen[iDet][iYZ]=nini;
  if (iDet+1>mNDets) mNDets=iDet+1;;
  mPars[iDet][iYZ]=mDat+mNPars;
  mErrs[iDet][iYZ]=mDrr+mNPars;
  mNPars+=nini;
  for (int i=0;i<nini;i++) {
    int ipar = IPar(iDet,iYZ);
    assert(ipar>=0);
    mDat[ipar+i]=ini[i];
    if (mDat[ipar+i]< mMin[ipar+i]) mDat[ipar+i]= mMin[ipar+i];
    if (mDat[ipar+i]> mMax[ipar+i]) mDat[ipar+i]= mMax[ipar+i];
  }
}

//_____________________________________________________________________________
double HitPars_t::Dens(double rxy,int ntk) 
{
// pseudo-rapidity = -log(tan(theta/2)) theta angle btw track & beam


  return ntk/(4*3.14*rxy*rxy);
}  
//______________________________________________________________________________
void HitPars_t::HitCond(const MyPull& myRes,HitAccr &acc)
{
  memset(acc.A[0],0,sizeof(acc.A));
  acc.iDet = kind(myRes.xyz[0]);
  for (int iyz=0;iyz<2;iyz++) {
    acc.A[iyz][0]=1;
    acc.A[iyz][1]= 0.01*(200-fabs(myRes.xyz[2]));
    double ang;
    if (!iyz) {
      ang = myRes.psi; acc.Pull[0]=myRes.ypul; 
      acc.PredErr[0] = myRes.uyy;
    } else {
      ang = myRes.dip; acc.Pull[1]=myRes.zpul;
      acc.PredErr[1] = myRes.uzz;
    }
    double ca=cos(ang);
    if (ca<0.01) ca=0.01;
    double ca2 = ca*ca;
    double ta2=(1-ca2)/ca2;
    acc.A[iyz][1]/= ca2;
    acc.A[iyz][2] = ta2;
  }
}
//______________________________________________________________________________
double HitPars_t::Deriv(int npt, const MyPull *pt,TVectorD &Di,TMatrixD &Dij) const
{
// Ai   = ErrYi**2; Bi   = ErrZi**2;
// Wy_i = 1./Ai   ; Wz_i = 1./Bi  ;
// Area_i = Pi*sqrt(Ai*Bi);
// -log(Fcn_i) = 0.5*(1+2*fake*area_i*dens_i)*(Wy_i*(Yi-Pol_y(Li))**2
//                                            +Wz_i*(Zi-Pol_z(Li))**2)
//             - 0.5*log(1+2*fake*area_i*dens_i) 
//             - 0.5*log(Wyi) - 0.5*log(Wzi)
// Fcn = Fcn_0+Fcn_1+...


  Di.ResizeTo(mNPars);
  Dij.ResizeTo(mNPars,mNPars);
  int npt21 = npt*2+1;

  HitAccr ha;
  Poli2 ply(2);
  Poli2 plz(1);
  TVectorD wy(npt),wz(npt),cos2Psi(npt21);
  TMatrixD toPars(mNPars,npt21);
  toPars[0][0]=1;
  cos2Psi=1.;
  TPoliFitter pfy(2),pfz(1); //?????
  TCircleFitter cf;
#define  NEWPOLIFIT
#ifndef  NEWPOLIFIT
  double len = 0,oldCurv,oldXyz[3],nowXyz[3];
  assert(pt[0].xyz[0]<pt[npt-1].xyz[0]);
  for (int ipt=0;ipt<npt;ipt++) {
    HitCond(pt[ipt],ha);
    int iDet = ha.iDet;
    if (!mPars[iDet][0]) continue;
    double nowCurv=pt[ipt].curv;
    nowXyz[0] = pt[ipt].grf*cos(pt[ipt].gpf);
    nowXyz[1] = pt[ipt].grf*sin(pt[ipt].gpf);
    if (ipt) { //calc length
      double dl = sqrt(SQ(nowXyz[0]-oldXyz[0])+SQ(nowXyz[1]-oldXyz[1]));
      double curv=0.5*fabs(nowCurv+oldCurv);
      if (dl*curv<1e-3) dl*=(1.+SQ(dl*curv)/24);
      else              dl = 2*asin(0.5*dl*curv)/curv;
      len+=dl;
    }

    double dy = pt[ipt].xyz[1]-pt[ipt].yfit;
    double cosPsi = cos(pt[ipt].psi);
    cos2Psi[ipt+1] = cosPsi*cosPsi;
    dy *= cosPsi;
    double dz = pt[ipt].xyz[2]-pt[ipt].zfit;
    wy[ipt] = (1./Err(0,ha))/cos2Psi[ipt+1];
    wz[ipt] = (1./Err(1,ha));
    double fake = myFake( mDat[0],wy[ipt],wz[ipt],pt[ipt].dens,0,0);
    ply.Add(len,dy,wy[ipt]*fake);
    plz.Add(len,dz,wz[ipt]*fake);
//    pfy.Add(len,dy,1./(wy[ipt]*fake));//????
//    pfz.Add(len,dz,1./(wz[ipt]*fake));//????
//    double emx[3]={1./(wy[ipt]*fake),0,1./(wy[ipt]*fake)};
//    cf.Add(len,dy,emx);//????
    int n,ipar;
    for (int jk=0;jk<2;jk++) { 
      int ip = 1 +ipt + jk*npt;
      ipar = IPar(iDet,jk,&n);
      assert(ipar>=0);
      for (int in=0;in<n;in++) {toPars[ipar+in][ip]+=ha.A[jk][in];}
    } 
    memcpy(oldXyz,nowXyz,sizeof(oldXyz));
    oldCurv=nowCurv;
  }
#endif //0
#ifdef  NEWPOLIFIT
  TVectorD Y,Z,S;
  Prep(npt,pt,Y,Z,S,cos2Psi);
  for (int ipt=0;ipt<npt;ipt++) {
    HitCond(pt[ipt],ha);
    wy[ipt] = (1./Err(0,ha))/cos2Psi[ipt+1];
    wz[ipt] = (1./Err(1,ha));
    double fake = myFake( mDat[0],wy[ipt],wz[ipt],pt[ipt].dens,0,0);
    ply.Add(S[ipt],Y[ipt],wy[ipt]*fake);
    plz.Add(S[ipt],Z[ipt],wz[ipt]*fake);
    pfy.Add(S[ipt],Y[ipt],1./(wy[ipt]*fake));
    pfz.Add(S[ipt],Z[ipt],1./(wz[ipt]*fake));
    double emx[3]={1./(wy[ipt]*fake),0,1./(wy[ipt]*fake)};
    cf.Add(S[ipt],Y[ipt],emx);
    int n,ipar,iDet = ha.iDet;
static int myCall=0; myCall++;
    for (int jk=0;jk<2;jk++) { 
      int ip = 1 +ipt + jk*npt;
      ipar = IPar(iDet,jk,&n);
      assert(ipar>=0);
      for (int in=0;in<n;in++) {toPars[ipar+in][ip]+=ha.A[jk][in];}
    } 
  }
#endif //0
  ply.Init();
  plz.Init();

static int testIt=1;
  if (testIt) {testIt--; ply.TestIt(); plz.TestIt(); }
  
  TVectorD dW(npt21),dWy(npt),dWz(npt);
  TMatrixD dWW(npt21,npt21),dWWy(npt,npt),dWWz(npt,npt);
  double Xi2y = ply.Deriv(dWy,dWWy);
  double Xi2z = plz.Deriv(dWz,dWWz);
  double Fcn = Xi2y+Xi2z;

// 	account that W was multiplied by fake, where
//      fake = (1+2*mFake*pt[ipt].dens*Pi/sqrt(wx[ipt]*wy[ipt]))

  double gi[2][3],gj[2][3],G[2][3][3];
  for (int ipt=0;ipt<npt;ipt++) {
    int idx[3]={0,ipt+1,ipt+1+npt};
    myDers(mDat[0],wy[ipt],wz[ipt],pt[ipt].dens,gi,G);
    for (int i=0;i<3;i++) {
      dW[idx[i]] += dWy[ipt]*gi[0][i]+dWz[ipt]*gi[1][i];
      for (int j=0;j<=i;j++) {
        dWW[idx[i]][idx[j]] += dWy[ipt]*G[0][i][j]+dWz[ipt]*G[1][i][j];
    } }
   for (int jpt=0;jpt<npt;jpt++) {
      int jdx[3]={0,jpt+1,jpt+1+npt};
      myDers(mDat[0],wy[jpt],wz[jpt],pt[jpt].dens,gj,0);
      
       for (int i=0;i<3;i++) {
         for (int j=0;j<=i;j++) {
           if(idx[i]<jdx[j]) break;
           dWW[idx[i]][jdx[j]] += dWWy[ipt][jpt]*gi[0][i]*gj[0][j]
                                 +dWWz[ipt][jpt]*gi[1][i]*gj[1][j];
  } } } }


//	account log() terms
  double C,Cd[3],Cdd[3][3];
  for (int ipt=0;ipt<npt;ipt++) {
    int idx[3]={0,ipt+1,ipt+1+npt};
    C = myFake( mDat[0],wy[ipt],wz[ipt],pt[ipt].dens,Cd,Cdd);
    Fcn += - log(wy[ipt]) - log(wz[ipt]) - log(C);
    dW[idx[1]] += -1./wy[ipt];
    dW[idx[2]] += -1./wz[ipt];
    dWW[idx[1]][idx[1]]+= 1./pow(wy[ipt],2); 
    dWW[idx[2]][idx[2]]+= 1./pow(wz[ipt],2); 
    for (int i=0;i<3;i++) {
      dW[idx[i]] +=   - Cd[i]/C;
      for (int j=0;j<=i;j++) {
        dWW[idx[i]][idx[j]] += (-Cdd[i][j]+Cd[i]*Cd[j]/C)/C;
  } } }

//   	account Wy = wy/cos2Psi 
  for (int ii=0;ii<npt21;ii++) {
    if (ii && ii<=npt) wy[ii-1]*=cos2Psi[ii];
    dW[ii] /=cos2Psi[ii];
    for (int jj=0;jj<=ii;jj++) {
      dWW[ii][jj]/=cos2Psi[ii]*cos2Psi[jj];
  } }

//   	change W to E where W=1/E
  TVectorD wt(npt21);
  wt.SetSub(1,wy); wt.SetSub(1+npt,wz);
  for (int ii=1;ii<npt21;ii++) {
    double wi = wt[ii],wi2 =wi*wi;
    dW [ii]    *= -wi2;
    dWW[ii][0] *= -wi2;
    for (int jj=1;jj<=ii;jj++) {
      double wj2 = wt[jj]*wt[jj];
      dWW[ii][jj] *= wi2*wj2;
    }
    dWW[ii][ii] += -2*dW[ii]*wi;
  }


//	Symmetrisation
    for (int i=1;i<npt21;i++) {
    for (int j=0;j<    i;j++) { 
      assert(fabs(dWW[j][i])<=0);
      dWW[j][i] = dWW[i][j];
    } }

  Di = toPars*dW;
//  Dij = (toPars*dWW)*(toPars.T());
  myTCL::mxmlrtS(toPars,dWW,Dij);
  for (int i=0;i<mNPars;i++) {
    for (int j=0;j<i;j++) {
      assert(fabs(dWW[i][j]-dWW[j][i])<1e-10);}}
  return Fcn;
}
//______________________________________________________________________________
double HitPars_t::DERIV(int npt, const MyPull *pt,TVectorD &Di,TMatrixD &Dij,int maxTrk) 
{

  Di.ResizeTo(mNPars);
  Dij.ResizeTo(mNPars,mNPars);
  Di = 0.; Dij=0.; mNTrks=0;
  TVectorD Ti ,TiM (mNPars);
  TMatrixD Tij,TijM(mNPars,mNPars);
  int jl=0,trk=pt[0].trk; 
  double xl=0;
  double sum=0,sumM=0;
  int NSave = (int)sqrt((npt/30.));
  
  for (int jr=1; 1; jr++) {
    double xl0 = xl; xl = pt[jr].xyz[0];
    if (jr<npt && pt[jr].trk==trk && xl0<xl) continue;
    int n = jr-jl;
    if (n>15) {
      double tsum = Deriv(n,pt+jl,Ti,Tij);
      sum += tsum;
      Di  += Ti ;
      Dij += Tij;
      mNTrks++;
      if (mNTrks>=maxTrk) break;
      if (((mNTrks+1)%NSave)==0 ) {
        sumM += sum; sum= 0.;
        TiM  += Di ; Di = 0.;
        TijM += Dij; Dij= 0.;
      }
    }
    if (jr>=npt) break;
    trk = pt[jr].trk;
    jl = jr;
  }
  sum +=sumM;
  Di  +=TiM ;
  Dij +=TijM;
  return sum;  
}    
//______________________________________________________________________________
void HitPars_t::myDers( double fake,double wy, double wz, double dens
                      , double g[2][3],double G[2][3][3])
{
//A  = 3.14/sqrt(wy*wz)
//WY = wy*(1+2*fake*dens*A)
//WZ = wz*(1+2*fake*dens*A)
  double u[3]={fake,wy,wz};
  double Cd[3],Cdd[3][3];
  double (*myCdd)[3] = Cdd;
  if (!G) myCdd=0;
  double C0 = myFake(fake,wy,wz,dens,Cd,myCdd);

  memset(g[0],0,2*3*sizeof(g[0][0]));
  for (int iyz=0;iyz<2;iyz++) {
    for (int ivar=0;ivar<3;ivar++) {
      g[iyz][ivar] = u[iyz+1]*Cd[ivar];
      if (ivar==iyz+1) g[iyz][ivar]+= C0; }}

  if (!G) return;

  memset(G[0][0],0,2*3*3*sizeof(G[0][0][0]));
  for (int iyz=0;iyz<2;iyz++) {
    for (int i=0;i<3;i++) {
    for (int j=0;j<3;j++) {
      G[iyz][i][j] = u[iyz+1]*Cdd[i][j];
      if (i==iyz+1) G[iyz][i][j]+=Cd[j];
      if (j==iyz+1) G[iyz][i][j]+=Cd[i];
  } } }
      
}      
//______________________________________________________________________________
double HitPars_t::myFake( double fake,double wy, double wz, double dens
                        , double Cd[3],double Cdd[3][3])
{
//A  = 3.14/sqrt(wy*wz)
//WY = wy*(1+2*fake*dens*A)
//WZ = wz*(1+2*fake*dens*A)
  double dens2 = 2*dens;

  double A  = 3.14/sqrt(wy*wz);
  double Ay = -0.5*A/wy ;
  double Az = -0.5*A/wz ;

  double C0 = (1+fake*dens2*A);
  if (!Cd) return C0;
  Cd[0] =      dens2*A;
  Cd[1] = fake*dens2*Ay;
  Cd[2] = fake*dens2*Az;
  if (!Cdd) return C0;

  double Ayy = 0.5*(-Ay + A/wy)/wy;
  double Ayz = 0.5*(-Az       )/wy;
  double Azz = 0.5*(-Az + A/wz)/wz;

  Cdd[0][0] = 0;
  Cdd[0][1] =      dens2*Ay;
  Cdd[0][2] =      dens2*Az;
  Cdd[1][0] = Cdd[0][1];
  Cdd[1][1] = fake*dens2*Ayy; 
  Cdd[1][2] = fake*dens2*Ayz;
  Cdd[2][0] = Cdd[0][2];
  Cdd[2][1] = Cdd[1][2];
  Cdd[2][2] = fake*dens2*Azz;
  return C0;
}
//______________________________________________________________________________
void HitPars_t::Print(const HitPars_t *init) const
{
  const char *TYZ[]={"y","z"};
  const char *bnd;

  printf("HitPars(Fake ) ");
  bnd = (Lim(0))? "*":" ";
  if (init) printf("\t              ");
  printf("\tpout=%8.4g(+-%8.4g)%s\n",mDat[0],mDrr[0],bnd);
  
  for (int iDet=0;iDet<mNDets;iDet++) 	{
    for (int iYZ=0;iYZ<2;iYZ++) 	{
      int ln = Len(iDet,iYZ);
      int ipar0 = IPar(iDet,iYZ);
      assert(ipar0>=0);
      for (int ip=0;ip<ln;ip++) {
        bnd = (Lim(ipar0+ip))? "*":" ";
        double p = mPars[iDet][iYZ][ip];
        double e = mErrs[iDet][iYZ][ip];
        printf("HitPars(%d,%s,%d) ",iDet,TYZ[iYZ],ip);
	if (init) {
	  printf("\tinit=%8.4g ",init->mPars[iDet][iYZ][ip]);}
	printf("\tpout=%8.4g(+-%8.4g)%s",p,e,bnd);
        printf("\n");
  } } }

}
//______________________________________________________________________________
double HitPars_t::Diff(const HitPars_t &init) 	const
{
  double pctMax=0;
  for (int i=0;i<mNPars;i++) {
    double dif = fabs((*this)[i]-init[i]);
    double err = init.Err(i);
    if (err<=0) err = 0.5*((*this)[i]+init[i])+1e-10;
    dif/= err; 
    if (pctMax<dif) pctMax=dif;
  }
  return pctMax*100;
}
//______________________________________________________________________________
int HitPars_t::Test(int npt, const MyPull *pt) const
{
   enum {kNTrkTst=5};
   TVectorD Di ,DiT ;
   TMatrixD Dij,DijT;
   HitPars_t HP(*this);
   HP[0]=9;
   int npars = HP.NPars();
   double fcn = HP.DERIV(npt,pt,Di,Dij,kNTrkTst);
   for (int i=0;i<npars;i++) {
     double sav = HP[i];
     double delta = fabs(sav)*1e-3;
     if (delta<1e-6) delta=1e-6;
     HP[i]=sav+delta;
     double fcnT = HP.DERIV(npt,pt,DiT,DijT,kNTrkTst);
     HP[i]=sav;
     double ana = 0.5*(Di[i]+DiT[i]);
     double num = (fcnT-fcn)/delta;
     double dif = (num-ana)/(fabs(num+ana)+1e-5);
     if (fabs(dif)>0.001)
     printf("\nDer(%2d): ana = %g \tnum = %g \tdif = %g\n\n",i,ana,num,dif);
     for (int k=0;k<npars;k++) {
       ana = 0.5*(Dij[i][k]+DijT[i][k]);
       num = (DiT[k]-Di[k])/delta;
       dif = (num-ana)/(fabs(num+ana)+1e-5);
       if (fabs(dif)>0.001)
       printf("Der(%2d,%2d): ana = %g \tnum = %g \tdif = %g\n",i,k,ana,num,dif);
     }

   }
   return 0;    
}    
//______________________________________________________________________________
double HitPars_t::Err(const double Pars[3],int nPars,const double A[3])
{
static const double tenMicrons = 1e-3;
static const double min2Err = tenMicrons*tenMicrons;
static const double max2Err = 1.;
  double err = TCL::vdot(A,Pars,nPars);
  if (nPars<=1) return err;
  if (err<min2Err) err=min2Err;
  if (err>max2Err) err=max2Err;
  return err;
}
//______________________________________________________________________________
void HitPars_t::Limit()
{
  for (int i=0;i<mNPars;i++) {
    if (mDat[i]<mMin[i]) mDat[i]=mMin[i];
    if (mDat[i]>mMax[i]) mDat[i]=mMax[i];
  }
}
#if 1
//______________________________________________________________________________
void HitPars_t::Prep(int npt, const MyPull *pt,TVectorD &Y,TVectorD &Z
                      ,TVectorD &S,TVectorD &cos2Psi)
{
static int myDebug=0;

  Y.ResizeTo(npt);
  Z.ResizeTo(npt);
  S.ResizeTo(npt);
  if (myDebug) Show(npt,pt);
  int npt21 = npt*2+1;
  cos2Psi.ResizeTo(npt21);cos2Psi = 1.;

  THelixFitter helx;
  for (int ipt=0;ipt<npt;ipt++) {
    TVector3 point;
    point[0] = pt[ipt].xhg();
    point[1] = pt[ipt].yhg();
    point[2] = pt[ipt].zhg();
    helx.Add(point[0],point[1],point[2]);
    double emx[3]={pow(pt[ipt].hye,2),0,pow(pt[ipt].hye,2)};
    helx.AddErr(emx,pow(pt[ipt].hze,2));
  }
  helx.Fit();
  THelixTrack move(helx);
  double s = 0;
  for (int ipt=0;ipt<npt;ipt++) {
    TVector3 point;
    point[0] = pt[ipt].xhg();
    point[1] = pt[ipt].yhg();
    point[2] = pt[ipt].zhg();
    double ds = move.Path(point[0],point[1]);
    if (ipt) s+=ds;
    move.Move(ds);
    TVector3 pos(move.Pos());
    TVector3 dir(move.Dir());
    TVector3 nor(dir[1],-dir[0],0.); nor = nor.Unit();
    double dy = (point-pos)*nor;
    double dz = point[2]-pos[2];
    S[ipt]=s;
    Y[ipt]=dy;
    Z[ipt]=dz;
    cos2Psi[ipt+1]=pow(cos(pt[ipt].psi),2);
  }
}
#endif//1
//______________________________________________________________________________
void HitPars_t::Show(int npt, const MyPull *pt)
{
//  lev=0  draw all nodes
//  lev=1  draw hit nodes
//  lev=2  draw hits only 



static TCanvas *myCanvas=0;
static TGraph  *graph[3][3] = {{0,0,0},{0,0,0},{0,0,0}};
//        P  G         P  G
  float X[3][3][100],Y[3][3][100];
  int   N[3][3];

  if (!myCanvas) {myCanvas = new TCanvas("C1","",600,800);
                  myCanvas->Divide(1,3);}
  if(pt) { 
    double curv = 0,xPrev,yPrev; int nCurv = 0;
    for (int i=0;i<9;i++) {delete graph[0][i];graph[0][i]=0;}
    for (int ig=0;ig<3;ig++) {
      int n=0;
      double s = 0;
      xPrev = 3e33;
      for (int ipt=0;ipt<npt;ipt++) {
	const MyPull *node = pt+ipt;
//	S calculation common based on node x,y for both hit and node
        double xNode = node->x_g();
        double yNode = node->y_g();
	if (xPrev<3e33) {
	  double ds = sqrt(pow(xNode-xPrev,2)+pow(yNode-yPrev,2));
          double si = 0.5*ds*curv; if (si>0.99) si=0.99;
          if (si>0.01) ds = 2*asin(si)/curv;
	  s += ds;
        }
        xPrev = xNode;
        yPrev = yNode;

        if (ig==0) { curv += node->curv; nCurv++; continue;}
	if (ig==1) {//draw nodes
          X[0][ig][n] = node->x_g();
          Y[0][ig][n] = node->y_g();
          Y[2][ig][n] = node->z_g();
	} else {//draw hits only
          X[0][ig][n] = node->xhg();
          Y[0][ig][n] = node->yhg();
          Y[2][ig][n] = node->zhg();
	}

	if (n) {
          float xh = X[0][ig][n]-X[0][ig][0];
          float yh = Y[0][ig][n]-Y[0][ig][0];
	  float rh = xh*xh+yh*yh+1E-10;
	  X[1][ig][n-1] = xh/rh;
	  Y[1][ig][n-1] = yh/rh;
	}
	X[2][ig][n]=s;
	n++;
      }//end for nodes
      if (ig==0) { curv=fabs(curv)/nCurv; continue;}
      N[0][ig] = n;
      N[1][ig] = n-1;
      N[2][ig] = n;
    }//end for ig
    
    for (int ip=0;ip<3;ip++) {
      double xMin=999,xMax=-999,yMin=999,yMax=-999;
      for (int ig=1;ig<3;ig++) {
        for (int j=0;j<N[ip][ig];j++) {
           double x = X[ip][ig][j];
	   if (xMin> x) xMin = x;
	   if (xMax< x) xMax = x;
           double y = Y[ip][ig][j];
	   if (yMin> y) yMin = y;
	   if (yMax< y) yMax = y;
        }
      }
      X[ip][0][0] = xMin; Y[ip][0][0] = yMin;
      X[ip][0][1] = xMin; Y[ip][0][1] = yMax;
      X[ip][0][2] = xMax; Y[ip][0][2] = yMin;
      X[ip][0][3] = xMax; Y[ip][0][3] = yMax;
      N[ip][0] = 4;
    }
static const char *opt[]={"AP","Same CP","Same *"};  
    for (int ip=0;ip<3;ip++) {
      for (int ig =0;ig<3;ig++) {
        graph[ip][ig]  = new TGraph(N[ip][ig]  , X[ip][ig], Y[ip][ig]);
        if(ig==2) graph[ip][ig]->SetMarkerColor(kRed);
        myCanvas->cd(ip+1); graph[ip][ig]->Draw(opt[ig]);
      }//end for ig
    }//end ip

  }//end if


  if (!myCanvas) return;
  myCanvas->Modified();
  myCanvas->Update();
  while(!gSystem->ProcessEvents()){}; 
}  
//______________________________________________________________________________
int   HitPars_t::Test()
{
return 0;
}
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
double myTCL::vmaxa(const double *a,int na)
{ 
  double r=0;
  for (int i=0;i<na;i++){if (r < fabs(a[i])) r = fabs(a[i]);}
  return r;
}
//______________________________________________________________________________
double myTCL::vmaxa(const TVectorD &a)
{ 
  return vmaxa(a.GetMatrixArray(),a.GetNrows());
}
//______________________________________________________________________________
void myTCL::vfill(double *a,double f,int na) 
{
  for (int i=0;i<na;i++) {a[i]=f;}
}
//_____________________________________________________________________________
void myTCL::eigen2(const double err[3], double lam[2], double eig[2][2])
{

  double spur = err[0]+err[2];
  double det  = err[0]*err[2]-err[1]*err[1];
  double dis  = spur*spur-4*det;
  if (dis<0) dis = 0;
  dis = sqrt(dis);
  lam[0] = 0.5*(spur+dis);
  lam[1] = 0.5*(spur-dis);
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
//______________________________________________________________________________
/*
* $Id: fiterr.C,v 1.8 2019/07/02 20:48:18 perev Exp $
*
* $Log: fiterr.C,v $
* Revision 1.8  2019/07/02 20:48:18  perev
* Increaese array of hits
*
* Revision 1.7  2010/01/27 21:38:06  perev
* bugFix + remove not used code
*
* Revision 1.6  2008/01/20 00:43:02  perev
* Mess with timestamps fixed
*
* Revision 1.5  2007/04/26 04:25:32  perev
* Cleanup
*
* Revision 1.1.1.1  1996/04/01 15:02:13  mclareni
* Mathlib gen
*/
double  myTCL::simpson(const double *F,double A,double B,int NP)
{
  int N2=NP-1;
  assert(N2>0 && !(N2&1));
  double S1=F[N2-1];
  double S2=0;

  for (int N = 1;N<=N2-3;N+=2) {S1+=F[N];S2+=F[N+1];}
  S1=S1+S1+S2;
  double H=(F[0]+F[N2]+S1+S1)*(B-A)/(3*N2);
  return H;
}
//______________________________________________________________________________
void myTCL::mxmlrt(const TMatrixD &A,const TMatrixD &B,TMatrixD &X)  
{
  int nRowA = A.GetNrows();
  int nColA = A.GetNcols();
  int nRowB = B.GetNrows();
  int nColB = B.GetNcols(); if(nColB){}
  assert(nColA ==nRowB);
  X.ResizeTo(nRowA,nRowA);
  TCL::mxmlrt(A.GetMatrixArray(),B.GetMatrixArray()
	     ,X.GetMatrixArray(),nRowA,nColA);

}
//______________________________________________________________________________
void myTCL::mxmlrtS(const double *A,const double *B,double *X,int nra,int nca)  
{
   TCL::vzero(X,nra*nra);
   for (int i=0,ii=0;i<nra;i++,ii+=nca) 	{
     for (int j=0,jj=0;j<nca;j++,jj+=nca)  	{
       if(!A[ii+j]) 	continue;
       for (int k=0,kk=0;k<=i;k++,kk+=nca)	{
         double &Xik =X[i*nra+k];
         for (int l=0;l<nca;l++)  		{
           if(!A[kk+l]) continue;
           Xik +=A[ii+j]*A[kk+l]*B[jj+l];
   } } } }
   for (int i=0;i<nra;i++){
   for (int k=0;k<i  ;k++){X[k*nra+i] = X[i*nra+k];}}
}       
//______________________________________________________________________________
void myTCL::mxmlrtS(const TMatrixD &A,const TMatrixD &B,TMatrixD &X)  
{
  int nRowA = A.GetNrows();
  int nColA = A.GetNcols();
  int nRowB = B.GetNrows();
  int nColB = B.GetNcols(); if(nColB){}
  assert(nColA ==nRowB);
  X.ResizeTo(nRowA,nRowA);
  myTCL::mxmlrtS(A.GetMatrixArray(),B.GetMatrixArray()
	       ,X.GetMatrixArray(),nRowA,nColA);

}
//______________________________________________________________________________
double myTCL::vasum(const double *a, int na)
{
  double sum = 0;
  for (int i=0;i<na;i++) { sum += fabs(a[i]);}
  return sum;
}
//______________________________________________________________________________
double myTCL::vasum(const TVectorD &a)
{
  return vasum(a.GetMatrixArray(),a.GetNrows());
}
//______________________________________________________________________________
int myTCL::SqProgSimple(      TVectorD &x
                      ,const TVectorD &g,const TMatrixD &G 
		      ,const TVectorD &Min		   
		      ,const TVectorD &Max, int iAktp)
{
static int nCall=0; nCall++;
  enum {kINIT=0,kADDCONS,kFREECONS};
  int kase = kINIT;
  int nPars = g.GetNrows();
  TVectorD xx(x),gg(g),add(nPars);
  TArrayI Side(nPars);
  int nCons=0,addCons = -1,freCons=-1,freSide=0,addSide=0,con=0;
  double maxGra=3e33;
  int iAkt = iAktp;
  while(1946) {
// 	Eliminate outdated constrains
    freCons=-1; freSide=0;
    if (nCons && kase==kFREECONS ) {
      double tryGra=kSMALL; freCons=-1;
      for (int ix=0;ix<nPars;ix++) {
        if(!Side[ix])		continue;
        double gra = gg[ix]*Side[ix];
	if (gra< tryGra)	continue;
	if (gra>=maxGra)	continue;
        freCons=ix; tryGra=gra;
      }
      if (freCons>=0) {
        maxGra = tryGra;
        freSide = Side[freCons];
        Side[freCons]=0;
        nCons--;
      }
    }

    if(kase==kFREECONS && freCons<0) 	{  break;} //DONE
    
//	Make new matrix, etc...
    TMatrixD S(G);
    TVectorD B(gg);
    if (nCons) {
      for (int ix=0;ix<nPars;ix++) {
	if (Side[ix]==0) continue;
        for (int jx=0;jx<nPars;jx++) {S[ix][jx]=0; S[jx][ix]=0;}
        S[ix][ix]=1; B[ix]=0;
    } } 
    if (iAkt==0 ) {

      double det=S.Determinant(); 
      if (fabs(det)<1e-100) return -99;
      S.Invert(0);
//      if (det<0) iAkt=1;
//      else       add = (-1.)*(S*B);
      add = (-1.)*(S*B);
      double along = B*add;
      if (along>0) add*=(-1.);
    }

    if (iAkt==1 ) {
      double bb    = (B*B);
      double bSb   = (B*(S*B));
      double tau = -bb/(fabs(bSb)+3e-33);
      add = tau*B;

    }
    if(kase==kFREECONS && freSide) { //Free constrain case
      if (add[freCons]*freSide > -kSMALL) {
        Side[freCons]=freSide; nCons++; continue;}
    } 

//		Do we need new constrain?
    double fak=1;
    addCons = -1; addSide = 0;
    con = 0;
    for (int ix=0;ix<nPars;ix++) {
      if (Side[ix]) {add[ix]=0; con = 100*con+ix+1;continue;}
      double xi = xx[ix]+fak*add[ix];
      if (xi < Min[ix]){fak = (Min[ix]-xx[ix])/add[ix]; addCons=ix;addSide=-1;}
      if (xi > Max[ix]){fak = (Max[ix]-xx[ix])/add[ix]; addCons=ix;addSide= 1;}
      assert(fak<=1. && fak>=0.);
    }
    add*=fak;
    xx+= add;
    gg += G*add;
    maxGra=3e33;
    kase = kFREECONS; if (!addSide) continue;
    kase = kADDCONS;
    xx[addCons] = (addSide<0)? Min[addCons]:Max[addCons];
// 	Add new constrain;    
    Side[addCons] = addSide ;nCons++;

  } 				//end of while(1)
  x = xx;
  return abs(con);
}  	
#endif //!defined(__MAKECINT__)
