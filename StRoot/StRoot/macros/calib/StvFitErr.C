#include <stdio.h>
#include <iostream>
#include <fstream>
#include "TSystem.h"
#include "TMath.h"
#include "TFile.h"
#include "TTree.h"
#include "TH1.h"
#include "TF1.h"
#include "TMath.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "TCernLib.h"
#include "TMatrixD.h"
#include "TRandom.h"
#include "TVectorD.h"
#include "TVector2.h"
#include "TVector3.h"
#include "TTreeIter.h"
#include "TTable.h"
#include "TInterpreter.h"
#include "TMinuit.h"
#include "StvUtil/StvHitErrCalculator.h"
#include "TPolinom.h"
#include <vector>
//#define APPROX
static const double kWeight = 0.99;	//pars = oldPars*(1-kWeight) + newPars*kWeight
static int testOnly=0;
enum {kMinNodes = 10,kMinHits=25};
static const double kMaxDY=0.5,kMaxDZ=0.5,kErrFak=1.,kMaxCur=1./200,kPMin=0.5;
static const int gScale = 0;
static const double kUppPar=22,kLowPar=1e-8;

//______________________________________________________________________________
class FEEvent;
class FEFcn;
class StvHitErrCalculator;
class StvTpcHitErrCalculator;
int StvFitErr(const char *file="pulls.root");
int MyTest();
#if !defined(__MAKECINT__)
enum {kMaxPars = StvHitErrCalculator::kMaxPars};
TMatrixD T(TMatrixD a) { TMatrixD mx(a); return mx.T();}


//______________________________________________________________________________
class FERead : public TNamed
{
public:
FERead(const char *file);
FEEvent *ReadEvent();
private:
TTreeIter *mTreeIter;
FEEvent   *mEvent;
};

//______________________________________________________________________________
class Poli2 {
public:
Poli2(int npw,double reg);
Poli2(int npw,int n,const double *X,const double *Y,const double *W);
void SetReg(double reg) 		{fReg = reg;}
double Fit();
void Clear();
void Add(double x, double y, double w);
double Xi2() const  			{return fXi2;}
double LiH() const  			{return fXi2+fLiH+fKor;}
double dKordW() const  			{return fdKor;}
double Xi2(int ipt) const; 		
double Res(int ipt) const; 		
double Kor()	    const		{return fKor;} 		
double Kor(int ipt) const; 		
double dKordW(int ipt) const; 		
double EvalXi2() const; 		
int    Pw()  const 			{return fPw ;}
int    NPt() const 			{return fN  ;}
double Fun( double x ) const;
double FunErr2( double x ) const;
double Pull( double x ,double y,double yErr2) const;
double dXi2dW( int ipt ) const;
double d2Xi2dW( int k, int l ) const;
double dLiHdW( int ipt ) const;
double d2LiHdW( int kpt ,int lpt) const;
double Deriv( TVectorD &Di ) const;
double D2riv( TMatrixD &Dik) const;
void   TestIt() const;
void   MyTest() const;
static void Test();
static void Test2();
static void TestErr();
private:
static double F(double T) { return TMath::Erf(T)/M_2_SQRTPIl;}
private:
int fPw;
char   fBeg[1];
int    fN;
double fReg,fKor,fdKor;
double fX[100];
double fY[100];
double fW[100];
double fXi2;
double fLiH;
double fX0,fY0;
char   fEnd[1];
TVectorD fP;
TVectorD fB;
TMatrixD fA;
TMatrixD fAi;
}; 


//______________________________________________________________________________
class FEEvent 
{
public:
  FEEvent(TTreeIter &th);
  int NextTrack(int &jr);
 void Reset() {mLTrack=0;mRTrack=-1;}
public:
int mLTrack,mRTrack;
const   int &run;
const   int &evt;
const   int   &nGHits;
const   float *&mCurv;
const   float *&mChi2;
const   float *&mPt;
const   short *&mTrackNumber;
const unsigned char *&nAllHits; 	//number of all hits in track
const unsigned char *&nTpcHits; 	//number of tpc hits in track
const unsigned char *&nFtpcHits; 	//number of ftpc hits in track
const unsigned char *&nSsdHits; 	//number of ssd hits in track
const unsigned char *&nRndHits; 	//number of RND hits in track
const unsigned char *&mDetector;	
const   float *&lYHit;
const   float *&lZHit;
const   float *&lLen;
const   float *&gPhiHP;			//  phi    of normal vector of hit plane in global Stv frame
const   float *&gLamHP;			//  lambda of normal vector of hit plane in global Stv frame
const   float *&gPsi;
const   float *&gDip;
const   float *&gRFit;
const   float *&gPFit;
const   float *&gZFit;
};
//______________________________________________________________________________
class FECalcHolder
{
public:
  FECalcHolder();	
  FECalcHolder(int id,StvHitErrCalculator* calc,const double miMax[2][2]);	
        StvHitErrCalculator *GetCalc() const 		{return mCalc;} 
virtual StvHitErrCalculator *GetCalc(const float x[3])  {if(x){};return mCalc;} 
virtual int FixPars(FEFcn * /*fcn*/) 			{return 0;}
  int GetId() {return mId;}
 void SetOffset(int off) 	{mOffset=off;}
  int GetOffset() const  	{return mOffset;}
  int GetNPars()  const   	{return mCalc->GetNPars();}
const double *GetPars() const   {return mCalc->GetPars() ;}
const char   *GetName() const   {return mCalc->GetName() ;}
 void Update(const double *a);
 void DbLoad();
 void DbSave();
 void Scale(double fak);

 void AvInit();
 void AvAdd(const double hRR[3],double yXi2,double zXi2);
 void AvEnd();

private:
char    mBeg[1];
TTable *mTab;
int mId;
int mOffset;
public:
double mAve[10];
double mAveDer[kMaxPars][3];
double mMiMax[2][2];
StvHitErrCalculator* mCalc;
char    mEnd[1];
};

//______________________________________________________________________________
class FETpcCalcHolder: public FECalcHolder
{
public:
  FETpcCalcHolder(StvHitErrCalculator* calc,const double miMax[2][2])
                 :FECalcHolder(1,calc,miMax){;}
virtual StvHitErrCalculator *GetCalc(const float x[3]);
virtual int FixPars(FEFcn *fcn);
};
//______________________________________________________________________________
class FETstCalcHolder: public FECalcHolder
{
public:
  FETstCalcHolder();
};

//______________________________________________________________________________
class FENode
{
public:
float  tkDir[3];
float  hiPos[3];
float  hiDir[3][3];
float  s; 
float  yz[2]; 
float  detId;
};
typedef std::vector<FENode> FENodes;

//______________________________________________________________________________
class FETrak
{
public:
void  Add(const FENode &node)		{mNodes.push_back(node);}
int   NNodes() const    		{return mNodes.size();  }
const FENodes &Nodes()      		{return mNodes;         }
private:
FENodes mNodes;
};
typedef std::vector<FETrak> FETraks;
//______________________________________________________________________________
class FEFcn 
{
public:
    FEFcn();
    void Clear();
    void Add(FECalcHolder *holder);
    void Add();
    void DbLoad();
    void DbSave();
     int Add(FEEvent *event);

    void InitFitter();
     int GetNPars() const 		{return mNPars;}
double*  GetPars()       		{return mPars ;}
     int GetIPar(const char* name) const;
     int GetNFixd() const 		{return mNFixd;}
    void Update(const double *upd);
    void Synchro(char from,const double *par=0);
    void Approx(int nonBias);
FECalcHolder *GetCalc(int id,const float hiPos[3]);
   void  Eval(int  npar, double* grad, double& fval, double* par, int flag);
   void  FixPar(int iPar,int fix=1);
    int  FixPars();
    int  IsFixed(int iPar) const 	{return mFix[iPar];}
    int  Fit();
   void  AvErr(const char *tit);
    int  IsEnded() const;
static  void Fcn(int &npar, double* grad, double& fval, double* par, int flag);


private:
char mFix[400];
int  mNPars;
int  mNFixd;
int  mNEvs;
int  mNTks;
int  mNHits;
int  mNCall;
double mPars[100];
double mFist[100];
double mVal00;
double mVal99;
double mXi2[3];
double mRes[4];

char mEnd[1];
TString mNams[100];
std::vector<FECalcHolder*> 	mCas;
FETraks 			mTks;
static FEFcn *mgInst;
};
#ifdef APPROX
//______________________________________________________________________________
class FEApprox 
{
public:
    FEApprox(FEFcn *fefcn);
int Prepare();
int Quadr();
int Approx();
int Test();
private:
FEFcn *mFcn;
int mNPars;
TMatrixD mG,mGi,mC;
TVectorD mB,mP,mPrev,mClo,mIclo,mCup,mIcup;
TVectorD mXlo,mIxlo,mXup,mIxup;
double mXi2;
};
#endif //APPROX


FEFcn *FEFcn::mgInst=0;

//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
void CalcInit();
FEFcn myFcn;
TMinuit myFitter(100);
//______________________________________________________________________________
int MyTest() 
{
  Poli2::Test();
  Poli2::TestErr();
  return 0;
} 
//______________________________________________________________________________
int StvFitErr(const char *file)
{
  testOnly = strcmp(file,"test")==0;
  printf("StvFitErr(%s) started",file);

  CalcInit();
  myFcn.DbLoad();
  myFcn.InitFitter();
  
  int nEv=0;
  FEEvent *ev=0;
  int eot=0;
  if (!testOnly) {
    FERead input(file);
    while ((ev=input.ReadEvent()))
    {
  //    printf ("Event %d\n",nEv);
      nEv++; eot = myFcn.Add(ev);
      if (eot) break;
    }
    printf ("StvFitErr: %d Events used\n",nEv);
  #ifdef APPROX
    FEApprox app(&myFcn);
  #endif
  } else {
//		TestOnly  
    myFcn.Add();  
  }
  myFcn.AvErr("Before Fit");
  int ans =0;
  ans = myFcn.Fit();
  myFcn.AvErr("After Fit");
  myFcn.DbSave();
  return (myFcn.IsEnded())? 99:0;
}
//______________________________________________________________________________
void CalcInit()
{
   const char*  innOutNames[4]  ={"StvTpcInnerHitErrsFE"    ,"StvTpcOuterHitErrsFE"    
				 ,"StvTpcInnerPromptErrsFE" ,"StvTpcOuterPromptErrsFE" };
//    const double innOutPars[2][6]={{0.047,0.107,5e-5,1./12,0.0011, 0.0012}
//                                  ,{0.035,0.077,5e-5,1./12,0.0011, 0.0012}};
   double TpcMiMax[2][2]={{0.05,0.2},{0.05,0.2}};


  myFcn.Clear();
  if (testOnly) {
    FECalcHolder *hold= new FETstCalcHolder();
    myFcn.Add(hold);
    return;
  }

  for (int io=0;io<4;io++) {
    StvHitErrCalculator *hec = (io<2) ? new StvTpcHitErrCalculator(innOutNames[io])
                                      : new StvHitErrCalculator   (innOutNames[io]);
    FECalcHolder *hold= new FETpcCalcHolder(hec,TpcMiMax);
    myFcn.Add(hold);
  }
}
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________


//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
Poli2::Poli2(int npw,double reg):fP(npw+1),fB(npw+1),fA(npw+1,npw+1),fAi(npw+1,npw+1)
{
  fPw = npw;
  Clear();  
  fReg = reg;
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
  assert(fN<=100);
}
//______________________________________________________________________________
double Poli2::Fit()
{ 
  double Wt=0,Wx=0,Wy=0;
  for (int i=0;i<fN;i++) { Wt += fW[i]; Wx += fW[i]*fX[i];Wy += fW[i]*fY[i];}
  fX0 = Wx/Wt; fY0 = Wy/Wt;
  fX0 = 0; fY0=0;
  for (int i=0;i<fN;i++) { fX[i]-=fX0; fY[i]-=fY0;}

  double A[3][3]={{0}},B[3]={0};
  double x[3]={1};
  fXi2=0;fLiH=0;fKor=0,fdKor=0;
  for (int i=0;i<fN;i++) {
    double w = fW[i];
    x[1] = fX[i]; x[2]=x[1]*x[1];
    double y = fY[i],y2=y*y;
    for (int j=0;j<=fPw;j++) { B[j]    +=w*x[j]*y;
    for (int k=0;k<=j  ;k++) { A[j][k] +=w*x[j]*x[k];}}
    fXi2 +=w*y2;
    fLiH -= log(w);
    if (!fReg) continue;
    fKor += Kor(i);
    fdKor+= dKordW(i)*fW[i];
  }
  for (int j=0;j<=fPw;j++){ fB[j] = B[j];
  for (int k=0;k<=j  ;k++){ fA(j,k) = A[j][k]; fA(k,j)=A[j][k];}}

  double det=0;
  fAi=fA;fAi.InvertFast(&det);
  fP=fAi*fB;
  fXi2 -= (fB*fP);
  return fXi2;
}
//______________________________________________________________________________
double Poli2::Kor(int ipt) const
{
  if (!fReg) return 0;
  double t = fReg*sqrt(fW[ipt]/2);
  double kor = 2*TMath::Erfc(t);
  return kor;
}
//______________________________________________________________________________
double Poli2::dKordW(int ipt) const
{
  if (!fReg) return 0;
// kor =  2*TMath::Erfc(t);
//d/dW = X2 - 1/W + 1/W * 1/F(A*sqrt(W)) * A*sqrt(W) *exp(-A*A*W/2)
  double t = fReg*sqrt(fW[ipt]/2);
  double dkor =  M_2_SQRTPIl*exp(-t*t)*fReg/(sqrt(2*fW[ipt]));
  return dkor;
}   


//______________________________________________________________________________
double Poli2::Fun( double x ) const
{
// Polinom of x == a0+a1*x+a2*x*x

  x -= fX0;
  double xx[3]={1,x,x*x};
  TVectorD XX(fPw+1,xx);
  return fP*XX + fY0;
}
//______________________________________________________________________________
//______________________________________________________________________________
double Poli2::FunErr2( double x ) const
{
  x -= fX0;
  double xx[3]={1,x,x*x};
  TVectorD XX(fPw+1,xx);
  return XX*(fAi*XX);
}
//______________________________________________________________________________
double Poli2::Pull( double x ,double y,double yErr2) const
{
  double dif = y-Fun(x);
  double err = yErr2-FunErr2(x);
  if (err<1e-10) {
    printf("Poli2::Pull Error2 too small %g replaced tp 1e-10\n",err);
    err = 1e-10;
  }
  return dif/sqrt(err);
}
//______________________________________________________________________________
double Poli2::Xi2(int ipt) const
{
   double dy = Fun(fX[ipt]+fX0) - (fY[ipt]+fY0);
   return dy*dy*fW[ipt];
}
//______________________________________________________________________________
double Poli2::Res(int ipt) const
{
   assert(ipt<fN);
   double dy = Fun(fX[ipt]+fX0) - (fY[ipt]+fY0);
   return dy;
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
//   Xi2 = Y*Y*W -B*Ai*B
//  dXi2/dWk = Yk*Yk - (2*dBk*Ai*B - B*Ai*dAk*Ai*B)
//  dXi2/dWk = Yk*Yk - (2*dBk*Ai*B - P*dAk*P

   double der = fY[ipt]*fY[ipt];
   TVectorD dB(fPw+1);
   TMatrixD dA(fPw+1,fPw+1);
   double x[3]={1,fX[ipt],fX[ipt]*fX[ipt]};
   for (int j=0;j<=fPw;j++) { dB[j] = x[j]*fY[ipt];   
   for (int k=0;k<=fPw;k++) { dA[j][k] = x[j]*x[k]; }}  
   der -= (2*(dB*(fAi*fB))-(fP*(dA*fP)));
   return der;
}
//______________________________________________________________________________
double Poli2::d2Xi2dW( int kpt ,int lpt) const
{
//   Xi2 = Y*Y*W -B*Ai*B
// dAI/dWk = - Ai*dAk*Ai
// d2Ai/dWk/dWl = -2* dAIl*dAk*Ai
// d2Ai/dWk/dWl = +2* Ai*dAl*Ai*dAk*Ai

//   dXi2/dWk = Yk*Yk-2dBk*Ai*B -B*dAIk*B
//   d2Xi2/dWk/dWl = -2dBk*Ail*B -2dBk*Ai*dBl-2dBl*dAIk*B-B*dAIkl*B


   double der = 0;
   TVectorD dBk(fPw+1),dBl(fPw+1);
   TMatrixD dAk(fPw+1,fPw+1),dAl(fPw+1,fPw+1);
   {
     double x[3]={1,fX[kpt],fX[kpt]*fX[kpt]};
     for (int j=0;j<=fPw;j++) { dBk[j] = x[j]*fY[kpt];   
     for (int k=0;k<=fPw;k++) { dAk[j][k] = x[j]*x[k]; }}  
   }
   {
     double x[3]={1,fX[lpt],fX[lpt]*fX[lpt]};
     for (int j=0;j<=fPw;j++) { dBl[j] = x[j]*fY[lpt];   
     for (int k=0;k<=fPw;k++) { dAl[j][k] = x[j]*x[k]; }}  
   }




   TMatrixD dAIk(fPw+1,fPw+1),dAIl(fPw+1,fPw+1),dAIkl(fPw+1,fPw+1);
   dAIk = -1.* fAi*dAk*fAi;
   dAIl = -1.* fAi*dAl*fAi;
   dAIkl = 2.* fAi*dAl*fAi*dAk*fAi;
   der = 2.*(dBk*(dAIl*fB) + dBk*(fAi*dBl)+dBl*(dAIk*fB))+fB*(dAIkl*fB);


   return -der;
}
//______________________________________________________________________________
double Poli2::dLiHdW( int ipt ) const
{
   double dd = dXi2dW(ipt)-1./fW[ipt];
   if (!fReg) return dd;
   dd += dKordW(ipt);
   return dd;
}
//______________________________________________________________________________
double Poli2::d2LiHdW( int kpt ,int lpt) const
{
   double qwe = 0;
   if (kpt==lpt) { qwe = 1./(fW[kpt]*fW[kpt])		;}
   return d2Xi2dW(kpt,lpt)+qwe;
}
//______________________________________________________________________________
//______________________________________________________________________________
double Poli2::Deriv( TVectorD &Di ) const
{
  Di.ResizeTo(fN);
  for (int ipt=0;ipt<fN;ipt++) {
    Di[ipt]= dLiHdW(ipt);
  }
  return fXi2;
}
//______________________________________________________________________________
double Poli2::D2riv( TMatrixD &Dik ) const
{
  Dik.ResizeTo(fN,fN);
  for (int ipt=0;ipt<fN;ipt++) {
    for (int kpt=0;kpt<=ipt;kpt++) {
      double qwe = d2LiHdW(ipt,kpt);
      Dik[ipt][kpt] = qwe; Dik[kpt][ipt] = qwe;}}
  return fXi2;
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
  double A[3]={3,0.02,0.05};

  int npw=2;
  double X[20],Y[20],W[20],YY[20];
  for (int i=0;i<20;i++) {
    X[i]=i;
    Y[i]= A[0]+X[i]*(A[1]+A[2]*X[i]);
    W[i]= 1+10./Y[i];
    YY[i]=Y[i];
    Y[i]+=gRandom->Gaus(0,sqrt(1./W[i]));
  }
  Poli2 pp(npw,20,X,Y,W);
  pp.Fit(); 
//  pp.MyTest();
  double Xi2 = pp.Xi2();
  double Xi2Eva = pp.EvalXi2(); if ( Xi2Eva) {};
  printf ("Xi2 = %g Fun(10)= %g\n",Xi2,pp.Fun(10.));

  TPoliFitter pf(2);
  for (int i=0;i<20;i++) { pf.Add(X[i],Y[i],1./W[i]);}
  Xi2 =pf.Fit()*(20-3);
  printf ("Xi2 = %g Fun(10)= %g\n",Xi2,pf.Eval(10.));
   

//   for (int i=0; i<20; i++) {
//     printf(" %g %g : %g\n",X[i],YY[i],pp.Fun(X[i]));
//   }
// check d/dW
  printf("\n check d/dWi\n");
  double myDelta = 1e-3,delta,maxDif=0;
  for (int k=0;k<20;k++) {
    Poli2 ppp(npw,0);
    for (int i=0;i<20;i++) {
      double w = W[i];
      if (i==k) { delta = w*myDelta; w+=delta;}
      ppp.Add(X[i],Y[i],w);
    }
    ppp.Fit();
    double ana = pp.dXi2dW(k);
    double num = (ppp.Xi2()-pp.Xi2())/delta;
    double dif = 2*(num-ana)/(fabs(num+ana)+3e-33);
    if (maxDif<fabs(dif)) maxDif=fabs(dif);
    if (fabs(dif)<1e-3) continue;
    printf ("dXi2dW(%2d) \tana=%g \tnum = %g \tdif=%g\n",k,ana,num,dif);
  }
  printf ("Test dXi2dW(...) maxDif=%g\n",maxDif);
}
//______________________________________________________________________________
void Poli2::Test2()
{
  int npw=2;
  double X[20],Y[20],W[20],YY[20];
  for (int i=0;i<20;i++) {
    X[i]=i;
    Y[i]= 3+X[i]*(.02+.03*X[i]);
    W[i]= 1+10./Y[i];
    YY[i]=Y[i];
    Y[i]+=gRandom->Gaus(0,sqrt(1./W[i]));
  }
  Poli2 pp(npw,20,X,Y,W);
  pp.Fit(); 
//  pp.MyTest();
  TVectorD myDer(20),nyDer(20);
  pp.Deriv(myDer);
  printf("\n check d/dWi\n");
  double myDelta = 1e-3,delta,maxDif=0;
  for (int k=0;k<20;k++) {
    Poli2 ppp(npw,0);
    for (int i=0;i<20;i++) {
      double w = W[i];
      if (i==k) { delta = w*myDelta; w+=delta;}
      ppp.Add(X[i],Y[i],w);
    }
    ppp.Fit();
    ppp.Deriv(nyDer);
    for (int l=0;l<=k; l++) {

      double ana = 0.5*(pp.d2Xi2dW(k,l)+ppp.d2Xi2dW(k,l));
      double num = (nyDer(l)-myDer(l))/delta;
      double dif = 2*(num-ana)/(fabs(num+ana)+3e-33);
      if (fabs(dif)> maxDif) maxDif =fabs(dif);
      if (fabs(dif)<1e-3) continue;
      printf ("d2Xi2dW(%2d,%2d) \tana=%g \tnum = %g \tdif=%g\n",k,l,ana,num,dif);
    }
  }
  printf ("Test2 d2Xi2dW(...) maxDif=%g\n",maxDif);
  

}
//______________________________________________________________________________
void Poli2::TestIt() const
{
  TVectorD g,gT;
  Poli2 pp(*this);
  pp.Fit(); 
  double Xi2 = pp.Deriv(g);

  double delta = 1e-3;
  for (int k=0;k<fN;k++) {
    double sav = pp.fW[k];
    pp.fW[k]=sav+delta;
    pp.Fit(); 
    double Xi2T = pp.Deriv(gT);
    pp.fW[k]=sav;

    double ana = 0.5*(g[k]+gT[k]);
    double num = (Xi2T-Xi2)/delta;
    double dif = 2*(num-ana)/(fabs(num+ana)+1e-10);
    if (fabs(dif)>0.01)
    printf ("\ndXi2dW(%2d) \tana=%g \tnum = %g \tdif=%g\n\n",k,ana,num,dif);
  }


}
//______________________________________________________________________________
void Poli2::TestErr()
{
  double A[3]={3,0.02,0.05};

  int npw=2;
  double X[20],Y[20],W[20],YY[20];
  for (int i=0;i<20;i++) {
    X[i]=i;
    YY[i]= A[0]+X[i]*(A[1]+A[2]*X[i]);
    W[i]= 1+10./YY[i];
  }
  int nEv=10000,nTot=0;
  double av=0,rms=0;
  for (int iev=0;iev<nEv;iev++) 
  {

    for (int i=0;i<20;i++) {
      Y[i]= gRandom->Gaus(YY[i],sqrt(1./W[i]));
    }
    Poli2 pp(npw,20,X,Y,W);
    pp.Fit(); 
    for (int ix=0;ix<20;ix++) {
      double delta = (pp.Fun(X[ix])-YY[ix])/sqrt(pp.FunErr2(X[ix]));
      av+=delta;rms+=delta*delta;nTot++;
    }
  }
  av/=nTot; rms/=nTot; rms=sqrt(rms);


  printf ("TestErr Av=%g Rms=%g\n",av,rms);
}
//______________________________________________________________________________
void poli2()
{
Poli2::Test();
Poli2::Test2();
Poli2::TestErr();
}

//______________________________________________________________________________
FEEvent::FEEvent(TTreeIter &th):
  run 		( th("mRun")),
  evt 		( th("mEvt")),
  nGHits    	( th("mHitsG")),
  mCurv    	( th("mHitsG.mCurv")),
  mChi2    	( th("mHitsG.mChi2")),
  mPt      	( th("mHitsG.mPt")),
  mTrackNumber 	( th("mHitsG.mTrackNumber")),
  nAllHits 	( th("mHitsG.nAllHits")),
  nTpcHits 	( th("mHitsG.nTpcHits")),
  nFtpcHits 	( th("mHitsG.nFtpcHits")),
  nSsdHits 	( th("mHitsG.nSsdHits")),
  nRndHits 	( th("mHitsG.nRndHits")),
  mDetector 	( th("mHitsG.mDetector")),
  lYHit 	( th("mHitsG.lYHit")),
  lZHit 	( th("mHitsG.lZHit")),
  lLen  	( th("mHitsG.lLen")),
  gPhiHP	( th("mHitsG.gPhiHP")),		
  gLamHP	( th("mHitsG.gLamHP")),
  gPsi  	( th("mHitsG.gPsi")),
  gDip  	( th("mHitsG.gDip")),
  gRFit    	( th("mHitsG.gRFit")),
  gPFit    	( th("mHitsG.gPFit")),
  gZFit    	( th("mHitsG.gZFit"))
{
 mLTrack=0;mRTrack=-1;

}
//______________________________________________________________________________
int FEEvent::NextTrack(int &jr)
{
  mLTrack = mRTrack+1;
  if (mLTrack>=nGHits) { jr=-1; return -1;}
  mRTrack=mLTrack;
  int tkNum= mTrackNumber[mLTrack];
  for (int j=mLTrack+1; j<nGHits && mTrackNumber[j]==tkNum;j++){mRTrack++;}
  jr = mRTrack;
  return mLTrack;
}
//______________________________________________________________________________
FERead::FERead(const char *file): TNamed(file,"")
{
  mTreeIter = new TTreeIter();
  mTreeIter->AddFile(file);
  mEvent = new FEEvent(*mTreeIter);
}
//______________________________________________________________________________
FEEvent* FERead::ReadEvent()
{
  mEvent->Reset();
  int nbytes = mTreeIter->Next();
  if (!nbytes) return 0;
  return mEvent;
}

//______________________________________________________________________________
FECalcHolder::FECalcHolder(int id,StvHitErrCalculator* calc
                          ,const double miMax[2][2])	
{  
  memset(mBeg,0,mEnd-mBeg+1);
  mId=id; mCalc=calc; 
  memcpy(mMiMax[0],miMax[0],sizeof(mMiMax));
  double *par = calc->GetPars();
  for (int j=0;j<2;j++) {par[j] = 0.5*(miMax[j][0]+miMax[j][1]);}
}
//______________________________________________________________________________
FECalcHolder::FECalcHolder()	
{  
  memset(mBeg,0,mEnd-mBeg+1);
}
//______________________________________________________________________________
 void FECalcHolder::Update(const double *a)	
 {
    double b[100];
    int n = mCalc->GetNPars();
    for (int i=0;i<n;i++) {
      b[i]=a[i+mOffset]; if (b[i]<1e-8) b[i]=1e-8;
    }
    mCalc->SetPars(b);
}
//______________________________________________________________________________
void FECalcHolder::DbLoad()
{
  gSystem->Load("libStDb_Tables.so");
  TString dbName(GetName());


  TString dbFile("StarDb/Calibrations/tracker/");
  dbFile += dbName; dbFile += ".C";
  int myN = mCalc->GetNPars();if(myN){};
  if (!gSystem->AccessPathName(dbFile)) {//file exists
    printf("FECalcHolder::DbLoad: %s\n",dbFile.Data());
    TString command (".L "); command += dbFile;
    gInterpreter->ProcessLine(command);
    mTab = (TTable *) gInterpreter->Calc("CreateTable()");
    command.ReplaceAll(".L ",".U "); 
    gInterpreter->ProcessLine(command);
    mCalc->SetPars((double*)mTab->GetArray());
//??    Scale(1./kErrFak);
  } else {				//file does not exist
    printf("FECalcHolder::DbLoad: %s NOT FOUND. Default is used)\n",dbFile.Data());
//     mTab = (TTable *)gInterpreter->Calc("new St_StvHitErrs(\"someHitError\",1)"); 
//     mTab->SetName(dbName);
//     mTab->SetUsedRows(1);
//     memcpy(mTab->GetArray(),mCalc->GetPars(),myN*sizeof(double));
  }

}
//______________________________________________________________________________
void FECalcHolder::DbSave()
{
  if (testOnly) return;
  int myN = mCalc->GetNPars();
  memcpy(mTab->GetArray(),mCalc->GetPars(),myN*sizeof(double));
  TString dbFile("StarDb/Calibrations/tracker/");
  dbFile += GetName();
  dbFile += ".C";
  if (!gSystem->AccessPathName(dbFile)) {//file exists
    TString ts(dbFile);
    ts +=".BAK"; gSystem->Rename(dbFile,ts);
  }
  {	//save FE file
    std::ofstream ofs(dbFile);

//		Save increased errors
//??    Scale(kErrFak);
    mTab->SavePrimitive(ofs);
//??    Scale(1./kErrFak);
  }
//		Save production errors
  {
    dbFile.ReplaceAll("FE.C",".C");
    std::ofstream ofs(dbFile);
    TString ts(mTab->GetName());
    assert(ts.EndsWith("FE"));
    ts.Chop();ts.Chop();
    mTab->SetName(ts);
    mTab->SavePrimitive(ofs);
    ts+="FE";
    mTab->SetName(ts);
  }
}
//______________________________________________________________________________
void FECalcHolder::Scale(double fak)
{
  int n = mTab->GetRowSize()/sizeof(double);
  double *d = (double*)mTab->GetArray();
  TCL::vscale(d,fak,d,n);
}
//______________________________________________________________________________
void FECalcHolder::AvInit()
{
   memset(mAve      ,0,sizeof(mAve   ));
   memset(mAveDer[0],0,sizeof(mAveDer));
}
//______________________________________________________________________________
void FECalcHolder::AvAdd(const double hRR[3],double yXi2,double zXi2)
{
//  double der[kMaxPars][3];
  mAve[0]++;
  mAve[1]+=hRR[0];
  mAve[2]+=hRR[0]*hRR[0];
  mAve[3]+=hRR[2];
  mAve[4]+=hRR[2]*hRR[2];
  mAve[5]+=yXi2;
  mAve[6]+=zXi2;

//  mCalc->CalcDcaDers(der);
//  TCL::vadd(mAveDer[0],der[0],mAveDer[0],kMaxPars*3);
}
//______________________________________________________________________________
void FECalcHolder::AvEnd()
{
    for (int i=1;i<7;i++) {mAve[i]/=mAve[0];}
    for (int i=2;i<5;i+=2){mAve[i] -= pow(mAve[i-1],2);}

    printf(" %s\t yRes=%g  zRes=%g \n\n",GetName(),sqrt(myFcn.mRes[1]),sqrt(myFcn.mRes[3]));
    printf(" %s\t yErr= %5.2g(+-%5.2g) \tzErr = %5.2g(+-%5.2g)  Hits=%g\n"
          ,GetName()
          ,sqrt(mAve[1]), mAve[2]/(2*sqrt(mAve[1]))   
          ,sqrt(mAve[3]), mAve[4]/(2*sqrt(mAve[3])), mAve[0]);  
    printf(" %s\t yXi2=%g  zXi2=%g \n\n",GetName(),mAve[5],mAve[6]);

    
}
//______________________________________________________________________________
StvHitErrCalculator *FETpcCalcHolder::GetCalc(const float x[3])
{
static const double Rinner =120.8;
static const double Dinner =120.8/cos(15./180*M_PI);
static const double kZPrompt = 205;
   int inOut = -1;
   if (strstr(mCalc->GetName(),"TpcInnerHit"   )) inOut=0;;  
   if (strstr(mCalc->GetName(),"TpcOuterHit"   )) inOut=1;;  
   if (strstr(mCalc->GetName(),"TpcInnerPrompt")) inOut=2;;  
   if (strstr(mCalc->GetName(),"TpcOuterPrompt")) inOut=3;;  
   assert(inOut>=0);
   float rxy = x[0]*x[0]+x[1]*x[1];
   int jk=-1;
   if (rxy<Rinner*Rinner) jk=0;
   if (rxy>Dinner*Dinner) jk=1;
   if (jk==-1) {
     float gphi = atan2(x[1],x[0])/M_PI*180;
     gphi = fmod(gphi+15,30.)-15;
     gphi = fmod(gphi+15,30.)-15;
     jk = (sqrt(rxy)*cos(gphi/180*M_PI) <Rinner)? 0:1;
   }
   if (fabs(x[2])>kZPrompt) jk+=2; 
   return (inOut==jk)? mCalc:0;
}
//______________________________________________________________________________
int FETpcCalcHolder::FixPars(FEFcn *fcn)
{
enum {
kYErr  	=0, 	/* Intrinsic resolution, padrow or Y direction		*/
kZErr  	=1, 	/* Intrinsic resolution, z direction			*/
kThkDet	=2,	/* detector thickness**2 , not fitted			*/
kYYDiff	=3,  	/* Diffusion in XY direction *yFactor			*/
kZZDiff	=4,  	/* Diffusion in Z direction  *ZFactor			*/
kYFact 	=5, 	/*	Error factor in Y-direction 			*/
kZFact 	=6, 	/*	Error factor in Z-direction 			*/
kZAB2  	=7};	/* Constant member in Z direction (a*b)**2		*/

    if (strstr(GetName(),"Prompt")) return 0;
    fcn->FixPar(kZAB2+mOffset);
    fcn->FixPar(kThkDet+mOffset);
    return 2;
}
//______________________________________________________________________________
FETstCalcHolder::FETstCalcHolder()
{

 mId = 1946;
 mCalc = new StvHitErrCalculator("TestHitErrCalc",2);
 double pars[2]={0.04,0.01};
 mCalc->SetPars(pars);
}

//______________________________________________________________________________
//______________________________________________________________________________
FEFcn::FEFcn()
{ 
  Clear();
  mgInst = this;
}
//______________________________________________________________________________
void FEFcn::Clear()
{
  memset(mFix,0,mEnd-mFix);
  mTks.clear();
  mCas.clear();
}
//______________________________________________________________________________
void FEFcn::FixPar(int ipar,int fix)
{
  assert(mFix[ipar]!=2);
  mFix[ipar]=fix;
  if (fix) {myFitter.FixParameter(ipar);mNFixd++;}
  else     {myFitter.Release(ipar)     ;mNFixd--;}
}
//______________________________________________________________________________
int FEFcn::GetIPar(const char* name) const
{
   for (int ip=0;ip<mNPars;ip++) {if (mNams[ip]==name) return ip;}
   printf("FEFcn::GetIPar Parameter %s NOT FOUND\n",name);
   return -1;
}
//______________________________________________________________________________
void FEFcn::Add(FECalcHolder* holder)
{ 
  holder->SetOffset(mNPars);
  mNPars+=holder->GetNPars();
  mCas.push_back(holder);
}
//______________________________________________________________________________
int FEFcn::FixPars()
{ 
  int n = mCas.size();
  int nn = 0;
  for (int i=0;i<n;i++) {
    nn+= mCas[i]->FixPars(this);
  }
 return nn;

}
//______________________________________________________________________________
void FEFcn::InitFitter()
{
  myFitter.SetMaxIterations(2000);
  myFitter.SetPrintLevel(1);
  Synchro('C');
  TCL::ucopy(mPars,mFist,mNPars);

  myFitter.SetFCN(&Fcn);
  int n = FixPars();
  printf("FEFcn::InitFitter() %d fixed params\n",n);

}
//______________________________________________________________________________
void FEFcn::DbLoad()
{
  for (int i=0;i<(int)mCas.size();i++) { mCas[i]->DbLoad(); }
}
//______________________________________________________________________________
void FEFcn::DbSave()
{
  for (int i=0;i<(int)mCas.size();i++) { mCas[i]->DbSave(); }
}

//______________________________________________________________________________
void FEFcn::Update(const double *upd)
{
  for (int i=0;i<(int)mCas.size();i++) { 
    mCas[i]->Update(upd);
  }
}
//______________________________________________________________________________
int FEFcn::Fit()
{
  int stat=0;
  stat=myFitter.Command("SET GRAD ");
  stat=myFitter.Command("SET GRAD 1");
  assert(!stat);
  stat=myFitter.Command("SET STRATEGY 0 ");
  assert(!stat);
//stat=myFitter.Command("SET ERRORDEF 0.0001");
  stat=myFitter.Command("SHOW EPS");
  stat=myFitter.Command("SHOW ERR");
  mVal99 = 0;
  for (int it=0;it<10;it++) {  
    mVal00 = mVal99;
    Approx(0);
    stat = myFitter.Migrad();
    printf("%d Command Migrad == %d\n",it,stat);
    if (!stat) {break;}
    stat = myFitter.Command("SCAN");
    printf("%d Command Scan== %d\n",it,stat);
    if (stat) break;
  }
//  myFitter.Command("IMPROVE");
  Approx(1);
  printf("FEFcn::Fit() Average Xi2/Ndf = %g(y) %g(z) %g\n",mXi2[0],mXi2[1],mXi2[2]);
  return 0;

}
//______________________________________________________________________________
FECalcHolder *FEFcn::GetCalc(int id,const float hiPos[3])
{
    for (int i=0;i<(int)mCas.size();i++) {
      if (id!=mCas[i]->GetId()) continue;
      StvHitErrCalculator *calc = mCas[i]->GetCalc(hiPos);
      if (calc) return mCas[i];
    }
    assert(0 && "HitErrCalculator not found");
    return 0;
}

//______________________________________________________________________________
int FEFcn::Add(FEEvent* ev)
{ 
  mNEvs++;
  int jl,jr;
  FENode node;
  while ((jl=ev->NextTrack(jr))>-1) 
  {
    if (fabs(ev->mCurv[jl])>kMaxCur) 			continue;
    if (ev->nAllHits[jl]   <kMinHits) 			continue;
    if (fabs(ev->mPt[jl]/cos(ev->gDip[jl]))<kPMin) 	continue;

    int nNodes=0;
    double y00=0,z00=0;
    FETrak myTrak;

//		drop 5 worst hits
//     int n = jr-jl+1;
//     int idx[100];
//     TMath::Sort(n,ev->mChi2+jl,idx);
//     float minXi2 = ev->mChi2[jl+idx[4]];
    for (int j=jl;j<=jr;j++) {   
      if (fabs(ev->lYHit[j])>kMaxDY) 	continue;
      if (fabs(ev->lZHit[j])>kMaxDZ) 	continue;
      double psi = ev->gPsi[j];
      double dip = ev->gDip[j];
      node.tkDir[0] = cos(dip)*cos(psi);
      node.tkDir[1] = cos(dip)*sin(psi);
      node.tkDir[2] = sin(dip);

      double phi = ev->gPFit[j];
      assert(fabs(phi)<7);
      double rxy = ev->gRFit[j];
      assert(rxy<300);
      node.hiPos[0] = rxy*cos(phi);
      node.hiPos[1] = rxy*sin(phi);
      node.hiPos[2] = ev->gZFit[j];
      assert(fabs(node.hiPos[2])<300);

      phi = ev->gPhiHP[j];
      dip = ev->gLamHP[j];
      node.hiDir[0][0] = cos(dip)*cos(phi);
      node.hiDir[0][1] = cos(dip)*sin(phi);
      node.hiDir[0][2] = sin(dip);

      node.hiDir[1][0] =-sin(phi);
      node.hiDir[1][1] = cos(phi);
      node.hiDir[1][2] = 0;

      node.hiDir[2][0] =-sin(dip)*cos(phi);
      node.hiDir[2][1] =-sin(dip)*sin(phi);
      node.hiDir[2][2] = cos(dip);

      node.s = ev->lLen[j];
      if (node.s>=300) continue;
      assert(node.s<300);
      if (!y00) { y00=ev->lYHit[j]; z00=ev->lZHit[j];}
      node.yz[0] = ev->lYHit[j]-y00;
      node.yz[1] = ev->lZHit[j]-z00;
      node.detId = ev->mDetector[j];
      nNodes++;
      myTrak.Add(node);
    }    
    assert(myTrak.NNodes()==nNodes);
    if (nNodes<kMinNodes) continue;
    mNHits += nNodes;
    mNTks++;
    mTks.push_back(myTrak);
    assert(nNodes==mTks.back().NNodes());
  }        
  assert(mTks.size());
  return 0;
}

//______________________________________________________________________________
void FEFcn::Add()
{ 

static const double rmsDY=0.1,rmsDZ=0.2,deltaS=3;
enum {kMinNodes = 10,kMinHits=15,kMaxXi2=30,kNTracks=10000};
  mNEvs++;
  FENode node;
  for (int itk=0;itk<kNTracks;itk++) 
  {
    int nNodes=0;
    FETrak myTrak;

    double psi = 0;
    double dip = 0;
    double s = 0;
    for (int ihit=0;ihit<25;ihit++) {   
      double lYHit = gRandom->Gaus(0.,rmsDY);
      double lZHit = gRandom->Gaus(0.,rmsDZ);
      if (fabs(lYHit)>kMaxDY) 	continue;
      if (fabs(lZHit)>kMaxDZ) 	continue;
      node.tkDir[0] = cos(dip)*cos(psi);
      node.tkDir[1] = cos(dip)*sin(psi);
      node.tkDir[2] = sin(dip);

      double phi = 0;
      double rxy = 100;
      node.hiPos[0] = rxy*cos(phi);
      node.hiPos[1] = rxy*sin(phi);
      node.hiPos[2] = 100;
     
      phi = 0; dip=0;

      node.hiDir[0][0] = cos(dip)*cos(phi);
      node.hiDir[0][1] = cos(dip)*sin(phi);
      node.hiDir[0][2] = sin(dip);

      node.hiDir[1][0] =-sin(phi);
      node.hiDir[1][1] = cos(phi);
      node.hiDir[1][2] = 0;

      node.hiDir[2][0] =-sin(dip)*cos(phi);
      node.hiDir[2][1] =-sin(dip)*sin(phi);
      node.hiDir[2][2] = cos(dip);

      node.s = s;s+=deltaS;
      node.yz[0] = lYHit;
      node.yz[1] = lZHit;
      node.detId = 1946;
      nNodes++;
      myTrak.Add(node);
    }    
    mNHits += nNodes;
    mNTks++;
    mTks.push_back(myTrak);
    assert(nNodes==mTks.back().NNodes());
  }        
  assert(mTks.size());
  return;
}

//______________________________________________________________________________
void FEFcn::Eval(int npar, double* gradp, double& fvalp, double* par, int flag)
{
   assert(npar<=mNPars);
   mNCall++;
   long double grad[100]={0},fval=0,myXi2[3]={0};
   long double gra2[100]={0},fval2=0;
   int myN=0,myNy=0,myNz=0;

   memset(mRes,0,sizeof(mRes));
   int myNHits = 0;
//	Update calc's params by current ones
   Synchro('A',par);
//??   Poli2 poliSY(2),poliSZ(1);
   Poli2 poliSY(2,kMaxDY),poliSZ(1,kMaxDZ);
   for (int itk=0;itk<(int)mTks.size();itk++) {//Loop over tracks
     const FENodes &nodes = mTks[itk].Nodes();
     int nNodes = nodes.size();;
     poliSY.Clear(); poliSZ.Clear();
     FECalcHolder *hold[100];
     double hRR[3],dRR[10][3];
     for (int iNode=0;iNode<nNodes;iNode++) {//1st Loop over nodes
       const FENode &n = nodes[iNode];
       hold[iNode] = GetCalc((int)n.detId,n.hiPos);
       StvHitErrCalculator *calc = hold[iNode]->GetCalc();
       calc->SetTrack(n.tkDir);
       calc->CalcDcaErrs(n.hiPos,n.hiDir,hRR);
       assert(hRR[0]>0);
       assert(hRR[2]>0);
       assert(hRR[0]*hRR[2]>hRR[1]*hRR[1]);
       poliSY.Add(n.s,n.yz[0],1./hRR[0]); 
       poliSZ.Add(n.s,n.yz[1],1./hRR[2]); 
     } // end 1st Loop over nodes

     poliSY.Fit(); 
     poliSZ.Fit(); 
     fval += ((poliSY.LiH()+poliSZ.LiH()))-mVal00/mTks.size();
     myXi2[0] += poliSY.Xi2()-(nNodes);
     myXi2[1] += poliSZ.Xi2()-(nNodes);

     myXi2[2] = 0.5*(myXi2[0]+myXi2[1]);
     fval2 += poliSY.Xi2()+poliSZ.Xi2()- 2*nNodes;
     myNy+=(nNodes);
     myNz+=(nNodes);
     myN+=myNy+myNz;

     myNHits+=nNodes;
     for (int iNode=0;iNode<nNodes;iNode++) {//residuals
       double resY = poliSY.Res(iNode);
       mRes[0]+=resY; mRes[1]+=resY*resY;
       double resZ = poliSZ.Res(iNode);
       mRes[2]+=resZ; mRes[3]+=resZ*resZ;
     }
     if (flag==2) {

       for (int iNode=0;iNode<nNodes;iNode++) {//2nd Loop over nodes
	 const FENode &n=nodes[iNode];
	 StvHitErrCalculator *calc = hold[iNode]->GetCalc();
	 calc->SetTrack(n.tkDir);
	 calc->CalcDcaErrs(n.hiPos,n.hiDir,hRR);
	 calc->CalcDcaDers(dRR);
  //		dLih/dW*dW/dErr2
	 double dLihdErrY = -poliSY.dLiHdW(iNode)/(hRR[0]*hRR[0]);
	 double dLihdErrZ = -poliSZ.dLiHdW(iNode)/(hRR[2]*hRR[2]);
	 double dXi2dErrY = -poliSY.dXi2dW(iNode)/(hRR[0]*hRR[0]);
	 double dXi2dErrZ = -poliSZ.dXi2dW(iNode)/(hRR[2]*hRR[2]);
	 int nPar=calc->GetNPars();
	 int offs=hold[iNode]->GetOffset();
	 for (int jPar=0;jPar<nPar;jPar++) {// Loop over Calc params
  //    		dErr2/dPar      
            double qwe =(dLihdErrY*dRR[jPar][0]+dLihdErrZ*dRR[jPar][2]);
            grad[offs+jPar]+=qwe;
                   qwe =(dXi2dErrY*dRR[jPar][0]+dXi2dErrZ*dRR[jPar][2]);
            gra2[offs+jPar]+=qwe;
 	 }// end Loop over Calc params

      }// end 2nd Loop over nodes

    }//endif flag==2
  }//End tracks
  mXi2[0] = 1+myXi2[0]/myNy;
  mXi2[1] = 1+myXi2[1]/myNz;
  mXi2[2] = 1+myXi2[2]/myN;

  fvalp = fval; if (!mVal99 || mVal99 > fval+mVal00) mVal99 = fval+mVal00;
  if (!mVal00) { mVal00 = mVal99; fvalp = 0;}
  TCL::vscale(mRes,(1./myNHits),mRes,4);


  if (flag==2)   {
    for (int i=0;i<mNPars;i++){
      gradp[i]=grad[i]; }}
}
//______________________________________________________________________________
void FEFcn::Approx(int nonBias)
{
   Synchro('M');
   long double myXi2[3]={0},mydKor=0,myPull=0;
   int myN=0;
   if (nonBias) {//Dempfer result , weighting with the previous one
     TCL::vlinco(mPars,kWeight,mFist,1.-kWeight,mPars,mNPars);
   }

//	Update calc's params by current ones
//??   Poli2 poliSY(2),poliSZ(1);
   Poli2 poliSY(2,kMaxDY),poliSZ(2,kMaxDZ);
   for (int itk=0;itk<(int)mTks.size();itk++) {//Loop over tracks 
     const FENodes &nodes = mTks[itk].Nodes();
     int nNodes = nodes.size();
     poliSY.Clear(); poliSZ.Clear();
     FECalcHolder *hold[100];
     double hRR[3];
     for (int iNode=0;iNode<nNodes;iNode++) {//1st Loop over nodes
       const FENode &n = nodes[iNode];
       hold[iNode] = GetCalc((int)n.detId,n.hiPos);
       StvHitErrCalculator *calc = hold[iNode]->GetCalc();
       calc->SetTrack(n.tkDir);
       calc->CalcDcaErrs(n.hiPos,n.hiDir,hRR);
       assert(hRR[0]>0);
       assert(hRR[2]>0);
       poliSY.Add(n.s,n.yz[0],1./hRR[0]); 
       poliSZ.Add(n.s,n.yz[1],1./hRR[2]); 
     } // end 1st Loop over nodes

     poliSY.Fit(); 
     poliSZ.Fit(); 
     myXi2[0] += poliSY.Xi2()-nNodes;
     myXi2[1] += poliSZ.Xi2()-nNodes;

     myXi2[2]  = 0.5*(myXi2[0]+myXi2[1]);
     mydKor += poliSY.dKordW()+poliSZ.dKordW();
     myN+=nNodes;

     for (int iNode=0;iNode<nNodes;iNode++) {//1st Loop over nodes
       const FENode &n = nodes[iNode];
       hold[iNode] = GetCalc((int)n.detId,n.hiPos);
       StvHitErrCalculator *calc = hold[iNode]->GetCalc();
       calc->SetTrack(n.tkDir);
       calc->CalcDcaErrs(n.hiPos,n.hiDir,hRR);
       assert(hRR[0]>0);
       assert(hRR[2]>0);
       myPull += pow(poliSY.Pull(n.s,n.yz[0],hRR[0]),2);
       myPull += pow(poliSZ.Pull(n.s,n.yz[1],hRR[2]),2);

     } // end 1st Loop over nodes

  }//End tracks
  
  mXi2[0] = 1+myXi2[0]/myN;
  mXi2[1] = 1+myXi2[1]/myN;
  mXi2[2] = 0.5*(mXi2[0]+mXi2[1]);
  myPull /=(2*myN);
  mydKor /=(2*myN);
  printf("Approx::Aver: Xi2=%g(y) %g(z) %g Pull=%g\n"
        ,mXi2[0],mXi2[1],mXi2[2],double(myPull));

  if (gScale) {
    double korFak = (1.+mydKor/mXi2[2]);
    if (!nonBias)  {TCL::vscale(mPars,      mXi2[2]*korFak ,mPars,mNPars);}
    else           {TCL::vscale(mPars,double(myPull*korFak),mPars,mNPars);}
  }

  Synchro('P');
}
//______________________________________________________________________________
void FEFcn::Synchro(char from,const double* Arr )
{
// from: 0=from StvHitErrCalc's; 1=from Minuit; 2 = FEFcn::mPar; 3=from array

  switch (from) {
  case 'C': {// input from StvHitErrCals's
    int nump = 0;  
    for (int i=0;i<(int)mCas.size();i++) {
      StvHitErrCalculator *calc = mCas[i]->GetCalc();
      int nP = calc->GetNPars();
      const double *p = calc->GetPars();
      for (int ip=0;ip<nP;ip++) {
        TString ts(calc->GetName());ts+="_";ts+=ip;
        double qwe = p[ip]; if (qwe<1e-8) qwe=1e-8;
        double stp = qwe*0.1; if (stp<1e-8) stp = 0.8e-6;
        myFitter.DefineParameter(nump,ts.Data(), qwe, stp, kLowPar, kUppPar);
        mNams[nump] = ts;
        mPars[nump] = qwe;
        nump++;
      }
    }
    if (!mNPars) mNPars = nump;
    assert(mNPars==nump);
    break;}

  case 'M': {// input from Minuit
     double err;
     for (int ipar=0;ipar<mNPars;ipar++) {
       myFitter.GetParameter(ipar,mPars[ipar],err);}
     Update(mPars);
     break; }
     
  case 'P': {// input from FEFcn::mPar
    Update(mPars);
    TString chnam;
    double val,err,xlolim,xuplim;
    int iuint,ierr;
    for (int ipar=0;ipar<mNPars;ipar++) {
      myFitter.mnpout(ipar,chnam,val, err,xlolim,xuplim,iuint);
      val = mPars[ipar];
      myFitter.mnparm(ipar,chnam,val,err,xlolim,xuplim, ierr);
    }
    break; }   

  case 'A': {// input from array
    TCL::ucopy(Arr,mPars,mNPars);
    Update(mPars);
    break;}

  default: assert(0 && "Undefined case");
  }//end switch

}

//______________________________________________________________________________
void FEFcn::Fcn(int &npar, double* grad, double& fval, double* par, int flag)
{
  mgInst->Eval(npar,grad,fval,par,flag);
}

//______________________________________________________________________________
void FEFcn::AvErr(const char *tit)
{
  for (int ih=0;ih<(int)mCas.size();ih++) 
  {
    FECalcHolder *hold = mCas[ih]; hold->AvInit();
  }
  Poli2 poliSY(2,kMaxDY),poliSZ(2,kMaxDZ);
  for (int itk=0;itk<(int)mTks.size();itk++) {//Loop over tracks 
    const FENodes &nodes = mTks[itk].Nodes();
    int nNodes = nodes.size();
    poliSY.Clear(); poliSZ.Clear();
    FECalcHolder *hold[100];
    double errs[100][3];
    double hRR[3];
    for (int iNode=0;iNode<nNodes;iNode++) {//1st Loop over nodes
      const FENode &n = nodes[iNode];
      hold[iNode] = GetCalc((int)n.detId,n.hiPos);
      StvHitErrCalculator *calc = hold[iNode]->GetCalc();
      calc->SetTrack(n.tkDir);
      calc->CalcDcaErrs(n.hiPos,n.hiDir,hRR);
      memcpy(errs[iNode],hRR,sizeof(hRR));
      poliSY.Add(n.s,n.yz[0],1./hRR[0]); 
      poliSZ.Add(n.s,n.yz[1],1./hRR[2]); 
    } // end 1st Loop over nodes

     poliSY.Fit(); 
     poliSZ.Fit(); 

    for (int iNode=0;iNode<nNodes;iNode++) {//1st Loop over nodes
      double yXi2 = poliSY.Xi2(iNode);
      double zXi2 = poliSZ.Xi2(iNode);
      hold[iNode]->AvAdd(errs[iNode],yXi2,zXi2);
    } // end 2nd Loop over nodes
  }//End tracks

  printf("\n ====== AvErr(%s) Events=%d  Tracks=%d Hits=%d\n"
        ,tit,mNEvs,mNTks,mNHits);

  for (int ih=0;ih<(int)mCas.size();ih++) 
  {
    FECalcHolder *hold = mCas[ih];
    hold->AvEnd();
  }
  return;
}
//______________________________________________________________________________
int FEFcn::IsEnded() const
{
  double sum = 0,dlt=0;
  for (int i=0;i<mNPars;i++) {
    sum += mPars[i];
    double myDlt = fabs(mPars[i]-mFist[i]);
    if (dlt<myDlt) dlt=myDlt;
  }
  dlt /= sum/mNPars;
  printf(" *** IsEnded with*** %g\n",dlt);
  if (dlt > 1e-2) return 0;
  printf(" *** Fit CONVERGED Hurragh***\n");
  return 1;
}  
  
  
//______________________________________________________________________________
//______________________________________________________________________________
#ifdef APPROX
//______________________________________________________________________________
FEApprox::FEApprox(FEFcn *fefcn)
{
  mFcn=fefcn;
  mNPars = mFcn->GetNPars();
  mG.ResizeTo(mNPars,mNPars);
  mGi.ResizeTo(mNPars,mNPars);
  mB.ResizeTo(mNPars);
  mP.ResizeTo(mNPars);
  mPrev.ResizeTo(mNPars);
  mPrev = TVectorD(mNPars,mFcn->GetPars());
  mXlo.ResizeTo(mNPars);
  mIxlo.ResizeTo(mNPars);mIxlo =1.;
  mXup.ResizeTo(mNPars);
  mIxup.ResizeTo(mNPars);mIxup =1.;
}
//______________________________________________________________________________
int FEApprox::Prepare()
{
   assert(mNPars<100);
   long double myXi2=0,myB[100]={0},myG[100][100]={{0}};
   int myN=0;
   TMatrixD d2LdWWy,d2LdWWz,dSdPy,dSdPz;
   TMatrixD dWdSy,dWdSz,d2WdSSy,d2WdSSz;
   TVectorD dLdWy,dLdWz;

//??   Poli2 poliSY(2),poliSZ(1);
   Poli2 poliSY(2),poliSZ(2);
   const FETrak* trak = mFcn->GetTrak();
   for (;trak;trak=trak->Next()) {   
     int nNodes = trak->GetNNodes();
     poliSY.Clear(); poliSZ.Clear();
     FECalcHolder *hold[100];
     double hRR[3],dRR[10][3];
     for (int iNode=0;iNode<nNodes;iNode++) {//1st Loop over nodes
       const FENode *n = trak->GetNode(iNode);
       hold[iNode] = mFcn->GetCalc((int)n->detId,n->hiPos);
       StvHitErrCalculator *calc = hold[iNode]->GetCalc();
       calc->SetTrack(n->tkDir);
       calc->CalcDcaErrs(n->hiPos,n->hiDir,hRR);
       assert(hRR[0]>0);
       assert(hRR[2]>0);
       poliSY.Add(n->s,n->yz[0],1./hRR[0]); 
       poliSZ.Add(n->s,n->yz[1],1./hRR[2]); 
     } // end 1st Loop over nodes

     poliSY.Fit(); 
     poliSZ.Fit(); 
     myXi2 +=poliSY.LiH()+poliSZ.LiH();
     poliSY.Deriv(dLdWy); poliSY.D2riv(d2LdWWy);
     poliSZ.Deriv(dLdWz); poliSZ.D2riv(d2LdWWz);
     dWdSy.ResizeTo(nNodes,nNodes); dWdSy=0.;
     dWdSz.ResizeTo(nNodes,nNodes); dWdSz=0.;
     d2WdSSy.ResizeTo(nNodes,nNodes); d2WdSSy=0.;
     d2WdSSz.ResizeTo(nNodes,nNodes); d2WdSSz=0.;
     dSdPy.ResizeTo(mNPars,nNodes); dSdPy=0.;
     dSdPz.ResizeTo(mNPars,nNodes); dSdPz=0.;
     dSdPy.ResizeTo(mNPars,nNodes); dSdPy=0.;
     myN++;

     StvHitErrCalculator *calc=0;const FENode *n=0;
     for (int iNode=0;iNode<nNodes;iNode++) {//2nd Loop over nodes
       n=trak->GetNode(iNode);
       calc = hold[iNode]->GetCalc();
       calc->SetTrack(n->tkDir);
       calc->CalcDcaErrs(n->hiPos,n->hiDir,hRR);
       calc->CalcDcaDers(dRR);
       int offs=hold[iNode]->GetOffset();
       int nP = calc->GetNPars();
       for (int jp=0;jp<nP; jp++) {
         dWdSy[iNode][iNode]   = -1./(hRR[0]*hRR[0]);
         dWdSz[iNode][iNode]   = -1./(hRR[2]*hRR[2]);
         d2WdSSy[iNode][iNode] =  dLdWy[iNode]*2./(hRR[0]*hRR[0]*hRR[0]);
         d2WdSSz[iNode][iNode] =  dLdWz[iNode]*2./(hRR[2]*hRR[2]*hRR[2]);
         dSdPy[offs+jp][iNode] = dRR[jp][0];
         dSdPz[offs+jp][iNode] = dRR[jp][2];
       }
     }// end 2nd Loop over nodes
     
     TVectorD dLdPy(dSdPy*(dWdSy*dLdWy));
     TVectorD dLdPz(dSdPz*(dWdSz*dLdWz));
     dLdPz+=dLdPy;
     TMatrixD d2LdPPy(dSdPy*(dWdSy*d2LdWWy*dWdSy+d2WdSSy)*T(dSdPy));
     TMatrixD d2LdPPz(dSdPz*(dWdSz*d2LdWWz*dWdSz+d2WdSSz)*T(dSdPz));
     d2LdPPz+=d2LdPPy;

     for (int ip=0;ip<mNPars;ip++) {
       myB[ip]+=dLdPz[ip];
     for (int jp=0;jp<=ip;jp++) {
       myG[ip][jp] += d2LdPPz[ip][jp];
     } }


  }//End tracks
  myN = int(log(double(myN))/log(2.)); myN = 1<<myN;

  double spur=0;
  mXi2 = myXi2/myN;
  for (int i=0;i<mNPars;i++) {  
    mB[i] = myB[i]/myN;
    for (int j=0;j<=i;j++) { mG[i][j] = myG[i][j]/myN; mG[j][i]=mG[i][j];}
    if (mG[i][i]<0) printf("FEApprox::Prepare() G[%d][%d]=%g is NEGATIVE\n",i,i,mG[i][i]);

    spur+=mG[i][i];
  }
  spur*= 0.1/mNPars;
  //for (int i=0;i<mNPars;i++) { mG[i][i] += spur; }

//	Prepare boundaries
  for (int ipar=0;ipar<mNPars;ipar++) {
     double delta = 0.1,myDel;
     if (mG[ipar][ipar]>0) {
       myDel = 1./sqrt(mG[ipar][ipar]);
       if (delta > myDel) delta = myDel;
       myDel = fabs(mB[ipar]/mG[ipar][ipar]);
       if (delta > myDel) delta = myDel;
     } else {
       myDel = fabs(mB[ipar]/mG[ipar][ipar])*2/0.1;
       if (delta > myDel) delta = myDel;
     }
     mXlo[ipar] = mPrev[ipar]-delta; 
     if (mXlo[ipar]<1e-8) mXlo[ipar]=1e-8;
     mXup[ipar] = mPrev[ipar]+delta; 
  }

//	Prepare inequalities

  int nCas = mFcn->mCas.size();  
  mC.ResizeTo(nCas*2,mNPars); mC = 0.;
  mClo.ResizeTo(nCas*2);
  mIclo.ResizeTo(nCas*2); mIclo = 1;
  mCup.ResizeTo(nCas*2);
  mIcup.ResizeTo(nCas*2); mIcup = 1;
  int iRow = 0;
  for (int ic=0;ic<nCas;ic++) {
    FECalcHolder *hold = mFcn->mCas[ic];
    StvHitErrCalculator *calc = hold->GetCalc();
    int iCol = hold->GetOffset();
    int nP = calc->GetNPars();
    for (int ip=0;ip<nP;ip++) {
      mC[iRow+0][iCol+ip] = hold->mAveDer[ip][0];
      mC[iRow+1][iCol+ip] = hold->mAveDer[ip][2];
    }
    mClo[iRow+0] = hold->mMiMax[0][0];
    mClo[iRow+1] = hold->mMiMax[1][0];
    mCup[iRow+0] = hold->mMiMax[0][1];
    mCup[iRow+1] = hold->mMiMax[1][1];
    iRow+=2;
  }
  return 0;
}
#include "Riostream.h"
#include "TMath.h"
#include "TSystem.h"

#include "TMatrixD.h"
#include "TMatrixDSym.h"
#include "TVectorD.h"
#include "TQpProbDens.h"
#include "TGondzioSolver.h"
#include "TMehrotraSolver.h"

//______________________________________________________________________________
int FEApprox::Test()
{
  mP = TVectorD(mNPars,mFcn->GetPars());
  Prepare(); 
//  mB.Print();
  TVectorD baseDer(mB);
  TMatrixD baseD2r(mG);
  double baseFcn = mXi2;
  double part = 3e-2,minStp=3e-4;
  for (int ip=0;ip<mNPars;ip++) {
    if (mFcn->IsFixed(ip)) continue;
    double eps = mP[ip]*part; if (eps <minStp) eps = minStp;
    double sav = mP[ip];
    mP[ip]+=eps;
    mFcn->Update(mP.GetMatrixArray());
    Prepare(); 
    double nowFcn = mXi2;
    double num = (nowFcn-baseFcn)/eps;
    double ana = mB[ip];
    double dif = 2*(num-ana)/(fabs(num+ana)+3e-33);
    printf ("dXi2dP(%2d) \tana=%g \tnum = %g \tdif=%g\n",ip,ana,num,dif);
    for (int jp=0;jp<=ip;jp++) {
      if (mFcn->IsFixed(jp)) continue;
      num = (mB[jp]-baseDer[jp])/eps;
      ana = (mG[ip][jp]+baseD2r[ip][jp])/2;
      dif = 2*(num-ana)/(fabs(num+ana)+3e-33);
      printf ("\td2Xi2dPP(%2d,%2d) \tana=%g \tnum = %g \tdif=%g\n",ip,jp,ana,num,dif);
    }
    mP[ip]=sav;
    mFcn->Update(mP.GetMatrixArray());
  }
  
  return 0;
}		
//______________________________________________________________________________
int FEApprox::Quadr()
{
const int nrVar=mNPars;
const int nrEqual   = mFcn->GetNFixd()+3;
const int nrInEqual = mC.GetNrows();
TVectorD    c = mB;
TMatrixDSym Q(nrVar,mG.GetMatrixArray());

 // equality equation
  TMatrixD A(nrEqual,nrVar); 
  TVectorD b(nrEqual);
  int iiEqual=0;
  for (int ipar=0;ipar<mNPars;ipar++) {
    if (!mFcn->IsFixed(ipar)) continue;
    A[iiEqual][ipar] = 1;
    b[iiEqual] 	     = 0;
    iiEqual++;
  }
  A[iiEqual][4]=1; A[iiEqual][4+6]=-1.;iiEqual++;
  A[iiEqual][5]=1; A[iiEqual][5+6]=-1.;iiEqual++;
  A[iiEqual][2]=1; A[iiEqual][2+6]=-1.;iiEqual++;


  // inequality equation
  //
  // - although not applicable in the current situatio since nrInEqual = 0, one
  //   has to specify not only clo and cup but also an index vector iclo and icup,
  //   whose values are either 0 or 1 . If iclo[j] = 1, the lower boundary condition 
  //   is active on x[j], etc. ...

//   TMatrixD C   (nrInEqual,nrVar);
//   TVectorD clo (nrInEqual);
//   TVectorD cup (nrInEqual);
//   TVectorD iclo(nrInEqual);
//   TVectorD icup(nrInEqual);

  // simple square boundary condition : 0 <= x_i, so only xlo is relevant .
  // Like for clo and cup above, we have to define an index vector ixlo and ixup .
  // Since each variable has the lower boundary, we can set the whole vector
  // ixlo = 1

//   TVectorD xlo (nrVar); xlo  = 1e-8;
//   TVectorD xup (nrVar); xup  = 10;
//   TVectorD ixlo(nrVar); ixlo = 1;
//   TVectorD ixup(nrVar); ixup = 1;
  
  // setup the quadratic programming problem . Since a small number of variables are
  // involved and "Q" has everywhere entries, we chose the dense version "TQpProbDens" .
  // In case of a sparse formulation, simply replace all "Dens" by "Sparse" below and
  // use TMatrixDSparse instead of TMatrixDSym and TMatrixD

  TQpProbDens *qp = new TQpProbDens(nrVar,nrEqual,nrInEqual);

  // stuff all the matrices/vectors defined above in the proper places

  TQpDataDens *prob = (TQpDataDens *)qp->MakeData(c,Q,mXlo,mIxlo,mXup,mIxup,A,b,mC,mClo,mIclo,mCup,mIcup);


  // setup the nrVar variables, vars->fX will contain the final solution

  TQpVar      *vars  = qp->MakeVariables(prob);
  TQpResidual *resid = qp->MakeResiduals(prob);
  
  // Now we have to choose the method of solving, either TGondzioSolver or TMehrotraSolver
  // The Gondzio method is more sophisticated and therefore numerically more involved
  // If one want the Mehrotra method, simply replace "Gondzio" by "Mehrotra" .

  TGondzioSolver  *s = new TGondzioSolver(qp,prob);
//TMehrotraSolver *s = new TMehrotraSolver(qp,prob);
  const Int_t status = s->Solve(prob,vars,resid);
  assert(!status);
  mP = vars->fX;



  return 0;
}  

//______________________________________________________________________________
int FEApprox::Approx()
{
  int iter = 0,nCutStp=0;
  double myXi2 = 1000.;
  for (;iter<26;iter++)
  { 
    Prepare(); 
    if (mXi2 > myXi2 && nCutStp>5) {
      nCutStp++;
      mP = 0.5*(mP+mPrev);
      mFcn->Update(mP.GetMatrixArray());
      continue;
    }
    nCutStp=0;
    int ans = Quadr();  if(ans){};
    double eps = 0,maxpar=0;int ips=-1;
    for (int ip=0;ip<mNPars;ip++) {
      if (maxpar<fabs(mP[ip]   )) maxpar=fabs(mP[ip]   );
      if (maxpar<fabs(mPrev[ip])) maxpar=fabs(mPrev[ip]);
    }

    for (int ip=0;ip<mNPars;ip++) {
      double dif = (fabs(mP[ip]-mPrev[ip]))/maxpar;
      if (eps<dif) {ips=ip;eps=dif;}
    }
    printf("Approx: iter=%d eps[%d]=%g Parameters:\n",iter,ips,eps);
    mP.Print();
    mPrev = mP;
    mFcn->Update(mP.GetMatrixArray());
    if (eps<0.03) break;
  }
// Now release all the parameters  
  for (int iPar=0;iPar<mNPars;iPar++) {
    if (mFcn->IsFixed(iPar)!=1) continue;
//    printf("    Approx:: release parameter %d\n",iPar);
    mFcn->FixPar(iPar,0);
  }
  
  return iter;
}		
#endif // APPROX
#endif

