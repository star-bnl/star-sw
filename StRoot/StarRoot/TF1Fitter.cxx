#include <assert.h>
#include "TF1Fitter.h"

//static Color_t gMyColors[] = {kRed,kBlue,kGreen,kMagenta,kCyan};
//_____________________________________________________________________________


TF1Fitter::TF1Fitter(const char *name,int nPars):TF1(name, static_cast<DummyFuncPtr_t>(nullptr), 0,0,nPars)
{
fMean=0; fSigm=0;fTH1 = 0;
}
//_____________________________________________________________________________
void TF1Fitter::Copy(TObject& f1) const
{
  TF1Fitter &obj = (TF1Fitter&)f1;
  obj.fTH1 = fTH1;
  TF1::Copy(obj);
}
//_____________________________________________________________________________
double TF1Fitter::operator()(const double* x, const double* params)
{
  if (!params) params = GetParameters();
  return EvalPar(x,params);
}
//_____________________________________________________________________________
double TF1Fitter::EvalPar(const double *x,const double *par)
{
  assert(0);
  return 0;
}
//_____________________________________________________________________________
void TF1Fitter::SetHist(TH1 *hist)
{

  fTH1= hist;
  if(!fTH1) return;
  if (*GetName()==0) SetName(fTH1->GetName());
  fMean = fTH1->GetMean();
  fSigm = fTH1->GetRMS();
  if (fXmin >= fXmax)	{
    int nBins = fTH1->GetNbinsX();
    double Xmin = fTH1->GetBinLowEdge(1);
    double Xmax = fTH1->GetBinLowEdge(nBins)+fTH1->GetBinWidth(nBins);
    SetRange(Xmin,Xmax );
  }
}

//_____________________________________________________________________________
void TF1Fitter::Draw(const char *opt)
{
  SetLineColor(kRed);
  TF1::Draw(opt);
}


//_____________________________________________________________________________
double TF1GausFitter::EvalPar(const double *x,const double *par)
{
  double p1 = 1./(par[1]*par[1]);
  return par[2]*exp(-0.5*p1*(x[0]-par[0])*(x[0]-par[0]));
}
//_____________________________________________________________________________
void TF1GausFitter::Init()
{
static const double SQR2 = sqrt(2.);
   SetParName(0,"Mean");
   SetParName(1,"Sigm");
   SetParName(2,"Norm");


   double gral = fTH1->Integral();
   double erf = 0.5*TMath::Erf((fXmax-fMean)/(SQR2*fSigm));
   SetParameter(0,fMean);
   SetParameter(1,fSigm);
   SetParameter(2,gral/erf);
   double xLow = GetParameters()[0]-9*GetParameters()[1];
   double xUpp = GetParameters()[0]+9*GetParameters()[1];
   SetParLimits(0,xLow,xUpp);
   SetParLimits(1,GetParameters()[1]*0.1,GetParameters()[1]*10);
   SetParLimits(2,             0,GetParameters()[2]*10);
}


//_____________________________________________________________________________
double TF1TwoGausFitter::EvalPar(const double *x,const double *par)
{
  double sig1 = par[1];
  double sig2 = par[1]+par[4];
  double w1 = 1./(sig1*sig1);
  double w2 = 1./(sig2*sig2);
  double c1 = par[2]*(1-par[5]);
  double c2 = par[2]*(  par[5]);

  return c1*exp(-0.5*w1*(x[0]-par[0])*(x[0]-par[0]))
        +c2*exp(-0.5*w2*(x[0]-par[3])*(x[0]-par[3]));
}

//_____________________________________________________________________________
void TF1TwoGausFitter::Init()
{
static const double SQR2 = sqrt(2.);
   SetParName(0,"Mean");
   SetParName(1,"Sigm");
   SetParName(2,"Norm");
   SetParName(3,"MeanBak");
   SetParName(4,"AddSigmBak");
   SetParName(5,"ContriBak");


   double gral = fTH1->Integral();
   double erf = 0.5*TMath::Erf((fXmax-fMean)/(SQR2*fSigm));
   SetParameter(0,fMean);
   SetParameter(1,fSigm);
   SetParameter(2,gral/erf);
   double xLow = GetParameters()[0]-9*GetParameters()[1];
   double xUpp = GetParameters()[0]+9*GetParameters()[1];
   SetParLimits(0,xLow,xUpp);
   SetParLimits(1,GetParameters()[1]*0.1,GetParameters()[1]*10);
   SetParLimits(2,             0,GetParameters()[2]*10);

   SetParameter(3,fMean);
   SetParameter(4,fSigm);
   SetParameter(5,0.1);

   SetParLimits(3,xLow,xUpp);
   SetParLimits(4,0,GetParameters()[1]*10);
   SetParLimits(5,0,1);

}
//_____________________________________________________________________________
double TF1GausPol2Fitter::EvalPar(const double *x,const double *par)
{
  double sig1 = par[1];
  double w1 = 1./(sig1*sig1);
  double c1 = par[2];
  double p0 = par[3];
  double p1 = par[4];
  double p2 = par[5];

  double bak = p0+x[0]*(p1 + x[0]*p2);
  if (bak<0) bak = 0;
  return c1*exp(-0.5*w1*(x[0]-par[0])*(x[0]-par[0])) + bak;
}
//_____________________________________________________________________________
void TF1GausPol2Fitter::Init()
{
static const double SQR2 = sqrt(2.);
   SetParName(0,"Mean");
   SetParName(1,"Sigm");
   SetParName(2,"Norm");
   SetParName(3,"b0");
   SetParName(4,"b1");
   SetParName(5,"b2");


   double gral = fTH1->Integral();
   double erf = 0.5*TMath::Erf((fXmax-fMean)/(SQR2*fSigm));
   SetParameter(0,fMean);
   SetParameter(1,fSigm);
   SetParameter(2,gral/erf);
   double xLow = GetParameters()[0]-9*GetParameters()[1];
   double xUpp = GetParameters()[0]+9*GetParameters()[1];
   SetParLimits(0,xLow,xUpp);
   SetParLimits(1,GetParameters()[1]*0.1,GetParameters()[1]*10);
//    SetParLimits(2,             0,GetParameters()[2]*10);
//    SetParLimits(3,      0,999999);
//    SetParLimits(5,-999999,     0);

   SetParameter(3,0);
   SetParameter(4,0);
   SetParameter(5,0);
}

















#if 0
//_____________________________________________________________________________
TwoGausFitter::TwoGausFitter(TH1 *th,const char *name):TH1Fitter(th,name)
{
}
//_____________________________________________________________________________
void TwoGausFitter::Prep()
{
static const double SQR2 = sqrt(2.);
   if (mTF1) return;
   TF1 *gaus1 = new TF1Gaus("Signal");		Add(gaus1);
   TF1 *gaus2 = new TF1Gaus("BackGround");	Add(gaus2);

   double mean = mTH1->GetMean();
   printf("mean=%g\n",mean);
   double sigm = mTH1->GetRMS();
   printf("sigm=%g\n",sigm);
   double gral = mTH1->Integral();
   printf("gral=%g\n",gral);
   double erf = 0.5*TMath::Erf((mXmax-mean)/(SQR2*sigm));
   printf("erf=%g\n",erf);
          erf+= 0.5*TMath::Erf((mean-mXmin)/(SQR2*sigm));
   printf("erf=%g\n",erf);
   mPars[0] = mean;
   mPars[1] = sigm;
   mPars[2] = gral/erf;
   GetTF1();
   mTF1->SetParLimits(0,mPars[0]-3*sigm,mPars[1]+10*sigm);
   mTF1->SetParLimits(1,mPars[1]*0.1,mPars[1]*10);
   mTF1->SetParLimits(2,           0,mPars[2]*1000);


   mPars[3] = mean;
   mPars[4] = mPars[1]*0.1;
   mPars[5] = mPars[2]*0.1;

   mTF1->SetParLimits(3,mPars[0]-10*sigm,mPars[1]+10*sigm);
   mTF1->SetParLimits(4,mPars[4]*0.1,mPars[4]*10);
   mTF1->SetParLimits(5,0.          ,mPars[5]*1000);

}
//_____________________________________________________________________________
double TwoGausFitter::Fit(const char *opt)
{
  if (!mTF1) Prep();
  return TH1Fitter::Fit(opt);
}
//_____________________________________________________________________________
TH1Fitter::TH1Fitter(TH1 *th,const char *name):TNamed(name,"")
{
 memset(mBeg,0,mEnd-mBeg);
 Set(th);
}

//_____________________________________________________________________________
void TH1Fitter::Set(TH1 *th)
{
 mTH1 = th;
 if (!mTH1) 		return;
 if (*GetName()==0) SetName(mTH1->GetName());
 if (mXmin < mXmax)	return;
 int nBins = mTH1->GetNbinsX();
 mXmin = mTH1->GetBinLowEdge(1);
 mXmax = mTH1->GetBinLowEdge(nBins)+mTH1->GetBinWidth(nBins);
}
//_____________________________________________________________________________
void TH1Fitter::Add(TF1 *fcn)
{
  assert(!mTF1);
  mTF1s[mNTF1s++] = fcn;
  mNPars += fcn->GetNpar();
  assert(mNTF1s<10);
}
//_____________________________________________________________________________
TF1 *TH1Fitter::GetTF1() 
{
  if (!mTF1){//Create gloabal TF1
//		create TF1 main
    mTF1 = new TF1AuxFitter(mNPars, mNTF1s, mTF1s);
    mTF1->SetName(GetName());
  }
  mTF1->SetRange(mXmin,mXmax);
  return mTF1;
}  
//_____________________________________________________________________________
double TH1Fitter::Fit(const char *opt)
{
 GetTF1();
 mTF1->SetParameters(mPars);
 mTH1->Fit(mTF1,opt);
 mTF1->GetParameters(mPars);
 return mTF1->GetChisquare();
}
//_____________________________________________________________________________
void TH1Fitter::Draw(const char *)
{
  double par[100],*ipar=par;
  mTF1->GetParameters(par);
  
  for (int i = -1;i<mNTF1s;i++) {
     TF1 *tf = mTF1s[i];
     tf->SetLineColor(gMyColors[(i+1)%5]);
     tf->SetParameters(ipar); 
     tf->Draw("same");
     if (i>-1) ipar += tf->GetNpar();
  }
}
#endif
