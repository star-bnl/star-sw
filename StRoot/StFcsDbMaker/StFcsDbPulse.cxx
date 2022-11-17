#include "StFcsDbPulse.h"

ClassImp(StFcsDbPulse);

StFcsDbPulse::StFcsDbPulse(const char* name) : TDataSet(name)
{
  mTail = 0;
  mTBPerRC = 8;

  mGSigma = 0;
  mA1     = 0;
  mA2     = 0;
  mXoff1  = 0;
  mXoff2  = 0;
  mTau1   = 0;
  mTau2   = 0;
  mP1     = 0;
  mP2     = 0;
}

StFcsDbPulse::~StFcsDbPulse(){}

int StFcsDbPulse::Init()
{

  mGSigma = 24.5;    // signal nominal sigma
  //GSigmaSigma = 0;  // distibution of signal sigma
  mA1    = 1.0/sqrtpi()/0.155/129;
  mA2    = 0.2/sqrtpi()/0.155/129;
  mXoff1 = 70 - 129;
  mXoff2 = 220 - 129;
  mTau1  = 200.0;
  mTau2  = 40.0;
  mP1    = 1.0;
  mP2    = 1.0;

  return 0;
}

void StFcsDbPulse::setTail(int tail)
{
  mTail = tail;
  if( tail == 0 ){return;}
  if( tail == 1 )
    {
      //Data from Gerard 2020 summer
      mGSigma    = 24.5/nsecPerTB();
      mA1        = 1.0/0.154/mGSigma;
      mA2        = 0.2/0.154/mGSigma;
      mXoff1     = (70 - 129)/nsecPerTB();
      mXoff2     = (220 - 129)/nsecPerTB();
      mTau1      = 200.0/nsecPerTB();
      mTau2      = 40.0/nsecPerTB();
      mP1        = 1.0;
      mP2        = 1.0;
    }
  else if( tail == 2 )
    {
      //Data from WAH with real detector/LED system 2021 Jan
      mGSigma    = 2.347;
      mA1        = 2543.0/854.0/mGSigma;
      mA2        = 0.0;
      mXoff1     = 211.3-215.7;
      mXoff2     = 0.0;
      mTau1      = 4.375;
      mTau2      = 0.0;
      mP1        = 1.0;
      mP2        = 0.0;
    }
  else{ LOG_WARN << "StFcsDbPulse::setTail - Invalid Tail value " << tail << endm; }
}

void StFcsDbPulse::setTGraphAsymmErrors(TGraphAsymmErrors* gae, const int &i, const double &adc, double Yerr, double YerrSat){
  if( gae == 0 ){LOG_ERROR << "StFcsDbPulse::setTGraphAsymmErrors - Graph object cannot be zero" << endm; return;}
  double HighY = 0;
  if(adc<mAdcSaturation) { HighY=Yerr; }
  else                   { HighY=YerrSat; }
  gae->SetPointError(i,0,0,Yerr,HighY);
}

//this one is just shower shape function
double StFcsDbPulse::pulseShape(double* x, double* p) {
    double ret =  p[0]*exp(-0.5*pow((x[0]-p[1])/p[2],2));
    if(mTail>0){
      double x1 = x[0] - p[1] - mXoff1;
      if(x1>0){
        double a0 = p[0] * p[2];
        ret += a0*mA1/mTau1/mTau1*pow(x1,mP1)*exp(-x1/mTau1);
	if(mA2>0){
	  double x2 = x[0] - p[1] - mXoff2;
	  if(x2>0){
	    ret += a0*mA2/mTau2/mTau2*pow(x2,mP2)*exp(-x2/mTau2);
	  }
	}
      }
    }
    return ret;
}

double StFcsDbPulse::multiPulseShape(double* x, double* p) {
    int npulse = p[0];
    double ret = p[1];
    for(int i=0; i<npulse; i++){
	ret += pulseShape(x, &p[2+i*3]);
    }    
    return ret;
}

TF1* StFcsDbPulse::createPulse(double xlow, double xhigh, int npars)
{
  if( npars<5 ){LOG_WARN << "StFcsDbPulse::createPulse - npars must be greater than or equal to 5 paramaters" <<endm; return 0;}
  else if( xlow>=xhigh ){LOG_ERROR << "StFcsDbPulse::createPulse - Invalid range" <<endm; return 0;}
    else{return new TF1("F1_fcsDbPulse",this,&StFcsDbPulse::multiPulseShape,xlow,xhigh,npars);}//More unique name in future??
}

int StFcsDbPulse::GenericPadPos(int value, int Nvals, int PadNums )
{
  if( value<=0 ){return ceil( static_cast<double>(value+(Nvals*PadNums))/static_cast<double>(Nvals) );}
  else{ return GenericPadPos(value-(Nvals*PadNums), Nvals, PadNums); }
}

int StFcsDbPulse::PadNum4x4(int det, int col, int row)
{
  int ncol = 0;
  int nrow = 0;
  if( det<=1 )
    {
      ncol = 2;
      nrow = 3;
    }
  else if( det<=3 )
    {
      ncol = 2;
      nrow = 2;
    }
  else{ LOG_ERROR << "This only works for Ecal and Hcal" << endm; return 0;}
  int padcol = GenericPadPos(col,ncol,4);
  int padrow = GenericPadPos(row,nrow,4);
  return 4*(padrow-1)+padcol;
}

Int_t StFcsDbPulse::getYMinMax(TGraphAsymmErrors* gae, Double_t &Ymin, Double_t &Ymax, Double_t xmin, Double_t xmax)
{
  if( gae==0 ){ return -1; }
  Int_t index = -1;
  //Start with invalid values
  Double_t MinY = Ymax;
  Double_t MaxY = Ymin;
  for( int i=0; i<gae->GetN(); ++i )
    {
      Double_t X; Double_t Y;
      gae->GetPoint(i,X,Y);
      if( X<xmin || X>xmax ){continue;}
      if( Y>MaxY ){ MaxY=Y; index=i;}
      if( Y<MinY ){ MinY=Y; }
    }
  if( index<0 ){std::cout << "Unable to find a maximum ADC value" << std::endl; return index;}
  Ymin=MinY;
  Ymax=MaxY;
  return index;
}

void StFcsDbPulse::Print(Option_t* opt) const
{
  std::cout << "Constants"
	    << "|sqrtpi:"  << sqrtpi()
	    << "|sqrt2pi:" << sqrt2pi()
	    << std::endl;
  std::cout << "TimebinInfo"
	    << "|TBPerRC:" << TBPerRC()
	    << "|nsecPerTB:"<< nsecPerTB()
	    << "|BeamLengthSig:"<< BeamLengthSig()
	    << std::endl;
  std::cout << "PulseInfo"
	    << "|GSigma:" << GSigma()
	    << "|A1:"     << A1()
	    << "|A2:"     << A2()
	    << "|Xoff1:"  << Xoff1()
	    << "|Xoff2:"  << Xoff2()
	    << "|Tau1:"   << Tau1()
	    << "|Tau2:"   << Tau2()
	    << "|P1:"     << P1()
	    << "|P2:"     << P2()
	    << std::endl;
}

