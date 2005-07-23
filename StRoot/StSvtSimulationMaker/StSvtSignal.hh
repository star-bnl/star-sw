#ifndef STSVTSIGNAL_HH
#define STSVTSIGNAL_HH


#include <Stiostream.h>
#include <string.h>
#include <math.h>

class StSvtElectronCloud;

class StSvtSignal
{
public:
  StSvtSignal();
  ~StSvtSignal();

  void setOption(int option);
  void setAnodeTimeBinSizes(double timBinSize, double anodeSize);
  void setDriftVelocity(double driftVelocity);
  void setCloud(StSvtElectronCloud* elCloud);
  double chargeFraction(int an, double anHit);
  int timeCenterAndWidth(double anHit,double timeHit);
  void setTimeWidth(double timWidth);
  void calcConvSignal(double chargeOnAnode);
  void selemonSignal(int nMin,int nMax, double tStep, double charge);
  void rykovSignal(int nMin,int nMax, double tStep);
  void pasaRelatedStuff();
  void doPasaOnly(int option);
  void peakingTimeR();
  void halfWidthAtHalfMaxR();
  void peakingTimeS();
  void halfWidthAtHalfMaxS();
  void unNormPasaConst();
  void normPasaConst();
  void arrays();
  double signal(double t);
  double getShortSignal(double localTime);
  double getLongSignal(double localTime);
  double useArrays5And6(double ds1,double dsc);
  double useArrays3And4Or1And2(double ds1,double dsc);
  double numConvInt(int nMin , int n, int numOfIntPoints, double tStep, double t);
  double analConvInt(double tim, double sigmat, double tc);
  double simpsonInt(int numOfIntPoints,double lowlim, double step, double t); 
  double gausInput(double tim);
  double pasaRes(double tim);
  double freq(double num);
  double prob1(double anOrTimeDiff , double  sigma);
  double prob2(double num , double  sigma);
  int getLowTBin();
  int getHiTBin();
  int getTrackId() const 		{return mTrackId;}
  double getTimeCenter();
  double getTimeWidth();
  double getPeak();
  double getMinUnderShoot();
  double getSignal(int n);
  void resetPeakAndUnderShoot();
  void resetSignal(int lBin, int hBin);

private:
  double mTotalHitCharge;
  double mAnRightEdge;
  double mAnLeftEdge;
  double mFractionOfCharge;
  double mCollectedCharge;

  int mOption;
  int mLowTBin;
  int mHiTBin;
  int mTrackId;

  double mDriftVel;
  double mTimeBinSize;
  double mAnodeSize;

  double mSigmaMajor;
  double mSigmaMinor;
  double mTimeCenter;
  double mTimeWidth;
  double mPeakSignal;
  double mMinUnderShoot;

  double GAP_TWIDTH;
  double mPasaGain;

  //for selemon's piece of code
  double mPasaNorm;
  double mPeakTimeS;
  double mPasaMaxS;         
  double mFwhmS;

  //for Rykoves piece of code( fortran converted to c++)
  double mPasaMaxR;
  double mPeakTimeR;
  double mFwhmR;
    
  double mTau_s;
  double mTau_l;

  double mC1;
  double mC2;
  double mC3;

  double mSignal[128];    
  double mPasa[5];
  double mArray1[9];
  double mArray2[9];
  double mArray3[7];
  double mArray4[7];
  double mArray5[6];
  double mArray6[6];

  //ClassDef(StSvtSignal,1)

};

/*inline void StSvtSignal::arrays()
{
 mArray1[0] = 2.46196981473530512524*::pow(0.1,10);
 mArray1[1] = 0.564189564831068821977;
 mArray1[2] = 7.46321056442269912687;
 mArray1[3] = 48.6371970985681366614;
 mArray1[4] = 196.520832956077098242;
 mArray1[5] = 526.445194995477358631;
 mArray1[6] = 934.528527171957607540;
 mArray1[7] = 1027.55188689515710272;
 mArray1[8] = 557.535335369399327526;

 mArray2[0] = 1.0;
 mArray2[1] = 13.2281951154744992508;
 mArray2[2] = 86.7072140885989742329;
 mArray2[3] = 354.937778887819891062;
 mArray2[4] = 975.708501743205489753;
 mArray2[5] = 1823.90916687909736289;
 mArray2[6] = 2246.33760818710981792;
 mArray2[7] = 1656.66309194161350182;
 mArray2[8] = 557.535340817727675546;

 mArray3[0] = 0.0;
 mArray3[1] = 0.564189583547755073984;
 mArray3[2] = 1.275366707599781044160;
 mArray3[3] = 5.019050422511804774140;
 mArray3[4] = 6.160210979930535851950;
 mArray3[5] = 7.409742699504489391600;
 mArray3[6] = 2.978866653721002406700;

 mArray4[0] = 1.0;
 mArray4[1] = 2.26052863220117276590;
 mArray4[2] = 9.39603524938001434673;
 mArray4[3] = 12.0489539808096656605;
 mArray4[4] = 17.0814450747565897222;
 mArray4[5] = 9.60896809063285878198;
 mArray4[6] = 3.36907645100081516050;

 mArray5[0] = 0.0;
 mArray5[1] = 9.60497373987051638749;
 mArray5[2] = 90.0260197203842689217;
 mArray5[3] = 2232.00534594684319226;
 mArray5[4] = 7003.32514112805075473;
 mArray5[5] = 55592.3013010394962768;

 mArray6[0] = 1.0;
 mArray6[1] = 33.5617141647503099647;
 mArray6[2] = 521.357949780152679795;
 mArray6[3] = 4594.32382970980127987;
 mArray6[4] = 22629.0000613890934246;
 mArray6[5] = 49267.3942608635921086;  
 
 }*/

#endif 
