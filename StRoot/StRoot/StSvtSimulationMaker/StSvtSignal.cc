/***************************************************************************
 *
 * $Id: StSvtSignal.cc,v 1.19 2015/07/29 01:48:02 smirnovd Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: calculates PASA signal for a gaussian input
 *
 ***************************************************************************
 *
 * $Log: StSvtSignal.cc,v $
 * Revision 1.19  2015/07/29 01:48:02  smirnovd
 * Added std:: to resolve ambiguity for isnan for g++ (4.8)
 *
 * Revision 1.18  2009/08/10 05:23:14  baumgart
 * Adjust mPasaGain due to adjustments in initial cloud size
 *
 * Revision 1.17  2009/07/29 18:42:08  baumgart
 * Increased mPasaGain to compensate for edge effects subtraction in StSvtElectronCloud.cc
 *
 * Revision 1.16  2009/07/02 20:29:56  baumgart
 * Suppression of cout statements in function calcConvSignal
 *
 * Revision 1.15  2009/06/28 03:59:37  baumgart
 * Compensate for addition of angular dependence in StSvtElectronCloud.cc
 *
 * Revision 1.14  2009/06/11 23:19:40  baumgart
 * Small increase of mPasaGain during tune
 *
 * Revision 1.13  2009/03/13 22:29:37  baumgart
 * Update mPasaGain to reflect new tune after bug fix
 *
 * Revision 1.12  2009/03/09 20:11:45  caines
 * Fix to make different Rykov and Selemon methods have same gain
 *
 * Revision 1.11  2009/02/21 14:19:12  caines
 * change gain to better reproduce data
 *
 * Revision 1.10  2008/09/22 16:03:55  caines
 * Changed gain as needed for tuning CuCu data
 *
 * Revision 1.9  2005/07/23 03:37:34  perev
 * IdTruth + Cleanup
 *
 * Revision 1.8  2005/02/09 14:33:35  caines
 * New electron expansion routine
 *
 * Revision 1.7  2003/11/13 16:24:59  caines
 * Further improvements to get simulator looking like reality
 *
 * Revision 1.6  2003/09/02 17:59:09  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.5  2003/07/31 19:18:10  caines
 * Petrs improved simulation code
 *
 * Revision 1.4  2001/11/06 20:12:06  caines
 * Add include for new compiler
 *
 * Revision 1.3  2001/08/13 15:34:18  bekele
 * Debugging tools added
 *
 * Revision 1.2  2001/04/25 19:04:38  perev
 * HPcorrs
 *
 * Revision 1.1  2000/11/30 20:47:49  caines
 * First version of Slow Simulator - S. Bekele
 *
 **************************************************************************/
 
#include "StSvtElectronCloud.hh"
#include "StSvtSignal.hh"

//ClassImp(StSvtElectronCloud)

fstream pasaOutN,pasaOut;
double sumAt;
//_______________________________________________
StSvtSignal::StSvtSignal()
{
 mLowTBin  = 1;
 mHiTBin = 128;
 mTotalHitCharge = 0.0;
 mFractionOfCharge = 0.0;
 mCollectedCharge = 0.0;

 mSigmaMajor = 0.0;
 mSigmaMinor = 0.0;
 mDriftVel = 0.0;                             //[in mm/micro seconds];
 mTimeCenter = 0.0;
 mTimeWidth = 0.0;

 GAP_TWIDTH = 36*1.e-6;

 memset(mSignal,0,sizeof(mSignal[0])*128);

 // mPasaGain = 7.2;      // uV/e Original number
 mPasaGain = 13.4; // Continuing tuning
 
 for(int i = 0; i < 4; i++)
    mPasa[i] = 0.0;

 mTau_s = 11.0*0.001;   //in micro seconds
 mTau_l = 500.0*0.001;  //in micro seconds
 
}
//_______________________________________________
StSvtSignal::~StSvtSignal()
{

}
//_______________________________________________
void StSvtSignal::setAnodeTimeBinSizes(double timeBinSize,double anodeSize)
{
  mTimeBinSize = timeBinSize;
  mAnodeSize = anodeSize;
 
}

//_______________________________________________
void StSvtSignal::setDriftVelocity(double driftVelocity)
{
mDriftVel = driftVelocity;
}

//_______________________________________________
void StSvtSignal::setOption(int option)
{
  mOption = option;
}

//_______________________________________________
void StSvtSignal::pasaRelatedStuff()
{

 //**** pasa related stuff

 //for Rykov's piece of code

 unNormPasaConst();

 //To speed things up do not calculate these values every time, just use the already calculated values

 mPeakTimeR = 0.04308;
 mPasaMaxR = 4.4026;
 mFwhmR = 0.049364;

 /*
  peakingTimeR();   //  [micro seconds]
 cout<<"mPeakTimeR = "<<mPeakTimeR<<endl;
 cout<<"mPasaMaxR = "<<mPasaMaxR<<endl;
 halfWidthAtHalfMaxR();
 cout<<"mFwhmR = "<<mFwhmR<<endl;
 */

 normPasaConst();
 arrays();

 //for Selemons piece of code
 //To speed things up do not calculate these values every time, just use the already calculated values

 mPeakTimeS = 0.04308;
 mPasaMaxS = 2.74619e-09;
 mFwhmS = 0.0494;
 // mPasaNorm = 2.62181e+09; // [micro volts]/[micro seconds**4]-e 
 mPasaNorm = mPasaGain*3.64140278e+08; // Automatic Conversion

 /*
 peakingTimeS();
 cout<<"\nmPeakTimeS = "<<mPeakTimeS<<endl;
 cout<<"mPasaMaxS = "<<mPasaMaxS<<endl;
 halfWidthAtHalfMaxS();
 cout<<"mFwhmS = "<<mFwhmS<<endl;
 mPasaNorm = mPasaGain/mPasaMaxS; // [micro volts]/[micro seconds**4]-e
 cout<<"mPasaNorm = "<<mPasaNorm<<endl;
 */
}

//_______________________________________________
void StSvtSignal::doPasaOnly(int option){
 double t = 0.0;  
 // double tStep = 0.04;
 double tStep = mTimeBinSize*0.001;     //microseconds
 double pasaFunValue;

 if(option)
   pasaOutN.open("pasaNorm.dat",ios::out);
 else
   pasaOut.open("pasa.dat",ios::out);

 unNormPasaConst();

 for(int j = 0; j <25000; j++){

     t = t + tStep;

     pasaFunValue = pasaRes(t);

     if(option){
       pasaFunValue = mPasaNorm*pasaFunValue;
       pasaOutN<<t<<setw(20)<<pasaFunValue<<endl;

     } else {

       pasaOut<<t<<setw(20)<<pasaFunValue<<endl;
     }
     /*
     if(j < 100){
	cout<<t<<"\t"<<pasaFunValue<<endl;
     }
     */
    } 

 halfWidthAtHalfMaxS();

 if(option)
   pasaOutN.close();
 else
   pasaOut.close();

}
//_______________________________________________
void StSvtSignal::setCloud(StSvtElectronCloud* elCloud)
{
 double sigmaSqDiff,sigmaSqDiff2;
 double cosPhi,cosPhi2,sinPhi,sinPhi2;

 /* old stuff
 sigmaMajor2 = elCloud->getSigma1Sq();
 sigmaMinor2 = elCloud->getSigma2Sq();
 
 sigmaMajor = ::sqrt(sigmaMajor2);               // [mm]
 sigmaMinor = ::sqrt(sigmaMinor2);               // [mm]
 sigmaSqDiff = sigmaMajor2 - sigmaMinor2;
 sigmaSqDiff2 = sigmaSqDiff*sigmaSqDiff;

 mSigmaMajor = elCloud->getSigma1();               // [mm]
 mSigmaMinor = elCloud->getSigma2();              // [mm]
 //mSigmaMinor = (mSigmaMinor/0.24)*(4.78/128);      // [micro seconds]
 //mSigmaMinor = mSigmaMinor*0.0001555989583;         // [micro seconds]
 */

 mTrackId = elCloud->getTrackId();
 mSigmaMajor = elCloud->getSigmaDrift();
 double sigmaMajor2 = mSigmaMajor*mSigmaMajor;
 mSigmaMinor = elCloud->getSigmaAnode(); 
 double sigmaMinor2 = mSigmaMinor*mSigmaMinor;
 sigmaSqDiff = sigmaMajor2 - sigmaMinor2;
 sigmaSqDiff2 = sigmaSqDiff*sigmaSqDiff;

 mTotalHitCharge =  elCloud->getChargeAtAnode();

 cosPhi = cos(elCloud->getPhi());
 sinPhi = sin(elCloud->getPhi());
 cosPhi2 = cosPhi*cosPhi;
 sinPhi2 = sinPhi*sinPhi;

 mC1 = (-0.39894228*cosPhi*sinPhi*sigmaSqDiff)/mSigmaMajor;        // [mm]
 mC2 = (0.39894228*cosPhi2*sinPhi2*sigmaSqDiff2)/sigmaMajor2;     // [mm]**2
 mC3 = (sigmaMajor2*sigmaMinor2/sigmaMajor2) + (cosPhi2*sinPhi2*sigmaSqDiff2/sigmaMajor2);  // [mm]**2
}

//_______________________________________________
double  StSvtSignal::chargeFraction(int an, double anHit)
{
  //anodes are numbered from 1
  double anodeHitCent = anHit*mAnodeSize;
  
  mAnRightEdge = an*mAnodeSize;
  mAnLeftEdge = (an - 1)*mAnodeSize;

 //Fraction of total charge collected

  mFractionOfCharge = prob1(mAnRightEdge - anodeHitCent,mSigmaMajor) - prob1(mAnLeftEdge - anodeHitCent,mSigmaMajor);
  
  //cout<<"mAnRightEdge = "<<mAnRightEdge<<endl;
  //cout<<"mAnLeftEdge = "<<mAnLeftEdge<<endl;
  //cout<<"anodeHitCent = "<<anodeHitCent<<endl;
  //cout<<"mSigmaMajor = "<<mSigmaMajor<<endl;

 if(mFractionOfCharge < 0.000001)
   {
    mCollectedCharge = 0.0;
   }
 else 
   {
     //cout<<"mFractionOfCharge = "<<mFractionOfCharge<<endl;
    mCollectedCharge = mTotalHitCharge*mFractionOfCharge;
   }

 return mCollectedCharge;

}

//_______________________________________________
//relTimeCenter = Center of gravity relative to the cloud y-center of gravity

int StSvtSignal::timeCenterAndWidth(double anHit,double timeHit)
{
 double leftArgument,leftArgument2,rightArgument,rightArgument2;
 double exp1,exp2,relTimeCenter,relTimeCenter2,timeWidth2,driftVel2;

 double anodeHitCent = anHit*mAnodeSize;
 double mTimeHit = timeHit*mTimeBinSize;

 leftArgument = (mAnLeftEdge - anodeHitCent)/mSigmaMajor;
 leftArgument2 = leftArgument*leftArgument;
 rightArgument = (mAnRightEdge - anodeHitCent)/mSigmaMajor;
 rightArgument2 = rightArgument*rightArgument;

 //cout<<"leftArgument = "<<leftArgument<<endl;
 //cout<<"rightArgument =  "<<rightArgument<<endl;

 exp1 = exp(-0.5*leftArgument2);
 exp2 = exp(-0.5*rightArgument2);

 //if(exp1 < 1e-30)
 // exp1 = 0.0;
 //if(exp2 < 1e-30)
 //exp2 = 0.0;

 //cout<<"exp1 = "<<exp1<<endl;
 //cout<<"exp2 = "<<exp2<<endl;
 //cout<<"mC1 = "<<mC1<<endl;
 //cout<<"mC2 = "<<mC2<<endl;
 //cout<<"mC3 = "<<mC3<<endl;

 relTimeCenter = mC1*(exp1 - exp2);     //mm
 //cout<<"relTimeCenter = "<<relTimeCenter<<endl;
 timeWidth2 = mC2*(leftArgument*exp1 - rightArgument*exp2) + mC3*mFractionOfCharge;   //[mm]**2

 // center of gravity
 relTimeCenter = relTimeCenter/mFractionOfCharge;
 //cout<<"relTimeCenter = "<<relTimeCenter<<endl;
 relTimeCenter2 = relTimeCenter*relTimeCenter;
 timeWidth2 = timeWidth2/mFractionOfCharge;
 timeWidth2 = fabs(timeWidth2 - relTimeCenter2);
 //cout<<"timeWidth2 = "<<timeWidth2<<endl;

 driftVel2 = mDriftVel*mDriftVel;

 mTimeCenter = mTimeHit + (relTimeCenter/mDriftVel);       //micro seconds
 //cout<<" mTimeCenter = "<< mTimeCenter<<endl;
 mTimeWidth = ::sqrt((timeWidth2/driftVel2) + GAP_TWIDTH);          //micro seconds 
 //cout<<"mTimeWidth = "<<mTimeWidth<<endl;

 if(std::isnan(mTimeWidth))
   return 1;
 else return 0;
 
}

//_______________________________________________
void StSvtSignal::resetPeakAndUnderShoot()
{
  mPeakSignal = 0.0;
  mMinUnderShoot = 0.0;
}
//_______________________________________________
void StSvtSignal::setTimeWidth(double timWidth)
{
 mTimeWidth = timWidth;
 //cout<<"mTimeWidth = "<<mTimeWidth<<endl;
}

//_______________________________________________
void StSvtSignal::calcConvSignal(double chargeOnAnode)
{
 int nMin, nMax;
 double tStep = 0;

 resetSignal(mLowTBin,mHiTBin);
 resetPeakAndUnderShoot();

 //4.78/128 = 0.03734375; 
 tStep = mTimeBinSize;     //microseconds

  //nMin = (int)((mTimeCenter - 4*mTimeWidth)/tStep);
  int mTCenter = (int)(mTimeCenter/tStep);
  nMin = mTCenter - 10;
  if(nMin <= 0) nMin = 1;

  mLowTBin = nMin;

  //nMax = (int)((mTimeCenter + 5*mTimeWidth)/tStep);
  nMax = mTCenter + 20;
  if(nMax > 128) nMax = 128;

  mHiTBin = nMax;

 if(mOption == 1)
   {
     //cout<<"using Rykove's method"<<endl;
    rykovSignal(nMin,nMax, tStep);
   }
 else if(mOption == 2 )
   {
     //cout<<"using  Selemon's version"<<endl; 
     selemonSignal(nMin,nMax,tStep,chargeOnAnode);
   }
 else
   {
     //cout<<"using both versions"<<endl;
     if(mTimeWidth< 0.14)
      //if(mTimeWidth > 0.02 && mTimeWidth< 0.14)
     {
       //cout<<"now using Rykove's version"<<endl;
       //cout << "Rykov" << mTCenter << endl;
      rykovSignal(nMin,nMax, tStep);
     }
    else
     {
       //cout<<"now using selemons version"<<endl;
       //cout << "Selemon" << mTCenter << endl;
      selemonSignal(nMin,nMax,tStep,chargeOnAnode);
     }
   }

}
//_______________________________________________
void  StSvtSignal::rykovSignal(int nMin,int nMax, double tStep)
{
  double t = 0;

  for(int n = nMin; n <= nMax ; n++)
    {
      t = n*tStep;
      mSignal[n - 1] = signal(t)*1000000.0;      // [micro volts]
      if(n < (int)(mTimeCenter/tStep) && mSignal[n - 1] < 0.0)
	mSignal[n - 1] = 0;
      if(mSignal[n - 1] > mPeakSignal)
	mPeakSignal = mSignal[n - 1];      
      if(mSignal[n - 1] < mMinUnderShoot)
        mMinUnderShoot = mSignal[n - 1];
    }
}
//_______________________________________________
void  StSvtSignal::selemonSignal(int nMin,int nMax, double tStep, double charge)
{
  double t = 0, sig = 0;
  int numOfIntPoints = 0;
  sumAt = 0.0;

  for(int n = nMin; n <= nMax ; n++)
   {
    t = n*tStep;
    if(mTimeWidth <=  0.06)
     {
      sig = analConvInt(t,mTimeWidth,mTimeCenter);
      mSignal[n - 1] = charge*mPasaNorm*sig; // [micro volts]
      if(n < (int)(mTimeCenter/tStep) && mSignal[n - 1] < 0.0)
	mSignal[n - 1] = 0;
      if(mSignal[n - 1] > mPeakSignal)
         mPeakSignal = mSignal[n - 1];
       if(mSignal[n - 1] < mMinUnderShoot)
        mMinUnderShoot = mSignal[n - 1];     
      }
    else
      {
       numOfIntPoints = 2;
       sig = numConvInt(nMin, n, numOfIntPoints, tStep, t);
       //cout<<"sig = "<<sig<<endl;
       mSignal[n - 1] = charge*mPasaNorm*sig; // [micro volts]
       if(n < (int)(mTimeCenter/tStep) && mSignal[n - 1] < 0.0)
	mSignal[n - 1] = 0;
       if(mSignal[n - 1] > mPeakSignal)
         mPeakSignal = mSignal[n - 1];
       if(mSignal[n - 1] < mMinUnderShoot)
        mMinUnderShoot = mSignal[n - 1];      
      }
   }
    
}

//_______________________________________________
//pasa dependent coefficients
void StSvtSignal::unNormPasaConst()
{

 double p = mTau_s/mTau_l;
 double q = 1.0/(1.0 - p);
 mPasa[4] = p;

 for(int i = 4; i > 0; i--)
   {
    mPasa[i-1] = i*mPasa[i]*q;
   }
 mPasa[4] = 1.0;

}
//_______________________________________________
void StSvtSignal::peakingTimeS()
{

  double t = 0.0;  
 // double tStep = 0.04;
 double tStep = mTimeBinSize*0.001;     //microseconds
 double pasaFunValue = -1.e20;
 mPasaMaxS = -2.0e20;

 do {
     mPasaMaxS = pasaFunValue;
     t = t + tStep;

     pasaFunValue = pasaRes(t);
     //cout<<"t = "<<t<<"\tpasaFunValue = "<<pasaFunValue<<endl;

    } while( pasaFunValue > mPasaMaxS);
   
   mPeakTimeS = t - tStep;

}
//_______________________________________________
void StSvtSignal::peakingTimeR()
{

  double t = 0.0;  
 // double tStep = 0.04;
 double tStep = (mTimeBinSize*0.001)/mTau_s;     //microseconds
 double pasaFunValue = -1.e20;
 mPasaMaxR = -2.0e20;

 do {
     mPasaMaxR = pasaFunValue;
     t = t + tStep;
     pasaFunValue = mPasa[0];
     double c1 = 1.0;
     
     for(int i = 1; i <= 4; i++)
      {
       c1 = c1*t;
       pasaFunValue = pasaFunValue + mPasa[i]*c1;
      }
     
     pasaFunValue = pasaFunValue*exp(-t) - mPasa[0]*exp(-(mTau_s/mTau_l)*t);
     //cout<<"t = "<<t<<"\tpasaFunValue = "<<pasaFunValue<<endl;
    
    } while( pasaFunValue > mPasaMaxR);
   
   double peakTime = t - tStep;
   mPeakTimeR = mTau_s*peakTime;

}
//_______________________________________________
void StSvtSignal::halfWidthAtHalfMaxS()
{
 double t;
 //double tStep = 0.04;
 double tStep = mTimeBinSize*0.001;     //microseconds
 double pasaFunValue = 0.0;
 mFwhmS = 0.0;

 for(int i = 0; i < 2; i++)
   {
    mFwhmS = -1.0*mFwhmS;
    t = mPeakTimeS;
    pasaFunValue = mPasaMaxS;   // from previous function
    
    do {
        t = t - tStep;
	pasaFunValue = pasaRes(t);
           
    } while( pasaFunValue > 0.5*mPasaMaxS);

    mFwhmS = mFwhmS + t;
    tStep = -tStep;
   }

}
//_______________________________________________
void StSvtSignal::halfWidthAtHalfMaxR()
{
 double t;
 //double tStep = 0.04;
 double tStep = mTimeBinSize*0.001;     //microseconds
 double pasaFunValue = 0.0;
 mFwhmR = 0.0;

 for(int i = 0; i < 2; i++)
   {
    mFwhmR = -1.0*mFwhmR;
    t = mPeakTimeR/mTau_s;
    pasaFunValue = mPasaMaxR;   // from previous function
    
    do {
        t = t - tStep;
        pasaFunValue = mPasa[0];
        double c1 = 1.0;
	
        for(int i = 1; i <= 4; i++)
	  {
           c1 = c1*t;
           pasaFunValue = pasaFunValue + mPasa[i]*c1;
	   }
	
        pasaFunValue = pasaFunValue*exp(-t) - mPasa[0]*exp(-(mTau_s/mTau_l)*t);

    } while( pasaFunValue > 0.5*mPasaMaxR);

    mFwhmR = mFwhmR + t;
    tStep = -tStep;
   }

 mFwhmR = mTau_s*mFwhmR;
}
//_______________________________________________
void StSvtSignal::normPasaConst()
{
 double s1 = mPasaGain*1.e-6/mPasaMaxR;
 
 for(int i = 0; i <= 4; i++)
    mPasa[i] = s1*mPasa[i];

 //p1_ras1 = mTau_l*mPasa[0];
 
}


void StSvtSignal::arrays()
{
 mArray1[0] = 2.46196981473530512524e-10;
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
 
}
//_______________________________________________
double StSvtSignal::signal(double t)
{
 double signal;
 double localTime = 0.0;

 localTime = t - mTimeCenter;

 if(mCollectedCharge < 1.0)
   signal = 0;
 else if(localTime + 5.0*mTimeWidth < 0.0)
   signal = 0;
 else if(mTimeWidth <=  0.02*mTau_s)
   signal = getShortSignal(localTime);
 else
   signal = getLongSignal(localTime);

 return signal;

}
 //_______________________________________________   
double StSvtSignal::getShortSignal(double localTime)
 {
  double signal, sig_s, sig_l;

  signal = -mPasa[0]*exp(-localTime/mTau_l);
  localTime = localTime/mTau_s;
  sig_s = 0.0;
  sig_l = 1.0;

  for(int i = 0; i <= 4; i++)
   {
    sig_s += mPasa[i]*sig_l;
    sig_l = sig_l*localTime;
   }

  signal = mCollectedCharge*( signal + sig_s*exp(-localTime));

  return signal;
 }    

//_______________________________________________
double StSvtSignal::getLongSignal(double localTime)
 {
  double ds0, ds1, ds2, dsc, dsc0, dsc1, dsc2, dsc3;
  double da1,da2,a2,a3,a4;

  da1 = mTimeWidth/mTau_s;
  da2 = da1*da1;
  a2 = 0.5*mCollectedCharge/da1;
  a3 = mTimeWidth/mTau_l;
  a4 = 0.5*a3*a3;

  ds0 = localTime/mTimeWidth;
  ds1 = ds0 - da1;
  ds2 = da1*ds1;
  dsc = fabs(0.7071067811865476*ds1);
  dsc2 = dsc*dsc;
  dsc3 = exp(-0.5*ds0*ds0);

  if(dsc < 1.0)
    dsc0 = useArrays5And6(ds1,dsc);
  else
    dsc0 = useArrays3And4Or1And2(ds1,dsc);

  dsc1 = 0.3989422805274159*da1;
  if(dsc >= 1.0 && ds1 < 0.0)
    dsc1 = dsc1 + ds2*dsc0;
  else
    {
     dsc0 = dsc0*exp(-da1*(ds1 + 0.5*da1));
     dsc1 = dsc1*dsc3 + ds2*dsc0;
    }

  double sig0 = mPasa[0]*dsc0 + mPasa[1]*dsc1;
  for(int i = 2; i <= 4; i++)
    {
     dsc2 = da2*(i - 1)*dsc0 + ds2*dsc1;
     sig0 = sig0 + mPasa[i]*dsc2;
     dsc0 = dsc1;
     dsc1 = dsc2;
    }

  if(dsc >= 1.0 && ds1 < 0.0)
    sig0 = sig0*dsc3;

  double sig1 = ds0 - a3;
  
  sig0 = sig0 - mPasa[0]*exp(a4 - localTime/mTau_l)*prob1(sig1,1.0);
  double signal = mCollectedCharge*sig0;

  return signal;
}
//_______________________________________________
double StSvtSignal::useArrays5And6(double ds1, double dsc)
 {
  double sigUp,sigDn;

  sigUp = mArray5[0];
  sigDn = mArray6[0];

  for(int i = 1; i <= 5; i++)
   {
    sigUp = sigUp*dsc*dsc + mArray5[i];
    sigDn = sigDn*dsc*dsc + mArray6[i];
   }

   double dsc0 = 0.5 - 0.5*dsc*sigUp/sigDn;
   if(ds1 > 0.0)
     dsc0 = 1.0 - dsc0;
 
   return dsc0;
}
//_______________________________________________
double  StSvtSignal::useArrays3And4Or1And2(double ds1,double dsc)
 {
  double sigUp,sigDn;

  if(dsc >= 8.0)
   {
    sigUp = mArray3[0];
    sigDn = mArray4[0];
            
    for(int i = 1; i <= 6; i++)
     {
      sigUp = sigUp*dsc + mArray3[i];
      sigDn = sigDn*dsc + mArray4[i];
     }
    }
   else
    {
     sigUp = mArray1[0];
     sigDn = mArray2[0];
             
     for(int i = 1; i <= 8; i++)
      {
       sigUp = sigUp*dsc + mArray1[i];
       sigDn = sigDn*dsc + mArray2[i];
      }
     }

   double dsc0 = 0.5*sigUp/sigDn;
   if(ds1 > 0.0)
     dsc0 = 1.0 - dsc0*exp(-dsc*dsc);
      
   return dsc0;
 }

//_______________________________________________
double StSvtSignal::numConvInt(int nMin , int n, int numOfIntPoints, double tStep, double t)
{
 double  At = 0.0,lowlim = 0;

 for(int p = nMin; p <= n ; p++)
 {
 //lowlim = -mLowLim*(tStep/100);
 //At = simpsonInt(mLowLim,lowlim,tStep/100,t);
  lowlim = (p - 1)*mTimeBinSize;
   At += simpsonInt(numOfIntPoints,lowlim,tStep/numOfIntPoints,t);
		  
  }

 return At;

}


//_______________________________________________
double StSvtSignal::analConvInt(double tim, double sigmat, double tc)
{
 double a, b, c, bc5, bc4, bc3, bc2, ac1;
 double At = 0,tb, ta, ta2,ta3, ta4,Errb,Erra,expb,expa,gaus ;
 double sigmat2,sigmat3,sigmat4;

  sigmat2 = sigmat*sigmat;
  sigmat3 = sigmat2*sigmat;
  sigmat4 = sigmat3*sigmat;

  //a = 1.0/mTau_s;          b = 1.0/mTau_l;         c = b - a;
  a = 90.909090909091;   b = 2.0;             c = -88.909090909091;

  //bc5 = b/::pow(c,5);   bc4 = b/::pow(c,4);    bc3 = b/(2*::pow(c,3));   bc2 = b/(6*c*c);    ac1 = a/(24*c);

    bc5 = -0.00000000035999; bc4 = 0.00000003200702; 
    bc3 = -0.00000142285777;  bc2 = 0.00004216833039; 
    ac1 = -0.04260395364689;

  
   tb = tim - tc - b*sigmat2;
   ta = tim - tc - a*sigmat2;
   ta2 = ta*ta; ta3 = ta2*ta; ta4 = ta3*ta;

   Errb = prob2(tb,sigmat);  expb = exp(-b*(tim - tc - b*(sigmat2*0.5)));
   Erra = prob2(ta,sigmat);  expa = exp(-a*(tim - tc - a*(sigmat2*0.5)));


   gaus = (sigmat/::sqrt(2*M_PI))*exp(-0.5*ta2/sigmat2);

  //double c1d1 = expb*Errb;
  //double c2d2 = expa*Erra;
  //double c2d3 = expa*(ta*Erra + gaus);
  //double c2d4 = expa*((ta2 + sigmat2)*Erra + ta*gaus);
  //double c2d5 = expa*((ta3 + 3.0*ta*sigmat2)*Erra + (ta2 + 2.0*sigmat2)*gaus);
  //double c2d6 = expa*((ta4 + 6.0*ta2*sigmat2 + 3.0*sigmat4)*Erra + (ta3 + 5.0*ta*sigmat2)*gaus);

 //At = (bc5*c1d1 - bc5*c2d2 + bc4*c2d3 - bc3*c2d4 + bc2*c2d5 - ac1*c2d6);

 At = bc5*expb*Errb + ((-bc5 + bc4*ta - bc3*(ta2 + sigmat2) + bc2*(ta3 + 3.0*ta*sigmat2) 
       - ac1*(ta4 + 6.0*ta2*sigmat2 + 3*sigmat4))*Erra + (bc4 - bc3*ta + bc2*(ta2 + 2.0*sigmat2)
       - ac1*(ta3 + 5.0*ta*sigmat2))*gaus)*expa;            
       
 /*
 if(t < tc && At < 0)
   {At = 0;
    cout<<t<<setw(20)<<At<<endl;
   continue;}
 */

 //output<<t<<setw(20)<<At<<endl;
 //cout<<t<<setw(20)<<At<<endl;

 return At;
}
//_______________________________________________ 
double StSvtSignal::simpsonInt(int numOfIntPoints,double lowlim, double step, double t) 
 {
   double firstTerm = 0, evenTerm = 0, oddTerm = 0, lastTerm = 0, mTpr = 0;
  
      for(int m = 0; m <= numOfIntPoints ; m++)
       {
         mTpr = lowlim + m*step;   //mTpr is the integration variable

         if(m == 0)
	   firstTerm = gausInput(mTpr - mTimeCenter)*pasaRes(t - mTpr);      //(1/micro seconds)*(micro seconds**4)
         else if((m != numOfIntPoints) && (m%2 == 0))
           evenTerm +=  gausInput(mTpr - mTimeCenter)*pasaRes(t - mTpr);
         else if((m != numOfIntPoints) && (m%2 == 1))
	   oddTerm +=  gausInput(mTpr - mTimeCenter)*pasaRes(t - mTpr);
         else if(m == numOfIntPoints)
           lastTerm =  gausInput(mTpr - mTimeCenter)*pasaRes(t - mTpr);
  
       }

    double signal =  (step*0.33333333333333)*(firstTerm + lastTerm + 4.0*oddTerm + 2.0*evenTerm); //(micro seconds**4)
  //cout<<"signal = "<<signal<<endl;
  return signal;         
} 

//_______________________________________________
double StSvtSignal::gausInput(double tim)
{
  double tPr2, PI,Exp1;
  
  PI = M_PI;
 //cout<<"tim = "<<tim<<endl;
 // 1.0/::sqrt(2*PI) = 0.39894228040143;

 tPr2 = 0.5*tim*tim/(mTimeWidth*mTimeWidth);

 Exp1 = (0.39894228040143/mTimeWidth)*exp(-tPr2);

 return Exp1;        //[micro seconds]**(-1)

}
//_______________________________________________
double StSvtSignal::pasaRes(double tim)
{
 double  a, b, c, bc5, bc4, bc3, bc2, ac1, t2, t3,t4;
 double Fpasa = 0;

 if(tim >= 0)
 {
  
  //a = 1.0/mTau_s;          b = 1.0/mTau_l;         c = b - a;
  a = 90.909090909091;   b = 2.0;             c = -88.909090909091;

  //bc5 = b/::pow(c,5);   bc4 = b/::pow(c,4);    bc3 = b/(2*::pow(c,3));   bc2 = b/(6*c*c);    ac1 = a/(24*c);
    bc5 = -0.00000000035999; bc4 = 0.00000003200702; 
    bc3 = -0.00000142285777;  bc2 = 0.00004216833039; 
    ac1 = -0.04260395364689;
 
  t2 = tim*tim;
  t3 = tim*t2;
  t4 = tim*t3;

  Fpasa = (bc5*exp(-b*tim)) + ((-bc5 + bc4*tim - bc3*t2 + bc2*t3 - ac1*t4)*exp(-a*tim));
 }

  return Fpasa;
 
}
//_______________________________________________
double StSvtSignal::prob1(double anOrTimeDiff , double  sigma)
{
   double num = 0;
 
   num = anOrTimeDiff/(M_SQRT2*sigma);

   double fraction = 0.5*(1 + erf(num));

  return fraction; 

}
//_______________________________________________
double StSvtSignal::freq(double num)
{
 double frq = 0;

 if(num > 0.0)
  frq = 0.5 + 0.5*erfc( num/M_SQRT2);
 else
  frq =  0.5* erfc(num/M_SQRT2);

 return frq;

}

//_______________________________________________
double StSvtSignal::prob2(double num , double  sigma)
{
   double mSum = 0, mErrf = 0;
   double mFactorial = 0;
   double mPowerTerm = 0;
   double mPowerTermSquared = 0;
   double mCountTerm = 0;

  if(fabs(num) < 5*sigma)
    {
     for(int j = 0; j<=150; j++)
      {
    
      if(j==0)
       {
        mFactorial = 1.0;
        mPowerTerm = fabs(num)/(M_SQRT2*sigma);
        mPowerTermSquared = mPowerTerm*mPowerTerm;
        mCountTerm = mPowerTerm;
       }

     else 
       {
        mFactorial = (double)j*mFactorial;
        mPowerTerm = (mPowerTermSquared)*(double)fabs(mPowerTerm);

        if(j%2 == 1)
         mPowerTerm = (-1)*mPowerTerm;
      
         mCountTerm =(mPowerTerm /( mFactorial*(double)(2*j + 1)));

        }

       mSum += mCountTerm;
    
     }

     mErrf = (2.0/::sqrt(M_PI))*mSum;

     if(num < 0.0)
       mErrf = (-1.0)*mErrf;
   }

  else if(num > 5*sigma)
     mErrf = 1.0;
  else 
     mErrf = -1.0;

  return  0.5*(1.0 + mErrf);
}
//_______________________________________________
int StSvtSignal::getLowTBin()
{
 return mLowTBin;
}
//_______________________________________________
int StSvtSignal::getHiTBin()
{
 return mHiTBin;
}
//_______________________________________________
double StSvtSignal::getTimeWidth()
{
 return mTimeWidth;
}
//_______________________________________________
double StSvtSignal::getTimeCenter()
{
 return mTimeCenter;
}
//_______________________________________________
double StSvtSignal::getPeak()
{
  return mPeakSignal*0.001;      //[mV]
}
//_______________________________________________
double StSvtSignal::getMinUnderShoot()
{

 return mMinUnderShoot*0.001;
}
//_______________________________________________
double StSvtSignal::getSignal(int n)
{
  return mSignal[n]*0.001;      //[mV]
}
//_______________________________________________
void StSvtSignal::resetSignal(int lBin, int hBin)
{
 for(int i = lBin; i <= hBin; i++)
  mSignal[i-1] = 0.0;
}
