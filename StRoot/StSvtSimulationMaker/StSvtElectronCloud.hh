#ifndef STSVTELECTRONCLOUD_HH
#define STSVTELECTRONCLOUD_HH

#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>

class StSvtElectronCloud
{
public:
  StSvtElectronCloud(char* option,int option1, int option2);
  ~StSvtElectronCloud();

  void openFiles();
  void closeFiles();
  void setPar(double energy,double theta, double phi,double timeBinSize);
  void setSiliconProp();
  void setInitWidths(double w1, double w2);
  void setElectronLifeTime(double tLife);
  void setDriftVelocity(double driftVel);
  void setTrappingConst(double trapConst);
  void setDiffusionConst(double diffConst);
  void calculateWidthAtAnode(double mTc);
  int runge_kutta4(int stepBefore, int numBinDiv, double steplen);
  int adamsBushFort(int stepBefore, int numBinDiv, double steplen);
  double sigmaXSqFunc(double tim, double sigmaSq1, double sigmaSq2,double phi);
  double sigmaYSqFunc(double tim, double sigmaSq1, double sigmaSq2,double phi);
  double sigmaXYSqFunc(double tim, double sigmaSq1, double sigmaSq2,double phi);
  double func1(double tim, double sigmaSq1, double sigmaSq2);
  double func2(double tim, double sigmaSq1, double sigmaSq2);
  double getSigma1();
  double getSigma2();
  double getSigma1Sq();
  double getSigma2Sq();
  double getPhi();
  double getChargeAtAnode();

private:

  char* mOption;
  int  mWrite,mFineDiv;

  double mSigma10;
  double mSigma1;
  double mSigma20;
  double mSigma2;
  double mSigmaXSqPrev ;
  double mSigmaYSqPrev ;
  double mSigmaXYSqPrev ;
  double mSigmaXSqNow ;
  double mSigmaYSqNow ;
  double mSigmaXYSqNow ;
  /*
    double mSigmaSq1[129]; 
    double mSigmaSq2[129];
  */
  double dSigmaXSqBydt[4];
  double dSigmaYSqBydt[4];
  double dSigmaXYSqBydt[4];

 double mChargeNow;

  double mEnergy;
  double mTheta;
  double mPhi;
  double mInitPhi;

  double mDriftVel;
  double mTimBinSize;
  double mTotCharge;
  
  double mSDD_thickness;                      //  [mm]
  double mTrapConst;
  double mDiffusionConst;                           //  [mm**2/micro seconds] X and Y are calculated from this
  double mDiffConstX;                          //  [mm**2/micro seconds] in drift direction
  double mDiffConstY;                          //  [mm**2/micro seconds] in anode direction
  double mSi_DielConst;
  double mSi_EnergyGap;
  double mPermitivity;                        // [e/(mm-V)]
  double mSi_Mobility;                     // [mm**2/(V-micro seconds)]
  double mLifeTime;                           // [micro seconds]

  void CalculateDiffXY();                   
};

#endif
