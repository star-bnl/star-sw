#ifndef STSVTELECTRONCLOUD_HH
#define STSVTELECTRONCLOUD_HH

#include <iostream.h>
#include <math.h>
#include <iomanip.h>
#include <stdlib.h>

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
void calculateWidthAtAnode(double mTc);
int runge_kutta4(int stepBefore, int numBinDiv, double steplen);
int adamsBushFort(int stepBefore, int numBinDiv, double steplen);
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
 double mTimBinSize;
 double mTotCharge;
 double mEnergy;
 double mTheta;
 double mPhi;
 double mChargeNow;
 double mSigma10;
 double mSigma1;
 double mSigma20;
 double mSigma2;
 double mSigmaSq1Prev ;
 double mSigmaSq2Prev ;
 double mSigmaSq1Now ;
 double mSigmaSq2Now ; 
  /*
 double mSigmaSq1[129]; 
 double mSigmaSq2[129];
  */
 double dSigma1SqBydt[4];
 double dSigma2SqBydt[4];

 double mSDD_thickness;                      //  [mm]
 double mDiffConst;                          //  [mm**2/micro seconds]
 double mSi_DielConst;
 double mSi_EnergyGap;
 double mPermitivity;                        // [e/(mm-V)]
 double mSi_Mobility;                     // [mm**2/(V-micro seconds)]
 double mLifeTime;                           // [micro seconds]

};

#endif
