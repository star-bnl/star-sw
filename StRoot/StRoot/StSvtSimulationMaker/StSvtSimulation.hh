#ifndef STSVTSIMULATION_HH
#define STSVTSIMULATION_HH

/*
#include <iostream.h>
#include <fstream.h>
*/
#include <Stiostream.h>
#include <math.h>
#include <stdlib.h>

#include "StSvtElectronCloud.hh"

class StSvtSignal;
class StSvtHybridPixelsD;

#define SvtSim_MaxBufferSize 20

struct  PasaSignalAttributes
{
   int anode         [SvtSim_MaxBufferSize];              //actual anode
   double mPeak      [SvtSim_MaxBufferSize];
   double mTimeCenter[SvtSim_MaxBufferSize];
   double mTimeWidth [SvtSim_MaxBufferSize];
   double mUnderShoot[SvtSim_MaxBufferSize];
   double mTempBuffer[SvtSim_MaxBufferSize][128];
   double mCharge    [SvtSim_MaxBufferSize];
};

class StSvtSimulation:public TObject
{
public:
  StSvtSimulation();
  ~StSvtSimulation();

  void setOptions(int option);
  void setElCloud(StSvtElectronCloud* elCloud);
  void setAnodeTimeBinSizes(double timBinSize, double anodeSize);
  void setDriftVelocity(double driftVelocity);
  void setTrappingConst(double trapConst);
  void setPasaSigAttributes(int pasaSigAttributes, int numOfAnodesPerHit=0);
 
  void doCloud(double time, double Energy,double mTheta,double mPhi,int trackId);
  void fillBuffer(double mAnHit, double mTimeHit, StSvtHybridPixelsD *svtSimDataPixels); 
 
  PasaSignalAttributes getPasaSigAttributes();
  double getPeak();

  
private:

  void resetAnodeAttributes();
  void resetBuffer();

  int mNumOfAnodesPerHit;
  int mUpperAn;
  int mLowerAn;

  int mPasaDebug; //for debugging

  double mTimeBinSize;
  double mAnodeSize;
  double mDriftVelocity;
  int mSignalOption;
  double mPeakSignal;

  StSvtElectronCloud* mElectronCloud;  //!
  StSvtSignal* mSvtSignal;             //!

  //structure used for debugging
  PasaSignalAttributes mPasaSignals;   //!

  ClassDef(StSvtSimulation,2)

};

inline PasaSignalAttributes  StSvtSimulation::getPasaSigAttributes(){return mPasaSignals;}
inline double  StSvtSimulation::getPeak(){return mPeakSignal;}

#endif
