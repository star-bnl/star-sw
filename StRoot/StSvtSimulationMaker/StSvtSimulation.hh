#ifndef STSVTSIMULATION_HH
#define STSVTSIMULATION_HH

#include <iostream.h>
#include <math.h>
#include <iomanip.h>
#include <stdlib.h>

#include "StSvtElectronCloud.hh"
#include "StThreeVector.hh"

class StSvtSignal;
class StSvtHybridPixels;
class StSvtHybridPixelsD;
//class StSvtWaferCoordinate;
//class svg_geom_st;

typedef struct  PasaSignalAttributes
 {

   int anode[10];                   //actual anode
   double mPeak[10];
   double mTimeCenter[10];
   double mTimeWidth[10];
   double mUnderShoot[10];
   double mTempBuffer[10][128];
   double mCharge[10];
 
 } PasaSignalAttributes;

class StSvtSimulation
{
public:
  StSvtSimulation();
  ~StSvtSimulation();

  void setOptions(Bool_t backgr,int option);
  void setPointers(StSvtElectronCloud* elCloud ,StSvtAngles* svtAngles);
  void setAnodeTime(double timBinSize, double anodeSize,double driftVelocity);
  

  void openFiles(int k, int option);
  void closeFiles(int k, int option);
  int writeFiles1(int c, int i, int n, double t, double adc);
  int writeFiles2(int c,int i, double timeBin,double timeCenter,double width, double peak);
  

  //StSvtWaferCoordinate toLocalCoord(StThreeVector<double>& x,StSvtCoordinateTransform  *coTransform);
  //void calcAngles(svg_geom_st *geom_st, double x, double y, double z, int mLayer, int mLadder, int mWafer );

  void doCloud_FixHitPos(double anode,double time, double Energy);
  void doCloud_VaryHitPos(double anode,double Energy);
  void doCloud(double time, double Energy,double mTheta,double mPhi);
  void calcPeakAndWidth(int k,double mAnHit, double mTimeHit, int option);
  void fillBuffer(double mAnHit, double mTimeHit, StSvtHybridPixelsD *svtSimDataPixels); 
 
  void setPasaSigAttributes(int pasaSigAttributes, int numOfAnodesPerHit=0);
  void resetAnodeAttributes(int numOfAnodes);
  void resetSignal(int an, int lTBin, int hTBin);

  double makeGausDev(double sigma);
  PasaSignalAttributes getPasaSigAttributes();
  double getPeak();

  
private:
  
  int mNumOfHybrids;
  int mNumOfHitsPerHyb;
  int mNumOfAnodesPerHit;
  int mLowBin;
  int mHiBin;
  int mUpperAn;
  int mLowerAn;
  int mPasaSigAttributes;

  double mTimeBinSize;
  double mAnodeSize;
  double mDriftVelocity;
  int mSignalOption;

  double mPeakSignal;

  Bool_t mBackGrOption;                 //!

  StSvtElectronCloud* mElectronCloud;  //!
  StSvtSignal* mSvtSignal;             //!
  StSvtAngles* mSvtAngles;             //!
  PasaSignalAttributes mPasaSignals;   //!

  // ClassDef(StSvtSimulation,1)

};

inline PasaSignalAttributes  StSvtSimulation::getPasaSigAttributes(){return mPasaSignals;}
inline double  StSvtSimulation::getPeak(){return mPeakSignal;}

#endif
