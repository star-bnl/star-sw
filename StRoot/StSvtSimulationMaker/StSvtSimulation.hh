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
//class StSvtWaferCoordinate;
//class svg_geom_st;

typedef struct  PasaSignalAttributes
 {

   int anode[7];                   //actual anode
   double mPeak[7];
   double mTimeCenter[7];
   double mTimeWidth[7];
   double mUnderShoot[7];
   double mTempBuffer[7][128];
   double mCharge[7];
 
 } PasaSignalAttributes;

class StSvtSimulation
{
public:
  StSvtSimulation();
  ~StSvtSimulation();

  void setOptions(char* backgr);
  void setPointers(StSvtElectronCloud* elCloud, StSvtSignal* svtSignal,StSvtAngles* svtAngles);
  void setAnodeTime(double timBinSize, double anodeSize);

  void openFiles();
  int writeFiles1(int c, int i, int n, double t, double adc);
  int writeFiles2(int c,int i, double width, double peak);
  void closeFiles();

  //StSvtWaferCoordinate toLocalCoord(StThreeVector<double>& x,StSvtCoordinateTransform  *coTransform);
  //void calcAngles(svg_geom_st *geom_st, double x, double y, double z, int mLayer, int mLadder, int mWafer );
  void doCloud(double time, double Energy,double mTheta,double mPhi);

  void fillBuffer(double mAnHit, double mTimeHit, double backgrsigma, StSvtHybridPixels *svtSimDataPixels);
  void setPasaSigAttributes(int pasaSigAttributes,int numOfHitsPerHyb);
  void resetPasaSignalAttributes();

  double makeGausDev(double sigma);
  PasaSignalAttributes* getPasaSigAttributes();

  
private:
  
  int mNumOfHybrids;
  int mNumOfHitsPerHyb;
  int mPasaSigAttributes;

  double mTimeBinSize;
  double mAnodeSize;

  char* mBackGrOption;                 //!

  StSvtElectronCloud* mElectronCloud;  //!
  StSvtSignal* mSvtSignal;             //!
  StSvtAngles* mSvtAngles;             //!
  PasaSignalAttributes* mPasaSignals;   //!

  // ClassDef(StSvtSimulation,1)

};

inline PasaSignalAttributes*  StSvtSimulation::getPasaSigAttributes(){return mPasaSignals;}


#endif
