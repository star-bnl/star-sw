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
class StSvtWaferCoordinate;
class svg_geom_st;


class StSvtSimulation
{
public:
  StSvtSimulation();
  ~StSvtSimulation();

  void setOptions(char* backgr);
  void setPointers(StSvtElectronCloud* elCloud, StSvtSignal* svtSignal,StSvtAngles* svtAngles);
  void setParam(double timBinSize, double anodeSize);

  void openFiles();
  int writeFiles1(int c, int i, int n, double t, double adc);
  int writeFiles2(int c,int i, double width, double peak);
  void closeFiles();

  StSvtWaferCoordinate toLocalCoord(StThreeVector<double>& x,StSvtCoordinateTransform  *coTransform);
  void calcAngles(svg_geom_st *geom_st, double x, double y, double z, int mLayer, int mLadder, int mWafer );
  void doCloud(double time, double Energy,double mTheta,double mPhi);
  void reset();
  void fillBuffer(double mAnHit, double mTimeHit, double backgrsigma, StSvtHybridPixels *svtSimDataPixels);

  double makeGausDev(double sigma);
  double getTempBuffer(int i, int j);
  double getCharge(int i);

  
private:
  
  int mNumOfHybrids;
  double mTimeBinSize;
  double mAnodeSize;
  double mTempBuffer[7][128];
  double mCharge[7];
  double mPeakArray[7];

  char* mBackGrOption;                 //!

  StSvtElectronCloud* mElectronCloud;  //!
  StSvtSignal* mSvtSignal;             //!
  StSvtAngles* mSvtAngles;             //!

  // ClassDef(StSvtSimulation,1)

};

inline double StSvtSimulation::getTempBuffer(int i, int j){return mTempBuffer[i][j];}
inline double StSvtSimulation::getCharge(int i){return mCharge[i];}

#endif
