#ifndef STSVTELECTRONCLOUD_HH
#define STSVTELECTRONCLOUD_HH

#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>
#include "StObject.h"

/*!
 *
 * \class  StSvtElectronCloud
 * \author Chaloupka
 * \date   2004/11/23
 * \brief SVT electron cloud expansion routines
 *        Simulates electron cloud expansion inside of the silicon wafer  
 */

class StSvtElectronCloud:public StObject
{
public:
  StSvtElectronCloud();
  ~StSvtElectronCloud();

  //routines to set
  void setSiliconProp();
  void setElectronLifeTime(double tLife);
  void setDriftVelocity(double driftVel);
  void setTrappingConst(double trapConst);
  void setDiffusionConst(double diffConst);

  void setPar(double energy,double theta, double phi,double timeBinSize,int trackId); 
  void CalcExpansion(double mTc);
  void runge_kutta4(int steps, double t0, double steplen,int save);
  void adamsBushFort(int steps, double t0, double steplen);

  double getSigmaDrift();
  double getSigmaAnode();
  double getSigmaCorr();
  double getSigmaMajor();
  double getSigmaMinor();
  double getPhi();
  double getChargeAtAnode();
  int    getTrackId() const {return mTrackId;}
  // double getTotCharge(){return mTotCharge;}; 
private:
  void setInitWidths(double w1, double w2);
 
  double mSigX;
  double mSigY ;
  double mSigXY;
 
  double m_dSigX[4];
  double m_dSigY[4];
  double m_dSigXY[4];

  double mChargeNow;
  double mTotCharge;
  
  double mEnergy;
  double mTheta;
  double mPhi;
  double mInitPhi;

  double mDriftVel;
  double mTimBinSize;
  
  double mSDD_thickness;                      //  [mm]
  double mInitHitSize;                      //  [mm]
  double mTrapConst;
  double mDiffusionConst;                           //  [mm**2/micro seconds] X and Y are calculated from this
  double mDiffConstX;                          //  [mm**2/micro seconds] in drift direction
  double mDiffConstY;                          //  [mm**2/micro seconds] in anode direction
  double mSi_DielConst;
  double mSi_EnergyGap;
  double mPermitivity;                        // [e/(mm-V)]
  double mSi_Mobility;                     // [mm**2/(V-micro seconds)]
  double mLifeTime;                           // [micro seconds]
  int    mTrackId;
  void CalculateDiffXY();          
  void GetDerivatives(double &dSx,double &dSy,double &dSxy,double SX,double SY,double SXY,double time);

  ClassDef(StSvtElectronCloud,1)         
};

#endif
