/**********************************************************
 * $Id: StRichRingPoint.h,v 1.2 2000/05/19 19:06:11 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRingPoint.h,v $
 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 **********************************************************/

#ifndef StRichRingPoint_h
#define StRichRingPoint_h

#include "StRichTrack.h"
#include "StParticleDefinition.hh"
#include "StRichRingDefinition.h" 
#include "StRichMaterialsDb.h"
#include "StThreeVector.hh"

class  StRichRingPoint {
public:
  
  StRichRingPoint(StRichTrack* track, StRichRingDefinition type);
  ~StRichRingPoint();
  double rotatedFunction(double psi);
  bool   getPoint(double psi, StThreeVector<double>& point);
  void   setPoint(StThreeVector<double>& sPoint);
  void   setParticleType(StParticleDefinition* particle);  
  StRichTrack* getTrack(); 
  

private:
  // ring parameters
  StRichRingDefinition mRingType;
  double mInnerWavelength;
  double mOuterWavelength;
  double mMeanWavelength;
  
  // track parameters
  double mTrackTheta;
  double mTrackPhi;
  double mPsi;
  double mMomentum;
  double mMass;
  double mBeta;
  double mCher;
  double mTrackCosTheta;
  double mTrackSinTheta;
  double mTrackCosPhi;
  double mTrackSinPhi;
  double mTrackTanTheta;
  double mTanCher;

  StThreeVector<double> mImpactPoint;
  bool mFastEnough;
  StRichTrack* mTrack;

  // detector parameters
  double mDepthRad;
  double mMeanDepthRad;
  double mDepthQuar;
  double mDepthProx;
  double mIndexRad[3];
  double mIndexQuartz[3];
  double mIndexMeth[3];

  // detector parameters
  StRichMaterialsDb* richMaterialsDb;

  // ray propagation
  double mRadiatorAngle;
  double mQuartzAngle;
  double mMethaneAngle;
  double mPsiPrime;
  StThreeVector<double>  mRefractedAway;
  bool status;

  // rotated function
  StThreeVector<double> tempPoint;
  StThreeVector<double> minPoint;

};



#endif





