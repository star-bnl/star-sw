/**********************************************************
 * $Id: StRichTrack.h,v 1.2 2000/05/19 19:06:11 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTrack.h,v $
 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  changed StRichRingPoint  HUGE_VALUE   ---> MAXFLOAT for default value
 *
 *  Revision 2.1  2000/09/29 01:35:38  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.2  2000/05/19 19:06:11  horsley
#include "StTrack.h"

#include "StRrsMaker/StRichGeometryDb.h"
#include "StRichMaterialsDb.h"
#define STRICHTRACK_H

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#include "StThreeVector.hh"


#include "StPhysicalHelix.hh"
#include "StPhysicalHelixD.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "SystemOfUnits.h"

#endif


  StRichTrack();
  StRichTrack(StTrack* tpcTrack, double magField);
  
  virtual ~StRichTrack();
  virtual StTrack* getStTrack();
  
  virtual StThreeVector<double> getMIP();
  virtual StThreeVector<double> getImpactPoint();
  virtual StThreeVector<double> getMomentum();

  virtual double  getTheta();
  virtual double  getPhi();
  virtual int      getCharge();
  virtual int      fastEnough(StParticleDefinition* particle);
  virtual double  getPathLength();
  
  virtual int     fastEnough(StParticleDefinition* particle);
  virtual void  setTheta(double the);
  virtual void  setPhi(double phi);     
  virtual void  setMomentum(StThreeVector<double>& momentum);
  virtual void  setImpactPoint(StThreeVector<double>& impact);
  virtual void  setCharge(int ch); 
  virtual void  setMIP(StThreeVector<double>& mip);
  virtual void  setPathLength(double p);
  
  // data members
  StTrack* mStTrack;
  int mCharge;
  
  double mPath;
  double mPhi;
  double mTheta; 
  StThreeVector<double>  mImpactPoint;
  StThreeVector<double>  mMomentum;
  StThreeVector<double>  mMIP;


    StThreeVectorF mMomentum; // at Radiator
    StThreeVectorF mMomentumAtPadPlane;
    
    StThreeVectorF mProjectedMIP;
    StThreeVectorF mProjectedCTB;
    StThreeVectorF mLastHit;
    
    // geometry db, transformations ...











    StRichGeometryDb*          myGeometryDb;
    StRichCoordinateTransform* coordinateTransformation;
    StRichMomentumTransform*   momentumTransformation;
    StRichMaterialsDb*         myMaterialsDb;
};

#endif
