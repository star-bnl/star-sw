/**********************************************************
 * $Id: StRichTrack.h,v 1.1 2000/04/03 19:36:09 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTrack.h,v $
 *  Revision 1.1  2000/04/03 19:36:09  horsley
 *  initial revision
 *
 *  
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
  StRichTrack(StTrack* tpcTrack, double magFieldStrength);
  StRichTrack(StThreeVector<double>& tpcMom, 
	      StThreeVector<double>& impact);
  
  ~StRichTrack();

  StTrack* getTrackPointer();

  StThreeVector<double> getImpactPoint();
  StThreeVector<double> getMomentum();
  double  getTheta();
  double  getPhi();
  void  setMomentum(StThreeVector<double>& momentum);
  void  setImpactPoint(StThreeVector<double>& impact);
  int   fastEnough(StParticleDefinition* particle);
  void  setPID(int hits, double area, StParticleDefinition* particle);
  void   getPID(int& hits, double& area, StParticleDefinition* particle);
  void printPID();
  
 protected:
  void  setTheta(double the);
  void  setPhi(double phi);     
 
 // data members
  StTrack* mTrack;

  double mPhi,mTheta; 
  StThreeVector<double>  mImpactPoint,mMomentum;
  double magFieldStrength;
  
  int pionHits;
  int kaonHits;
  int protonHits;
  int electronHits;

  double electronArea;
  double pionArea;
  double kaonArea;
  double protonArea;
 

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
