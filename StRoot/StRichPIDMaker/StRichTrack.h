/**********************************************************
 * $Id: StRichTrack.h,v 2.0 2000/08/09 16:26:21 gans Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTrack.h,v $
 *  Revision 2.0  2000/08/09 16:26:21  gans
 *  Naming Convention for TDrawable Ojects. All drawable objects now in StRichDisplayMaker
 *
 *  problems
 *
 *  Revision 2.3  2000/10/03 19:26:02  horsley
 *  fixed error in StRichTrack correct member function, now returns bool.
 *
 *  Revision 2.2  2000/09/29 17:55:51  horsley
 *  fixed bug in Minimization routine, included StMagF stuff (commented out)
 *  changed StRichRingPoint  HUGE_VALUE   ---> MAXFLOAT for default value
 *
 *  Revision 2.1  2000/09/29 01:35:38  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.3  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
#define STRICHTRACK_H

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#include "StThreeVector.hh"


#include "StPhysicalHelix.hh"


#include "StThreeVectorF.hh"
#include "TNtuple.h"

#include "StPhysicalHelix.hh"
#include "StPhysicalHelixD.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "SystemOfUnits.h"

// used in track coordinate transformations
#endif

class StSPtrVecRichHit;
#endif


#include "StRrsMaker/StRichGeometryDb.h"
#include "StRrsMaker/StRichCoordinates.h"
#include "StRrsMaker/StRichMomentumTransform.h"

#include "StEvent/StTrack.h"
class StRichPidTraits;
class StRichHit;
    virtual ~StRichTrack();
    virtual StTrack* getStTrack();
#ifdef RICH_WITH_L3_TRACKS 
    virtual globalTrack* getL3Track(){ return mL3Track;};
#endif
    virtual StThreeVector<double> getProjectedMIP();
    virtual StThreeVector<double> getAssociatedMIP();
    virtual double getAssociatedMIPCharge();
    virtual StThreeVector<double> getImpactPoint();
    virtual StThreeVector<double> getMomentum();
    virtual StThreeVector<double> getMomentumAtPadPlane();
    
    virtual double  getTheta();
    virtual double  getPhi();
    virtual int     getCharge();
    virtual int     fastEnough(StParticleDefinition* particle);
    virtual double  getPathLength();
    virtual void    assignMIP(const StSPtrVecRichHit*);
    virtual double  getZVertex();
    virtual bool     isGood(StParticleDefinition* );
  virtual int     fastEnough(StParticleDefinition* particle);
    virtual void  setTheta(double the);
    virtual void  setPhi(double phi);     
    virtual void  setMomentum(StThreeVector<double>& momentum);
    virtual void  setImpactPoint(StThreeVector<double>& impact);
    virtual void  setCharge(int ch); 
    virtual void  setProjectedMIP(StThreeVector<double>& mip);
    virtual void  setAssociatedMIP(StThreeVector<double>& mip);
    virtual void  setPathLength(double p);
        
    // data members
    StTrack* mStTrack;
  StRichHit* mAssociatedMIP;

  vector<StRichRingHit*> mPionList;
    int mCharge;
    double mAssociatedMIPCharge;

    vector<StRichRingHit*> mPionList;   //!
    double mMagneticField;
    StThreeVector<double>  mImpactPoint;
    globalTrack *mL3Track;
    StThreeVector<double>  mMomentum; // at Radiator
    StThreeVector<double>  mMomentumAtPadPlane;
    
    StThreeVector<double>  mProjectedMIP;
    StThreeVector<double>  mAssociatedMIP;
    StThreeVectorF mImpactPoint;
    
    StThreeVectorF mRichNormal;
    StThreeVectorF mRadiatorGlobal;
    StThreeVectorF mAnodeGlobal;
    StThreeVectorF mCTBGlobal;

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
