/**********************************************************
 * $Id: StRichTrack.h,v 2.3 2000/10/03 19:26:02 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTrack.h,v $
 *  Revision 2.3  2000/10/03 19:26:02  horsley
 *  fixed error in StRichTrack correct member function, now returns bool.
 *
 *  Revision 2.5  2000/11/01 17:43:13  lasiuk
 *  default arguments initialization in c'tor.  Addition of init() member
 *  function to handle generic DB initialization and removal of virtual keyword
 *
 *  Revision 2.4  2000/10/19 01:13:23  horsley
 *  added member functions to StRichPIDMaker to make cuts on hits, tracks, events.
 *  added normal distance sigma cut on hits, quartz and radiator pathlengths
 *  for individual photons, modified minimization routine to correct boundary
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
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
#define STRICHTRACK_H

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#include "StEvent/StTrack.h"


#include "StPhysicalHelix.hh"


#include "StThreeVectorF.hh"
#include "TNtuple.h"

#include "StParticleDefinition.hh"
#include "StPhysicalHelix.hh"
#include "StPhysicalHelixD.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "SystemOfUnits.h"

// used in track coordinate transformations
#endif
#include "StRrsMaker/StGlobalCoordinate.h"
#include "StRrsMaker/StRichRawCoordinate.h"
#include "StRrsMaker/StRichLocalCoordinate.h"
#include "StRrsMaker/StRichCoordinateTransform.h"
#include "StRrsMaker/StRichGeometryDb.h"

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif


#include "StRrsMaker/StRichGeometryDb.h"
#include "StRrsMaker/StRichCoordinates.h"
#include "StRrsMaker/StRichCoordinateTransform.h"
#include "StRrsMaker/StRichMomentumTransform.h"

#include "StEvent/StTrack.h"

class StRichPidTraits;
class StRichHit;
class StRichRingHit;

class StRichTrack {
    virtual globalTrack* getL3Track(){ return mL3Track;};
    StRichTrack();
    StRichTrack(StTrack* tpcTrack, double magField);
    StRichTrack(StThreeVectorF, StThreeVectorF);
  virtual ~StRichTrack();
  virtual StTrack*         getStTrack();
  virtual StRichHit*       getAssociatedMIP();
  virtual void  addHit(StRichHit*, double, double, double, double, StParticleDefinition* );
  virtual StRichPidTraits* getPidTrait();  
  virtual void             addPidTrait(StRichPidTraits* ); 
#ifdef RICH_WITH_L3_TRACKS
  
  virtual void  clearHits();
  virtual void  addHit(StRichHit*, double, double, double, double, double, StParticleDefinition* );
  virtual vector<StRichRingHit*> getRingHits(StParticleDefinition* );

  virtual StThreeVectorF& getProjectedCTBPoint();
  virtual StThreeVectorF& getLastHit();
  virtual StThreeVectorF& getUnCorrectedImpactPoint();
  virtual StThreeVectorF& getUnCorrectedMomentum(); 
  virtual StThreeVectorF& getUnCorrectedProjectedMIP(); 
  virtual StThreeVectorF& getProjectedMIP(); 
  virtual StThreeVectorF& getImpactPoint();
  virtual StThreeVectorF& getMomentum();
  virtual StThreeVectorF& getMomentumAtPadPlane();
  
  virtual double  getTheta();
  virtual double  getPhi();
  virtual double  getPathLength();  
  virtual double  getZVertex();
  virtual double  getUnCorrectedTheta();
  virtual double  getUnCorrectedPhi( );
  virtual double  getLastHitDCA();
  virtual double  getExpectedNPhots(StParticleDefinition* particle);

  virtual bool    isGood(StParticleDefinition* );
  virtual bool    correct();
  virtual void    useUnCorrected();
  
  virtual int     fastEnough(StParticleDefinition* particle);
  virtual int     getMaxGap();
  virtual int     getMaxChain();
  virtual int     getFirstRow();
  virtual int     getLastRow();  
    globalTrack* getL3Track(){ return mL3Track;};
    
    int     fastEnough(StParticleDefinition* particle);
  virtual void  setUnCorrectedTheta(double );
  virtual void  setUnCorrectedPhi(double );
  virtual void  setUnCorrectedImpactPoint(StThreeVectorF& );
  virtual void  setUnCorrectedMomentum(StThreeVectorF& );
  virtual void  setUnCorrectedProjectedMIP(StThreeVectorF& );
  virtual int   maxSeq(vector<int>&); 
  virtual void  setTheta(double the);
  virtual void  setPhi(double phi);     
  virtual void  setMomentum(StThreeVectorF& momentum);
  virtual void  setImpactPoint(StThreeVectorF& impact);
  virtual void  setProjectedMIP(StThreeVectorF& mip);
  virtual void  setPathLength(double p);

  virtual void  setMaxGap(int);
  virtual void  setMaxChain(int);
  virtual void  setFirstRow(int);
  virtual void  setLastRow(int);
  virtual void  setProjectedCTB(StThreeVectorF& );
  virtual void  setLastHit(StThreeVectorF );
  virtual void  setLastHitDCA(double);


  // data members
  StTrack* mStTrack;
  StRichPidTraits* mPidTrait;
  StRichHit* mAssociatedMIP;

  vector<StRichRingHit*> mPionList;
  vector<StRichRingHit*> mKaonList;
  vector<StRichRingHit*> mProtonList;
    StRichPidTraits* mPidTrait; //!
    StRichHit* mAssociatedMIP;  //!
    double mMagneticField;

    vector<StRichRingHit*> mPionList;   //!
    double mMagneticField;
    vector<StRichRingHit*> mKaonList;   //!
    vector<StRichRingHit*> mProtonList; //!

#ifdef RICH_WITH_L3_TRACKS
    globalTrack *mL3Track;
#endif
    
    double mUnCorrectedTheta;
    double mUnCorrectedPhi;
    double mLastHitDCA;
    double mPath;
    double mPhi;

    int    mLastRow;
    int    mFirstRow;
    int    mMaxGap;

    
    StThreeVectorF mImpactPoint;
    
    StThreeVectorF mUnCorrectedImpactPoint;  

    StThreeVectorF mUnCorrectedMomentum;  
    
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
