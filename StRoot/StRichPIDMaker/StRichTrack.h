/**********************************************************
 * $Id: StRichTrack.h,v 2.7 2000/11/14 22:31:54 lasiuk Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTrack.h,v $
 *  Revision 2.7  2000/11/14 22:31:54  lasiuk
 *  associated MIP (commented)
 *  return copy instead of reference
 *
 *  Revision 2.6  2000/11/07 14:13:27  lasiuk
 *  add possibility of .4*px/pz correction to the track extrapolation
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
 *
 *  Revision 1.1  2000/04/03 19:36:09  horsley
 *  initial revision
 **********************************************************/
#ifndef STRICHTRACK_H
#define STRICHTRACK_H

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#include "StRichTrackingControl.h"

#include "TNtuple.h"

#include "StParticleDefinition.hh"
#include "StPhysicalHelix.hh"
#include "StPhysicalHelixD.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "SystemOfUnits.h"

#ifdef RICH_WITH_L3_TRACKS
#include "StDaqLib/L3/L3_Reader.hh"
#endif

#include "StRichMaterialsDb.h"

#include "StRrsMaker/StRichGeometryDb.h"
#include "StRrsMaker/StRichCoordinates.h"
#include "StRrsMaker/StRichCoordinateTransform.h"
#include "StRrsMaker/StRichMomentumTransform.h"

#include "StEvent/StTrack.h"

class StRichPidTraits;
class StRichHit;
class StRichRingHit;

class StRichTrack {
public:
    StRichTrack();
    StRichTrack(StTrack* tpcTrack, double magField);
    StRichTrack(StThreeVectorF, StThreeVectorF);

#ifdef RICH_WITH_L3_TRACKS
    StRichTrack(globalTrack *track,double magField);
    globalTrack* getL3Track(){ return mL3Track;};
    double getL3ZVertex();
#endif

    virtual ~StRichTrack();

    void init();

    StTrack*         getStTrack();
    StRichHit*       getAssociatedMIP();
    void             assignMIP(const StSPtrVecRichHit*);  

    //
    // Pid Trait manipulation
    //
    StRichPidTraits* getPidTrait();  
    void             addPidTrait(StRichPidTraits*); 
    
    void  clearHits();
    void  addHit(StRichHit*, double, double, double, double, double, StParticleDefinition* );
    vector<StRichRingHit*> getRingHits(StParticleDefinition* );

    StThreeVectorF& getProjectedCTBPoint();
    StThreeVectorF& getLastHit();
    StThreeVectorF& getUnCorrectedImpactPoint();
    StThreeVectorF& getUnCorrectedMomentum(); 
    StThreeVectorF& getUnCorrectedProjectedMIP(); 
    StThreeVectorF& getProjectedMIP(); 
    StThreeVectorF& getImpactPoint();
    StThreeVectorF& getMomentum();
    StThreeVectorF& getMomentumAtPadPlane();
    
    double  getTheta();
    double  getPhi();
    double  getPathLength();  
    double  getZVertex();
    double  getUnCorrectedTheta();
    double  getUnCorrectedPhi( );
    double  getLastHitDCA();
    double  getExpectedNPhots(StParticleDefinition* particle);

    bool    isGood(StParticleDefinition* );
    bool    correct();
    void    useUnCorrected();
    
    int     fastEnough(StParticleDefinition* particle);
    int     getMaxGap();
    int     getMaxChain();
    int     getFirstRow();
    int     getLastRow();  
    
protected:
  
    void  setUnCorrectedTheta(double );
    void  setUnCorrectedPhi(double );
    void  setUnCorrectedImpactPoint(StThreeVectorF& );
    void  setUnCorrectedMomentum(StThreeVectorF& );
    void  setUnCorrectedProjectedMIP(StThreeVectorF& );
    int   maxSeq(vector<int>&); 
    void  setTheta(double the);
    void  setPhi(double phi);     
    void  setMomentum(StThreeVectorF& momentum);
    void  setImpactPoint(StThreeVectorF& impact);
    void  setProjectedMIP(StThreeVectorF& mip);
    void  setPathLength(double p);
    
    void  setMaxGap(int);
    void  setMaxChain(int);
    void  setFirstRow(int);
    void  setLastRow(int);
    void  setProjectedCTB(StThreeVectorF& );
    void  setLastHit(StThreeVectorF );
    void  setLastHitDCA(double);
    
protected:
    StTrack* mStTrack;          //!
    StRichPidTraits* mPidTrait; //!
    StRichHit* mAssociatedMIP;  //!
    double mMagneticField;

    vector<StRichRingHit*> mPionList;   //!
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
    double mTheta; 
    int    mLastRow;
    int    mFirstRow;
    int    mMaxGap;
    int    mMaxChain;
    
    StThreeVectorF mImpactPoint;
    
    StThreeVectorF mUnCorrectedImpactPoint;  
    StThreeVectorF mUnCorrectedProjectedMIP;  
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
