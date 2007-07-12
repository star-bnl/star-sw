/**********************************************************
 * $Id: StRichTrack.cxx,v 2.25 2007/07/12 19:57:10 fisyak Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTrack.cxx,v $
 *  Revision 2.25  2007/07/12 19:57:10  fisyak
 *  Add includes for ROOT 5.16
 *
 *  Revision 2.24  2003/10/23 04:26:28  perev
 *  Init to zero added
 *
 *  Revision 2.23  2003/09/02 17:58:54  perev
 *  gcc 3.2 updates + WarnOff
 *
 *  Revision 2.22  2001/10/04 19:45:18  lasiuk
 *  Remove the px/pz correction for track extrapolation
 *
 *  Revision 2.21  2001/02/22 21:06:30  lasiuk
 *  momentumLoss calcuation called when momentum of track is set
 *
 *  Revision 2.20  2001/02/07 16:00:14  lasiuk
 *  inline functions made.  Data members are set directly.
 *  residual cut is a data member
 *  momentum loss in fastEnough() (uses local momentum)
 *  store momentum loss for individual particles
 *  xCorrection() made into a member function
 *
 *  Revision 2.19  2001/02/01 17:55:30  horsley
 *  set energy loss in CTB at 20 MeV (default)
 *  ifdef'd out the TrackEntryClass
 *  StRichTrack::fastEnough() has materialsDB input for wavelenght's
 *
 *  Revision 2.18  2001/01/30 22:13:10  horsley
 *  trajectory correction now default, added trajectory correction comments for log file
 *
 *  Revision 2.17  2001/01/30 16:38:44  horsley
 *  updated PID maker for next production run, included new class for TTree
 *
 *  Revision 2.16  2000/12/14 19:20:49  horsley
 *  added event run id to dist ntuple,
 *
 *  added flag to bit mask in dist ntuple to indictae which checkTrack
 *  check made entry to ntuple,
 *
 *  dist ntuple has global p_vec and not local p_vec
 *
 *  commented out StRichTrack pathlength check in constructor
 *
 *  Revision 2.15  2000/12/08 20:09:32  horsley
 *  updated monte carlo ntuples, member functions in StRichMCTrack, StRichPIDMaker
 *  changed monte carlo double xCorrection = 0 in StRichTrack to xCorrection = 0
 *  with no declaration of the double
 *
 *  Revision 2.14  2000/12/08 15:01:07  horsley
 *  pion switch in getNew, Orig hits
 *  case -221: -> case -211:
 *
 *  Revision 2.13  2000/12/08 06:32:02  lasiuk
 *  correctedMomentum()
 *  xcorrection
 *  comment setMomentum()
 *
 *  Revision 2.12  2000/12/08 04:56:30  lasiuk
 *  MC define switch for xCorrection
 *  energyLoss() members
 *  orignal/newhits (for refit)
 *  assignHits()
 *  correct-->correctTrajectory()
 *
 *  Revision 2.11  2000/11/30 23:30:16  lasiuk
 *  Warning in isGood() added
 *
 *  Revision 2.10  2000/11/28 19:18:54  lasiuk
 *  Include protection/error warning if no MIP
 *
 *  Revision 2.9  2000/11/25 12:27:42  lasiuk
 *  implement algorithm for finding MIP
 *
 *  Revision 2.8  2000/11/21 19:46:09  lasiuk
 *  px/pz correction uncommented
 *
 *  Revision 2.7  2000/11/14 22:31:51  lasiuk
 *  associated MIP (commented)
 *  return copy instead of reference
 *
 *  Revision 2.6  2000/11/07 14:13:24  lasiuk
 *  add possibility of .4*px/pz correction to the track extrapolation
 *
 *  Revision 2.5  2000/11/01 17:43:10  lasiuk
 *  default arguments initialization in c'tor.  Addition of init() member
 *  function to handle generic DB initialization and removal of virtual keyword
 *
 *  Revision 2.4  2000/10/19 01:13:23  horsley
 *  added member functions to StRichPIDMaker to make cuts on hits, tracks, events.
 *  added normal distance sigma cut on hits, quartz and radiator pathlengths
 *  for individual photons, modified minimization routine to correct boundary
 *  problems
 *
 *  Revision 2.3  2000/10/03 19:26:01  horsley
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
#include "StRichTrack.h"

#include "StRichMcSwitch.h"

#include <Stiostream.h>
#include <math.h>
#include <string>
#include <vector>
#include <numeric>
#include <algorithm>

#ifdef __SUNPRO_CC
#include <utility> //bool is defined here in SUN
#ifndef ST_NO_NAMESPACES
using std::pair;
using std::adjacent_difference;
using std::max_element;
using std::sort;
#endif
#endif

// SCL
#include "StParticleTypes.hh"
#include "StParticleDefinition.hh"
#include "SystemOfUnits.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

//StEvent
#include "StEventTypes.h"
#include "StRichPidTraits.h"
#include "StRichHit.h"

//PID
#include "StRichMaterialsDb.h"
#include "StRichRingPoint.h"
#include "StRichRingHit.h"

// RRS
#include "StRrsMaker/StRichGeometryDb.h"


//
// For sorting the possible MIP candiates
//
typedef pair<StRichHit*, double> candidate;
bool operator<(const candidate &a, const candidate &b) {return (a.second<b.second);};


StRichTrack::StRichTrack()
: 
mStTrack(0),          
mPidTrait(0), 
mAssociatedMIP(0),  
mMagneticField(0),
mResidualCut(0),
mUnCorrectedTheta(0),
mUnCorrectedPhi(0),
mLastHitDCA(0),
mPath(0),
mPhi(0),
mTheta(0), 
mLastRow(0),
mFirstRow(0),
mMaxGap(0),
mMaxChain(0),
mDoMomentumLoss(0),
mEnergyLoss(0),
mPiondPdx(0),
mKaondPdx(0),
mProtondPdx(0),
mPionMass(0),
mKaonMass(0),
mProtonMass(0),
mRefit(0),
myGeometryDb(0),
coordinateTransformation(0),
momentumTransformation(0),
myMaterialsDb(0)

{/* nopt */}

StRichTrack::StRichTrack(StThreeVectorF mom, StThreeVectorF imp)
: 
mStTrack(0),          
mPidTrait(0), 
mAssociatedMIP(0),  
mMagneticField(0),
mResidualCut(0),
mUnCorrectedTheta(0),
mUnCorrectedPhi(0),
mLastHitDCA(0),
mPath(0),
mPhi(0),
mTheta(0), 
mLastRow(0),
mFirstRow(0),
mMaxGap(0),
mMaxChain(0),
mDoMomentumLoss(0),
mEnergyLoss(0),
mPiondPdx(0),
mKaondPdx(0),
mProtondPdx(0),
mPionMass(0),
mKaonMass(0),
mProtonMass(0),
mRefit(0),
myGeometryDb(0),
coordinateTransformation(0),
momentumTransformation(0),
myMaterialsDb(0)
{
    //
#ifdef RICH_WITH_L3_TRACKS
  mL3Track  = 0; 
#endif  

  this->init();
  
  // define system parameters
  this->setMomentum(mom);
  this->setImpactPoint(imp);
}

#ifdef RICH_WITH_L3_TRACKS
StRichTrack::StRichTrack(globalTrack *track, double magField)
: 
mStTrack(0),          
mPidTrait(0), 
mAssociatedMIP(0),  
mMagneticField(0),
mResidualCut(0),
mUnCorrectedTheta(0),
mUnCorrectedPhi(0),
mLastHitDCA(0),
mPath(0),
mPhi(0),
mTheta(0), 
mLastRow(0),
mFirstRow(0),
mMaxGap(0),
mMaxChain(0),
mDoMomentumLoss(0),
mEnergyLoss(0),
mPiondPdx(0),
mKaondPdx(0),
mProtondPdx(0),
mPionMass(0),
mKaonMass(0),
mProtonMass(0),
mRefit(0),
myGeometryDb(0),
coordinateTransformation(0),
momentumTransformation(0),
myMaterialsDb(0)
{
    //
    // define system parameters
    //
    
    this->init();
    
    // need to include mwpc gap correction, 
    // origin is located in center of gap, not on padplane --> 2 mm
    double anodeDistanceToPadPlane = myGeometryDb->anodeToPadSpacing();
    StRichLocalCoordinate edgeOfRichRadiator_Local(0.0,
						   0.0,
						   myGeometryDb->proximityGap() +
						   myGeometryDb->quartzDimension().z() +
						   myGeometryDb->radiatorDimension().z());
    
    // MIP is measured at anode position
    StRichLocalCoordinate richAnodeWirePlane_Local(0.,
						   0.,
						   0.+anodeDistanceToPadPlane);	
    StGlobalCoordinate edgeOfRichRadiator_Global;
    (*coordinateTransformation)(edgeOfRichRadiator_Local,
				edgeOfRichRadiator_Global);
    
    StGlobalCoordinate richAnodeWirePlane_Global;
    (*coordinateTransformation)(richAnodeWirePlane_Local,
				richAnodeWirePlane_Global);
    

    // L3 Stuff
    double x0=track->r0*cos(track->phi0);
    double y0=track->r0*sin(track->phi0);
    double z0=track->z0;

    //momentum
    double px=track->pt * cos(track->psi);
    double py=track->pt * sin(track->psi);
    double pz=track->pt * track->tanl;

    StPhysicalHelixD mHelix(StThreeVectorD(px,py,pz),
			    StThreeVectorD(x0,y0,z0),
			    magField*kilogauss,
			    track->q);    
    
    StThreeVectorD rr(edgeOfRichRadiator_Global.position().x(),
		      edgeOfRichRadiator_Global.position().y(),
		      edgeOfRichRadiator_Global.position().z());
    
    StThreeVectorD rp(richAnodeWirePlane_Global.position().x(),
		      richAnodeWirePlane_Global.position().y(),
		      richAnodeWirePlane_Global.position().z());
    
    double mPathLengthAtRadiator  = mHelix.pathLength(rr,richNormal);
    double mPathLengthAtPadPlane  = mHelix.pathLength(rp,richNormal);
    
    StThreeVectorD tpcMom = mHelix.momentumAt(mPathLengthAtRadiator,magField*kilogauss);	
    StThreeVector<double> tpcMomentum(tpcMom.x(),tpcMom.y(),tpcMom.z());    
    
    // do the momentum vector rotation here
    StThreeVector<double> richLocalMomentum;
    momentumTransformation->localMomentum(tpcMomentum,richLocalMomentum);
    
    // impact point on radiator
    StGlobalCoordinate globalImpactPoint(mHelix.x(mPathLengthAtRadiator),
					 mHelix.y(mPathLengthAtRadiator),
					 mHelix.z(mPathLengthAtRadiator));
    
    StRichLocalCoordinate richTransformedImpactPoint(-999,-999,-999);
    (*coordinateTransformation)(globalImpactPoint,richTransformedImpactPoint);

    double xCorrection = 0.4*(mMomentumAtPadPlane.x()/mMomentumAtPadPlane.z());

#ifdef myRICH_WITH_MC
    cout << "StRichTrack::StRichTrack() (L3)\n";
#endif
    richTransformedImpactPoint.position().setX(richTransformedImpactPoint.position().x()
					       - this->xCorrection());

    StThreeVectorD ppMomentum;
    
    // pad plane intersection (projected MIP)
    StGlobalCoordinate    globalImpactPointAtAnodeWirePlane(-999,-999,-999);
    StRichLocalCoordinate localImpactPointAtAnodeWirePlane(-999,-999,-999);  
    
    globalImpactPointAtAnodeWirePlane.position().setX(mHelix.x(mPathLengthAtPadPlane));
    globalImpactPointAtAnodeWirePlane.position().setY(mHelix.y(mPathLengthAtPadPlane));
    globalImpactPointAtAnodeWirePlane.position().setZ(mHelix.z(mPathLengthAtPadPlane));	
    
    (*coordinateTransformation)(globalImpactPointAtAnodeWirePlane,
				localImpactPointAtAnodeWirePlane);
    
    localImpactPointAtAnodeWirePlane.position()
      .setX(localImpactPointAtAnodeWirePlane.position().x() - this->xCorrection());
    
    ppMomentum =  mHelix.momentumAt(mPathLengthAtPadPlane,magField*kilogauss);


    StThreeVector<double> padPlaneMomentum(ppMomentum.x(),ppMomentum.y(),ppMomentum.z());
    momentumTransformation->localMomentum(padPlaneMomentum,mMomentumAtPadPlane);

    this->setMomentum(richLocalMomentum);
    //this->setUnCorrectedMomentum(richLocalMomentum); --> replace with:
    mUnCorrectedMomentum = richLocalMomentum;
    //this->setImpactPoint(richTransformedImpactPoint.position()); --> replace with:
    mImpactPoint = richTransformedImpactPoint.position();
    //this->setProjectedMIP(localImpactPointAtAnodeWirePlane.position()); --> replace with:
    mProjectedMIP = localImpactPointAtAnodeWirePlane.position();
    //this->setPathLength(mPathLengthAtRadiator); --> replace with:
    mPath = mPathLengthAtRadiator;
}
#endif

StRichTrack::StRichTrack(StTrack* tpcTrack, double magField)
    : mStTrack(tpcTrack),       mPidTrait(0),  mAssociatedMIP(0),
      mMagneticField(magField)
{
    this->init();

    //
    // need to include mwpc gap correction, 
    // origin is located in center of gap, not on padplane --> 2 mm
    // make correction for gap distance of 4 mm
    //
    StRichLocalCoordinate
	edgeOfRichRadiator_Local(0., 0., myGeometryDb->proximityGap() +
				 myGeometryDb->quartzDimension().z() +
				 myGeometryDb->radiatorDimension().z());
    
    //
    // MIP is measured at anode position
    //
    StRichLocalCoordinate
	richAnodeWirePlane_Local(0., 0., myGeometryDb->anodeToPadSpacing());
    
    //
    // CTB in local coord.
    //
    StRichLocalCoordinate CTB_Local(0,0,240.0-214.0);

    StGlobalCoordinate edgeOfRichRadiator_Global;
    (*coordinateTransformation)(edgeOfRichRadiator_Local,edgeOfRichRadiator_Global);
    
    StGlobalCoordinate richAnodeWirePlane_Global;
    (*coordinateTransformation)(richAnodeWirePlane_Local,
				richAnodeWirePlane_Global);
    
    StGlobalCoordinate CTB_Global;
    (*coordinateTransformation)(CTB_Local,CTB_Global);

    
    StPhysicalHelixD  mHelix = tpcTrack->geometry()->helix();    

    mRadiatorGlobal.setX(edgeOfRichRadiator_Global.position().x());
    mRadiatorGlobal.setY(edgeOfRichRadiator_Global.position().y());
    mRadiatorGlobal.setZ(edgeOfRichRadiator_Global.position().z());
		     
 
    mAnodeGlobal.setX(richAnodeWirePlane_Global.position().x());
    mAnodeGlobal.setY(richAnodeWirePlane_Global.position().y());
    mAnodeGlobal.setZ(richAnodeWirePlane_Global.position().z());
		
    mCTBGlobal.setX(CTB_Global.position().x());
    mCTBGlobal.setY(CTB_Global.position().y());
    mCTBGlobal.setZ(CTB_Global.position().z());
    
    float mPathLengthAtRadiator  = mHelix.pathLength(mRadiatorGlobal,mRichNormal);
    float mPathLengthAtPadPlane  = mHelix.pathLength(mAnodeGlobal,mRichNormal);
    float mPathLengthAtCTB       = mHelix.pathLength(mCTBGlobal,mRichNormal);
    
    StThreeVectorF tpcMom;
    tpcMom = mHelix.momentumAt(mPathLengthAtRadiator,mMagneticField*kilogauss); 
    StThreeVector<double> tpcMomentum(tpcMom.x(), tpcMom.y(), tpcMom.z());
    

    // do the momentum vector rotation here
    StThreeVector<double> tempRichLocalMomentum(0,0,0);
    momentumTransformation->localMomentum(tpcMomentum,tempRichLocalMomentum);
    
    StThreeVectorF richLocalMomentum(tempRichLocalMomentum.x(),
				     tempRichLocalMomentum.y(),
				     tempRichLocalMomentum.z());

    // impact point in CTB
    StThreeVectorF localCTBImpactPoint(-999,-999,-999);
    StGlobalCoordinate CTBGlobal(mHelix.x(mPathLengthAtCTB),
				 mHelix.y(mPathLengthAtCTB),
				 mHelix.z(mPathLengthAtCTB));
    
    StRichLocalCoordinate richTransformedCTBPoint(-999,-999,-999);
    (*coordinateTransformation)(CTBGlobal,richTransformedCTBPoint);
    
    localCTBImpactPoint.setX(richTransformedCTBPoint.position().x());
    localCTBImpactPoint.setY(richTransformedCTBPoint.position().y());
    localCTBImpactPoint.setZ(richTransformedCTBPoint.position().z());
    
    
    // impact point on radiator
    StGlobalCoordinate globalImpactPoint(mHelix.x(mPathLengthAtRadiator),
					 mHelix.y(mPathLengthAtRadiator),
					 mHelix.z(mPathLengthAtRadiator));

    StRichLocalCoordinate richTransformedImpactPoint(-999,-999,-999);
    (*coordinateTransformation)(globalImpactPoint,richTransformedImpactPoint);
        
    StThreeVectorF ppMomentum;

    //
    // pad plane intersection (projected MIP)
    //
    StGlobalCoordinate    globalImpactPointAtAnodeWirePlane(-999,-999,-999);
    StRichLocalCoordinate localImpactPointAtAnodeWirePlane(-999,-999,-999);  
#if 0    
    globalImpactPointAtAnodeWirePlane.position().setX(mHelix.x(mPathLengthAtPadPlane));
    globalImpactPointAtAnodeWirePlane.position().setY(mHelix.y(mPathLengthAtPadPlane));
    globalImpactPointAtAnodeWirePlane.position().setZ(mHelix.z(mPathLengthAtPadPlane));
#else
    globalImpactPointAtAnodeWirePlane.setPosition(mHelix.at(mPathLengthAtPadPlane));
#endif    
    (*coordinateTransformation)(globalImpactPointAtAnodeWirePlane,
				localImpactPointAtAnodeWirePlane);
    
    ppMomentum =  mHelix.momentumAt(mPathLengthAtPadPlane,mMagneticField*kilogauss);
    
    
    StThreeVector<double> padPlaneMomentum(ppMomentum.x(),ppMomentum.y(),ppMomentum.z());
    StThreeVector<double> tempMomentum;
    momentumTransformation->localMomentum(padPlaneMomentum,tempMomentum);
    mMomentumAtPadPlane.setX(tempMomentum.x());
    mMomentumAtPadPlane.setY(tempMomentum.y());
    mMomentumAtPadPlane.setZ(tempMomentum.z());
    
    float lastHitDCA = mHelix.distance(tpcTrack->detectorInfo()->lastPoint());

    //
    // Topology Map (functionality should probably be put in Topology Map)
    //
    const StTrackTopologyMap& tMap = mStTrack->topologyMap();
    vector<int> rows;
    vector<int> emptyRows(1,0);
    for (int i=1; i<46; ++i) {
	if (tMap.hasHitInRow(kTpcId,i)) { rows.push_back(i);} 
	else { emptyRows.push_back(i);} 
    }
    emptyRows.push_back(46);
    
    this->setMaxGap(maxSeq(rows)); // max number of continuous empty rows
    this->setMaxChain(maxSeq(emptyRows)); // max number of continuous used rows
    this->setFirstRow(rows.front());
    this->setLastRow(rows.back());

    this->setMomentum(richLocalMomentum);
    this->setUnCorrectedMomentum(richLocalMomentum);

    //double xCorrection = 0.4*(mMomentumAtPadPlane.x()/mMomentumAtPadPlane.z());
    
    StThreeVectorF tempHit(richTransformedImpactPoint.position().x() - this->xCorrection(),
			   richTransformedImpactPoint.position().y(),
			   richTransformedImpactPoint.position().z());
    
    this->setImpactPoint(tempHit);

    //
    // Likewise the MIP position must be corrected
    // for the same effect
    //
    StThreeVectorF tempMip(localImpactPointAtAnodeWirePlane.position().x() - this->xCorrection(),
			   localImpactPointAtAnodeWirePlane.position().y(),
			   localImpactPointAtAnodeWirePlane.position().z());

    // replace with:
//     this->setProjectedMIP(tempMip);
//     this->setProjectedCTB(localCTBImpactPoint);
//     this->setPathLength(mPathLengthAtRadiator);
//     this->setLastHitDCA(lastHitDCA);
//     this->setLastHit(tpcTrack->detectorInfo()->lastPoint());
    mProjectedMIP = tempMip;
    mProjectedCTB = localCTBImpactPoint;
    mPath         = mPathLengthAtRadiator;
    mLastHitDCA   = lastHitDCA;
    mLastHit      = tpcTrack->detectorInfo()->lastPoint();
    
}

StRichTrack::~StRichTrack() {

    this->clearHits();
}

double StRichTrack::xCorrection() const {
    
    //
    // This is the radiator impact position
    // the x component must be shifted by:
    // .4*px/pz
    //
    // correction to the x position of the impact point
    // If this correction is really due to the effect of
    // the offline software cluster finder artificially
    // shifting the x position (z Global) of the track,
    // this may not be appropriate in the L3 unless we
    // use the offline hits as well
    //
    //

#ifdef myRICH_WITH_MC
    cout << "StRichTrack::xCorrection()\n";
    cout << "\t monte carlo.  Dip Angle correction turned OFF" << endl;

    return (0.0);
#else
    //return  ( 0.4*(mMomentumAtPadPlane.x()/mMomentumAtPadPlane.z()) );
    // 2000data reproduction
    return(0.0);
#endif
}

void StRichTrack::init()
{
    myGeometryDb  = StRichGeometryDb::getDb();  
    myMaterialsDb = StRichMaterialsDb::getDb();  
    coordinateTransformation = StRichCoordinateTransform::getTransform(myGeometryDb);
    momentumTransformation   = StRichMomentumTransform::getTransform(myGeometryDb);  

    mDoMomentumLoss = false;
    
    mResidualCut=3.0*centimeter;
    
    mRichNormal.setX(myGeometryDb->normalVectorToPadPlane().x());
    mRichNormal.setY(myGeometryDb->normalVectorToPadPlane().y());
    mRichNormal.setZ(myGeometryDb->normalVectorToPadPlane().z());

    mPionMass = StPionMinus::instance()->mass();
    mKaonMass = StKaonMinus::instance()->mass();
    mProtonMass = StAntiProton::instance()->mass();

    mRefit = 0;
}


void StRichTrack::calculateMomentumLoss() {
    //
    // N. Smirnov simulation (Feb 6, 2001)
    //

    double p = abs(mMomentum)/GeV;
    mPiondPdx   = (exp(5.5979-14.35*p)+16.642-1.077*p+.2146*p*p)*MeV;
    mKaondPdx   = (exp(5.985-5.6296*p)+17.343-.5007*p)*MeV;
    mProtondPdx = (exp(6.281-3.5417*p)+20.766-1.1245*p)*MeV;

//     cout << mPiondEdx/GeV << " "
// 	 << mKaondEdx/GeV << " "
// 	 << mProtondEdx/GeV << " " << endl;
}

void StRichTrack::setMomentumLoss() {

    mDoMomentumLoss = true;
    
//     //
//     // N. Smirnov simulation (Feb 6, 2001)
//     //

//     double p = abs(mMomentum)/GeV;
//     mPiondPdx   = (exp(5.5979-14.35*p)+16.642-1.077*p+.2146*p*p)*MeV;
//     mKaondPdx   = (exp(5.985-5.6296*p)+17.343-.5007*p)*MeV;
//     mProtondPdx = (exp(6.281-3.5417*p)+20.766-1.1245*p)*MeV;

// //     cout << mPiondEdx/GeV << " "
// // 	 << mKaondEdx/GeV << " "
// // 	 << mProtondEdx/GeV << " " << endl;
}

double StRichTrack::getMomentumLoss(StParticleDefinition* part) const
{
    //
    // This is called 8 times per track
    // 3 rings...
    
    double momentumLoss = 0;

    if(!mDoMomentumLoss) {
	return momentumLoss;
    }
    
    if( (part ==  StPionMinus::instance()) || (part == StPionPlus::instance()) ) {
	momentumLoss = mPiondPdx; 
    }
    else if( (part ==  StKaonMinus::instance()) || (part == StKaonPlus::instance()) ) {
	momentumLoss = mKaondPdx; 
    }
    else if( (part ==  StProton::instance()) || (part == StAntiProton::instance()) ) {
	momentumLoss = mProtondPdx; 
    }
    else {
	cout << "StRichTrack::getMomentumLoss()\n";
	cout << "\tUNKNOWN PARTICLE TYPE (" << part->pdgEncoding() << ")" << endl;
    }
    
    return momentumLoss;
}

vector<StRichRingHit*>
StRichTrack::getRingHits(StParticleDefinition* part) {

    if ( (part ==  StPionMinus::instance()) || (part == StPionPlus::instance())) {
	return mPionList; 
    }
  
    else if ( (part ==  StKaonMinus::instance()) || (part == StKaonPlus::instance())) {
	return mKaonList; 
    }
    
    else if ( (part ==  StProton::instance()) || (part == StAntiProton::instance())) {
	return mProtonList; 
    }
  
    vector<StRichRingHit*> tempvec(0);
    tempvec[0]=0;
    return tempvec;
}


void StRichTrack::clearHits() {

    size_t l;
    for(l=0; l<mPionList.size(); l++) {
	delete mPionList[l];
	mPionList[l] = 0;
    }
    
    mPionList.clear();
    mPionList.resize(0);
  
    for(l=0; l<mKaonList.size(); l++) {
	delete mKaonList[l];
	mKaonList[l] =0;
    }
    
    mKaonList.clear();
    mKaonList.resize(0);
  
    for(l=0; l<mProtonList.size(); l++) {
	delete mProtonList[l];
	mProtonList[l] = 0;
    }
    mProtonList.clear();
    mProtonList.resize(0); 
}


void StRichTrack::addHit(StRichHit* hit, double dist, double sigma, 
			 double angle, double radPath, 
			 double quaPath, StParticleDefinition* part) {
  
  if( (part ==  StPionMinus::instance()) || (part == StPionPlus::instance()) ) {
      mPionList.push_back(new StRichRingHit(hit,angle,dist,sigma,radPath,quaPath)); 
  }
  
  else if( (part ==  StKaonMinus::instance()) || (part == StKaonPlus::instance()) ) {
      mKaonList.push_back(new StRichRingHit(hit,angle,dist,sigma,radPath,quaPath)); 
  }
  
  else if( (part ==  StProton::instance()) || (part == StAntiProton::instance()) ) {
      mProtonList.push_back(new StRichRingHit(hit,angle,dist,sigma,radPath,quaPath));  
  }
}

bool StRichTrack::correctTrajectory() {

    if (!mStTrack) {
	cout << "StRichTrack::correctTrajectory()\n";
	cout << "\tWARNING:\n";
	cout << "\tLost the mStTrack" << endl;
	return false;
    }
  
    StThreeVectorF tempMip(-999,-999,-999);

    if (mAssociatedMIP) {
	tempMip = mAssociatedMIP->local();
    }
    else {
	cout << "StRichTrack::correctTrajectory()\n";
	cout << "\tWARNING\n";
	cout << "\tNo Assocatied MIP\n";
	cout << "\tSkip this one" << endl;
	return false;
    }
  
    StThreeVectorF residual = tempMip-mProjectedMIP;

    if (residual.perp() > mResidualCut) {
	cout << "StRichTrack::correctTrajectory()\n";
	cout << "\tWarning\n";
	cout << "\tResidual > " << (mResidualCut/centimeter) << " cm.\n";
	cout << "\tSkipping..." << endl;
	return false;
    }

    //
    // store original values
    //
    //   this->setUnCorrectedProjectedMIP( this->getProjectedMIP() );
    //   this->setUnCorrectedImpactPoint( this->getImpactPoint() );
    //   this->setUnCorrectedMomentum( this->getMomentum() );

    mUnCorrectedProjectedMIP = mProjectedMIP;
    mUnCorrectedImpactPoint  = mImpactPoint;
    mUnCorrectedMomentum     = mMomentum;

    //
    // set refit check flag
    //

    mRefit = true;

    //
    // get momentum, x at assumed interaction point in CTB
    //
  
    StPhysicalHelixD mHelix     = mStTrack->geometry()->helix();
    float mPathLengthToCTB      = mHelix.pathLength(mCTBGlobal,mRichNormal);
    
    //
    // lets move the parameters to somethin initialized
    // in the c'tor
    //
    
    if ( (mPathLengthToCTB>10000)  || (mPathLengthToCTB<0) ) return false;
    
    StThreeVectorF CTBPoint(mHelix.x(mPathLengthToCTB)-(this->xCorrection()),
			    mHelix.y(mPathLengthToCTB),
			    mHelix.z(mPathLengthToCTB));
    
    StThreeVectorF globalMomentum =
	mHelix.momentumAt(mPathLengthToCTB, mMagneticField*kilogauss);
    
    float initialTheta = globalMomentum.theta();
    float initialPhi   = globalMomentum.phi();
    
    float thetaRange = 5.0*degree;
    float phiRange = 10.0*degree;
    
    float thetaTrials=100.0;
    float phiTrials=100.0;
    
    float thetaStart  = initialTheta - thetaRange;
    float thetaFinish = initialTheta + thetaRange;
    
    float phiStart    = initialPhi - phiRange;
    float phiFinish   = initialPhi + phiRange;
    
    float deltaTheta = thetaRange/thetaTrials;
    float deltaPhi   = phiRange/phiTrials;
    
    StThreeVectorF correctedMomentum;
    StThreeVectorF correctedProjectedMIP;
    StThreeVectorF correctedResidual = residual;
    StThreeVectorF tempMomentum;
    
    for (float theta=thetaStart; theta<thetaFinish; theta +=deltaTheta) {
	for (float phi=phiStart; phi<phiFinish; phi+=deltaPhi) {
	    float p = globalMomentum.mag();
	    tempMomentum.setX(p*sin(theta)*cos(phi));
	    tempMomentum.setY(p*sin(theta)*sin(phi));
	    tempMomentum.setZ(p*cos(theta));
	    
	    StPhysicalHelixD tempHelix(tempMomentum,
				       CTBPoint,
				       mMagneticField*kilogauss,
				       mStTrack->geometry()->charge());
	    
	    float path = tempHelix.pathLength(mAnodeGlobal,mRichNormal);
	    
	    if (path>0 && path<10000) { 
		StGlobalCoordinate globalPadPlane(tempHelix.x(path),
						  tempHelix.y(path),
						  tempHelix.z(path));
		
		StRichLocalCoordinate localPadPlane;
		(*coordinateTransformation)(globalPadPlane,localPadPlane);
		StThreeVectorF tempLocal(localPadPlane.position().x(),
					 localPadPlane.position().y(),
					 localPadPlane.position().z());
		
		StThreeVectorF tempResidual = tempLocal - tempMip;
		
		if ( tempResidual.perp() < correctedResidual.perp() ) {
		    correctedMomentum     = tempMomentum;
		    correctedResidual     = tempResidual;
		    correctedProjectedMIP = tempLocal;
		}
		
	    } // path cut
	    
	} // global momentum

    } // phi

  
    if(correctedMomentum.mag()>0) {
      // impact point on radiator
	StPhysicalHelixD tempHelix(correctedMomentum,
				   CTBPoint,
				   mMagneticField*kilogauss,
				   mStTrack->geometry()->charge());
	float path = tempHelix.pathLength(mRadiatorGlobal,mRichNormal);
  
	if (path>0 && path<10000) {
	    StGlobalCoordinate globalImpactPoint(tempHelix.x(path),
						 tempHelix.y(path),
						 tempHelix.z(path));
	    StRichLocalCoordinate richTransformedImpactPoint;
	    (*coordinateTransformation)(globalImpactPoint,richTransformedImpactPoint);
	    
	    StThreeVectorF impact(richTransformedImpactPoint.position().x(),
				  richTransformedImpactPoint.position().y(),
				  richTransformedImpactPoint.position().z());
      
	    // do the momentum vector rotation here
	    StThreeVector<double> tempRichLocalMom;
	    StThreeVector<double> tempRichGlobalMom(correctedMomentum.x(),
						    correctedMomentum.y(),
						    correctedMomentum.z());
	    
	    momentumTransformation->localMomentum(tempRichGlobalMom,tempRichLocalMom);
	    StThreeVectorF tempRichLocalMomentum(tempRichLocalMom.x(),
						 tempRichLocalMom.y(),
						 tempRichLocalMom.z());  
	    //StThreeVectorF normalVector(0,0,-1);
	    
	    
// 	    cout << "TrajectoryCorrection: original impact point: " 
// 		 << mImpactPoint << endl;
// 	    cout << "TrajectoryCorrection: new  impact point:     " 
// 		 << impact << endl;
	    
	    //this->setProjectedMIP(correctedProjectedMIP); -- replace with:
	    mProjectedMIP = correctedProjectedMIP;
	    //this->setImpactPoint(impact); -- replace with:
	    mImpactPoint = impact;
	}
    }
    
    //this->setCorrectedMomentum(correctedMomentum);
    return true;
}

void StRichTrack::assignMIP(const StSPtrVecRichHit* hits) {
    
    //
    // proximity matching between TPC track's predicted MIP and 
    // RICH pad plane MIP 
    //
    
    double smallestResidual=10.e10;
    double testThisResidual=0;

    if (!hits) {
	cout << "StRichTrack::assignMIP()\n";
	cout << "\tHits were not passed properly\n";
	cout << "\tReturning" << endl;
	return;
    }

    vector<candidate> candidateHits;
    
    StSPtrVecRichHitConstIterator hitIndex;
    for (hitIndex = hits->begin(); hitIndex != hits->end(); hitIndex++) { 
	testThisResidual = ((*hitIndex)->local() - mProjectedMIP).perp();      

	if(testThisResidual>5.*centimeter) continue;
	
	if(testThisResidual < 2.*centimeter)
	    candidateHits.push_back( candidate((*hitIndex), testThisResidual) );

	if (testThisResidual<smallestResidual) {
	    smallestResidual = testThisResidual;   
	    mAssociatedMIP   = *hitIndex;
	}
	
    }
    
    //
    // if there is more than 1 candidate
    // take the one with the highest charge
    //
    
    if(candidateHits.size()>1) {
 	cout << "StRichTrack::associateMIP()\n";
 	cout << "\tMore than 1 hit *CAN BE* associated ";
	cout << "(" << candidateHits.size() << ")\n";
 	cout << "\tTake highest amplitude with smallest residual\n";

	//
	// sort from smallest residual to highest
	// see operator< defined at the top of this file
	//
	sort(candidateHits.begin(), candidateHits.end());

	double highestAmplitude = 0;
	for(size_t ii=0; ii<candidateHits.size(); ii++) {

	    if( candidateHits[ii].first->isSet(eMip) ) {
		mAssociatedMIP = candidateHits[ii].first;
		highestAmplitude = candidateHits[ii].first->charge();
		break;
	    }
	}
	
	if(!mAssociatedMIP) {
	    //cout << "StRichTrack::assignMIP()\n";
	    //cout << "\tTake smallest Residual" << endl;
	    mAssociatedMIP = candidateHits[0].first;
	    highestAmplitude = candidateHits[0].first->charge();
	}
	    
    }
    //
    // set the associated MIP
    // In the best case the eMip and eAssociatedMip flag should be set
    //

    if(mAssociatedMIP) {
	mAssociatedMIP->setBit(eAssociatedMip);
    }
    else {
	cout << "StRichTrack::associateMIP()\n";
	cout << "\tWARNING no assocatied MIP" << endl;
    }

}

double StRichTrack::getZVertex(){
#ifdef RICH_WITH_L3_TRACKS
    globalTrack* track = this->getL3Track();
    if(track)
	return track->z0-track->tanl*track->r0*cos(track->psi-track->phi0);
#endif
    return -999;
}

int StRichTrack::maxSeq(vector<int>& seqrows) {
    vector<int> diffs(seqrows.size());
    adjacent_difference(seqrows.begin(), seqrows.end(), diffs.begin());
    vector<int>::iterator max = max_element(diffs.begin()+1, diffs.end());
    return max!=diffs.end() ? *max : -999;
}

bool StRichTrack::fastEnough(StParticleDefinition* particle) {

    bool status = false;

    //
    // Remember the momentum loss in the momentum calculation
    //
    
    double p = mMomentum.mag() - this->getMomentumLoss(particle);
    double m = particle->mass();
    
    double indexOfRefraction1 = 
	myMaterialsDb->indexOfRefractionOfC6F14At(myMaterialsDb->innerWavelength());
    double indexOfRefraction2 = 
	myMaterialsDb->indexOfRefractionOfC6F14At(myMaterialsDb->outerWavelength());
    
    //cout << "inner = " << indexOfRefraction1 << endl;
    //cout << "outer = " << indexOfRefraction2 << endl;

    if ( p/::sqrt(p*p + m*m) > (1./indexOfRefraction1) && 
	 p/::sqrt(p*p + m*m) > (1./indexOfRefraction2)) { 
	status = true;
    }
    
    return status;
}

void StRichTrack::useUnCorrected() {

    this->setMomentum(mUnCorrectedMomentum);

    //this->setImpactPoint(mUnCorrectedImpactPoint);  -- replace with:
    mImpactPoint = mUnCorrectedImpactPoint;
    //this->setProjectedMIP(mUnCorrectedProjectedMIP);  -- replace with:
    mProjectedMIP = mUnCorrectedProjectedMIP;
}

void StRichTrack::setMomentum(StThreeVectorF& momentum) {

    mMomentum = momentum;
    StThreeVectorF normalVector(0,0,-1);

    if(mMomentum.mag()) {
	//this->setTheta(acos(normalVector.dot(momentum)/momentum.mag()));
	mTheta = acos(normalVector.dot(momentum)/momentum.mag());
    }
    if(momentum.y() == 0 && momentum.x() == 0) {
	//this->setPhi(0.0);
	mPhi = (0.0);
    }
    else {
	//setPhi(momentum.phi());
	mPhi = momentum.phi();
    }

    this->calculateMomentumLoss();
}

bool StRichTrack::isGood(StParticleDefinition* particle) {   

    StRichRingPoint OuterRing(this,eOuterRing);
    OuterRing.setParticleType(particle);
    StThreeVectorF otemp;

    //
    // See If valid point at back of ring
    // cut:   changed from pi ----> pi/2 
    //            very back         at least half ring is ok
    //
    
    bool goodTrack = OuterRing.getPoint(M_PI/2.0,otemp);

    if(!goodTrack) {
	cout << "StRichTrack::isGood()\n";
	cout << "\tWARNING:\n";
	cout << "\ttrack has ring refracted away at 90 degrees" << endl;
    }
    
    return goodTrack;
}

double StRichTrack::getExpectedNPhots(StParticleDefinition* particle) {
  
  double pathlengthInc6f14 = myGeometryDb->radiatorDimension().z()*cos(mTheta);
  double pathlengthInquartz = myGeometryDb->quartzDimension().z()*cos(mTheta);
  double pathlengthInmeth = myGeometryDb->proximityGap()*cos(mTheta);

  double particleBeta = mMomentum.mag()
    /::sqrt(particle->mass()*particle->mass() + mMomentum.mag()*mMomentum.mag());
  
  
  double startLamda = 170.0;
  double endLamda   = 200.0;
  double nSamples   = 200;
  double deltaLamda = (endLamda-startLamda)/nSamples;
 
  double nphots=0;
  double rawNumber=0;
  double c6f14AbsCorrect=0;
  double quartzAbsCorrect=0;
  double methAbsCorrect=0;
  double csiQE = 0;
  
  for (double i=startLamda;i<endLamda;i=i+deltaLamda) {
    
      rawNumber = (1.0/(i*i*i))*(1.0 - 1.0/(particleBeta*particleBeta*myMaterialsDb->indexOfRefractionOfC6F14At(i*nanometer)*myMaterialsDb->indexOfRefractionOfC6F14At(i*nanometer)));
      
      c6f14AbsCorrect  = (exp(-(pathlengthInc6f14/myMaterialsDb->absorptionCoefficientOfC6F14At(i*nanometer))));

      quartzAbsCorrect = (exp(-(pathlengthInquartz/myMaterialsDb->absorptionCoefficientOfQuartzAt(i*nanometer))));

      methAbsCorrect   = (exp(-(pathlengthInmeth/myMaterialsDb->absorptionCoefficientOfMethaneAt(i*nanometer))));

      csiQE  = myMaterialsDb->quantumEfficiencyOfCsIAt(i*nanometer);

      nphots = nphots + rawNumber*c6f14AbsCorrect*quartzAbsCorrect*methAbsCorrect*csiQE;
  }
  
  return nphots/6.05e-9;
}
