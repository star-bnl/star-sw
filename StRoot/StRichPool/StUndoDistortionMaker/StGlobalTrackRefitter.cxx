//StGlobalTrackRefitter.cxx
//M.L. Miller (Yale Software)
//04/01
//modified by bum.

#include <Stiostream.h>
#include <math.h>
#include <assert.h>

// SCL
#include "StFastCircleFitter.hh"
//#include "SystemOfUnits.h"
#include "PhysicalConstants.h"

// StEvent
#include "StEventTypes.h"
#include "StMcEventTypes.hh"
#include "StParticleDefinition.hh"

// MLM
#include "StFastLineFitter.h"
#include "StGlobalTrackRefitter.h"

#include "StTptCircleFitter.h"

#include "TRandom.h"


inline bool sortInc(StTpcHit* hita, StTpcHit* hitb)
{
  return hita->position().perp() < hitb->position().perp();
}

inline bool sortMcInc(StMcTpcHit* hita, StMcTpcHit* hitb)
{
  return hita->position().perp() < hitb->position().perp();
}

inline bool sortPairInc(HITPAIR a, HITPAIR b)
{
  return a.first->perp()< b.first->perp();
}

ClassImp(StGlobalTrackRefitter)

StGlobalTrackRefitter::StGlobalTrackRefitter() 
    : mMinPadrow(0), mMaxPadrow(1000), 
  mDoError(false), mDoSameError(false),
  mDoFlagFilter(false), mDoFitFilter(true),  mUseTptCircleFitter(true), 
  mDebug(0),mBField(.25*tesla)
						   
{
    mCircleFitter = new StFastCircleFitter();
    mLineFitter = new StFastLineFitter();
    mTptCircleFitter = new StTptCircleFitter();
    mRefitHelix = new StPhysicalHelixD();

    const int nHit = 100; // for safety.  only 45+(1) hits
    mErrorArray = new StThreeVectorF[nHit];

}

StGlobalTrackRefitter::~StGlobalTrackRefitter()
{
    delete mCircleFitter;
    mCircleFitter=0;
    delete mLineFitter;
    mLineFitter=0;
    delete mRefitHelix;
    mRefitHelix=0;
    delete mTptCircleFitter;

    delete[] mErrorArray;
}

/*
  clear all the fit classes and the vector which stores the hits
*/

void
StGlobalTrackRefitter::clear()
{
  mTptCircleFitter->clear();
  mCircleFitter->clear();
  mLineFitter->clear();
  mVecHitPair.clear();
  
}

inline void
StGlobalTrackRefitter::setBField(Float_t val)
{
  mBField = val*tesla;
}

/*
  helix fit for rc tracks
*/

const StPhysicalHelixD* 
StGlobalTrackRefitter::refit(StTrack* track)
{
  bool hitsOk = fillRcHits(track);
  if(!hitsOk) return 0;
     
  // no change of direction? this seems dangerous.
  //
  const StPhysicalHelixD& prehelix = track->geometry()->helix();

  if(!doHelixRefit(prehelix.h())) return 0; 
  
  if(mDebug){
    cout << "gl pt=" << track->geometry()->momentum().perp()
	 << ", charge=" << track->geometry()->charge() << endl
	 <<"Prehelix: "<<prehelix<<endl
	 <<"Fithelix: "<<*mRefitHelix<<endl;
  }
  
  return mRefitHelix;
}  

const StPhysicalHelixD* 
StGlobalTrackRefitter::refitNoFill(StTrack* track)
{
//  bool hitsOk = fillRcHits(track);
//  if(!hitsOk) return 0;
    mTptCircleFitter->clear();
    mCircleFitter->clear();
    mLineFitter->clear();

  // no change of direction? this seems dangerous.
  //
  const StPhysicalHelixD& prehelix = track->geometry()->helix();

  if(!doHelixRefit(prehelix.h())) return 0; 
  
  if(mDebug){
    cout << "gl pt=" << track->geometry()->momentum().perp()
	 << ", charge=" << track->geometry()->charge() << endl
	 <<"Prehelix: "<<prehelix<<endl
	 <<"Fithelix: "<<*mRefitHelix<<endl;
  }
  
  return mRefitHelix;
}  

/*
  helix fit for mc tracks 
 */
const StPhysicalHelixD* 
StGlobalTrackRefitter::refit(StMcTrack* track)
{
  bool hitsOk = fillMcHits(track);
  if(!hitsOk) return 0;

  double h = (mBField*track->particleDefinition()->charge()>0) ? -1: 1;

  return doHelixRefit(h);

}  

/*
  line fit for rc tracks
 */

const StPhysicalHelixD* 
StGlobalTrackRefitter::refitLine(StTrack* track)
{
  bool hitsOk = fillRcHits(track);
  if(!hitsOk) return 0;

  return doLineRefit();

}  

/*
  line fit for mc track
 */

const StPhysicalHelixD*
StGlobalTrackRefitter::refitLine(StMcTrack* track)
{
  bool hitsOk = fillMcHits(track);
  if(!hitsOk) return 0;
  return doLineRefit();

}

/*
  fills the hit vector with rc hits and error.
  returns false if something's wrong.
 */

void 
StGlobalTrackRefitter::addPoint(StThreeVectorF& pos,StThreeVectorF& err) 
{
    mVecHitPair.push_back(HITPAIR(&pos,&err));
    sort(mVecHitPair.begin(),mVecHitPair.end(),sortPairInc);
}

    
bool
StGlobalTrackRefitter::fillRcHits(StTrack *track)
{

  const char* method = "StGlobalTrackRefitter::fillRcHits";

  double rndm=999; 
  double debugValue = 1.;
  if(mDebug>1) rndm = gRandom->Rndm();
  bool debug = (mDebug>1&&rndm<debugValue);
  bool debugPlus = (mDebug>2&&rndm<debugValue);


  const StPtrVecHit& vec = track->detectorInfo()->hits(kTpcId);
  if (vec.size()==0) return false; //No hits in TPC
  
  clear(); // all classes and hits vector
  
  int nUsedInFit(0),nAllHit(0),nUsedInRefit(0);
  int nStEventFitHit = track->fitTraits().numberOfFitPoints(kTpcId);
  int nStEventAllHit = track->detectorInfo()->numberOfPoints(kTpcId);

  if(debug){
    cout<<"----------------------------------------------------" << endl; 
    cout << method << endl;
  }
  if(debugPlus)
    cout << "\toriginal hits: "<< endl;

  for (unsigned int i=0; i<vec.size(); i++){
    StTpcHit* hit = dynamic_cast<StTpcHit*>(vec[i]);
    if(!hit){
      cout << "\t##not a tpc hit?" << endl;
      continue;
    }
    nAllHit++;
    if(hit->usedInFit()) nUsedInFit++;
    if(debugPlus){
      cout << "\t" << i+1 << " r=" << hit->position().perp()
	   << ", row=" << hit->padrow() 
	   << ", sec=" << hit->sector()
	   << ", flag=" << hit->flag() 
	   << endl 
	   << "\t\tpostion=" << "(" << hit->position() << ")"
	   << endl	
	   << "\t\terr=" << "(" << hit->positionError() << ")"
	   << ", fit?=" << (hit->usedInFit() ? "y" : "n") ;
    }
    if (filter(hit)) {
      if(debugPlus) cout << ", Refit" << endl;
      nUsedInRefit++;
      StThreeVectorF* error = hitError(hit);
      mVecHitPair.push_back(HITPAIR(&hit->position(),error));
    }
    else{
      if(debugPlus) cout << endl;
    }
  }
  
  
  sort(mVecHitPair.begin(),mVecHitPair.end(),sortPairInc);
  
  if(debugPlus){
    cout << "here are the sorted hits: " << endl;
    for(unsigned int i=0; i<mVecHitPair.size(); i++){
      cout << "r: " << mVecHitPair[i].first->perp() 
	   << ", position=(" << *mVecHitPair[i].first << ")"
	   << "\n\terror=" << "(" << *mVecHitPair[i].second <<")"
	   <<endl;
    }
  }
    
  if(debug){
    cout <<  endl
	 << ">>>>\tstevent fit hits : " << nStEventFitHit 
	 << ", stevent all hits : " << nStEventAllHit << endl  
	 << ">>>>\tcounted fit hits: " << nUsedInFit 
	 << ", counted all hits: " << nAllHit
	 << ", refit hits: " << nUsedInRefit  << endl << endl;
      
  }

  return true;
}

/*
  fills the vector with mc hits.
  returns false if something's wrong.
*/

bool
StGlobalTrackRefitter::fillMcHits(StMcTrack* track)
{

  const char* method = "StGlobalTrackRefitter::fillMcHits()";
  
  const StPtrVecMcTpcHit& vec = track->tpcHits();
  if(!vec.size()) return false;

  clear();

  //sort (i think the mc hits are already sorted, but...)

  for (unsigned int i=0; i<vec.size(); i++){
    StMcTpcHit* hit = dynamic_cast<StMcTpcHit*>(vec[i]);
    if(!hit){
      cout << method << "\t##not a tpc hit?" << endl;
      continue;
    }
    // equal error of 1
    if (filter(hit)){
      StThreeVectorF* error = hitError(hit);
      mVecHitPair.push_back(HITPAIR(&hit->position(),error));
    }
  }
    
  sort(mVecHitPair.begin(),mVecHitPair.end(),sortPairInc);

  return true;
}

/*
  fit to helix and returns a (member) helix pointer.
 */

const StPhysicalHelixD*
StGlobalTrackRefitter::doHelixRefit(double h)
{
  const char* method = "StGlobalTrackRefitter::doHelixRefit()";

  //Do circle fit in x-y
  // do fit inside out.  i dont think it matters.
  
  int nHit = mVecHitPair.size();

  if(nHit<=3){
    if(mDebug) 
      cout << method << endl 
	   << "\tnot enough hits to do refit : " << nHit << endl;
    return 0;
  }

  const StThreeVectorF& firstHit = *mVecHitPair[0].first;

  for(int i=0; i<nHit; i++){
    const StThreeVectorF& position = *mVecHitPair[i].first;

    float error = mVecHitPair[i].second->perp();

    if(mUseTptCircleFitter){
      mTptCircleFitter->addPoint(position.x(), position.y(),error);
    }
    else{
      mCircleFitter->addPoint(position.x(),position.y());
    }
  } 

  bool circle_rc = 0;
  if(mUseTptCircleFitter){
    circle_rc = mTptCircleFitter->fit();
  }
  else{
    circle_rc = mCircleFitter->fit();
  }
  
  if (!circle_rc) {
    if(mDebug){
      cout << method << endl
	   << "\tcircle fitter crapped out with return code "
	   << mCircleFitter->rc() << endl
	   << "\tfit points=" << nHit << endl;
    }
    return 0;
  }
  
  //
  // reset h if using the tpt fitter
  // note: StPhysicalHelixD defines h=(q*B<0)? 1: -1
  //
  //  if(mUseTptCircleFitter)
  //   h = (mBField*mTptCircleFitter->sign()<0) ? 1 : -1;

  double radius(0),ycenter(0),xcenter(0);
  
  if(mUseTptCircleFitter){
    radius = mTptCircleFitter->radius();
    ycenter = mTptCircleFitter->ycenter();
    xcenter = mTptCircleFitter->xcenter();
  }
  else{
    radius = mCircleFitter->radius();
    ycenter = mCircleFitter->ycenter();
    xcenter = mCircleFitter->xcenter();
  }
  
  //Do line fit in s-z, where s is the 2-d pathlength along circle
  // line fit done inside out...

  double s2d=0.;
  
  for(int i=0; i<nHit; i++){
    const StThreeVectorF& position = *mVecHitPair[i].first;
    float zError = mVecHitPair[i].second->z();
    float weight = (zError) ? 1./zError : 1.;
    s2d = pathlength(radius, xcenter,
		     ycenter, firstHit, position);
    mLineFitter->addPoint(s2d, position.z(), weight); 
  }
  
  bool line_rc = mLineFitter->fit();
  if (!line_rc){
    if(mDebug){
      cout << method << endl 
	   << "\tline fitter crapped out" << endl
	   << "\tfit points=" << nHit << endl;
    }
    return 0;
  }
  //I'll list all of these to be explicit
 
  double curvature = 1./radius;
  double dipangle = atan( mLineFitter->slope() );
  double phi0 = atan2( (firstHit.y()-ycenter),
		       (firstHit.x()-xcenter) );
  double x0 = radius*cos(phi0) + xcenter;
  double y0 = radius*sin(phi0) + ycenter;
  double z0 = mLineFitter->intercept();
  StThreeVectorD origin(x0, y0, z0);
  double phase = phi0; //
  
  mRefitHelix->setParameters( curvature, dipangle, phase, origin, (int)h);
  mRefitPoints = mLineFitter->numberOfPoints();

  if(mRefitPoints!=nHit){
    if(mDebug)
      cout << method << endl
	   << "\thelix fit: DIFFERENT NUMBER OF POINTS?" << endl
	   << "refit : " << mRefitPoints 
	   << ",vector: " << mVecHitPair.size() << endl;
  }

  return mRefitHelix;
}

/*
  fit to straight line.
  also returns a pointer to a helix class.
  note... 
  for StPhysicalHelixD the convention for a straight line fit is:
  1. curvature=0.
  2. h=1. so psi = phase+hpi/2 
  
 */

const StPhysicalHelixD*
StGlobalTrackRefitter::doLineRefit()
{
  const char* method = "StGlobalTrackRefitter::doLineRefit()";

  int nHit = mVecHitPair.size();
  
  if(nHit<=3){
    if(mDebug) 
      cout << method << endl
	   << "\tnot enough hits to do refit : " << nHit << endl;
    return 0;
  }

  const StThreeVectorF& firstHit = *mVecHitPair[0].first;
  const StThreeVectorF& lastHit = *mVecHitPair[nHit-1].first;
  
  // first guess.
  // rotate the coordinate by the approximate slope.
  // i think the fits are more accurate when the slope is small.

  double offset = 0.;
  double dY = (lastHit-firstHit).y();
  double dX = (lastHit-firstHit).x();
  double rotAngle = atan2(dY,dX)+offset;
  double cosRot   = cos(rotAngle);
  double sinRot   = sin(rotAngle);

  //
  // fit in xy plane
  //
  
  for(int i=0; i<nHit; i++){
    const StThreeVectorF& position = *mVecHitPair[i].first;
    // just using padrow errors for simplicity.
    float padError = mVecHitPair[i].second->perp(); 
    float weight = (padError) ? 1./padError : 1.;
    float posx   = cosRot*position.x() + sinRot*position.y();
    float posy   = -sinRot*position.x() + cosRot*position.y();
    mLineFitter->addPoint(posx, posy, weight);
  } 
  bool rc = mLineFitter->fit();

  if (!rc) {
    if(mDebug){
      cout << method << endl
	   << "\tline fit in xy crapped" << endl
	   << "\tfit points=" << nHit << endl;
    }
    return 0;
  }

  // save some info now since we're reusing the line fitter for s-z
  // be explicit for clarity.  
  // we need to be careful about how we determine the 'phase'
  // note that the intercept and slope are from the rotated system.
  double yintercept = mLineFitter->intercept();
  double xydip       = atan(mLineFitter->slope());

  // psi is the direction the track is pointing.
  // phase by convention for straight lines in StHelix is psi-hpi/2.
  //

  // a first stab at the phase.  may be off by pi.
  double phase = xydip + rotAngle - pi/2.;

  if(fabs(phase) > pi) phase = atan2(sin(phase),cos(phase));

  // find the point on the line closest to the first hit in the xy plane.
  // this will be the origin in the xy plane.
  // pseudoOrigin is just a known point on the line
  // (in the original coordinate system)
  double pseudoOriginX = -sinRot*yintercept;
  double pseudoOriginY = cosRot*yintercept;

  StThreeVectorF atDca = lineAt2D(phase,pseudoOriginX,
				  pseudoOriginY,firstHit);
  

  // reality check.  is atDca really on the line?
  if(mDebug>2){
    double slope = tan(xydip+rotAngle);
    double checky = (atDca.x()-pseudoOriginX)*slope + pseudoOriginY;

    StThreeVectorF p = atDca-firstHit;
    double cosTheta = p.x()*firstHit.x()+p.y()*firstHit.y();
    cosTheta /= (p.perp()*firstHit.perp());
    double theta = (180./pi)*acos(cosTheta);
    cout << "-------------------------------------" << endl
	 << method << endl
	 << "first hit position=(" << firstHit << ")"<< endl
	 << "at dca =(" << atDca << ")" << endl
	 << "y according to equation= " << checky << endl
	 << "angle b/ perp and first hit=" << theta << endl
	 << "psi (in degrees)= " << (xydip+rotAngle)*180./pi << endl;
  }

  // find the pathlength between the first and last hit.
  // if this is negative, our phase is off by pi.

  double path = pathlength(phase,atDca.x(),atDca.y(),lastHit);
  if(path<0){
    phase += pi;
    if(fabs(phase)>pi) phase = atan2(sin(phase),cos(phase));
  }

  //
  // Do line fit in s-z,
  //
  mLineFitter->clear();
  
  for(int i=0; i<nHit; i++){
    const StThreeVectorF& position = *mVecHitPair[i].first;
    float zError = mVecHitPair[i].second->z();
    float weight = (zError) ? 1./zError : 1.;
    float s = pathlength(phase,atDca.x(),atDca.y(),position);
    mLineFitter->addPoint(s, position.z(), weight); 
  }
  
  rc = mLineFitter->fit();
  if (!rc){
    if(mDebug){
      cout << method << endl 
	   << "\tline fitter crapped out" << endl
	   << "\tfit points=" << nHit << endl;
    }
    return 0;
  }

  // 'helix' parameters for straight lines.
  // let's be explicit.
  double zintercept = mLineFitter->intercept();
  double rzslope = mLineFitter->slope();

  double curvature = 0;
  double h = 1; // by convention
  double dipangle = atan(rzslope);
  StThreeVectorD origin(atDca.x(),atDca.y(),zintercept);

  mRefitHelix->setParameters( curvature, dipangle, phase, origin, (int)h);
  mRefitPoints = mLineFitter->numberOfPoints();

  return mRefitHelix;
}

/*
  pathlength along the circle
 */

double
StGlobalTrackRefitter::pathlength(double radius, 
				  double xcenter, double ycenter,
				  const StThreeVectorF& hit1, 
				  const StThreeVectorF& hit2)
{
  double phi1 = atan2( (hit1.y()-ycenter), (hit1.x()-xcenter) );
  double phi2 = atan2( (hit2.y()-ycenter), (hit2.x()-xcenter) );
  return fabs(radius*(phi2-phi1));
}


bool 
StGlobalTrackRefitter::filter(const StHit* hit) const
{
    return (flagFilter(hit) && fitFilter(hit) && padrowFilter(hit) );
}

bool 
StGlobalTrackRefitter::filter(const StMcHit* hit) const
{
  return padrowFilter(hit);
}

bool 
StGlobalTrackRefitter::flagFilter(const StHit* hit) const
{
    return (!mDoFlagFilter) ? true : (hit->flag()==0);
}

bool 
StGlobalTrackRefitter::fitFilter(const StHit* hit) const
{
    return (!mDoFitFilter) ? true : (hit->usedInFit());
}

// INCLUDES min padrow and max padrow

bool 
StGlobalTrackRefitter::padrowFilter(const StHit* hit) const
{
   const StTpcHit* tpcHit = dynamic_cast<const StTpcHit*>(hit);
   if(!tpcHit) return true;
   if(mDoError) return true;
   return (tpcHit->padrow()>=mMinPadrow && tpcHit->padrow()<=mMaxPadrow);
}

bool 
StGlobalTrackRefitter::padrowFilter(const StMcHit* hit) const
{
  const StMcTpcHit* tpcHit = dynamic_cast<const StMcTpcHit*>(hit);
  if(!tpcHit) return true;
  if(mDoError) return true;
  return (tpcHit->padrow()>=mMinPadrow && tpcHit->padrow()<=mMaxPadrow);
}

StThreeVectorF*
StGlobalTrackRefitter::hitError(const StHit* hit)
{
  const StTpcHit* tpcHit = dynamic_cast<const StTpcHit*>(hit);
  StThreeVectorF* error = &mErrorArray[mVecHitPair.size()];
  
  if(!tpcHit){
    if(mDebug) 
      cout << "StGlobalTrackRefitter::hitError" << endl
	   << "\tError : not a tpc hit?" << endl;
    error->setX(.1); error->setY(.1); error->setZ(.1);
  }  
  else if(mDoError&&
	  (tpcHit->padrow()<mMinPadrow || tpcHit->padrow()>mMaxPadrow)){
    error->setX(mPadrowError); error->setY(0); error->setZ(mZError);
  }
  else if(mDoSameError &&
	  tpcHit->padrow()>=mMinPadrow && tpcHit->padrow()<=mMaxPadrow){
    error->setX(1./::sqrt(2)); error->setY(1./::sqrt(2)); error->setZ(1);
  }
  else{
    *error = tpcHit->positionError();
  }
  return error;

}

StThreeVectorF*
StGlobalTrackRefitter::hitError(const StMcHit* hit)
{
  const StMcTpcHit* tpcHit = dynamic_cast<const StMcTpcHit*>(hit);
  StThreeVectorF* error = &mErrorArray[mVecHitPair.size()];

  if(!tpcHit){
    if(mDebug) cout << "Error : not a tpc hit?" << endl;
    error->setX(.1); error->setY(.1); error->setZ(.1);
  }  
  else if(mDoError&&
     (tpcHit->padrow()<mMinPadrow || tpcHit->padrow()>mMaxPadrow)){
    error->setX(mPadrowError); error->setY(0); error->setZ(mZError);
  }
  else{
    error->setX(1./::sqrt(2)); error->setY(1./::sqrt(2)); error->setZ(1.);
  }
  return error;
}

StThreeVectorF 
StGlobalTrackRefitter::lineAt2D(double phase, double xorigin,double yorigin,
				 const StThreeVectorF& p)
{
  double s = pathlength(phase,xorigin,yorigin,p);
  double x = xorigin -s*sin(phase);
  double y = yorigin + s*cos(phase);

  StThreeVectorF vec(x,y,0);
  
  return vec;
}
/*
  pathlength along line in xy plane.
*/
double
StGlobalTrackRefitter::pathlength(double phase,double xorigin,double yorigin,
				  const StThreeVectorF& p)
{
  double dx = p.x() - xorigin;
  double dy = p.y() - yorigin;

  return cos(phase)*dy-sin(phase)*dx;

}
