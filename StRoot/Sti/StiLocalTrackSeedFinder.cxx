///\file StiLocalTrackSeedFinder.cxx 
///\author M.L. Miller (Yale Software) 10/01
#include <stdexcept>
#include <math.h>
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StThreeVector.hh"
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiKalmanTrack.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiDetectorContainer.h"
#include "StiLocalTrackSeedFinder.h"
#include "StiSortedHitIterator.h"
#include "StiToolkit.h"
#include "Sti/Base/MessageType.h"
#include "Sti/Base/Messenger.h"
#include "Sti/Base/EditableParameter.h"
#include "StiMasterDetectorBuilder.h"

ostream& operator<<(ostream&, const StiDetector&);

StiLocalTrackSeedFinder::StiLocalTrackSeedFinder(const string& name,
						 Factory<StiKalmanTrack> * trackFactory,
						 StiHitContainer         * hitContainer,
						 StiDetectorContainer    * detectorContainer)
  : StiTrackSeedFinder(name,trackFactory,hitContainer,detectorContainer)
{
  _messenger <<"StiLocalTrackSeedFinder::StiLocalTrackSeedFinder() - INFO - Started"<<endl;
  initialize();
  _messenger <<"StiLocalTrackSeedFinder::StiLocalTrackSeedFinder() - INFO - Done"<<endl;
}

StiLocalTrackSeedFinder::~StiLocalTrackSeedFinder()
{
  _messenger <<"StiLocalTrackSeedFinder::~StiLocalTrackSeedFinder()"<<endl;
}

void StiLocalTrackSeedFinder::initialize()
{
	_messenger << "StiLocalTrackSeedFinder::initialize() -I- Started" << endl;
  Factory<EditableParameter> * f = StiToolkit::instance()->getParameterFactory();
  if (!f)
    {
      cout << "StiLocalTrackSeedFinder::initialize() -F- Parameter factory is null" << endl;
      throw logic_error("StiLocalTrackSeedFinder::initialize() -F- Parameter factory is null");
    }
  add(f->getInstance()->set("DeltaY",    "Delta-Y",        &mDeltaY,        4., 0.5, 20., 0.1, 0));
  add(f->getInstance()->set("DeltaZ",     "Delta-Z",       &mDeltaZ,       10., 0.5, 20., 0.1, 0));
  add(f->getInstance()->set("SeedLength", "Seed Length",   &mSeedLength,    2,  2, 6, 1, 0));
  add(f->getInstance()->set("extraDeltaY","extra-Delta-Y", &mExtrapDeltaY, 1., 0.5, 10., 0.1, 0));
  add(f->getInstance()->set("extraDeltaZ","extra-Delta-Z", &mExtrapDeltaZ, 2., 0.5, 10., 0.1, 0));

  add(f->getInstance()->set("MaxSkipped","Max Layers Skipped",  &mMaxSkipped, 2, 0, 5, 1, 0));
  add(f->getInstance()->set("ExtrapMinLength","Min Length of Extrapolation", &mExtrapMinLength , 4, 1, 10, 1, 0));
  add(f->getInstance()->set("ExtrapMaxLength","Max Length of Extrapolation", &mExtrapMaxLength,  5, 1, 10, 1, 0));
  add(f->getInstance()->set("UseOrigin","Use Origin in Fit", &mUseOrigin, true, 0));
  add(f->getInstance()->set("DoHelixFit","Do Helix Fit",  &mDoHelixFit, true, 0));

  StiMasterDetectorBuilder * builder = StiToolkit::instance()->getDetectorBuilder();
  if (!builder)
    throw runtime_error("StiCompositeSeedFinder::build() -F- builder==0 ");
  for (unsigned int row=0;row<builder->getNRows();row++)
    {
      for (unsigned int sector=0;sector<builder->getNSectors(row);sector++)
	{
	  StiDetector * detector = builder->getDetector(row,sector);
	  if (!detector)
	    {
	      cout << "StiCompositeSeedFinder::build() row:"<<row<<" sector:"<<sector<<" ERROR" << endl;
	      throw runtime_error("StiCompositeSeedFinder::build() -F- detector==0 ");
	    }
	  addLayer(detector);
	}
    }
  _messenger << "StiLocalTrackSeedFinder::initialize() -I- Done" << endl;
}

/// Produce the next track seed 
/// Loop through available hits and attempt to form a track seed
/// Only use hits that have not been already used in fully formed
/// tracks. 
StiKalmanTrack* StiLocalTrackSeedFinder::next()
{
  //_messenger <<"StiLocalTrackSeedFinder::next() -I- Started"<<endl;
  StiKalmanTrack* track = 0;
  while (_hitIter!=end() && track==0)
    {
      try 
	{
	  if ( (*_hitIter).timesUsed()==0 ) 
	    {
	      track = makeTrack(&*_hitIter);
	    } 

	}
      catch(runtime_error & rte )
	{
	  _messenger<< "StiLocalTrackSeedFinder::next() -W- Run Time Error :" << rte.what() << endl;
	}
      ++_hitIter;
    }
  //_messenger <<"StiLocalTrackSeedFinder::next() -I- Done"<<endl;
  return track;
}

/// Extend the track seed starting from the given hit.
/// The extension proceeds from the outside-in. It stops
/// whenever the inner most detector hits have been used.
bool StiLocalTrackSeedFinder::extendHit(StiHit* hit)
{
  //_messenger <<"StiLocalTrackSeedFinder::extendHit(StiHit* hit) -I- Started"<<endl;
  // Set the detector location to that of the given hit.
  // and attempt to move in from there.
  _detectorContainer->setToDetector( hit->detector() );
  //Done if the inner most detector is reached.
  if ( _detectorContainer->moveIn()==false ) return false;
  const StiDetector* newLayer = **_detectorContainer;
  _hitContainer->setDeltaD(mDeltaY);
  _hitContainer->setDeltaZ(mDeltaZ);
  _hitContainer->setRefPoint(newLayer->getPlacement()->getCenterRadius(),
			     newLayer->getPlacement()->getCenterRefAngle(),
			     hit->y(), hit->z());
  //Loop on hits, find closest in z:
  //This too should be replaced by an algorithm call which can be inlined.
  int nhits=0;
  StiHit* closestHit = 0;
  double  closestDistance = DBL_MAX;
  while (_hitContainer->hasMore()) 
    {
      ++nhits;
      StiHit* theHit = _hitContainer->getHit();//Get hit and increment
      if (theHit->timesUsed()==0) 
	{
	  //double theDeltaZ = fabs( theHit->z()-hit->z() );
	  double theDeltaZ = theHit->z()-hit->z();
	  double theDeltaY = theHit->y()-hit->y();
	  double distance = theDeltaY*theDeltaY+theDeltaZ*theDeltaZ;
	  if (distance < closestDistance )
	    {
	      closestHit = theHit;
	      closestDistance  = distance;
	    }
	}
    }
  _messenger <<"StiLocalTrackSeedFinder::extendHit(StiHit* hit) -I- Found "<<nhits<<" candidate hits"<<endl;
  //Check if a hit satisfied the search, add it to the list of hits, and return true,
  //if not return false.
  bool returnValue = false;
  if (closestHit ) 
    {
      returnValue = true;
      mSeedHitVec.push_back(closestHit);
    }
  _messenger <<"StiLocalTrackSeedFinder::extendHit(StiHit* hit) -I- Done; Extended:"<<returnValue<<endl;
  return returnValue;
}

/// Make a track seed starting at the given hit.
/// The track is extended iteratively with the "extendHit" method.
StiKalmanTrack* StiLocalTrackSeedFinder::makeTrack(StiHit* hit)
{
  _messenger <<"StiLocalTrackSeedFinder::makeTrack() -I- Started"<<endl;
  StiKalmanTrack* track = 0;
  mSeedHitVec.clear();
  mSeedHitVec.push_back(hit);
  //Recursively extend track:
  bool go=true;
  while ( go && mSeedHitVec.size()<(unsigned int) mSeedLength) 
    {
      go = extendHit( mSeedHitVec.back() );
    }
  //Extension failed if current track length less than mSeedLength
  //Return 0.
  if ( mSeedHitVec.size()<(unsigned int)mSeedLength ) 
    {
      _messenger <<"StiLocalTrackSeedFidnder::makeTrack() -W-"
		 <<" Hit extension failed because size()<mSeedLength"<<endl;
      return track;
    }
  //now use straight line propogation to recursively extend
  mSkipped = 0;
  go=true;
  while ( go && mSkipped<=mMaxSkipped && mSeedHitVec.size()<=(unsigned int)( mSeedLength+mExtrapMaxLength) ) 
    {
      go = extrapolate();
    }
  //Extension failed if current track length less than mSeedLength+mExtrapMinLength
  //Return 0.
  if ( mSeedHitVec.size()<(unsigned int)( mSeedLength+mExtrapMinLength) )
    {
      _messenger <<"StiLocalTrackSeedFinder::makeTrack() -W-"
		 <<" Hit extension failed because size()<mSeedLength+mExtrapMinLength"<<endl;
      return track;
    }
  //Test: Scale the errors (MLM 12/10/01)
  for_each( mSeedHitVec.begin(), mSeedHitVec.end(), ScaleHitError(10.) );
  track = initializeTrack(_trackFactory->getInstance());
  _messenger <<"StiLocalTrackSeedFinder::makeTrack() -I- Done"<<endl;
  return track;
}

/* We define the following picture
   r
   ^
   |
   |
   ----> y or z, depending on projection

   -------------- x -------------- pt3


   ------------------- x --------- pt2
   \
   \   
   ---------------------- x ------ pt1

   We try to extrapolate the segment (pt1->pt2) to predict pt3.  We
   do this in two projections (r,y) and (r,z) where y is the distance
   along pad, z is the global z, and r is the inward pointing sector
   normal (i.e., StiPlacement->getCenterRadius() )

   In the r,y plane, e.g., we define r = m*y + b s.t.
   m = (r2-r1) / (y2-y1)
   b = r2 - m2
   then
   y3 = (r3 - b) / m, which is our prediction, since we know r3
   
*/
bool StiLocalTrackSeedFinder::extrapolate()
{
  //This is a little ugly, but it's faster than manipulating 3-vectors
  _messenger <<"StiLocalTrackSeedFinder::extrapolate()"<<endl;
  //Calculate slope and offset in r-z and r-y projections, then extend
  const StiHit* hit1 = *( mSeedHitVec.begin()+mSeedHitVec.size() -2 ); //second to last
  const StiHit* hit2 = mSeedHitVec.back();

  //Get the next detector plane:
    
  double dr = hit2->x()-hit1->x();
  double dy = hit2->y()-hit1->y();
  double dz = hit2->z()-hit1->z();
  if (dr==0.) //// || dy==0. || dz==0.) 
    throw logic_error("StiLocalTrackSeedFinder::extrapolate() -E- Seed aborted because dr==0 ");
  //Now look for a hit in the next layer in:
  _detectorContainer->setToDetector( hit2->detector() );
  //Test to see if move in worked
  for (int i=0; i<=mSkipped; ++i) 
    {
      if ( _detectorContainer->moveIn()==false) 
	{
	  _messenger<<"StiLocalTrackSeedFinder::extrapolate() -W- Nowhere to move in to. Seed done"<<endl;
	  return false;
	}
    }

  const StiDetector* newLayer = **_detectorContainer;
  double r3 = newLayer->getPlacement()->getCenterRadius();
  //Temp hack by Mike
  if (r3<=60.) { return false; }
    
  //First, r-y plane
  //double m_ry = dr/dy;
  //double b_ry = hit2->x() - m_ry * hit2->y();
  //double y3 = (r3 - b_ry) / m_ry;
  double m_ryInv = dy/dr;
  double b_ry = hit2->x() - hit2->y()/m_ryInv;
  double y3 = (r3 - b_ry) * m_ryInv;

  //Now calculate the projection of window onto that plane:
  double beta_ry = atan2(dr, dy);
  double rho_ry = sqrt(dr*dr + dy*dy);
  double alpha_ry = atan2(mExtrapDeltaY, 2.*rho_ry);
  double tanplus_ry = tan(beta_ry+alpha_ry);
  double tanminus_ry = tan(beta_ry-alpha_ry);
  if (tanplus_ry==0. || tanminus_ry==0.) 
    _messenger<<"StiLocalTrackSeedFidner::extrapolate(). -E- tanplus_ry==0. || tanminus_ry==0."<<endl;
  
  double y3_minus = (r3-hit1->x())/tanplus_ry + hit1->y();
  double y3_plus = (r3-hit1->x())/tanminus_ry + hit1->y();
  //Next, r-z plane
  double m_rzInv = dz/dr;
  double b_rz = hit2->x() - hit2->z()/m_rzInv;
  double z3 = (r3 - b_rz) * m_rzInv;
    
  double beta_rz = atan2(dr, dz);
  double rho_rz = sqrt(dr*dr + dz*dz);
  double alpha_rz = atan2(mExtrapDeltaZ, 2.*rho_rz);
  double tanplus_rz = tan(beta_rz+alpha_rz);
  double tanminus_rz = tan(beta_rz-alpha_rz);
  if (tanplus_rz==0. || tanminus_rz==0.) 
    _messenger<<"StiLocalTrackSeedFidner::extrapolate(). -W- tanplus_rz==0. || tanminus_rz==0."<<endl;
  double z3_minus = (r3-hit1->x())/tanplus_rz + hit1->z();
  double z3_plus = (r3-hit1->x())/tanminus_rz + hit1->z();
  _messenger<<"beta_ry: "<<beta_ry<<" alpha_ry: "<<alpha_ry
	    <<" y3+: "<<y3_plus<<" y3: "<<y3<<" y3-: "<<y3_minus<<endl
	    <<"beta_rz: "<<beta_rz<<" alpha_rz: "<<alpha_rz
	    <<" z3+: "<<z3_plus<<" z3: "<<z3<<" z3-: "<<z3_minus<<endl
	    <<"query hit container for extension hits"<<endl;
  _hitContainer->setDeltaD( fabs(y3_plus-y3_minus) /2.);
  _hitContainer->setDeltaZ( fabs(z3_plus-z3_minus) /2. );
  //cout << " y3 Window:"<< fabs(y3_plus-y3_minus) /2.
  //     << "     z3 Window:"<< fabs(y3_plus-y3_minus) /2.<<endl;
  //_hitContainer->setDeltaD( 8.);
  //_hitContainer->setDeltaZ( 8.);
  _hitContainer->setRefPoint(r3, newLayer->getPlacement()->getCenterRefAngle(),
			     y3, z3);
  //Loop on hits, find closest to z3 prediction:
  //This too should be replaced by an algorithm call which can be inlined.
  //Add it to the tbd list!
    
  int nhits=0;
  StiHit* closestHit = 0;
  double dist_max = DBL_MAX;
    
  while (_hitContainer->hasMore()) {
    ++nhits;
    StiHit* theHit = _hitContainer->getHit(); //Get hit and increment
    //if (theHit->isUsed()==false) {
    if (theHit->timesUsed()==0) {
      double theDeltaZ = fabs( theHit->z() - z3 );
      double theDeltaY = fabs( theHit->y() - y3 );
      //double dist = theDeltaZ*theDeltaZ/max(double(theHit->z()),z3) +
      //	theDeltaY*theDeltaY/max(double(theHit->y()),y3);
      double dist = theDeltaZ*theDeltaZ+theDeltaY*theDeltaY;      
      if ( dist < dist_max ) {
	closestHit = theHit;
	dist_max = dist;
      }
    }
  }
  _messenger <<"StiLocalTrackSeedFinder.  Found "<<nhits<<" Candidate hits"<<endl;
  //Check if we satisfied the search:
  if ( !closestHit ) 
    {
      _messenger<<"StiLocalTrackSeedFinder::extrapolate() -W- No hits found in next layer."<<endl;
      ++mSkipped;
      return true;
    }
  mSkipped=0;
  mSeedHitVec.push_back(closestHit);
  return true;
}

/// Initialize a kalman track on the basis of hits held in mSeedHitVec
/// 
StiKalmanTrack* StiLocalTrackSeedFinder::initializeTrack(StiKalmanTrack* track)
{
  _messenger <<"StiLocalTrackSeedFinder::initializeTrack(StiKalmanTrack*) -I- Started"<<endl;
  if (mDoHelixFit && mSeedHitVec.size()>=3) 
    {
      bool status = fit(track);
      if (status)
	_messenger <<"StiLocalTrackSeedFinder::initializeTrack(SKT*) -I- Returning fitted track"<<endl;
      else
	{
	  _messenger <<"StiLocalTrackSeedFinder::initializeTrack(SKT*) -I- FIT FAILED - Returning 0"<<endl;
	  track = 0;
	}
    }
  else if (mUseOrigin==false && mSeedHitVec.size()>=3) 
    calculate(track);
  else if (mSeedHitVec.size()>=2) 
    // Too few points for a stand alone calculation, 
    // include and estimate of the origin of the track.
    calculateWithOrigin(track);
  else
    // Not enough data to make a track
    track = 0;
  _messenger<<"StiLocalTrackSeedFinder::initializeTrack(StiKalmanTrack*) -I- Done."<<endl;
  return track;
}

bool StiLocalTrackSeedFinder::fit(StiKalmanTrack* track)
{
  _messenger<<"StiLocalTrackSeedFinder::fit(StiKalmanTrack*) -I- Started"<<endl;
  mHelixFitter.reset();
  mHelixFitter.fit( mSeedHitVec );
  if (mHelixFitter.valid()==false ) 
    throw logic_error("StiLocalTrackSeedFinder::fit(StiKalmanTrack* track) - FATAL - No Helix Fitter");
  _messenger <<"origin: "<<mHelixFitter.xCenter()<<" "<<mHelixFitter.yCenter()<<" "
	     <<mHelixFitter.z0()<<" "
	     <<" curvature: "<<mHelixFitter.curvature()<<" "
	     <<" tanLambda: "<<mHelixFitter.tanLambda()<<endl;
  track->initialize( mHelixFitter.curvature(), mHelixFitter.tanLambda(),
		     StThreeVectorD(mHelixFitter.xCenter(), mHelixFitter.yCenter(), 0.),
		     mSeedHitVec);
  return true;
}

void StiLocalTrackSeedFinder::calculate(StiKalmanTrack* track)
{
  _messenger<<"StiLocalTrackSeedFinder::calculate(StiKalmanTrack*) -I- Started"<<endl;
  const StThreeVectorF& outside = mSeedHitVec.front()->globalPosition();
  const StThreeVectorF& middle = (*(mSeedHitVec.begin()+mSeedHitVec.size()/2))->globalPosition();
  const StThreeVectorF& inside = mSeedHitVec.back()->globalPosition();
  _messenger<<"StiLocalTrackSeedFinder::calculate(StiKalmanTrack*) -I- Calculate circle parameters:\t";
  mHelixCalculator.calculate( StThreeVector<double>( inside.x(), inside.y(), inside.z() ),
			      StThreeVector<double>( middle.x(), middle.y(), middle.z() ),
			      StThreeVector<double>( outside.x(), outside.y(), outside.z() ) );
  _messenger <<"  origin: "<<mHelixCalculator.xCenter()<<" "<<mHelixCalculator.yCenter()<<" "
	     <<mHelixCalculator.z0()<<" "
	     <<" curvature: "<<mHelixCalculator.curvature()<<" "
	     <<" tanLambda: "<<mHelixCalculator.tanLambda()<<endl;
  track->initialize( mHelixCalculator.curvature(), 
		     mHelixCalculator.tanLambda(),
		     StThreeVectorD(mHelixCalculator.xCenter(),mHelixCalculator.yCenter(), 0.),
		     mSeedHitVec);    
  _messenger<<"StiLocalTrackSeedFinder::calculate(StiKalmanTrack*) -I- Done"<<endl;
}

void StiLocalTrackSeedFinder::calculateWithOrigin(StiKalmanTrack* track)
{
  _messenger<<"StiLocalTrackSeedFinder::calculateWithOrigin(StiKalmanTrack*)"<<endl;
  const StThreeVectorF& outside = mSeedHitVec.front()->globalPosition();
  const StThreeVectorF& middle = mSeedHitVec.back()->globalPosition();
  
  _messenger<<"\tCalculate circle parameters:\t";
  mHelixCalculator.calculate( StThreeVector<double>(0., 0., 0.),
			      StThreeVector<double>( middle.x(), middle.y(), middle.z() ),
			      StThreeVector<double>( outside.x(), outside.y(), outside.z() ) );
  
  _messenger<<"\tdone."<<endl;
  _messenger <<"origin: "<<mHelixCalculator.xCenter()<<" "<<mHelixCalculator.yCenter()<<" "
	     <<mHelixCalculator.z0()<<" "
	     <<" curvature: "<<mHelixCalculator.curvature()<<" "
	     <<" tanLambda: "<<mHelixCalculator.tanLambda()<<endl;
  
  _messenger<<"\tInitialzie Track:\t";
  track->initialize( mHelixCalculator.curvature(), mHelixCalculator.tanLambda(),
		     StThreeVectorD(mHelixCalculator.xCenter(), mHelixCalculator.yCenter(), 0.),
		     mSeedHitVec);    
}

void StiLocalTrackSeedFinder::addLayer(StiDetector* det)
{
  mDetVec.push_back(det);
  sort(mDetVec.begin(), mDetVec.end(), RPhiLessThan());
}


//Non members

//sort in descending order in radius, and ascending order in phi
bool RPhiLessThan::operator()(const StiDetector* lhs, const StiDetector* rhs)
{
  StiPlacement* lhsp = lhs->getPlacement();
  StiPlacement* rhsp = rhs->getPlacement();
    
  if (lhsp->getCenterRadius()<rhsp->getCenterRadius())
    return false;
  else if (lhsp->getCenterRadius()>rhsp->getCenterRadius()) 
    return true;
  else
    return (lhsp->getCenterRefAngle()<rhsp->getCenterRefAngle());
}

void StiLocalTrackSeedFinder::print() const
{
  cout <<"StiLocalTrackSeedFinder Detectors:\n";
    
  for (vector<StiDetector*>::const_iterator it=mDetVec.begin(); it!=mDetVec.end(); ++it) {
    cout << **it <<endl;
  }
  cout <<"\n Search Window in Y:\t"<<mDeltaY<<endl;
  cout <<"\n Search Window in Z:\t"<<mDeltaZ<<endl;
}

void ScaleHitError::operator()(StiHit* hit) const
{
  hit->scaleError(scale);
}
