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
#include "StiMasterDetectorBuilder.h"

ostream& operator<<(ostream&, const StiDetector&);

StiLocalTrackSeedFinder::StiLocalTrackSeedFinder(const string& name,
						 const string& description, 
						 Factory<StiKalmanTrack> * trackFactory,
						 StiHitContainer         * hitContainer,
						 StiDetectorContainer    * detectorContainer)
  : StiTrackFinder(),
    _reset(true),
    _trackFactory(trackFactory),
    _hitContainer(hitContainer),
    _detectorContainer(detectorContainer)
{
  cout <<"StiLocalTrackSeedFinder::StiLocalTrackSeedFinder() -I- Started/Done"<<endl;
}

StiLocalTrackSeedFinder::~StiLocalTrackSeedFinder()
{
  cout <<"StiLocalTrackSeedFinder::~StiLocalTrackSeedFinder() -I- Started/Done"<<endl;
}

/// Produce the next track seed 
/// Loop through available hits and attempt to form a track seed
/// Only use hits that have not been already used in fully formed
/// tracks. 
StiTrack* StiLocalTrackSeedFinder::findTrack()
{
  StiKalmanTrack* track = 0;  
  if (isReset())
    { 
      //cout << "StiLocalTrackSeedFinder::findTrack() -I- Getting iterator" << endl;
      _hitIter = StiSortedHitIterator(_hitContainer,_detectorContainer->begin(),_detectorContainer->end());
    }
  while (_hitIter!=StiSortedHitIterator() && track==0)
    {
      try 
	{
	  if ( (*_hitIter).timesUsed()==0 ) track = makeTrack(&*_hitIter);
	}
      catch(runtime_error & rte )
	{
	  cout<< "StiLocalTrackSeedFinder::findTrack() -W- Run Time Error :" << rte.what() << endl;
	}
      ++_hitIter;
    }
  //cout <<"StiLocalTrackSeedFinder::findTrack() -I- Done"<<endl;
  return track;
}

/// Extend the track seed starting from the given hit.
/// The extension proceeds from the outside-in. It stops
/// whenever the inner most detector hits have been used.
bool StiLocalTrackSeedFinder::extendHit(StiHit& hit)
{
  //cout <<"StiLocalTrackSeedFinder::extendHit(StiHit& hit) -I- Started"<<endl;
  _detectorContainer->setToDetector( hit.detector() );
  //Done if the inner most detector is reached.
  if ( _detectorContainer->moveIn()==false ) return false;
  const StiDetector* newLayer = _detectorContainer->getCurrentDetector(); //**_detectorContainer;
  StiHit* closestHit = _hitContainer->getNearestHit(newLayer->getPlacement()->getCenterRadius(),
						    newLayer->getPlacement()->getCenterRefAngle(),
						    hit.y(), hit.z(),_pars._deltaY, _pars._deltaZ);
  bool returnValue;
  if (closestHit ) 
    {
      _seedHits.push_back(closestHit);
      returnValue = true;
    }
  else
    returnValue = false;
  //cout <<"StiLocalTrackSeedFinder::extendHit(StiHit* hit) -I- Done; Extended:"<<returnValue<<endl;
  return returnValue;
}

/// Make a track seed starting at the given hit.
/// The track is extended iteratively with the "extendHit" method.
StiKalmanTrack* StiLocalTrackSeedFinder::makeTrack(StiHit* hit)
{
  //cout <<"StiLocalTrackSeedFinder::makeTrack() -I- Started"<<endl;
  StiKalmanTrack* track = 0;
  _seedHits.clear();
  _seedHits.push_back(hit);
  //Recursively extend track:
  bool go=true;
  while ( go && _seedHits.size()<(unsigned int) _pars._seedLength) 
    {
      go = extendHit( *_seedHits.back() );
    }
  //Extension failed if current track length less than _pars._seedLength
  //Return 0.
  if ( _seedHits.size()<(unsigned int)_pars._seedLength ) 
    {
      //cout <<"StiLocalTrackSeedFidnder::makeTrack() -W- Hit extension failed because size()<_pars._seedLength"<<endl;
      return track;
    }
  //now use straight line propogation to recursively extend
  _skipped = 0;
  go=true;
  while ( go && _skipped<=_pars._maxSkipped && _seedHits.size()<=(unsigned int)( _pars._seedLength+_pars._extrapMaxLength) ) 
    {
      go = extrapolate();
    }
  //Extension failed if current track length less than _pars._seedLength+_pars._extrapMinLength
  //Return 0.
  if ( _seedHits.size()<(unsigned int)( _pars._seedLength+_pars._extrapMinLength) )
    {
      //cout <<"StiLocalTrackSeedFinder::makeTrack() -W- Extension failed size()<_pars._seedLength+_pars._extrapMinLength"<<endl;
      return track;
    }
  for_each( _seedHits.begin(), _seedHits.end(), ScaleHitError(10.) );
  track = initializeTrack(_trackFactory->getInstance());
  //cout <<"StiLocalTrackSeedFinder::makeTrack() -I- Done"<<endl;
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
  //cout <<"StiLocalTrackSeedFinder::extrapolate()"<<endl;
  //Calculate slope and offset in r-z and r-y projections, then extend
  const StiHit* hit1 = *( _seedHits.begin()+_seedHits.size() -2 ); //second to last
  const StiHit* hit2 = _seedHits.back();

  //Get the next detector plane:
    
  double dr = hit2->x()-hit1->x();
  double dy = hit2->y()-hit1->y();
  double dz = hit2->z()-hit1->z();
  if (dr==0.) //// || dy==0. || dz==0.) 
    throw logic_error("StiLocalTrackSeedFinder::extrapolate() -E- Seed aborted because dr==0 ");
  //Now look for a hit in the next layer in:
  _detectorContainer->setToDetector( hit2->detector() );
  //Test to see if move in worked
  for (int i=0; i<=_skipped; ++i) 
    {
      if ( _detectorContainer->moveIn()==false) 
	{
	  //cout<<"StiLocalTrackSeedFinder::extrapolate() -W- Nowhere to move in to. Seed done"<<endl;
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
  double rho_ry = ::sqrt(dr*dr + dy*dy);
  double alpha_ry = atan2(_pars._extrapDeltaY, 2.*rho_ry);
  double tanplus_ry = tan(beta_ry+alpha_ry);
  double tanminus_ry = tan(beta_ry-alpha_ry);
  if (tanplus_ry==0. || tanminus_ry==0.) 
    cout<<"StiLocalTrackSeedFidner::extrapolate(). -W- tanplus_ry==0. || tanminus_ry==0."<<endl;
  
  double y3_minus = (r3-hit1->x())/tanplus_ry + hit1->y();
  double y3_plus = (r3-hit1->x())/tanminus_ry + hit1->y();
  //Next, r-z plane
  double m_rzInv = dz/dr;
  double b_rz = hit2->x() - hit2->z()/m_rzInv;
  double z3 = (r3 - b_rz) * m_rzInv;
    
  double beta_rz = atan2(dr, dz);
  double rho_rz = ::sqrt(dr*dr + dz*dz);
  double alpha_rz = atan2(_pars._extrapDeltaZ, 2.*rho_rz);
  double tanplus_rz = tan(beta_rz+alpha_rz);
  double tanminus_rz = tan(beta_rz-alpha_rz);
  if (tanplus_rz==0. || tanminus_rz==0.) 
    cout<<"StiLocalTrackSeedFidner::extrapolate(). -W- tanplus_rz==0. || tanminus_rz==0."<<endl;
  double z3_minus = (r3-hit1->x())/tanplus_rz + hit1->z();
  double z3_plus = (r3-hit1->x())/tanminus_rz + hit1->z();
  /*
    cout<<"beta_ry: "<<beta_ry<<" alpha_ry: "<<alpha_ry
    <<" y3+: "<<y3_plus<<" y3: "<<y3<<" y3-: "<<y3_minus<<endl
    <<"beta_rz: "<<beta_rz<<" alpha_rz: "<<alpha_rz
    <<" z3+: "<<z3_plus<<" z3: "<<z3<<" z3-: "<<z3_minus<<endl
    <<"query hit container for extension hits"<<endl;
  */
  StiHit* closestHit = _hitContainer->getNearestHit(r3,
						    newLayer->getPlacement()->getCenterRefAngle(),
						    y3,z3,  
						    fabs(y3_plus-y3_minus) /2.,
						    fabs(z3_plus-z3_minus) /2.);
  if ( !closestHit ) 
    {
      //cout<<"StiLocalTrackSeedFinder::extrapolate() -W- No hits found in next layer."<<endl;
      ++_skipped;
      return true;
    }
  _skipped=0;
  _seedHits.push_back(closestHit);
  return true;
}

/// Initialize a kalman track on the basis of hits held in _seedHits
/// 
StiKalmanTrack* StiLocalTrackSeedFinder::initializeTrack(StiKalmanTrack* track)
{
  //cout <<"StiLocalTrackSeedFinder::initializeTrack(StiKalmanTrack*) -I- Started"<<endl;
  if (_pars._doHelixFit && _seedHits.size()>=3) 
    {
      bool status = fit(track);
      if (!status) track = 0;
    }
  else if (_pars._useOrigin==false && _seedHits.size()>=3) 
    calculate(track);
  else if (_seedHits.size()>=2) 
    // Too few points for a stand alone calculation, 
    // include and estimate of the origin of the track.
    calculateWithOrigin(track);
  else
    // Not enough data to make a track
    track = 0;
  //cout<<"StiLocalTrackSeedFinder::initializeTrack(StiKalmanTrack*) -I- Done."<<endl;
  return track;
}

bool StiLocalTrackSeedFinder::fit(StiKalmanTrack* track)
{
  //cout<<"StiLocalTrackSeedFinder::fit(StiKalmanTrack*) -I- Started"<<endl;
  _helixFitter.reset();
  _helixFitter.fit( _seedHits );
  if (_helixFitter.valid()==false ) 
    throw logic_error("StiLocalTrackSeedFinder::fit(StiKalmanTrack* track) - FATAL - No Helix Fitter");
  /*
    cout <<"origin: "<<_helixFitter.xCenter()<<" "<<_helixFitter.yCenter()<<" "
    <<_helixFitter.z0()<<" "
    <<" curvature: "<<_helixFitter.curvature()<<" "
    <<" tanLambda: "<<_helixFitter.tanLambda()<<endl;
  */
  track->initialize( _helixFitter.curvature(), _helixFitter.tanLambda(),
		     StThreeVectorD(_helixFitter.xCenter(), _helixFitter.yCenter(), 0.),
		     _seedHits);
  return true;
}

void StiLocalTrackSeedFinder::calculate(StiKalmanTrack* track)
{
  //cout<<"StiLocalTrackSeedFinder::calculate(StiKalmanTrack*) -I- Started"<<endl;
  const StThreeVectorF& outside = _seedHits.front()->globalPosition();
  const StThreeVectorF& middle = (*(_seedHits.begin()+_seedHits.size()/2))->globalPosition();
  const StThreeVectorF& inside = _seedHits.back()->globalPosition();
  //cout<<"StiLocalTrackSeedFinder::calculate(StiKalmanTrack*) -I- Calculate circle parameters:\t";
  _helixCalculator.calculate( StThreeVector<double>( inside.x(), inside.y(), inside.z() ),
			      StThreeVector<double>( middle.x(), middle.y(), middle.z() ),
			      StThreeVector<double>( outside.x(), outside.y(), outside.z() ) );
  /*
    cout <<"  origin: "<<_helixCalculator.xCenter()<<" "<<_helixCalculator.yCenter()<<" "
    <<_helixCalculator.z0()<<" "
    <<" curvature: "<<_helixCalculator.curvature()<<" "
    <<" tanLambda: "<<_helixCalculator.tanLambda()<<endl;
  */
  track->initialize( _helixCalculator.curvature(), 
		     _helixCalculator.tanLambda(),
		     StThreeVectorD(_helixCalculator.xCenter(),_helixCalculator.yCenter(), 0.),
		     _seedHits);    
  //cout<<"StiLocalTrackSeedFinder::calculate(StiKalmanTrack*) -I- Done"<<endl;
}

void StiLocalTrackSeedFinder::calculateWithOrigin(StiKalmanTrack* track)
{
  //cout<<"StiLocalTrackSeedFinder::calculateWithOrigin(StiKalmanTrack*)"<<endl;
  const StThreeVectorF& outside = _seedHits.front()->globalPosition();
  const StThreeVectorF& middle = _seedHits.back()->globalPosition();
  
  //cout<<"\tCalculate circle parameters:\t";
  _helixCalculator.calculate( StThreeVector<double>(0., 0., 0.),
			      StThreeVector<double>( middle.x(), middle.y(), middle.z() ),
			      StThreeVector<double>( outside.x(), outside.y(), outside.z() ) );
  
  /*
    cout <<"origin: "<<_helixCalculator.xCenter()<<" "<<_helixCalculator.yCenter()<<" "
    <<_helixCalculator.z0()<<" "
    <<" curvature: "<<_helixCalculator.curvature()<<" "
    <<" tanLambda: "<<_helixCalculator.tanLambda()<<endl
    <<"\tInitialzie Track:\t";
  */
  track->initialize( _helixCalculator.curvature(), _helixCalculator.tanLambda(),
		     StThreeVectorD(_helixCalculator.xCenter(), _helixCalculator.yCenter(), 0.),
		     _seedHits);    
}

/*
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
*/

void StiLocalTrackSeedFinder::print() const
{
  cout <<"StiLocalTrackSeedFinder::print() -I- ";
  for (vector<StiDetector*>::const_iterator it=_detectorContainer->begin(); 
       it!=_detectorContainer->end(); 
       ++it) 
    {
      cout << **it <<endl;
    }
  cout <<"\n Search Window in Y:\t"<<_pars._deltaY<<endl;
  cout <<"\n Search Window in Z:\t"<<_pars._deltaZ<<endl;
}

void ScaleHitError::operator()(StiHit* hit) const
{
  hit->scaleError(scale);
}

void StiLocalTrackSeedFinder::loadDS(TDataSet&ds)
{
  cout << "StiLocalTrackSeedFinder::loadDS(TDataSet&) -I- Started" << endl;
  _pars.loadDS(ds);
  cout << "StiLocalTrackSeedFinder::loadDS(TDataSet&) -I- Done" << endl;
}

void StiLocalTrackSeedFinder::loadFS(ifstream& inFile)
{
  cout << "StiLocalTrackSeedFinder::loadFS(ifstream& inFile) -I- Started" << endl;
  _pars.loadFS(inFile);
  cout << "StiLocalTrackSeedFinder::loadFS(ifstream& inFile) -I- Done" << endl;
}

ostream& operator<<(ostream& os, const StiLocalTrackSeedFinder & f)
{
  return os << " StiLocalTrackSeedFinder " << endl << f._pars << endl;
}
