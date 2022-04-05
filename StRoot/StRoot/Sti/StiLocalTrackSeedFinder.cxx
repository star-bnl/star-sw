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

//______________________________________________________________________________
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
  fRxyMin=0;
//  cout <<"StiLocalTrackSeedFinder::StiLocalTrackSeedFinder() -I- Started/Done"<<endl;
}

//______________________________________________________________________________
StiLocalTrackSeedFinder::~StiLocalTrackSeedFinder()
{
//  cout <<"StiLocalTrackSeedFinder::~StiLocalTrackSeedFinder() -I- Started/Done"<<endl;
}

//______________________________________________________________________________
/// Produce the next track seed 
/// Loop through available hits and attempt to form a track seed
/// Only use hits that have not been already used in fully formed
/// tracks. 
//______________________________________________________________________________
StiTrack* StiLocalTrackSeedFinder::findTrack(double rMin)
{
  fRxyMin = rMin;
  StiKalmanTrack* track = 0;  
  if (isReset())
    { 
//      cout << "StiLocalTrackSeedFinder::findTrack() -I- Getting iterator" << endl;

      _hitIter = StiSortedHitIterator(_hitContainer,_detectorContainer->begin(),_detectorContainer->end());
    }
  for (;_hitIter!=StiSortedHitIterator() && track==0;++_hitIter)
    {
      try 
	{
          StiHit *hit = &(*_hitIter);
	  if (hit->timesUsed()) continue;
	  if (fRxyMin && pow(hit->x(),2)+pow(hit->y(),2)<fRxyMin*fRxyMin) continue;
	  track = makeTrack(&*_hitIter);
	}
      catch(runtime_error & rte )
	{
	  //	  cout<< "StiLocalTrackSeedFinder::findTrack() -W- Run Time Error :" << rte.what() << endl;
	}
    }
  //cout <<"StiLocalTrackSeedFinder::findTrack() -I- Done"<<endl;
  fRxyMin = 0;
  return track;
}

//______________________________________________________________________________
/// Extend the track seed starting from the given hit.
/// The extension proceeds from the outside-in. It stops
/// whenever the inner most detector hits have been used.
bool StiLocalTrackSeedFinder::extendHit(StiHit& hit)
{
  //cout <<"StiLocalTrackSeedFinder::extendHit(StiHit& hit) -I- Started"<<endl;
  _detectorContainer->setToDetector( hit.detector() );
  //Done if the inner most detector is reached.
  if ( _detectorContainer->moveIn()==false ) return false;
  const StiDetector* d = _detectorContainer->getCurrentDetector(); //**_detectorContainer;
  StiPlacement *     p = d->getPlacement();
  if (p->getLayerRadius() < fRxyMin) return false;
  StiHit* closestHit = _hitContainer->getNearestHit(p->getLayerRadius(),
						    p->getLayerAngle(),
						    hit.y(), hit.z(),
						    StiLocalTrackSeedFinderParameters::instance()->deltaY(), 
						    StiLocalTrackSeedFinderParameters::instance()->deltaZ());
  
  if (!closestHit ) return false;
  _seedHits.push_back(closestHit);
   return true;
}

//______________________________________________________________________________
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
  StiLocalTrackSeedFinderParameters *seedPars=StiLocalTrackSeedFinderParameters::instance();
  int iSeedLength 	= seedPars->seedLength();
  int iExtrapMaxLength	= seedPars->extrapMaxLength();
  int iExtrapMinLength	= seedPars->extrapMinLength();
  int iMaxSkipped 	= seedPars->maxSkipped();

  while ( go && (int)_seedHits.size()<iSeedLength)
    {
      go = extendHit( *_seedHits.back() );
    }
  //Extension failed if current track length less than seedPars->seedLength()
  //Return 0.
  if ( (int)_seedHits.size()<iSeedLength ) 
    {
      return track;
    }
  //now use straight line propogation to recursively extend
  _skipped = 0;
  go=true;
  while ( go && _skipped<=iMaxSkipped && 
	  (int)_seedHits.size()<=( iSeedLength+iExtrapMaxLength)) 
    {
      go = extrapolate();
    }
  //Extension failed if current track length less than seedPars->seedLength()+
  //seedPars->extrapMinLength()
  //Return 0.
  if ( (int)_seedHits.size()<( iSeedLength+iExtrapMinLength) )
    {
      //seedPars->extrapMinLength()"<<endl;
      return track;
    }
  track = initializeTrack(_trackFactory->getInstance());
  return track;
}

//______________________________________________________________________________
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
   normal (i.e., StiPlacement->getNormalRadius() )

   In the r,y plane, e.g., we define r = m*y + b s.t.
   m = (r2-r1) / (y2-y1)
   b = r2 - m2
   then
   y3 = (r3 - b) / m, which is our prediction, since we know r3
   
*/
//______________________________________________________________________________
bool StiLocalTrackSeedFinder::extrapolate()
{
  //This is a little ugly, but it's faster than manipulating 3-vectors
  //cout <<"StiLocalTrackSeedFinder::extrapolate()"<<endl;
  //Calculate slope and offset in r-z and r-y projections, then extend
  const StiHit* hit1 = *( _seedHits.begin()+_seedHits.size() -2 ); //second to last
  const StiHit* hit2 = _seedHits.back();

  //Get the next detector plane:
    
  double r1 = hit1->x();
  double y1 = hit1->y();
  double z1 = hit1->z();
  double r2 = hit2->x();
  double y2 = hit2->y();
  double z2 = hit2->z();

  double dr = r2-r1;
  double dy = y2-y1;
  double dz = z2-z1;
  if (fabs(dr) <=1.e-3) return false;
  assert (fabs(dr) >1.e-3); //// || dy==0. || dz==0.);
  //Now look for a hit in the next layer in:
  _detectorContainer->setToDetector( hit2->detector());
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
  double r3 = newLayer->getPlacement()->getNormalRadius();
  
  //Temp hack by Mike
//VP  if (r3<=60.) { return false; }
  if (r3<=25.) { return false; } //VP avoid SVT from seed
    
  //First, r-y plane
  //double m_ry = dr/dy;
  //double b_ry = hit2->x() - m_ry * hit2->y();
  //double y3 = (r3 - b_ry) / m_ry;
  double y3 = y1 + dy/dr*(r3-r1);

  //Now calculate the projection of window onto that plane:
  double beta_ry = atan2(dr, dy);
  double rho_ry = ::sqrt(dr*dr + dy*dy);
  double alpha_ry = atan2(StiLocalTrackSeedFinderParameters::instance()->extrapDeltaY(), 2.*rho_ry);
  double tanplus_ry = tan(beta_ry+alpha_ry);
  double tanminus_ry = tan(beta_ry-alpha_ry);
  if (tanplus_ry==0. || tanminus_ry==0.) 
    cout<<"StiLocalTrackSeedFidner::extrapolate(). -W- tanplus_ry==0. || tanminus_ry==0."<<endl;
  
  double y3_minus = (r3-r1)/tanplus_ry  + y1;
  double y3_plus  = (r3-r1)/tanminus_ry + y1;
  //Next, r-z plane
  double z3 = z1 + dz/dr*(r3-r1);

  double beta_rz = atan2(dr, dz);
  double rho_rz = ::sqrt(dr*dr + dz*dz);
  double alpha_rz = atan2(StiLocalTrackSeedFinderParameters::instance()->extrapDeltaZ(), 2.*rho_rz);
  double tanplus_rz = tan(beta_rz+alpha_rz);
  double tanminus_rz = tan(beta_rz-alpha_rz);
  if (tanplus_rz==0. || tanminus_rz==0.) 
    cout<<"StiLocalTrackSeedFidner::extrapolate(). -W- tanplus_rz==0. || tanminus_rz==0."<<endl;
  double z3_minus = (r3-r1)/tanplus_rz  + z1;
  double z3_plus =  (r3-r1)/tanminus_rz + z1;
  /*
    cout<<"beta_ry: "<<beta_ry<<" alpha_ry: "<<alpha_ry
    <<" y3+: "<<y3_plus<<" y3: "<<y3<<" y3-: "<<y3_minus<<endl
    <<"beta_rz: "<<beta_rz<<" alpha_rz: "<<alpha_rz
    <<" z3+: "<<z3_plus<<" z3: "<<z3<<" z3-: "<<z3_minus<<endl
    <<"query hit container for extension hits"<<endl;
  */
  StiPlacement * p = newLayer->getPlacement();
  StiHit* closestHit = _hitContainer->getNearestHit(p->getLayerRadius(),
						    p->getLayerAngle(),
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


//______________________________________________________________________________
/// Initialize a kalman track on the basis of hits held in _seedHits
/// 
StiKalmanTrack* StiLocalTrackSeedFinder::initializeTrack(StiKalmanTrack* track)
{
  bool status = fit(track);
  if (!status) track = 0;
  return track;
}

//______________________________________________________________________________
bool StiLocalTrackSeedFinder::fit(StiKalmanTrack* track)
{
  int ierr = track->initialize(_seedHits);
  return (ierr==0);
}

//______________________________________________________________________________
void StiLocalTrackSeedFinder::print() const
{
  cout <<"StiLocalTrackSeedFinder::print() -I- ";
  for (vector<StiDetector*>::const_iterator it=_detectorContainer->begin(); 
       it!=_detectorContainer->end(); 
       ++it) 
    {
      cout << **it <<endl;
    }
  cout <<"\n Search Window in Y:\t"<<StiLocalTrackSeedFinderParameters::instance()->deltaY()<<endl;
  cout <<"\n Search Window in Z:\t"<<StiLocalTrackSeedFinderParameters::instance()->deltaZ()<<endl;
}

//______________________________________________________________________________
ostream& operator<<(ostream& os, const StiLocalTrackSeedFinder & f)
{
  return os << " StiLocalTrackSeedFinder " << endl;
}
