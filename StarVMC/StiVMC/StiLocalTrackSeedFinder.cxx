///\file StiLocalTrackSeedFinder.cxx 
///\author M.L. Miller (Yale Software) 10/01
#include <math.h>
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StThreeVector.hh"
#include "StiHit.h" 
#include "StiHitContainer.h"
#include "StiKalmanTrack.h"
#include "StiDetector.h"
#include "StiDetectorContainer.h"
#include "StiLocalTrackSeedFinder.h"
#include "StiSortedHitIterator.h"
#include "StiKalmanTrackContainer.h"
#include "StiToolkit.h"
#include "TMath.h"
ClassImp(StiLocalTrackSeedFinder);
ostream& operator<<(ostream&, const StiDetector&);
//______________________________________________________________________________
/// Produce the next track seed 
/// Loop through available hits and attempt to form a track seed
/// Only use hits that have not been already used in fully formed
/// tracks. 
//______________________________________________________________________________
StiKalmanTrack* StiLocalTrackSeedFinder::findTrack(double rMin) {
  fRxyMin = rMin;
  StiKalmanTrack* track = 0;  
  if (isReset())    { 
    cout << "StiLocalTrackSeedFinder::findTrack() -I- Getting iterator" << endl;
    
    _hitIter = StiSortedHitIterator(StiToolkit::instance()->HitContainer(),
				    StiToolkit::instance()->DetectorContainer()->begin(),
				    StiToolkit::instance()->DetectorContainer()->end());
  }
  for (;_hitIter!=StiSortedHitIterator() && track==0;++_hitIter)   {
    StiHit *hit = &(*_hitIter);
    if (hit->timesUsed()) continue;
    if (fRxyMin && 
	TMath::Power(hit->Detector()->NormalRadius(),2) +
	TMath::Power(hit->y(),2)<fRxyMin*fRxyMin) continue;
    track = makeTrack(&*_hitIter);
  }
  //cout <<"StiLocalTrackSeedFinder::findTrack() -I- Done"<<endl;
  fRxyMin = 0;
  return track;
}

//______________________________________________________________________________
/// Extend the track seed starting from the given hit.
/// The extension proceeds from the outside-in. It stops
/// whenever the inner most detector hits have been used.
Bool_t StiLocalTrackSeedFinder::ExtendHit(StiHit& hit) {
  //cout <<"StiLocalTrackSeedFinder::ExtendHit(StiHit& hit) -I- Started"<<endl;
  StiToolkit::instance()->DetectorContainer()->SetToDetector( hit.Detector() );
  //Done if the inner most detector is reached.
  if ( StiToolkit::instance()->DetectorContainer()->MoveIn()==kFALSE ) return kFALSE;
  const StiDetector* d = StiToolkit::instance()->DetectorContainer()->CurrentDetector(); 
  if (d->NormalRadius() < fRxyMin) return kFALSE;
  StiHit* closestHit = StiToolkit::instance()->HitContainer()->
    NearestHit(d, hit.y(), hit.z(),
	       StiLocalTrackSeedFinderParameters::instance()->deltaY(), 
	       StiLocalTrackSeedFinderParameters::instance()->deltaZ());
  if (!closestHit ) return kFALSE;
  _seedHits.push_back(closestHit);
  return kTRUE;
}

//______________________________________________________________________________
/// Make a track seed starting at the given hit.
/// The track is Extended iteratively with the "ExtendHit" method.
StiKalmanTrack* StiLocalTrackSeedFinder::makeTrack(StiHit* hit) {
  StiKalmanTrack* track = 0;
  _seedHits.clear();
  _seedHits.push_back(hit);
  //Recursively Extend track:
  Bool_t go=kTRUE;
  while ( go && _seedHits.size()<(UInt_t) StiLocalTrackSeedFinderParameters::instance()->seedLength()) {
    go = ExtendHit( *_seedHits.back() );
  }
  //Extension failed if current track length less than StiLocalTrackSeedFinderParameters::instance()->seedLength()
  if ( _seedHits.size()<(UInt_t)StiLocalTrackSeedFinderParameters::instance()->seedLength() ) return track;
  //now use straight line propogation to recursively Extend
  _skipped = 0;
  go=kTRUE;
  while ( go && _skipped<=StiLocalTrackSeedFinderParameters::instance()->maxSkipped() && 
	  _seedHits.size()<=(UInt_t)( StiLocalTrackSeedFinderParameters::instance()->seedLength()+
				      StiLocalTrackSeedFinderParameters::instance()->extrapMaxLength()) ) {
    go = Extrapolate();
  }
  //Extension failed if current track length less than StiLocalTrackSeedFinderParameters::instance()->seedLength()+
  //StiLocalTrackSeedFinderParameters::instance()->extrapMinLength()
  //Return 0.
  if ( _seedHits.size()<(UInt_t)( StiLocalTrackSeedFinderParameters::instance()->seedLength()+
				  StiLocalTrackSeedFinderParameters::instance()->extrapMinLength()) ) return track;
  track = InitializeTrack();
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

   We try to Extrapolate the segment (pt1->pt2) to predict pt3.  We
   do this in two projections (r,y) and (r,z) where y is the distance
   along pad, z is the global z, and r is the inward pointing sector
   normal (i.e., StiPlacement->NormalRadius() )

   In the r,y plane, e.g., we define r = m*y + b s.t.
   m = (r2-r1) / (y2-y1)
   b = r2 - m2
   then
   y3 = (r3 - b) / m, which is our prediction, since we know r3
   
*/
//______________________________________________________________________________
Bool_t StiLocalTrackSeedFinder::Extrapolate() {
  //This is a little ugly, but it's faster than manipulating 3-vectors
  //cout <<"StiLocalTrackSeedFinder::Extrapolate()"<<endl;
  //Calculate slope and offset in r-z and r-y projections, then Extend
  const StiHit* hit1 = *( _seedHits.begin()+_seedHits.size() -2 ); //second to last
  const StiHit* hit2 = _seedHits.back();

  //Get the next detector plane:
    
  double r1 = hit1->Detector()->NormalRadius();
  double y1 = hit1->y();
  double z1 = hit1->z();
  double r2 = hit2->Detector()->NormalRadius();
  double y2 = hit2->y();
  double z2 = hit2->z();

  double dr = r2-r1;
  double dy = y2-y1;
  double dz = z2-z1;
  if (fabs(dr) <1.e-3) //// || dy==0. || dz==0.) 
    throw logic_error("StiLocalTrackSeedFinder::Extrapolate() -E- Seed aborted because dr==0 ");
  //Now look for a hit in the next layer in:
  //Test to see if move in worked
  for (Int_t i=0; i<=_skipped; ++i) {
    if (StiToolkit::instance()->DetectorContainer()->MoveIn()==kFALSE) return kFALSE;
  }

  const StiDetector* newLayer = **StiToolkit::instance()->DetectorContainer();
  double r3 = newLayer->NormalRadius();
  
  //Temp hack by Mike
//VP  if (r3<=60.) { return kFALSE; }
  if (r3<=25.) { return kFALSE; } //VP avoid SVT from seed
    
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
    cout<<"StiLocalTrackSeedFidner::Extrapolate(). -W- tanplus_ry==0. || tanminus_ry==0."<<endl;
  
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
    cout<<"StiLocalTrackSeedFidner::Extrapolate(). -W- tanplus_rz==0. || tanminus_rz==0."<<endl;
  double z3_minus = (r3-r1)/tanplus_rz  + z1;
  double z3_plus =  (r3-r1)/tanminus_rz + z1;
  /*
    cout<<"beta_ry: "<<beta_ry<<" alpha_ry: "<<alpha_ry
    <<" y3+: "<<y3_plus<<" y3: "<<y3<<" y3-: "<<y3_minus<<endl
    <<"beta_rz: "<<beta_rz<<" alpha_rz: "<<alpha_rz
    <<" z3+: "<<z3_plus<<" z3: "<<z3<<" z3-: "<<z3_minus<<endl
    <<"query hit container for extension hits"<<endl;
  */
  StiHit* closestHit = StiToolkit::instance()->HitContainer()->NearestHit(newLayer,y3,z3,  
						    fabs(y3_plus-y3_minus) /2.,
						    fabs(z3_plus-z3_minus) /2.);
  if ( !closestHit ) 
    {
      //cout<<"StiLocalTrackSeedFinder::Extrapolate() -W- No hits found in next layer."<<endl;
      ++_skipped;
      return kTRUE;
    }
  _skipped=0;
  _seedHits.push_back(closestHit);
  return kTRUE;
}


//______________________________________________________________________________
/// Initialize a kalman track on the basis of hits held in _seedHits
/// 
StiKalmanTrack* StiLocalTrackSeedFinder::InitializeTrack() {
  StiKalmanTrack* track = StiToolkit::instance()->TrackFactory()->getInstance();
  Bool_t status = track->Initialize(_seedHits);
  track->SetId(StiToolkit::instance()->TrackContainer()->size()+1);
  if (status) track = 0;
  return track;
}
//______________________________________________________________________________
void StiLocalTrackSeedFinder::print() const {
  cout <<"StiLocalTrackSeedFinder::print() -I- ";
  for (vector<StiDetector*>::const_iterator it=StiToolkit::instance()->DetectorContainer()->begin(); 
       it!=StiToolkit::instance()->DetectorContainer()->end();  ++it)     {
    cout << **it <<endl;
  }
  cout <<"\n Search Window in Y:\t"<<StiLocalTrackSeedFinderParameters::instance()->deltaY()<<endl;
  cout <<"\n Search Window in Z:\t"<<StiLocalTrackSeedFinderParameters::instance()->deltaZ()<<endl;
}
