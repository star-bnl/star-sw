//////////////////////////////////////////////////////////////////////
//
// Revision 1.0  2000/12/15           Pablo Yepes: yepes@rice.edu
//
//////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "StPeCTrack.h"
#include "StEventTypes.h"

ClassImp(StPeCTrack)

StPeCTrack::StPeCTrack() {
}

StPeCTrack::~StPeCTrack() {
}

#ifndef __CINT__
StPeCTrack::StPeCTrack ( Int_t _primary, StTrack* trk) {
   set ( _primary, trk ) ;
}
void StPeCTrack::set ( Int_t _primary, StTrack* trk) {
  key     = trk->key() ;
  primary = _primary ;
  charge  = trk->geometry()->charge();
  pt      = trk->geometry()->momentum().perp();
  psi     = trk->geometry()->momentum().phi();
  eta     = -log(tan(trk->geometry()->momentum().theta()/2.));
  phi0    = trk->geometry()->origin().phi(); 
  z0      = trk->geometry()->origin().z(); 
  r0      = trk->geometry()->origin().perp(); 

  StSPtrVecTrackPidTraits& traits =  trk->pidTraits();
  dedx   = 0. ;
  if ( &traits ) {
     StDedxPidTraits *dedxPid = 0 ;
     Int_t NTraits = traits.size();
     Int_t i ;
     for( i=0; i<NTraits; i++) {
        if ( traits[i]->detector() == kTpcId ){
           dedxPid = dynamic_cast<StDedxPidTraits*>(traits[i]);
           if ( dedxPid && dedxPid->method() == kTruncatedMeanIdentifier )break;
        }
     }
     if ( dedxPid ) dedx = dedxPid->mean(); 
  }
  nHits = trk->detectorInfo()->numberOfPoints() ;
}
#endif /*__CINT__*/




