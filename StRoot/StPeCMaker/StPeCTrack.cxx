//////////////////////////////////////////////////////////////////////
//
// Revision 1.0  2000/12/15           Pablo Yepes: yepes@rice.edu
//
//////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include "StPeCTrack.h"
#include "StEventTypes.h"
// Temp fix to avoid Yuris new version
// use this for P00hm production 
// #include "BetheBloch.h"
#include "myBetheBloch.h"

#include <math.h>
ClassImp(StPeCTrack)

StPeCTrack::StPeCTrack() {
}

StPeCTrack::~StPeCTrack() {
}

#ifndef __CINT__
StPeCTrack::StPeCTrack ( Int_t _primary, StTrack* trk) {
   set ( _primary, trk ) ;
}

StPeCTrack::StPeCTrack(Int_t _primary, StMuTrack* trk)
{
   set (_primary, trk);
}

void StPeCTrack::set ( Int_t _primary, StTrack* trk) {
  key     = trk->key() ;
  primary = _primary ;
  charge  = trk->geometry()->charge();
  pt      = trk->geometry()->momentum().perp();
  p      = trk->geometry()->momentum().mag();
  psi     = trk->geometry()->momentum().phi();
  eta     = -::log(tan(trk->geometry()->momentum().theta()/2.));
  phi0    = trk->geometry()->origin().phi(); 
  z0      = trk->geometry()->origin().z(); 
  r0      = trk->geometry()->origin().perp(); 


  //   cout << "Flag: " << trk->flag() << " primary " << _primary << endl;
  dedx   = 0. ;
  dedxZel         = -9999. ;  
  dedxZmu         = -9999. ;
  dedxZpi         = -9999. ;
  dedxZk          = -9999. ;
  dedxZp          = -9999. ;
  StSPtrVecTrackPidTraits& traits =  trk->pidTraits();
  if ( &traits ) {
    StDedxPidTraits *dedxPid = 0 ;
     Int_t NTraits = traits.size();
     for( Int_t  i=0; i<NTraits; i++) {
        if ( traits[i]->detector() == kTpcId ){
           dedxPid = dynamic_cast<StDedxPidTraits*>(traits[i]);
           if ( dedxPid && dedxPid->method() == kTruncatedMeanIdentifier )break;
        }
     }
     if ( dedxPid ){
       dedx = dedxPid->mean(); 
       dedxZel       = getZdEdx(mMassElectron);
       dedxZmu       = getZdEdx(mMassMuon);
       dedxZpi       = getZdEdx(mMassPion);
       dedxZk        = getZdEdx(mMassKaon);
       dedxZp        = getZdEdx(mMassProton);
	
     }
     nHits = trk->detectorInfo()->numberOfPoints() ;
  }
//printf ( "pt psi eta r0 phi0 z0 nHits %f %f %f %f %f %f %f \n",
//   pt, psi, eta, r0, phi0, z0, nHits ) ;


}

void StPeCTrack::set(Int_t _primary, StMuTrack* trk)
{
   key = trk->id();
   primary = _primary;
   charge = trk->charge();
   pt = trk->momentum().perp();
   p = trk->momentum().mag();
   psi = trk->momentum().phi();
   eta = -::log(tan(trk->momentum().theta()/2.));
   phi0 = trk->firstPoint().phi();
   z0 = trk->firstPoint().z(); 
   r0 = trk->firstPoint().perp(); 


   dedx = 0.;
   dedxZel = -9999.;  
   dedxZmu = -9999.;
   dedxZpi = -9999.;
   dedxZk = -9999.;
   dedxZp = -9999.;


 
   dedx = trk->dEdx(); 
   dedxZel = trk->pidProbElectron();
   dedxZpi = trk->pidProbPion();
   dedxZk = trk->pidProbKaon();
   dedxZp = trk->pidProbProton();
   nHits = trk->nHits();

// printf ( "pt psi eta r0 phi0 z0 nHits %f %f %f %f %f %f %f \n",
//pt, psi, eta, r0, phi0, z0, nHits ) ;



   return;
}

// Private Helper 
Float_t StPeCTrack::getZdEdx(Float_t mass) {
  static myBetheBloch bb;
  
  if ( ! mass ) { return -9999; }
  Double_t betaGamma = p / mass ;
  Double_t dedxBB = bb(betaGamma);
  if (p && dedx && dedxBB ) {
    return log ( dedx/dedxBB);
  } else {
    // cout << "dEdx Pion F*** Up ! p " << p << " dedx "
    //	 <<dedx << " dedxBB " << dedxBB << endl;
    return -9999.;
  }
}

#endif /*__CINT__*/




