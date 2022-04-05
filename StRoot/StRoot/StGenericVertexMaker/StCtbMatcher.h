#ifndef ST_CTB_MATCHER_H
#define ST_CTB_MATCHER_H

#include <vector>
#include "StEvent/StTrack.h"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StThreeVectorD.hh"
#include "TMath.h"
struct ctbHit{
    double phi;
    double eta;
    float adc;
};


unsigned int EtaAndPhiToOrriginAtCTB(StTrack* rTrack, // Pointer to Track
				     vector<ctbHit>* ctbHits,
				     bool & shouldHit,
				     double & trackEtaInCTBFrame){
        
    const double Rctb=213.6; // (cm) radius of the CTB

    // Set Minimum pt to look at
    if(rTrack->geometry()->momentum().perp() < .20)
	return false;

    StPhysicalHelixD theHelix = rTrack->geometry()->helix();
    pairD pathLength = theHelix.pathLength(Rctb);
    
    if(pathLength.second < 0){
	cout << "pathLength is bad" << endl;
	return false;
    }
    
    StThreeVectorD pos = theHelix.at(pathLength.second);
    
    double phi = atan2(pos.y(),pos.x())*180/TMath::Pi();
    if(phi < 0)
	phi+= 360;
    
    double theta = atan2(pos.perp(),pos.z());
    double eta = -::log(tan(theta/2.0));
    trackEtaInCTBFrame = eta;
    
    // if |eta| < 1 then track should have hit the CTB.
    if(fabs(eta) < 1)
	shouldHit = true;
    else
	shouldHit = false;

    // Loop over CTB Hits To see if a match
    float deta,dphi;
    for(unsigned int i = 0; i < ctbHits->size() ; i++){
	ctbHit curHit = (*ctbHits)[i];
	deta = fabs(curHit.eta - eta);
	if(curHit.phi != 0){
	    dphi = fabs(curHit.phi - phi);
	    if( dphi < 3 && deta < .25){
		return static_cast<unsigned int>(curHit.adc);
	    }
	}
	else{
	    if( (phi < 3 || phi > 357) && deta < .25){
		return static_cast<unsigned int>(curHit.adc);
	    }
	}
    }
    // cout << endl;
    // Else return 0
    return 0;
}
   
void  ctb_get_slat_from_data(int slat, int tray, double & ctbphi, double & ctbeta) {
    // Function copied verbatum from St_dst_Maker/CtbResponce. We may want to clean this up.
    
    float phiZero1 = 72 ; // magic lines from Pablo & Herb
    float phiZero2 = 108 ;
    float deltaPhi = 6 ;
    // tray=0,1
    // slat=0,...,119
    
    int iz ;	    
    double phi;
    
    if ( tray < 60 )  {
	phi = phiZero1 - tray * deltaPhi ; 
	iz = 0 ;
  }  else {
      phi = phiZero2 + (tray-60) * deltaPhi ;
      iz = 1 ;
  }

    if ( phi <   0. ) phi += 360 ;
    if ( phi > 360. ) phi -= 360 ;
    
    ctbphi=phi;
    ctbeta =(1-2*iz)*(1+2*slat)*0.25;
    
    //printf("CTB hit: slat=%d, tray=%d,  phiDeg=%f/deg, eta=%f\n",slat,tray,ctbphi,ctbeta);
        
} 

#endif
