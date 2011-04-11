#include "CtbMatching.h"

#include "StMuLcp2TreeMaker.h"

#include "StCtbTriggerDetector.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StThreeVectorD.hh"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "TMath.h"

//________________________________________________
//________________________________________________
CtbMatching::CtbMatching() {
  ctbHits= new vector<ctbHit>();
  // define matching tolerance
  etaToll=0.; // was 0.1
  phiToll=0;  // (deg) was 1
  
}
//________________________________________________
//________________________________________________
void CtbMatching::loadHits(StMuEvent* muEve) {
 
  ctbHits->clear();

  StCtbTriggerDetector* ctbDet = &(muEve->ctbTriggerDetector());
  
  assert(ctbDet);
  float ctbSum = 0;
  for (UInt_t slat = 0; slat < ctbDet->numberOfSlats(); slat++) {
    for (UInt_t tray = 0; tray < ctbDet->numberOfTrays(); tray++) {
      ctbHit curHit;
      curHit.adc = ctbDet->mips(tray,slat,0);
      if(curHit.adc > 0){
	ctbSum += curHit.adc;
	ctb_get_slat_from_data(slat, tray, curHit.phi, curHit.eta);
	ctbHits->push_back(curHit);
	//	printf("CTB Hit  ADC=%f sum=%f\n",curHit.adc, ctbSum);
      }
    }
  }
  //printf("nCtbHit=%d, CtbSum=%f\n", ctbHits->size(),ctbSum);
}

  

//StCtbTriggerDetector& ctbDet=muEve->ctbTriggerDetector();

//________________________________________________
//________________________________________________
void CtbMatching::ctb_get_slat_from_data(int slat, int tray, double & ctbphi, double & ctbeta) {
    // Function copied verbatum from St_dst_Maker/CtbResponce. We may want to clean this up.
    
    float phiZero1 = 72 ; // magic lines from Pablo & Herb
    float phiZero2 = 108 ;
    float deltaPhi = 6 ;
    // tray=0,1
    // slat=0,...,119
    
    int iz ;	    
    double phi,eta ;
    
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
    eta = (1-2*iz)*(1+2*slat)*0.25;
    ctbeta =(1-2*iz)*(1+2*slat)*0.25;
    
    //printf("CTB hit: slat=%d, tray=%d,  phiDeg=%f/deg, eta=%f\n",slat,tray,ctbphi,ctbeta);
        
} 


//________________________________________________
//________________________________________________
unsigned int CtbMatching::match(const StMuTrack* rTrack) {
  
  // not used output from Jon
  bool  shouldHit;
  double  trackEtaInCTBFrame;
  double  trackPhiInCTBFrame;
  
  // the code
  const double Rctb=213.6; // (cm) radius of the CTB
  
  /*
    if(rTrack->geometry()->momentum().perp() < .20)
    return false;
  */

  StPhysicalHelixD theHelix = rTrack->helix();
  pairD pathLength = theHelix.pathLength(Rctb);
  
  if(pathLength.second < 0){
    cout << "pathLength is bad" << endl;
    return false;
  }
  
  const StThreeVectorD &pos = theHelix.at(pathLength.second);
  
  double phi = atan2(pos.y(),pos.x())*180/TMath::Pi();
  if(phi < 0)  phi+= 360;
  
  trackPhiInCTBFrame = phi;
  
  double theta = atan2(pos.perp(),pos.z());
  double eta = -log(tan(theta/2.0));
  trackEtaInCTBFrame = eta;
  
  // if |eta| < 1 then track should have hit the CTB.
  if(fabs(eta) < 1.+etaToll)
    shouldHit = true;
  else
    shouldHit = false;
  
  // Loop over CTB Hits To see if a match
  float deta,dphi;
  for(unsigned int i = 0; i < ctbHits->size() ; i++){
    ctbHit curHit = (*ctbHits)[i];
    deta = fabs(curHit.eta - eta);
    dphi = fabs(curHit.phi - phi);
    if(dphi>180) dphi=360.-dphi;
    if( dphi < (3.+phiToll) && deta < (.25+etaToll)){
      return static_cast<unsigned int>(curHit.adc);
    }
  }
  return 0;
}

#if 0
if(rTrack){
		    globalTrackJG* glTrack = new globalTrackJG();    
		    bool shouldHitCTB = false;
		    double etaInCTBFrame = -999;
		    double phiInCTBFrame = -999;
		    
		    glTrack->matchToCTB = EtaAndPhiToOrriginAtCTB(rTrack,ctbHits,shouldHitCTB,etaInCTBFrame,phiInCTBFrame);
		    glTrack->shouldMatchToCTB = shouldHitCTB;
		    glTrack->etaInCTBFrame = etaInCTBFrame;
		    glTrack->phiInCTBFrame = phiInCTBFrame;
		    
		

 unsigned int matchToCTB = EtaAndPhiToOrriginAtCTB(rTrack,ctbHits,shouldHitCTB,etaInCTBFrame,phiInCTBFrame);
       // rTrack is StTrack/StMuTrack
       // ctbHits is above vector
       // shouldHitCTB will be a boolean if track is withing
       // etaInCTBFrame, phiInCTBFrame is the phi/eta of where the track hits the CTB with the origin at 0,0,
#endif
