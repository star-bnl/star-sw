//////////////////////////////////////////////////////////////////////
//
// $Id: StppTrack.cxx,v 1.4 2003/09/11 18:14:18 thenry Exp $
// $Log: StppTrack.cxx,v $
// Revision 1.4  2003/09/11 18:14:18  thenry
// *** empty log message ***
//
// Revision 1.3  2003/09/02 17:59:01  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.2  2002/12/04 20:28:09  thenry
// StppuDstMaker was modified to allow multiple jet analysis modules to be
// run simultaneosly with various parameters while the Maker loads the events
// and analyses them.  Four different jet analyzers exist:
//
// Konstanin's Analyzers:
//     Kt type: StppKonstKtJetAnalyzer
//     Cone type: StppKonstConeJetAnalyzer
//
// Mike's Analyzers:
//     Kt type: StppMikeKtJetAnalyzer
//     Cone type: StppMikeConeJetAnalyzer
//
// These modules all require the StJetFinder modules.
//
// Revision 1.1  2002/01/16 20:22:53  akio
// First version
//
//
// Revision 1.0  2001/06/14 Akio Ogawa
// First Version of StppTrack 
//
//////////////////////////////////////////////////////////////////////
//
// StppTrack
//
// Light weighted Track class for Spin pp uDst
//
//////////////////////////////////////////////////////////////////////
#include <iostream>
#include <math.h>

#include "StppTrack.h"
#include "StEventTypes.h"
#include "BetheBloch.h"
#include "StTrack.h"
#include "StTrackGeometry.h"

ClassImp(StppTrack)

    StppTrack::StppTrack() {
    flag=-999;
    key =-999;
    charge=-999;
    pt=-999.0;
    p=-999.0;
    eta=-999.0;
    psi=-999.0; 
    phi0=-999.0;
    r0=-999.0;
    z0=-999.0;
    nHits=-999;
    nHitsMax=-999;
    chi2=-999.0;
    vertexDCAx=-999.0;
    vertexDCAy=-999.0;
    vertexDCAz=-999.0;
    dedx=-999.0;
  
    jetIndex = -1;
    for(int i = 0; i < MAXANALYZERS; i++)
        anaJetIndex[i] = -1;
  
};

StppTrack::~StppTrack(){};

#ifndef __CINT__

StppTrack::StppTrack(StTrack* track) {fill(track);};

void StppTrack::fill(StTrack* trk){

    //clear these counters first
    jetIndex = -1;
    for(int i = 0; i < MAXANALYZERS; i++)
        anaJetIndex[i] = -1;

    key     = trk->key() ;
    flag    = trk->flag() ;
    charge  = trk->geometry()->charge();
    pt      = trk->geometry()->momentum().perp();
    p       = trk->geometry()->momentum().mag();
    psi     = trk->geometry()->momentum().phi();
    eta     = -::log(tan(trk->geometry()->momentum().theta()/2.));
    phi0    = trk->geometry()->origin().phi(); 
    z0      = trk->geometry()->origin().z(); 
    r0      = trk->geometry()->origin().perp(); 
    // to be implemented!
    vertexDCAx = 0.0; 
    vertexDCAy = 0.0; 
    vertexDCAz = 0.0; 

    dedx   = 0.0;
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
	}
    }

    //  cout << "***akio " << nHits << " " << trk->fitTraits().numberOfFitPoints()
    //     << " " <<  trk->numberOfPossiblePoints() << " " << trk->fitTraits().chi2() 
    //     << " " << trk->flag() << endl;
    if(trk->fitTraits().numberOfFitPoints()>0){
	nHits = trk->fitTraits().numberOfFitPoints();
	chi2 = trk->fitTraits().chi2();
    }else{    
	nHits = trk->detectorInfo()->numberOfPoints() ;
    }
    nHitsMax = trk->numberOfPossiblePoints();
}

StTrack* StppTrack::getStTrack(){
    //  StPrimaryTrack *trk = new StPrimaryTrack;
    //StTrackGeometry *geom = new StTrackGeometry;
    //trk->setFlag((short)flag);
    //trk.key = (unsigned short) key;
    //trk->setGeometry(geom);
    //geom.charge() = charge;
    //to be implemented!
    return 0;
}

Float_t StppTrack::getZdEdx(Float_t mass) {
    static BetheBloch bb;  
    if (!mass ) { return -9999; }
    Double_t betaGamma = p / mass ;
    Double_t dedxBB = bb(betaGamma);
    if (p && dedx && dedxBB) {
	return log (dedx/dedxBB);
    } else {
	return -9999.;
    }
}

#endif /* __CINT__ */
