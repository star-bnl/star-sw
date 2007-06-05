//////////////////////////////////////////////////////////////////////
//
// $Id: StJet.cxx,v 1.2 2007/06/05 21:56:13 kocolosk Exp $
// $Log: StJet.cxx,v $
// Revision 1.2  2007/06/05 21:56:13  kocolosk
// added data members for zVertex and geometric trigger associations, plus methods for detEta (barrel only)
//
// Revision 1.1  2004/07/08 15:41:03  mmiller
// First release of StJetMaker.  Mike's commit of full clean of StJetFinder, StJetMaker, and StSpinMaker.  builds and runs in dev.
//
// Revision 1.5  2003/09/11 18:14:18  thenry
// *** empty log message ***
//
// Revision 1.4  2003/09/02 17:59:01  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.3  2002/12/04 20:28:07  thenry
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
// Revision 1.2  2002/06/24 13:22:59  akio
// numerous bug fix & updates
//
// Revision 1.1  2002/02/11 20:30:48  akio
// Many updates, including very first version of jet finder.
//
//
// Revision 1.0  2001/06/14 Akio Ogawa
//
//////////////////////////////////////////////////////////////////////
#include <iostream>
#include <stdio.h>
#include "StJet.h"

ClassImp(StJet)

StJet::~StJet()
{
}

Float_t StJet::detEta() const {
    if (zVertex > -998) return detEta(zVertex);
    return -999;
}

Float_t StJet::detEta(float vz, float r) const {
    float hold(0.),denom(0.);
    if (Theta()==TMath::PiOver2()) { // if Jet Theta = 90 then tan is undefined
        if (vz==0) {hold = TMath::PiOver2();}
        else {hold = atan2(r,vz);}
    }
    else
    {
        denom = (r/tan(Theta()))+vz;
        if (denom==0.) {hold = TMath::PiOver2();}
        if (denom!=0.) {hold = atan2(r,denom);}
    }
    return -TMath::Log(TMath::Tan(hold/2));
}

void StJet::addGeomTrigger(int trigId) {
    //check if the trigId is already in the vector -- not really necessary
    for(int i=0; i<mGeomTriggers.size(); i++) {
        if (mGeomTriggers[i] == trigId) return;
    }
    mGeomTriggers.push_back(trigId);
}

bool StJet::geomTrigger(int trigId) const {
    for(int i=0; i<mGeomTriggers.size(); i++) {
        if (mGeomTriggers[i] == trigId) return true;
    }
    return false;
}