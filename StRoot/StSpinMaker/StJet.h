//////////////////////////////////////////////////////////////////////
//
// $Id: StJet.h,v 1.7 2003/09/23 19:37:02 thenry Exp $
// $Log: StJet.h,v $
// Revision 1.7  2003/09/23 19:37:02  thenry
// Fixed et and ez (again) correctly this time.
//
// Revision 1.6  2003/09/22 22:09:28  thenry
// Fixed formula for Et and Ez.
//
// Revision 1.5  2003/09/02 17:59:01  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.4  2003/07/17 23:47:11  akio
// bug fix. Thanks Dylan
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
// Revision 1.0  2002/01/25 Akio Ogawa
// First Version of StJet 
//
//////////////////////////////////////////////////////////////////////
//
// StJet
//
// Event class for a Jet
//
//////////////////////////////////////////////////////////////////////
#ifndef StJet_h
#define StJet_h

#include "TObject.h"
#include "TLorentzVector.h"
#include <cmath>
class StEtCell;

class StJet : public TLorentzVector {

public:  
    StJet() : TLorentzVector(0,0,0,0), nCell(0), charge(0) {};
    StJet(double lE, double lpx, double lpy, double lpz, Int_t size, int c)
	: TLorentzVector(lpx, lpy, lpz, lE), nCell(size), charge(c) {};
    virtual ~StJet();

    void  add(StEtCell *);
    
    Int_t        nCell;
    int     charge;
    
    Float_t      et() const {return E()*sqrt(1.0-tanh(Eta())*tanh(Eta()));}
    Float_t      ez() const {return E()*fabs(tanh(Eta()));}
    ClassDef(StJet,3)
};

#endif
