//////////////////////////////////////////////////////////////////////
//
// $Id: StJet.cxx,v 1.4 2003/09/02 17:59:01 perev Exp $
// $Log: StJet.cxx,v $
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
#include <Stiostream.h>
#include <stdio.h>
#include "StEventTypes.h"
#include "StEvent.h"
#include "StJet.h"
#include "StEtCell.h"

ClassImp(StJet)

StJet::~StJet() {
} 

void StJet::add(StEtCell* cell){
  float oldet   = et();
  float oldex   = oldet*cos(Phi());
  float oldey   = oldet*sin(Phi());
  float etCell  = cell->et;
  float etaCell = cell->eta();
  float phiCell = cell->phi();
  
  float et = oldet + etCell;
  float ex = oldex + cos(phiCell)*etCell;
  float ey = oldey + sin(phiCell)*etCell;
  float lpt = ::sqrt(et*et-Mt2());
  float leta = (Eta()*oldet + etaCell*etCell) /et;
  float lphi = (float)atan2((double)ey,(double)ex);

  SetPtEtaPhiM(lpt, leta, lphi, M());  
  nCell++;
  cell->flipSign();
}
