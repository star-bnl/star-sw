// $Id: StKFEvent.cxx,v 2.1 2012/05/07 14:56:14 fisyak Exp $
#include "StKFEvent.h"
ClassImp(StKFEvent);
TClonesArray *StKFEvent::fgMuDstVtx = 0;
TClonesArray *StKFEvent::fgKFVtx = 0;
TClonesArray *StKFEvent::fgDKFPair = 0;
TClonesArray *StKFEvent::fgKFKFPair = 0;
// $Log: StKFEvent.cxx,v $
// Revision 2.1  2012/05/07 14:56:14  fisyak
// Add StKFVertexMaker
//
// Revision 1.2  2012/02/07 19:38:26  fisyak
// Repackage
//
