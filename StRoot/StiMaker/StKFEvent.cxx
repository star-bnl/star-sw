// $Id: StKFEvent.cxx,v 2.5 2015/12/20 01:35:12 fisyak Exp $
#include "StKFEvent.h"
ClassImp(StKFEvent);
TClonesArray *StKFEvent::fgMuDstVtx = 0;
TClonesArray *StKFEvent::fgKFVtx = 0;
TClonesArray *StKFEvent::fgDKFPair = 0;
TClonesArray *StKFEvent::fgKFKFPair = 0;
// $Log: StKFEvent.cxx,v $
// Revision 2.5  2015/12/20 01:35:12  fisyak
// Move back commits done by mistate
//
// Revision 2.3  2013/04/10 22:14:20  fisyak
// Roll back to version 04/04/2013
//
// Revision 2.1  2012/05/07 14:56:14  fisyak
// Add StKFVertexMaker
//
// Revision 1.2  2012/02/07 19:38:26  fisyak
// Repackage
//
