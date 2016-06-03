// $Id: StKFEvent.cxx,v 2.5.2.1 2016/06/03 15:48:59 smirnovd Exp $
#include "StKFEvent.h"
ClassImp(StKFEvent);
TClonesArray *StKFEvent::fgMuDstVtx = 0;
TClonesArray *StKFEvent::fgKFVtx = 0;
TClonesArray *StKFEvent::fgDKFPair = 0;
TClonesArray *StKFEvent::fgKFKFPair = 0;
// $Log: StKFEvent.cxx,v $
// Revision 2.5.2.1  2016/06/03 15:48:59  smirnovd
// Revert "Squashed commit of the following:"
//
// This reverts commit b0c5699a781ed8e5724e065390d3870af5de5b7c.
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
