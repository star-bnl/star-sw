// $Id: StKFEvent.cxx,v 2.6 2018/04/10 11:32:09 smirnovd Exp $
#include "StKFEvent.h"
TClonesArray *StKFEvent::fgMuDstVtx = 0;
TClonesArray *StKFEvent::fgKFVtx = 0;
TClonesArray *StKFEvent::fgDKFPair = 0;
TClonesArray *StKFEvent::fgKFKFPair = 0;
// $Log: StKFEvent.cxx,v $
// Revision 2.6  2018/04/10 11:32:09  smirnovd
// Minor corrections across multiple files
//
// - Remove ClassImp macro
// - Change white space
// - Correct windows newlines to unix
// - Remove unused debugging
// - Correct StTpcRTSHitMaker header guard
// - Remove unused preprocessor directives in StiCA
// - Minor changes in status and debug print out
// - Remove using std namespace from StiKalmanTrackFinder
// - Remove includes for unused headers
//
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
