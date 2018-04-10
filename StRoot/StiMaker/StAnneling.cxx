// $Id: StAnneling.cxx,v 2.2 2018/04/10 11:32:09 smirnovd Exp $
#include "StAnneling.h"
Double_t StAnneling::fChi2Cut     = 12.25; // 13.81551055; // Prob = 1e-3
Double_t StAnneling::fTemperature = 1; 
// $Log: StAnneling.cxx,v $
// Revision 2.2  2018/04/10 11:32:09  smirnovd
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
// Revision 2.1  2012/05/07 14:56:14  fisyak
// Add StKFVertexMaker
//
// Revision 1.2  2012/02/07 19:38:26  fisyak
// Repackage
//
