// $Id: StAnneling.cxx,v 2.2 2015/12/20 01:06:39 fisyak Exp $
#include "StAnneling.h"
Double_t StAnneling::fChi2CutUniq = 13.81551055; // Prob = 1e-3
Double_t StAnneling::fChi2Cut     = 32.2;        // Prob = 1e-7
Double_t StAnneling::fTemperature = 1;  
ClassImp(StAnneling);
// $Log: StAnneling.cxx,v $
// Revision 2.2  2015/12/20 01:06:39  fisyak
// Merge
//
// Revision 2.2  2015/01/05 21:04:31  fisyak
// Add access to TMVA ranking
//
// Revision 2.1  2012/05/07 14:56:14  fisyak
// Add StKFVertexMaker
//
// Revision 1.2  2012/02/07 19:38:26  fisyak
// Repackage
//
