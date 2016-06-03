// $Id: StAnneling.cxx,v 2.1.8.2 2016/06/03 16:07:14 smirnovd Exp $
#include "StAnneling.h"
Double_t StAnneling::fChi2Cut     = 12.25; // 13.81551055; // Prob = 1e-3
Double_t StAnneling::fTemperature = 1; 
ClassImp(StAnneling);
// $Log: StAnneling.cxx,v $
// Revision 2.1.8.2  2016/06/03 16:07:14  smirnovd
// Sync with MAIN branch as of 2016-05-31
//
// Revision 2.1  2012/05/07 14:56:14  fisyak
// Add StKFVertexMaker
//
// Revision 1.2  2012/02/07 19:38:26  fisyak
// Repackage
//
