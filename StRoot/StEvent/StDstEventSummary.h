/***************************************************************************
 *
 * $Id: StDstEventSummary.h,v 1.5 1999/04/30 13:16:27 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDstEventSummary.h,v $
 * Revision 1.5  1999/04/30 13:16:27  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.5  1999/04/30 13:16:27  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.4  1999/04/28 22:27:30  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.1  1999/01/15 22:53:34  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StDstEventSummary_hh
#define StDstEventSummary_hh

#include "StObject.h"
#include "StTHDefs.h"

class StDstEventSummary : public StObject {
public:
    StDstEventSummary();
    ~StDstEventSummary();
    // StDstEventSummary(const StDstEventSummary &right);   use default
#if 0    
    StVecTH1F& histograms1D();
    StVecTH2F& histograms2D();
#endif    
protected:
#if 0
    StVecTH1F mHistograms1D;
    StVecTH2F mHistograms2D;
#endif 
  ClassDef(StDstEventSummary,1)  //StDstEventSummary structure
};
#if 0
inline StVecTH1F& StDstEventSummary::histograms1D() { return mHistograms1D; }

inline StVecTH2F& StDstEventSummary::histograms2D() { return mHistograms2D; }
#endif 
#endif
