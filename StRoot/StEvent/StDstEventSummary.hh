/***************************************************************************
 *
 * $Id: StDstEventSummary.hh,v 1.1 1999/01/15 22:53:34 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDstEventSummary.hh,v $
 * Revision 1.1  1999/01/15 22:53:34  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StDstEventSummary_hh
#define StDstEventSummary_hh

#include "StEvent/StTHDefs.hh"

class StDstEventSummary {
public:
    StDstEventSummary();
    ~StDstEventSummary();
    // StDstEventSummary(const StDstEventSummary &right);   use default
    
    StVecTH1F& histograms1D();
    StVecTH2F& histograms2D();
    
protected:
    StVecTH1F mHistograms1D;
    StVecTH2F mHistograms2D;
};

inline StVecTH1F& StDstEventSummary::histograms1D() { return mHistograms1D; }

inline StVecTH2F& StDstEventSummary::histograms2D() { return mHistograms2D; }

#endif
