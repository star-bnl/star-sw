/***************************************************************************
 *
 * $Id: StEventSummary.hh,v 1.1 1999/01/15 20:39:45 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEventSummary.hh,v $
 * Revision 1.1  1999/01/15 20:39:45  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#ifndef StEventSummary_hh
#define StEventSummary_hh

#include "StTHDefs.hh"

class StEventSummary {
public:
    StEventSummary();
    ~StEventSummary();
    // StEventSummary(const StEventSummary &right);   use default
    
    StVecTH1F& histograms1D();
    StVecTH2F& histograms2D();
    
protected:
    StVecTH1F mHistograms1D;
    StVecTH2F mHistograms2D;
};

inline StVecTH1F& StEventSummary::histograms1D() { return mHistograms1D; }

inline StVecTH2F& StEventSummary::histograms2D() { return mHistograms2D; }

#endif
