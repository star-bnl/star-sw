/***************************************************************************
 *
 * $Id: StDstEventSummary.h,v 1.2 1999/02/09 20:00:47 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDstEventSummary.h,v $
 * Revision 1.2  1999/02/09 20:00:47  fisyak
 * Import new Torre staff
 *
 * Revision 1.4  1999/04/28 22:27:30  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.1  1999/01/15 22:53:34  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifdef __ROOT__
#include "TObject.h"
#endif
#define StDstEventSummary_hh

class StDstEventSummary : public TObject {
#include "StTHDefs.h"

class StDstEventSummary : public StObject {
public:
    
    ~StDstEventSummary();
    // StDstEventSummary(const StDstEventSummary &right);   use default
    
    StVecTH1F& histograms1D();
#endif    
protected:
#ifdef __ROOT__
	ClassDef(StDstEventSummary,1)  //StDstEventSummary structure
#endif
    StVecTH2F mHistograms2D;

  ClassDef(StDstEventSummary,1)  //StDstEventSummary structure
};
#if 0


inline StVecTH2F& StDstEventSummary::histograms2D() { return mHistograms2D; }
#endif 
#endif
