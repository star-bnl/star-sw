/***************************************************************************
 *
 * $Id: StRunSummary.h,v 1.6 1999/09/24 01:23:01 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 15/01/1999 T. Wenaus  Add table-based constructor
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRunSummary.h,v $
 * Revision 1.6  1999/09/24 01:23:01  fisyak
 * Reduced Include Path
 *
 * Revision 1.6  1999/09/24 01:23:01  fisyak
 * Reduced Include Path
 *
 * Revision 1.5  1999/04/30 13:16:29  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.4  1999/04/28 22:27:35  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.5  1999/03/04 18:17:12  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
 *
 * Revision 1.4  1999/03/04 15:57:00  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.3  1999/01/30 23:03:15  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:53:51  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StRunSummary_hh
#define StRunSummary_hh

#include "St_DataSet.h"
#include "TString.h"
#ifndef __ROOT__
#include <time.h>
#endif
#include "StTHDefs.h"
#include "dst_run_summary.h"
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

class StRunSummary : public St_DataSet {
public:
    StRunSummary();
    ~StRunSummary();
    StRunSummary(dst_run_summary_st*);
    // StRunSummary(const StRunSummary &right);   use default
    
    const TString& version() const;
    ULong_t numberOfEvents() const;
    ULong_t numberOfProcessedEvents() const;
    Long_t        startTime() const;
    Long_t        stopTime() const;
    Double_t        cpuSeconds() const;
#if 0
    StVecTH1F&    histograms1D();
    StVecTH2F&    histograms2D();
#endif    
    void setVersion(const Char_t*);
    void setNumberOfEvents(ULong_t);
    void setNumberOfProcessedEvents(ULong_t);
    void setStartTime(Long_t);
    void setStopTime(Long_t);
    void setCpuSeconds(Double_t);
    
protected:
    TString        mVersion;                   
    ULong_t mNumberOfEvents;
    ULong_t mNumberOfProcessedEvents;
    Long_t        mStartTime;
    Long_t        mStopTime;
    ULong_t mCpuSeconds;
#if 0
    StVecTH1F     mHistograms1D;
    StVecTH2F     mHistograms2D;                 
#endif
  ClassDef(StRunSummary,1)  //StRunSummary structure
};

inline const TString& StRunSummary::version() const { return mVersion; }

inline ULong_t StRunSummary::numberOfEvents() const { return mNumberOfEvents; }

inline ULong_t StRunSummary::numberOfProcessedEvents() const { return mNumberOfProcessedEvents; }

inline Long_t StRunSummary::startTime() const { return mStartTime; }

inline Long_t StRunSummary::stopTime() const { return mStopTime; }

inline Double_t StRunSummary::cpuSeconds() const { return mCpuSeconds; }
#if 0
inline StVecTH1F& StRunSummary::histograms1D() { return mHistograms1D; }

inline StVecTH2F& StRunSummary::histograms2D() { return mHistograms2D; }
#endif
#endif
