/***************************************************************************
 *
 * $Id: StRunSummary.hh,v 1.5 1999/03/04 18:17:12 ullrich Exp $
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
 * $Log: StRunSummary.hh,v $
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

#include <string>
#include <time.h>
#include "StEvent/StTHDefs.hh"
#include "tables/dst_run_summary.h"
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

class StRunSummary {
public:
    StRunSummary();
    ~StRunSummary();
    StRunSummary(dst_run_summary_st*);
    // StRunSummary(const StRunSummary &right);   use default
    
    const string& version() const;
    unsigned long numberOfEvents() const;
    unsigned long numberOfProcessedEvents() const;
    time_t        startTime() const;
    time_t        stopTime() const;
    double        cpuSeconds() const;
    StVecTH1F&    histograms1D();
    StVecTH2F&    histograms2D();
    
    void setVersion(const char*);
    void setNumberOfEvents(unsigned long);
    void setNumberOfProcessedEvents(unsigned long);
    void setStartTime(time_t);
    void setStopTime(time_t);
    void setCpuSeconds(double);
    
protected:
    string        mVersion;                   
    unsigned long mNumberOfEvents;
    unsigned long mNumberOfProcessedEvents;
    time_t        mStartTime;
    time_t        mStopTime;
    unsigned long mCpuSeconds;
    StVecTH1F     mHistograms1D;
    StVecTH2F     mHistograms2D;                 
};

inline const string& StRunSummary::version() const { return mVersion; }

inline unsigned long StRunSummary::numberOfEvents() const { return mNumberOfEvents; }

inline unsigned long StRunSummary::numberOfProcessedEvents() const { return mNumberOfProcessedEvents; }

inline time_t StRunSummary::startTime() const { return mStartTime; }

inline time_t StRunSummary::stopTime() const { return mStopTime; }

inline double StRunSummary::cpuSeconds() const { return mCpuSeconds; }

inline StVecTH1F& StRunSummary::histograms1D() { return mHistograms1D; }

inline StVecTH2F& StRunSummary::histograms2D() { return mHistograms2D; }

#endif
