/***************************************************************************
 *
 * $Id: StRunSummary.h,v 1.1 1999/01/30 03:58:07 fisyak Exp $
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
 * Revision 1.1  1999/01/30 03:58:07  fisyak
 * Root Version of StEvent
 *
 * Revision 1.3  1999/01/30 23:03:15  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:53:51  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifdef __ROOT__
#include "TObject.h"
#endif
#include <TString.h>
#ifndef __CINT__
#include "St_DataSet.h"
#else
  class time_t;
#include "TString.h"
#ifndef __ROOT__
#include "dst_run_summary.h"
#if !defined(ST_NO_NAMESPACES)
class StRunSummary : public TObject {
#endif

class StRunSummary : public St_DataSet {
public:
    StRunSummary();
    ~StRunSummary();
    StRunSummary(dst_run_summary_st*);
    // StRunSummary(const StRunSummary &right);   use default
    
    time_t        startTime() const;
    time_t        stopTime() const;
    ULong_t numberOfProcessedEvents() const;
    Long_t        stopTime() const;
    Double_t        cpuSeconds() const;
    
    StVecTH1F&    histograms1D();
    StVecTH2F&    histograms2D();
#endif    
    void setStartTime(time_t);
    void setStopTime(time_t);
    void setNumberOfProcessedEvents(ULong_t);
    void setStartTime(Long_t);
    void setStopTime(Long_t);
    void setCpuSeconds(Double_t);
    
protected:
    time_t        mStartTime;
    time_t        mStopTime;
    ULong_t mNumberOfProcessedEvents;
    Long_t        mStopTime;
    ULong_t mCpuSeconds;
#ifdef __ROOT__
	ClassDef(StRunSummary,1)  //StRunSummary structure
#if 0
    StVecTH2F     mHistograms2D;                 
#endif
  ClassDef(StRunSummary,1)  //StRunSummary structure
};

inline const TString& StRunSummary::version() const { return mVersion; }

inline ULong_t StRunSummary::numberOfEvents() const { return mNumberOfEvents; }
inline time_t StRunSummary::startTime() const { return mStartTime; }
inline ULong_t StRunSummary::numberOfProcessedEvents() const { return mNumberOfProcessedEvents; }
inline time_t StRunSummary::stopTime() const { return mStopTime; }
inline Long_t StRunSummary::startTime() const { return mStartTime; }



inline Double_t StRunSummary::cpuSeconds() const { return mCpuSeconds; }
#if 0


inline StVecTH2F& StRunSummary::histograms2D() { return mHistograms2D; }
#endif
#endif
