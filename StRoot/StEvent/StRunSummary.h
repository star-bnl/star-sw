/***************************************************************************
 *
 *
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
 * Revision 2.0  1999/10/12 18:42:40  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StRunSummary_hh
#define StRunSummary_hh

#include <iostream.h>
#include "StObject.h"
#include "StEnumerations.h"
#include "TString.h"
#include "TArrayF.h"

class dst_run_summary_st;

class StRunSummary : public StObject {
public:
    StRunSummary();
    StRunSummary(const dst_run_summary_st&);
    // StRunSummary& operator=(const StRunSummary&); use default
    // StRunSummary(const StRunSummary&);            use default
    virtual ~StRunSummary();
    
    ULong_t  numberOfEvents() const;
    ULong_t  numberOfProcessedEvents() const;
    Long_t   startTime() const;
    Long_t   stopTime() const;
    Float_t  cpuSeconds() const;
    Float_t  averageBeamPolarization(StBeamDirection, StBeamPolarizationAxis) const;
    Float_t  averageLuminosity() const;
    Float_t  meanEta() const;
    Float_t  rmsEta() const;
    Float_t  meanPt() const;
    Float_t  rmsPt() const;
    Float_t  meanNumberOfVertices() const;
    Float_t  rmsNumberOfVertices() const;
    Float_t  meanMultiplicity(StDetectorId) const;
    Float_t  rmsMultiplicity(StDetectorId) const;

    void setNumberOfEvents(ULong_t);
    void setNumberOfProcessedEvents(ULong_t);
    void setStartTime(Long_t);
    void setStopTime(Long_t);
    void setCpuSeconds(Float_t);
    void setAverageBeamPolarization(StBeamDirection, StBeamPolarizationAxis, Float_t);
    void setAverageLuminosity(Float_t);
    void setMeanEta(Float_t);
    void setRmsEta(Float_t);
    void setMeanPt(Float_t);
    void setRmsPt(Float_t);
    void setMeanNumberOfVertices(Float_t);
    void setRmsNumberOfVertices(Float_t);
    void setMeanMultiplicity(StDetectorId, Float_t);
    void setRmsMultiplicity(StDetectorId, Float_t);
    
protected:
    enum { mMultiplicityArraySize = 30 };
    
    ULong_t mNumberOfEvents;
    ULong_t mNumberOfProcessedEvents;
    Long_t  mStartTime;
    Long_t  mStopTime;
    Float_t mCpuSeconds;
    Float_t mAveragePolarizationEastL;
    Float_t mAveragePolarizationWestL;
    Float_t mAveragePolarizationEastT;
    Float_t mAveragePolarizationWestT;
    Float_t mAverageLuminosity;
    Float_t mMeanEta;
    Float_t mRmsEta;
    Float_t mMeanPt;
    Float_t mRmsPt;
    Float_t mMeanNumberOfVertices;
    Float_t mRmsNumberOfVertices;
    TArrayF mMeanMultiplicity;
    TArrayF mRmsMultiplicity;
    
    ClassDef(StRunSummary,1)
};
#endif
