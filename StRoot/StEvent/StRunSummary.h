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
 * Revision 2.1  2001/04/05 04:00:41  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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
    
    unsigned int  numberOfEvents() const;
    unsigned int  numberOfProcessedEvents() const;
    int    startTime() const;
    int    stopTime() const;
    float  cpuSeconds() const;
    float  averageBeamPolarization(StBeamDirection, StBeamPolarizationAxis) const;
    float  averageLuminosity() const;
    float  meanEta() const;
    float  rmsEta() const;
    float  meanPt() const;
    float  rmsPt() const;
    float  meanNumberOfVertices() const;
    float  rmsNumberOfVertices() const;
    float  meanMultiplicity(StDetectorId) const;
    float  rmsMultiplicity(StDetectorId) const;

    void setNumberOfEvents(unsigned int);
    void setNumberOfProcessedEvents(unsigned int);
    void setStartTime(int);
    void setStopTime(int);
    void setCpuSeconds(float);
    void setAverageBeamPolarization(StBeamDirection, StBeamPolarizationAxis, float);
    void setAverageLuminosity(float);
    void setMeanEta(float);
    void setRmsEta(float);
    void setMeanPt(float);
    void setRmsPt(float);
    void setMeanNumberOfVertices(float);
    void setRmsNumberOfVertices(float);
    void setMeanMultiplicity(StDetectorId, float);
    void setRmsMultiplicity(StDetectorId, float);
    
protected:
    enum { mMultiplicityArraySize = 30 };
    
    UInt_t   mNumberOfEvents;
    UInt_t   mNumberOfProcessedEvents;
    Int_t    mStartTime;
    Int_t    mStopTime;
    Float_t  mCpuSeconds;
    Float_t  mAveragePolarizationEastL;
    Float_t  mAveragePolarizationWestL;
    Float_t  mAveragePolarizationEastT;
    Float_t  mAveragePolarizationWestT;
    Float_t  mAverageLuminosity;
    Float_t  mMeanEta;
    Float_t  mRmsEta;
    Float_t  mMeanPt;
    Float_t  mRmsPt;
    Float_t  mMeanNumberOfVertices;
    Float_t  mRmsNumberOfVertices;
    TArrayF  mMeanMultiplicity;
    TArrayF  mRmsMultiplicity;
    
    ClassDef(StRunSummary,1)
};
#endif
