/***************************************************************************
 *
 * $Id: StRunSummary.cxx,v 2.3 2000/01/31 12:01:05 ullrich Exp $
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
 * $Log: StRunSummary.cxx,v $
 * Revision 2.3  2000/01/31 12:01:05  ullrich
 * Unique const_cast syntax for all platforms.
 *
 * Revision 2.2  1999/12/21 15:09:06  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.1  1999/10/28 22:26:30  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:36  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include "StRunSummary.h"
#include "tables/St_dst_run_summary_Table.h"

static const char rcsid[] = "$Id: StRunSummary.cxx,v 2.3 2000/01/31 12:01:05 ullrich Exp $";

ClassImp(StRunSummary)

StRunSummary::StRunSummary()
{
    mNumberOfEvents = 0;
    mNumberOfProcessedEvents = 0;
    mStartTime = 0;
    mStopTime = 0;
    mCpuSeconds = 0;
    mAveragePolarizationEastL = 0;
    mAveragePolarizationWestL = 0;
    mAveragePolarizationEastT = 0;
    mAveragePolarizationWestT = 0;
    mAverageLuminosity = 0;
    mMeanEta = 0;
    mRmsEta = 0;
    mMeanPt = 0;
    mRmsPt = 0;
    mMeanNumberOfVertices = 0;
    mRmsNumberOfVertices = 0;
    mMeanMultiplicity.Set(mMultiplicityArraySize);
    mRmsMultiplicity.Set(mMultiplicityArraySize);
}

StRunSummary::StRunSummary(const dst_run_summary_st& runSum)
{
    mNumberOfEvents = runSum.n_events_tot;
    mNumberOfProcessedEvents = runSum.n_events_good;
    mStartTime = runSum.time[0];
    mStopTime = runSum.time[1];
    mCpuSeconds = runSum.cpu_total;
    mAveragePolarizationEastL = runSum.east_pol_L;
    mAveragePolarizationWestL = runSum.west_pol_L;
    mAveragePolarizationEastT = runSum.east_pol_T;
    mAveragePolarizationWestT = runSum.west_pol_T;
    mAverageLuminosity = runSum.luminosity;
    mMeanEta = runSum.eta[0];
    mRmsEta = runSum.eta[1];
    mMeanPt = runSum.pt[0];
    mRmsPt = runSum.pt[1];
    mMeanNumberOfVertices = runSum.num_vert[0];
    mRmsNumberOfVertices = runSum.num_vert[1];
    mMeanMultiplicity.Set(mMultiplicityArraySize, const_cast<Float_t*>(runSum.mean_mult));
    mRmsMultiplicity.Set(mMultiplicityArraySize, const_cast<Float_t*>(runSum.rms_mult));
}

StRunSummary::~StRunSummary() { /* noop */ }
    
void
StRunSummary::setNumberOfEvents(ULong_t val) { mNumberOfEvents = val; }

void
StRunSummary::setNumberOfProcessedEvents(ULong_t val) { mNumberOfProcessedEvents = val; }

void
StRunSummary::setStartTime(Long_t val) { mStartTime = val; }

void
StRunSummary::setStopTime(Long_t val) { mStopTime = val; }

void
StRunSummary::setCpuSeconds(Float_t val) { mCpuSeconds = val; }

void
StRunSummary::setAverageBeamPolarization(StBeamDirection dir, StBeamPolarizationAxis axis, Float_t val)
{
    if (axis == transverse) {
        if (dir == east)
            mAveragePolarizationEastT = val;
        else
            mAveragePolarizationWestT = val;
    }
    else {
        if (dir == east)
            mAveragePolarizationEastL = val;
        else
            mAveragePolarizationWestL = val;
    }
}

void
StRunSummary::setAverageLuminosity(Float_t val) { mAverageLuminosity = val; }

void
StRunSummary::setMeanEta(Float_t val) { mMeanEta = val; }

void
StRunSummary::setRmsEta(Float_t val) { mRmsEta = val; }

void
StRunSummary::setMeanPt(Float_t val) { mMeanPt = val; }

void
StRunSummary::setRmsPt(Float_t val) { mRmsPt = val; }

void
StRunSummary::setMeanNumberOfVertices(Float_t val) { mMeanNumberOfVertices = val; }

void
StRunSummary::setRmsNumberOfVertices(Float_t val) { mRmsNumberOfVertices = val; }

void
StRunSummary::setMeanMultiplicity(StDetectorId id, Float_t val)
{
    unsigned int i = id-1;        // detector numbering scheme starts at 1
    if (i < mMultiplicityArraySize)
        mMeanMultiplicity[i] = val;
}

void
StRunSummary::setRmsMultiplicity(StDetectorId id, Float_t val)
{
    unsigned int i = id-1;        // detector numbering scheme starts at 1
    if (i < mMultiplicityArraySize)
        mRmsMultiplicity[i] = val;
}

Float_t
StRunSummary::meanMultiplicity(StDetectorId id) const
{
    unsigned int i = id-1;        // detector numbering scheme starts at 1
    //
    //  Oops, need workaround here since (i) TArrayF has no
    //  const version of operator[] and (ii) the SUN compiler
    //  has some problems with const_cast'ing non-pointers.
    //
    if (i < mMultiplicityArraySize)
        return const_cast<TArrayF&>(mMeanMultiplicity)[i];
    else
        return 0;
}

Float_t
StRunSummary::rmsMultiplicity(StDetectorId id) const
{
    unsigned int i = id-1;        // detector numbering scheme starts at 1
    if (i < mMultiplicityArraySize)
    //
    //  Oops, need workaround here since (i) TArrayF has no
    //  const version of operator[] and (ii) the SUN compiler
    //  has some problems with const_cast'ing non-pointers.
    //
        return const_cast<TArrayF&>(mRmsMultiplicity)[i];
    else
        return 0;
}

ULong_t
StRunSummary::numberOfEvents() const { return mNumberOfEvents; }

ULong_t
StRunSummary::numberOfProcessedEvents() const { return mNumberOfProcessedEvents; }

Long_t
StRunSummary::startTime() const { return mStartTime; }

Long_t
StRunSummary::stopTime() const { return mStopTime; }

Float_t
StRunSummary::cpuSeconds() const { return mCpuSeconds; }

Float_t
StRunSummary::averageBeamPolarization(StBeamDirection dir, StBeamPolarizationAxis axis) const
{
    if (axis == transverse)
        return dir == east ? mAveragePolarizationEastT : mAveragePolarizationWestT;
    else
        return dir == east ? mAveragePolarizationEastL : mAveragePolarizationWestL;
            
}

Float_t
StRunSummary::averageLuminosity() const { return mAverageLuminosity; }

Float_t
StRunSummary::meanEta() const { return mMeanEta; }

Float_t
StRunSummary::rmsEta() const { return mRmsEta; }

Float_t
StRunSummary::meanPt() const { return mMeanPt; }

Float_t
StRunSummary::rmsPt() const { return mRmsPt; }

Float_t
StRunSummary::meanNumberOfVertices() const { return mMeanNumberOfVertices; }

Float_t
StRunSummary::rmsNumberOfVertices() const { return mRmsNumberOfVertices; }
