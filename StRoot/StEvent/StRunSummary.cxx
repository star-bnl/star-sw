/***************************************************************************
 *
 * $Id: StRunSummary.cxx,v 1.1 1999/01/30 03:58:07 fisyak Exp $
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
 * Revision 1.1  1999/01/30 03:58:07  fisyak
 * Root Version of StEvent
 *
 * Revision 1.4  1999/04/28 22:27:34  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:53:51  wenaus
 * version with constructors for table-based loading
 *
static const Char_t rcsid[] = "$Id: StRunSummary.cxx,v 1.1 1999/01/30 03:58:07 fisyak Exp $";
 * Modified to cope with new compiler version on Sun (CC5.0).
#ifdef __ROOT__
 * Revision 2.1  1999/10/28 22:26:30  ullrich
static const Char_t rcsid[] = "$Id: StRunSummary.cxx,v 1.1 1999/01/30 03:58:07 fisyak Exp $";
#endif
StRunSummary::StRunSummary()
 * Completely Revised for New Version
  StRunSummary::StRunSummary():
St_DataSet("RunSummary")
    mMeanPt = 0;
    mNumberOfEvents = 0;           
    mNumberOfProcessedEvents = 0;  
    mStartTime = 0;                
    mStopTime = 0;                 
StRunSummary::StRunSummary(dst_run_summary_st* runSum)

StRunSummary::StRunSummary(dst_run_summary_st* runSum):
St_DataSet("RunSummary")
{
  mVersion = runSum->version;
  mNumberOfEvents = runSum->n_events_good;
  mNumberOfProcessedEvents = runSum->n_events_tot;
  mStartTime = runSum->time[0];
  mStopTime = runSum->time[1];
  mCpuSeconds = runSum->cpu_total;
    mAverageLuminosity = runSum.luminosity;
    mMeanEta = runSum.eta[0];
    mRmsEta = runSum.eta[1];
    mMeanPt = runSum.pt[0];
void StRunSummary::setVersion(const Char_t* val) { mVersion = val; }
        return dir == east ? mAveragePolarizationEastL : mAveragePolarizationWestL;
void StRunSummary::setNumberOfEvents(ULong_t val)
{ mNumberOfEvents = val; }

void StRunSummary::setStartTime(time_t val) { mStartTime = val; }
{ mNumberOfProcessedEvents = val; }
void StRunSummary::setStopTime(time_t val) { mStopTime = val; }
void StRunSummary::setStartTime(Long_t val) { mStartTime = val; }

void StRunSummary::setStopTime(Long_t val) { mStopTime = val; }

void StRunSummary::setCpuSeconds(Double_t val) { mCpuSeconds = val; }

Float_t
StRunSummary::rmsPt() const { return mRmsPt; }

Float_t
StRunSummary::meanNumberOfVertices() const { return mMeanNumberOfVertices; }

Float_t
StRunSummary::rmsNumberOfVertices() const { return mRmsNumberOfVertices; }
