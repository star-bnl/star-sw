/***************************************************************************
 *
 * $Id: StRunSummary.cc,v 1.2 1999/01/15 22:53:51 wenaus Exp $
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
 * $Log: StRunSummary.cc,v $
 * Revision 1.2  1999/01/15 22:53:51  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StRunSummary.hh"

static const char rcsid[] = "$Id: StRunSummary.cc,v 1.2 1999/01/15 22:53:51 wenaus Exp $";

StRunSummary::StRunSummary()
{
    mNumberOfEvents = 0;           
    mNumberOfProcessedEvents = 0;  
    mStartTime = 0;                
    mStopTime = 0;                 
    mCpuSeconds = 0;               
}

StRunSummary::StRunSummary(dst_run_summary_st* runSum)
{
  mVersion = runSum->version;
  mNumberOfEvents = runSum->n_events_good;
  mNumberOfProcessedEvents = runSum->n_events_tot;
  mStartTime = runSum->time[0];
  mStopTime = runSum->time[1];
  mCpuSeconds = runSum->cpu_total;
}

StRunSummary::~StRunSummary() { /* noop */ }
    
void StRunSummary::setVersion(const char* val) { mVersion = val; }

void StRunSummary::setNumberOfEvents(unsigned long val)
{ mNumberOfEvents = val; }

void StRunSummary::setNumberOfProcessedEvents(unsigned long val)
{ mNumberOfProcessedEvents = val; }

void StRunSummary::setStartTime(time_t val) { mStartTime = val; }

void StRunSummary::setStopTime(time_t val) { mStopTime = val; }

void StRunSummary::setCpuSeconds(double val) { mCpuSeconds = val; }
