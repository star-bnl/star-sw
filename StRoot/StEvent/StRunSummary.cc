/***************************************************************************
 *
 * $Id: StRunSummary.cc,v 1.1 1999/01/15 20:39:58 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRunSummary.cc,v $
 * Revision 1.1  1999/01/15 20:39:58  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#include "StRunSummary.hh"

StRunSummary::StRunSummary()
{
    mNumberOfEvents = 0;           
    mNumberOfProcessedEvents = 0;  
    mStartTime = 0;                
    mStopTime = 0;                 
    mCpuSeconds = 0;               
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
