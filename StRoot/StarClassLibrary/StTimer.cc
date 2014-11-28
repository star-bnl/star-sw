/***************************************************************************
 *
 * $Id: StTimer.cc,v 1.1 1999/04/27 19:23:44 ullrich Exp $
 *
 * Author: Thomas Ullrich, April 1999
 ***************************************************************************
 *
 * Description:  CPU timer
 *
 ***************************************************************************
 *
 * $Log: StTimer.cc,v $
 * Revision 1.1  1999/04/27 19:23:44  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <time.h>
#include <stdlib.h>
#include "StTimer.hh"

StTimer::StTimer() : mStartTime(0), mStopTime(0), mIsStopped(1)
{/* noop */}

double StTimer::resolution() const
{
    return 1./double(CLOCKS_PER_SEC);
}

double StTimer::elapsedTime() const
{
    return (mIsStopped ? mStopTime : absoluteTime()) - mStartTime;
}

void StTimer::reset()
{
    mStartTime = 0;
    mStopTime  = 0;
    mIsStopped = 1;
}

void StTimer::start()
{
    mStartTime = absoluteTime() - elapsedTime();
    mIsStopped = 0;
}

void StTimer::stop()
{
    mStopTime  = absoluteTime();
    mIsStopped = 1;
}


double StTimer::absoluteTime()
{
    return (double)clock()/CLOCKS_PER_SEC;
}

