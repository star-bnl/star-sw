/***************************************************************************
 *
 * $Id: StMuTimer.cxx,v 1.3 2003/10/15 17:34:17 laue Exp $
 *
 * Author: Thomas Ullrich, April 1999
 ***************************************************************************
 *
 * Description:  CPU timer
 *
 ***************************************************************************
 *
 * $Log: StMuTimer.cxx,v $
 * Revision 1.3  2003/10/15 17:34:17  laue
 * StMuDstMaker:  Reading fixed. Delete() changed back to Clear()
 * StMuEmcCollection: Re-implemented the DeleteThis() function,
 *                    This hoopefully fixed the memory leak when
 *                    writing MuDst again.
 * StMuTimer: ClassDef/ClassImp
 *
 * Revision 1.2  2003/09/09 18:16:53  laue
 * StMuIOMaker: embedded documentation added
 * StMuTimer: name of define changed (was same as StTimer)
 *
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 * Revision 1.1  1999/04/27 19:23:44  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <time.h>
#include <stdlib.h>
#include "StMuTimer.h"

ClassImp(StMuTimer)

StMuTimer::StMuTimer() : mStartTime(0), mStopTime(0), mIsStopped(1)
{/* noop */}

double StMuTimer::resolution() const
{
    return 1./double(CLOCKS_PER_SEC);
}

double StMuTimer::elapsedTime() const
{
    return (mIsStopped ? mStopTime : absoluteTime()) - mStartTime;
}

void StMuTimer::reset()
{
    mStartTime = 0;
    mStopTime  = 0;
    mIsStopped = 1;
}

void StMuTimer::start()
{
    mStartTime = absoluteTime() - elapsedTime();
    mIsStopped = 0;
}

void StMuTimer::stop()
{
    mStopTime  = absoluteTime();
    mIsStopped = 1;
}


double StMuTimer::absoluteTime()
{
    return (double)clock()/CLOCKS_PER_SEC;
}

