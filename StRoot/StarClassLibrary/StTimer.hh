/***************************************************************************
 *
 * $Id: StTimer.hh,v 1.1 1999/04/27 19:23:46 ullrich Exp $
 *
 * Author: Thomas Ullrich, April 1999
 ***************************************************************************
 *
 * Description:  CPU timer
 *
 ***************************************************************************
 *
 * $Log: StTimer.hh,v $
 * Revision 1.1  1999/04/27 19:23:46  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TIMER_HH
#define ST_TIMER_HH

class StTimer {
public:
    StTimer();
    
    double        elapsedTime() const;
    double        resolution() const;
    void          reset();
    void          start();
    void          stop();
    
private:
    double        mStartTime;
    double        mStopTime;
    int           mIsStopped;
    
    static double absoluteTime();
};

#endif
