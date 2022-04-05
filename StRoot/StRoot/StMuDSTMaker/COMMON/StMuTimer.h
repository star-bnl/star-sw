/***************************************************************************
 *
 * $Id: StMuTimer.h,v 1.4 2004/05/02 04:10:14 perev Exp $
 *
 * Author: Thomas Ullrich, April 1999
 ***************************************************************************
 *
 * Description:  CPU timer
 *
 ***************************************************************************
 *
 * $Log: StMuTimer.h,v $
 * Revision 1.4  2004/05/02 04:10:14  perev
 * private => protected
 *
 * Revision 1.3  2003/10/15 17:34:17  laue
 * StMuDstMaker:  Reading fixed. Delete() changed back to Clear()
 * StMuEmcCollection: Re-implemented the DeleteThis() function,
 *                    This hoopefully fixed the memory leak when
 *                    writing MuDst again.
 * StMuTimer: ClassDef/ClassImp
 *
 * Revision 1.2  2003/09/09 18:16:54  laue
 * StMuIOMaker: embedded documentation added
 * StMuTimer: name of define changed (was same as StTimer)
 *
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 * Revision 1.1  1999/04/27 19:23:46  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StMuTimer_h
#define StMuTimer_h

#include "TObject.h"

class StMuTimer : public TObject {
public:
    StMuTimer();
    
    double        elapsedTime() const;
    double        resolution() const;
    void          reset();
    void          start();
    void          stop();
    
protected:
    double        mStartTime;
    double        mStopTime;
    int           mIsStopped;
    
    static double absoluteTime();

    ClassDef(StMuTimer,0)
};

#endif
