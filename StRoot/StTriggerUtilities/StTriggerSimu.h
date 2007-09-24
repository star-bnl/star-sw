#ifndef STAR_StTriggerSimu
#define STAR_StTriggerSimu

// $Id: StTriggerSimu.h,v 1.1 2007/09/24 18:32:06 kocolosk Exp $

/*****************************************************************************
 * @class StTriggerSimu
 * @author A.Kocoloski
 *
 * Abstract base class defining an interface for subdetector trigger simulators.
 *****************************************************************************/
class StTriggerSimu
{
public:    
    /// returns 1 if passed, 0 if failed, -1 if don't care
    virtual short isTrigger(int trigId)                     = 0;
    
    virtual void Init();
    virtual void InitRun(int runnumber);
    virtual void Make()                                     = 0;
    virtual void Clear();
    
    /// 0 == real data, 1 == Monte Carlo
    virtual void setMC(int flag) { mMCflag = flag; }
    
protected:
    int mMCflag;
};
#endif

/*****************************************************************************
 * $Log: StTriggerSimu.h,v $
 * Revision 1.1  2007/09/24 18:32:06  kocolosk
 * added ABC class defining an interface for subdetector simulators
 *
 *****************************************************************************/
 
