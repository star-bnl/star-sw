#ifndef STAR_StVirtualTriggerSimu
#define STAR_StVirtualTriggerSimu

// $Id: StVirtualTriggerSimu.h,v 1.4 2007/11/08 20:59:34 kocolosk Exp $

/*****************************************************************************
 * @class StVirtualTriggerSimu
 * @author A.Kocoloski
 *
 * Abstract base class defining an interface for subdetector trigger simulators.
 *****************************************************************************/

enum StTriggerSimuDecision { kNo, kYes, kDoNotCare };

class StVirtualTriggerSimu
{
public:    
    virtual bool isTrigger(int trigId);
    
    /// like isTrigger(), but returns kDoNotCare if detector isn't a part of the given trigId
    virtual StTriggerSimuDecision triggerDecision(int trigId)   = 0;
    
    virtual void Init();
    virtual void InitRun(int runnumber)=0;
    virtual void Make()                                         = 0;
    virtual void Clear();
    
    /// 0 == real data, 1 or more == Monte Carlo
    virtual void setMC(int flag) { mMCflag = flag; }
    
protected:
    int mMCflag;
    int mYear;

};
#endif

/*****************************************************************************
 * $Log: StVirtualTriggerSimu.h,v $
 * Revision 1.4  2007/11/08 20:59:34  kocolosk
 * subdet isTrigger returns a bool
 * triggerDecision returns enumerator including kDoNotCare
 *
 * Revision 1.3  2007/10/22 23:09:48  balewski
 * split L2 to generic and year specific, not finished
 *
 * Revision 1.2  2007/10/12 20:10:24  balewski
 * cleanup
 *
 * Revision 1.1  2007/10/12 17:12:30  kocolosk
 * rename ABC class for subdetector trigger simulators
 * StTriggerSimu => StVirtualTriggerSimu
 *
 * Revision 1.1  2007/09/24 18:32:06  kocolosk
 * added ABC class defining an interface for subdetector simulators
 *
 *****************************************************************************/
 
