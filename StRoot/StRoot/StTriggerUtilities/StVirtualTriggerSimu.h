#ifndef STAR_StVirtualTriggerSimu
#define STAR_StVirtualTriggerSimu

// $Id: StVirtualTriggerSimu.h,v 1.7 2016/03/18 22:49:39 zchang Exp $

/*****************************************************************************
 * @class StVirtualTriggerSimu
 * @author A.Kocoloski
 *
 * Abstract base class defining an interface for subdetector trigger simulators.
 *****************************************************************************/

#include "TString.h"

enum StTriggerSimuDecision { kNo, kYes, kDoNotCare };

class StVirtualTriggerSimu
{
public:
    StVirtualTriggerSimu() : mSource("MuDst") {}
    virtual ~StVirtualTriggerSimu() {}
    virtual bool isTrigger(int trigId);
    
    /// like isTrigger(), but returns kDoNotCare if detector isn't a part of the given trigId
    virtual StTriggerSimuDecision triggerDecision(int trigId)   = 0;
    
    virtual void Init();
    virtual void InitRun(int runnumber)                         = 0;
    virtual void Make()                                         = 0;
    virtual void Clear();
    
    /// 0 == real data, 1 or more == Monte Carlo
    virtual void setMC(int flag) { mMCflag = flag; }
    virtual void setYear(int year) { mYear = year; }
    /// Options are: "MuDst", "StEvent"
    virtual void setSource(const char* source) { mSource = source; }

protected:
    int mMCflag;
    int mYear;
    TString mSource;
    ClassDef(StVirtualTriggerSimu,1)
};

#endif

/*****************************************************************************
 * $Log: StVirtualTriggerSimu.h,v $
 * Revision 1.7  2016/03/18 22:49:39  zchang
 * updating trigger simulator for run12 analysis
 *
 * Revision 1.6  2010/01/08 15:18:27  pibero
 * Default input source is "MuDst" for all subdetectors.
 *
 * Revision 1.5  2009/12/22 18:10:57  pibero
 * Added ability to set input source (MuDst or StEvent) for BBC trigger simulator.
 *
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
 
