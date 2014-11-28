//
// $Id: StL2TriggerSimu.h,v 0.01
// very light intervace from L2Emulator to StTriggerSimuMaker
//
#ifndef STAR_StL2TriggerSimu
#define STAR_StL2TriggerSimu

#include <TObject.h> 
#include <vector>

#include "StTriggerUtilities/StVirtualTriggerSimu.h"
class StGenericL2Emulator;
class StGenericL2Emulator2009;

class StL2TriggerSimu : public StVirtualTriggerSimu {
private:
    StGenericL2Emulator *mL2maker;
    StGenericL2Emulator2009 *mL2maker2009;

public:
    StL2TriggerSimu() : mL2maker(0), mL2maker2009(0) {}
    StL2TriggerSimu(StGenericL2Emulator *x) : mL2maker(x), mL2maker2009(0) {}
    StL2TriggerSimu(StGenericL2Emulator2009 *x) : mL2maker(0), mL2maker2009(x) {}
    void Init();
    void InitRun(int runnumber);
    void Clear(){};
    void Make(){};
    
    StTriggerSimuDecision triggerDecision(int trigId);
    
    /// bag of 64 bytes whose interpretation changes year-by-year
    const unsigned int* result() const;
    
    ClassDef(StL2TriggerSimu, 1)
};


#endif

//
// $Log: StL2TriggerSimu.h,v $
// Revision 1.10  2011/03/17 15:51:47  pibero
// Fixed nontrivial constructors. Thanks, Mike.
//
// Revision 1.9  2010/04/17 16:41:51  pibero
// *** empty log message ***
//
// Revision 1.8  2008/01/17 01:56:52  kocolosk
// export 128-byte emulated L2Result
//
// Revision 1.7  2007/11/18 21:58:54  balewski
// L2algos triggerId list fixed
//
// Revision 1.6  2007/11/08 20:59:58  kocolosk
// subdet isTrigger returns a bool
// triggerDecision returns enumerator including kDoNotCare
//
// Revision 1.5  2007/10/23 13:26:40  balewski
// more cleanup
//
// Revision 1.4  2007/10/23 03:43:06  balewski
// clenup
//
// Revision 1.3  2007/10/22 23:09:59  balewski
// split L2 to generic and year specific, not finished
//
// Revision 1.2  2007/10/12 17:12:48  kocolosk
// rename ABC class for subdetector trigger simulators
// StTriggerSimu => StVirtualTriggerSimu
//
// Revision 1.1  2007/10/11 21:22:57  balewski
// added L2-->L0 interface class
//
