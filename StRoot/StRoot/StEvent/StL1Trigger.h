/*!
 * \class StL1Trigger 
 * \author Thomas Ullrich, Nov 2001
 */
/***************************************************************************
 *
 * $Id: StL1Trigger.h,v 2.2 2002/02/22 22:56:48 jeromel Exp $
 *
 * Author: Thomas Ullrich, Nov 2001
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL1Trigger.h,v $
 * Revision 2.2  2002/02/22 22:56:48  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.1  2001/11/07 21:18:17  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StL1Trigger_hh
#define StL1Trigger_hh
#include "StTrigger.h"

class dst_L0_Trigger_st;
class dst_L1_Trigger_st;

class StL1Trigger : public StTrigger {
public:
    StL1Trigger();
    StL1Trigger(const dst_L0_Trigger_st&, const dst_L1_Trigger_st&);
    // StL1Trigger(const StL1Trigger&);            use default
    // StL1Trigger& operator=(const StL1Trigger&); use default
    ~StL1Trigger();

    unsigned int triggerWordPrime() const;
    void setTriggerWordPrime(unsigned int);
    
protected:
    UInt_t mTriggerWordPrime;
    ClassDef(StL1Trigger,1)
};
#endif
