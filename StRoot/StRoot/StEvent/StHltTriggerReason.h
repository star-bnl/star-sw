/***************************************************************************
 *
 * $Id: StHltTriggerReason.h,v 2.1 2011/02/01 19:45:48 ullrich Exp $
 *
 * Author: Liang Xue, Aihong Tang, Jan 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHltTriggerReason.h,v $
 * Revision 2.1  2011/02/01 19:45:48  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StHltTriggerReason_hh
#define StHltTriggerReason_hh

#include <Stiostream.h>
#include "StObject.h"
#include "StArray.h"

class StHltTriggerReasonCapable;

enum StHltTriggerReasonType { 
	kHighPt = 0x10000,                  ///< enum of high pt
	kDiElectron = 0x20000,              ///< enum of di-electron
	kHeavyFragment = 0x40000,           ///< enum of heavy fragment
	kAllEvents = 0x80000,               ///< enum of all events
	kRandomEvents = 0x100000,           ///< enum of random events
	kBESGoodEvents = 0x200000,          ///< enum of BES good events
};  

class StHltTriggerReason : public StObject {
public:
    StHltTriggerReason();
    ~StHltTriggerReason();
    
    StHltTriggerReasonType reasonBit() const;
    StHltTriggerReasonCapable* reason();
    const StHltTriggerReasonCapable* reason() const;
    
    void setReasonBit(StHltTriggerReasonType);
    void setReason(StHltTriggerReasonCapable*);
    
    
private:
    StHltTriggerReasonType mReasonBit;
#ifdef __CINT__
    StObjLink mReason;
#else
    StLink<StHltTriggerReasonCapable> mReason;
#endif //__CINT__
    
    ///< can be an pointer to StHltHighPt StHltHeavyFragment or StHltDielectron
    
    ClassDef(StHltTriggerReason,1)
};

inline StHltTriggerReasonType StHltTriggerReason::reasonBit() const {return mReasonBit;}
inline StHltTriggerReasonCapable* StHltTriggerReason::reason() { return mReason; }
inline const StHltTriggerReasonCapable* StHltTriggerReason::reason() const { return mReason; }

ostream& operator<<(ostream&, const StHltTriggerReason&); ///< print operator

#endif
