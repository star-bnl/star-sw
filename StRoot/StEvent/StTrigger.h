/***************************************************************************
 *
 * $Id: StTrigger.h,v 1.5 1999/04/30 13:16:30 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrigger.h,v $
 * Revision 1.5  1999/04/30 13:16:30  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.5  1999/04/30 13:16:30  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.4  1999/04/28 22:27:38  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.3  1999/03/23 21:54:12  ullrich
 * Member function made virtual
 *
 * Revision 1.2  1999/01/15 22:54:12  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StTrigger_hh
#define StTrigger_hh

#include "StObject.h"
class StTrigger : public StObject {
public:
    StTrigger();
    StTrigger(UShort_t aw, UShort_t w);
    virtual ~StTrigger();
    // StTrigger(const StTrigger&);  use default
    // const StTrigger & operator=(const StTrigger&);
    Int_t operator==(const StTrigger&) const;
    Int_t operator!=(const StTrigger&) const;

    virtual UShort_t triggerActionWord() const;
    virtual UShort_t triggerWord() const;

    virtual void setTriggerActionWord(UShort_t);
    virtual void setTriggerWord(UShort_t);
    
protected:
    UShort_t mTriggerActionWord;
    UShort_t mTriggerWord;
  ClassDef(StTrigger,1)  //StTrigger structure
};

inline UShort_t StTrigger::triggerActionWord() const { return mTriggerActionWord; }

inline UShort_t StTrigger::triggerWord() const { return mTriggerWord; }

#endif
