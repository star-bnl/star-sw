/***************************************************************************
 *
 * $Id: StTrigger.h,v 2.0 1999/10/12 18:43:13 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrigger.h,v $
 * Revision 2.0  1999/10/12 18:43:13  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StTrigger_hh
#define StTrigger_hh

#include "StObject.h"

class StTrigger : public StObject {
public:
    StTrigger();
    StTrigger(UShort_t aw, UShort_t w);
    // StTrigger(const StTrigger&);             use default
    // StTrigger& operator=(const StTrigger&);  use default
    virtual ~StTrigger();

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
#endif
