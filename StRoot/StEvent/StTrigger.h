/***************************************************************************
 *
 * $Id: StTrigger.h,v 2.1 2001/04/05 04:00:46 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrigger.h,v $
 * Revision 2.1  2001/04/05 04:00:46  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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
    StTrigger(unsigned short aw, unsigned short w);
    // StTrigger(const StTrigger&);             use default
    // StTrigger& operator=(const StTrigger&);  use default
    virtual ~StTrigger();

    int operator==(const StTrigger&) const;
    int operator!=(const StTrigger&) const;

    virtual unsigned short triggerActionWord() const;
    virtual unsigned short triggerWord() const;

    virtual void setTriggerActionWord(unsigned short);
    virtual void setTriggerWord(unsigned short);
    
protected:
    UShort_t mTriggerActionWord;
    UShort_t mTriggerWord;
    
    ClassDef(StTrigger,1)  //StTrigger structure
};
#endif
