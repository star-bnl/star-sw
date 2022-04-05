/*!
 * \class StTrigger 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StTrigger.h,v 2.3 2002/02/22 22:56:53 jeromel Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrigger.h,v $
 * Revision 2.3  2002/02/22 22:56:53  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/08/29 18:53:37  ullrich
 * Changed trigger words to UInt_t (was UShort_t)
 *
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
    StTrigger(unsigned int aw, unsigned int w);
    // StTrigger(const StTrigger&);             use default
    // StTrigger& operator=(const StTrigger&);  use default
    virtual ~StTrigger();

    int operator==(const StTrigger&) const;
    int operator!=(const StTrigger&) const;

    virtual unsigned int triggerActionWord() const;
    virtual unsigned int triggerWord() const;

    virtual void setTriggerActionWord(unsigned int);
    virtual void setTriggerWord(unsigned int);
    
protected:
    UInt_t mTriggerActionWord;
    UInt_t mTriggerWord;
    
    ClassDef(StTrigger,2)  //StTrigger structure
};
#endif
