/***************************************************************************
 *
 * $Id: StTrigger.h,v 1.1 1999/01/30 03:58:09 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrigger.h,v $
 * Revision 1.1  1999/01/30 03:58:09  fisyak
 * Root Version of StEvent
 *
 * Revision 1.3  1999/03/23 21:54:12  ullrich
 * Member function made virtual
 *
 * Revision 1.2  1999/01/15 22:54:12  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifdef __ROOT__
#include "TObject.h"
#endif
class StTrigger : public TObject {

#include "StObject.h"
class StTrigger : public StObject {
public:
    StTrigger();
    StTrigger(UShort_t aw, UShort_t w);
    virtual ~StTrigger();
    // StTrigger(const StTrigger&);  use default
    // const StTrigger & operator=(const StTrigger&);
    UShort_t triggerActionWord() const;
    UShort_t triggerWord() const;

    void setTriggerActionWord(UShort_t);
    void setTriggerWord(UShort_t);

    virtual void setTriggerActionWord(UShort_t);
    virtual void setTriggerWord(UShort_t);
    
#ifdef __ROOT__
	ClassDef(StTrigger,1)  //StTrigger structure
#endif
    UShort_t mTriggerActionWord;
    UShort_t mTriggerWord;
  ClassDef(StTrigger,1)  //StTrigger structure
};

inline UShort_t StTrigger::triggerActionWord() const { return mTriggerActionWord; }

inline UShort_t StTrigger::triggerWord() const { return mTriggerWord; }

#endif
