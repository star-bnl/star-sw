/***************************************************************************
 *
 * $Id: StTriggerId.h,v 2.5 2004/10/11 23:00:20 ullrich Exp $
 *
 * Author: Thomas Ullrich, January 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerId.h,v $
 * Revision 2.5  2004/10/11 23:00:20  ullrich
 * Add copy constructor and assign. op., implement ostream op., define to enum.
 *
 * Revision 2.4  2003/09/02 17:58:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.3  2003/02/18 22:19:58  jeromel
 * Skip mIdx
 *
 * Revision 2.2  2003/02/18 21:34:46  jeromel
 * Changed vector to arrays
 *
 * Revision 2.1  2003/01/30 18:14:15  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTriggerId_hh
#define StTriggerId_hh
#include "StObject.h"
#include <Stiostream.h>
#include <vector>

class StTriggerId : public StObject {
public:
    StTriggerId();
    StTriggerId(const StTriggerId &id);
    const StTriggerId &operator=(const StTriggerId &id);
    ~StTriggerId();
	
    unsigned int mask() const;
    
    unsigned int maxTriggerIds() const;
    bool isTrigger(unsigned int id) const;
    
    unsigned int version(unsigned int id) const;
    unsigned int nameVersion(unsigned int id) const;
    unsigned int thresholdVersion(unsigned int id) const;
    unsigned int prescaleVersion(unsigned int id) const;    

    unsigned int triggerId(const int idx) const;    
    vector<unsigned int> triggerIds() const;  


    void setMask(unsigned int);
    void addTrigger(unsigned int, unsigned int,
	          unsigned int, unsigned int, unsigned int);

    friend ostream& operator<<(ostream&, const StTriggerId&);
    
private:
    unsigned int index(unsigned int) const;
    
protected:
    enum {mMaxTriggerIds = 32};

private:
    UInt_t       mIdx;    //!
    UInt_t       mMask;
    UInt_t       mId[mMaxTriggerIds];
    UInt_t       mVersion[mMaxTriggerIds];
    UInt_t       mNameVersion[mMaxTriggerIds];
    UInt_t       mThresholdVersion[mMaxTriggerIds];
    UInt_t       mPrescaleVersion[mMaxTriggerIds];
    
    ClassDef(StTriggerId,3)
};

inline unsigned int
StTriggerId::maxTriggerIds() const {return  mMaxTriggerIds;}

inline unsigned int
StTriggerId::triggerId(const int idx) const
{
    if (idx>=0 && idx<mMaxTriggerIds)
        return mId[idx];
    else
        return 0;
}

#endif
