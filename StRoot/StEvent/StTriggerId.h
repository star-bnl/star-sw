/***************************************************************************
 *
 * $Id: StTriggerId.h,v 2.2 2003/02/18 21:34:46 jeromel Exp $
 *
 * Author: Thomas Ullrich, January 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerId.h,v $
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
#include <iostream.h>
#include <vector>

#define TRIGGER_ID_DIM 32

class StTriggerId : public StObject {
public:
    StTriggerId();
    ~StTriggerId();
	
    unsigned int mask() const;
    
    bool isTrigger(unsigned int id) const;
    
    unsigned int version(unsigned int id) const;
    unsigned int nameVersion(unsigned int id) const;
    unsigned int thresholdVersion(unsigned int id) const;
    unsigned int prescaleVersion(unsigned int id) const;    
    
    vector<unsigned int> triggerIds() const;  


    void setMask(unsigned int);
    void addTrigger(unsigned int, unsigned int,
		    unsigned int, unsigned int, unsigned int);
    
private:
    unsigned int index(unsigned int) const;
    
private:
    UInt_t       mIdx;
    UInt_t       mMask;
    UInt_t       mId[TRIGGER_ID_DIM];
    UInt_t	 mVersion[TRIGGER_ID_DIM];
    UInt_t	 mNameVersion[TRIGGER_ID_DIM];
    UInt_t	 mThresholdVersion[TRIGGER_ID_DIM];
    UInt_t	 mPrescaleVersion[TRIGGER_ID_DIM];
    
    ClassDef(StTriggerId,2)
};

ostream& operator<<(ostream&, const StTriggerId&);
#endif
