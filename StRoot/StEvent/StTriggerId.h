/***************************************************************************
 *
 * $Id: StTriggerId.h,v 2.1 2003/01/30 18:14:15 ullrich Exp $
 *
 * Author: Thomas Ullrich, January 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerId.h,v $
 * Revision 2.1  2003/01/30 18:14:15  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTriggerId_hh
#define StTriggerId_hh
#include "StObject.h"
#include <iostream.h>
#include <vector>

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
    UInt_t         mMask;
    vector<UInt_t> mId;
    vector<UInt_t> mVersion;
    vector<UInt_t> mNameVersion;
    vector<UInt_t> mThresholdVersion;
    vector<UInt_t> mPrescaleVersion;
    
    ClassDef(StTriggerId,1)
};

ostream& operator<<(ostream&, const StTriggerId&);
#endif
