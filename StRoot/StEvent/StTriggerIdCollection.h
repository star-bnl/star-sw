/***************************************************************************
 *
 * $Id: StTriggerIdCollection.h,v 2.1 2003/01/30 18:14:15 ullrich Exp $
 *
 * Author: Thomas Ullrich, January 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerIdCollection.h,v $
 * Revision 2.1  2003/01/30 18:14:15  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTriggerIdCollection_hh
#define StTriggerIdCollection_hh
#include "StTriggerId.h"

class StTriggerIdCollection : public StObject {
public:
    StTriggerIdCollection();
    ~StTriggerIdCollection();

    const StTriggerId* nominal() const;
    const StTriggerId* l1() const;
    const StTriggerId* l2() const;
    const StTriggerId* l3() const;

    void setL1(StTriggerId*);
    void setL2(StTriggerId*);
    void setL3(StTriggerId*);
    void setNominal(StTriggerId*);
    
private:
    StTriggerId  *mL1TriggerId;
    StTriggerId  *mL2TriggerId;
    StTriggerId  *mL3TriggerId;
    StTriggerId  *mNominalTriggerId; //$LINK
    
    ClassDef(StTriggerIdCollection,1)
};

#endif 
