/***************************************************************************
 *
 * $Id: StTriggerIdCollection.h,v 2.3 2006/05/04 19:07:49 ullrich Exp $
 *
 * Author: Thomas Ullrich, January 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerIdCollection.h,v $
 * Revision 2.3  2006/05/04 19:07:49  ullrich
 * Added L3 trigger expansion.
 *
 * Revision 2.2  2003/02/19 16:35:05  jeromel
 * $LINK mechanism removed
 *
 * Revision 2.1  2003/01/30 18:14:15  ullrich
 * Initial Revision.
 *
 **************************************************************************/
/*!
 *                                                                     
 * \class  StTriggerIdCollection
 * \author Thomas Ullrich
 * \date   2003/01/30
 * \brief  Collection of trigger ids as stored in StEvent
 *
 * This class stores the trigger ids for every event in StEvent.
 * Most users should only access the nominal(); everything else
 * is for detailed understanding of the triggers.
 *
 */    
#ifndef StTriggerIdCollection_hh
#define StTriggerIdCollection_hh
#include "StTriggerId.h"

class StTriggerIdCollection : public StObject {
public:
    StTriggerIdCollection();
    ~StTriggerIdCollection();

    const StTriggerId* nominal() const; // Most users should use this one
    const StTriggerId* l1() const;
    const StTriggerId* l2() const;
    const StTriggerId* l3() const;
    const StTriggerId* l3Expanded() const;

    void setL1(StTriggerId*);
    void setL2(StTriggerId*);
    void setL3(StTriggerId*);
    void setL3Expanded(StTriggerId*);
    void setNominal(StTriggerId*);
    
private:
    StTriggerId  *mL1TriggerId;
    StTriggerId  *mL2TriggerId;
    StTriggerId  *mL3TriggerId;
    StTriggerId  *mL3ExpandedTriggerId;
    StTriggerId  *mNominalTriggerId; 
    
    ClassDef(StTriggerIdCollection,2)
};

#endif 
