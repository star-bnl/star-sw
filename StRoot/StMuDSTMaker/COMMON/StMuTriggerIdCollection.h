/***************************************************************************
 *
 * $Id: StMuTriggerIdCollection.h,v 1.2 2003/03/19 18:58:05 laue Exp $
 *
 * Author: Frank Laue
 ***************************************************************************
 *
 * Description:
 * Wrapper around the StEvent/StTriggerIdCollection to be able to read it
 * in the muDst without loading of the libraries
 *
 ***************************************************************************/
#ifndef StMuTriggerIdCollection_hh
#define StMuTriggerIdCollection_hh
#include "StEvent/StTriggerId.h"
#include "StEvent/StTriggerIdCollection.h"

class StMuTriggerIdCollection : public StObject {
public:
    StMuTriggerIdCollection();
    StMuTriggerIdCollection(const StTriggerIdCollection*);
    ~StMuTriggerIdCollection();


    const StTriggerId& nominal() const;
    const StTriggerId& l1() const;
    const StTriggerId& l2() const;
    const StTriggerId& l3() const;

    void fill(const StTriggerIdCollection*);
    void setL1(const StTriggerId);
    void setL2(const StTriggerId);
    void setL3(const StTriggerId);
    void setNominal(const StTriggerId);
    
    static bool isEmpty(const StTriggerId&);
private:
    StTriggerId  mL1TriggerId;
    StTriggerId  mL2TriggerId;
    StTriggerId  mL3TriggerId;
    StTriggerId  mNTriggerId;  // When unrolling the TBranch, it's name exceeds the maximum. So we changed mNominalTriggerId to mNTriggerId 
    
    ClassDef(StMuTriggerIdCollection,1)
};

#endif 

/**************************************************************************
 *
 * $Log: StMuTriggerIdCollection.h,v $
 * Revision 1.2  2003/03/19 18:58:05  laue
 * StMuChainMaker: updates for moved file catalog
 * StTriggerIdCollection added to the createStEvent function in StMuDst.cxx
 *
 * Revision 1.1  2003/02/20 15:50:30  laue
 * New. Wrapper around StEVent/StStriggerIdCollection
 *
 *
 **************************************************************************/
