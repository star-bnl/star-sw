/***************************************************************************
 *
 * $Id: StMuTriggerIdCollection.h,v 1.4 2006/05/04 21:04:35 mvl Exp $
 *
 * Author: Frank Laue
 ***************************************************************************
 *
 * Description:
 * Wrapper around the StEvent/StTriggerIdCollection to be able to read it
 * in the muDst without loading of the libraries
 *
 ***************************************************************************/

/*!
 *                                                                     
 * \class  StMuTriggerIdCollection
 * \author Frank Laue
 * \date   2003/02/20
 * \brief  Collection of trigger ids as stored in MuDst
 *
 * This class stores the trigger ids for every event in the MuDst.
 * Most users should only access the nominal(); everything else
 * is for detailed understanding of the triggers.
 *
 */    
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
    const StTriggerId& l3Expanded() const;

    void fill(const StTriggerIdCollection*);
    void setL1(const StTriggerId);
    void setL2(const StTriggerId);
    void setL3(const StTriggerId);
    void setL3Expanded(const StTriggerId);
    void setNominal(const StTriggerId);
    
    static bool isEmpty(const StTriggerId&);
protected:
    StTriggerId  mL1TriggerId;
    StTriggerId  mL2TriggerId;
    StTriggerId  mL3TriggerId;
    StTriggerId  mNTriggerId;  // When unrolling the TBranch, it's name exceeds the maximum. So we changed mNominalTriggerId to mNTriggerId 
    StTriggerId  mLETriggerId; // When unrolling the TBranch, it's name exceeds the maximum. So we changed mL3ExpandedTriggerId to mLETriggerId 
    
    ClassDef(StMuTriggerIdCollection,2)
};

#endif 

/**************************************************************************
 *
 * $Log: StMuTriggerIdCollection.h,v $
 * Revision 1.4  2006/05/04 21:04:35  mvl
 * Additions for extra L3 information (from Jamie)
 *
 * Revision 1.3  2004/05/02 04:10:14  perev
 * private => protected
 *
 * Revision 1.2  2003/03/19 18:58:05  laue
 * StMuChainMaker: updates for moved file catalog
 * StTriggerIdCollection added to the createStEvent function in StMuDst.cxx
 *
 * Revision 1.1  2003/02/20 15:50:30  laue
 * New. Wrapper around StEVent/StStriggerIdCollection
 *
 *
 **************************************************************************/
