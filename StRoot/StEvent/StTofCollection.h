/***************************************************************************
 *
 * $Id: StTofCollection.h,v 2.1 2000/12/08 03:52:43 ullrich Exp $
 *
 * Author: Thomas Ullrich, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 * Persistent data which is written into StEvent
 * directly from the reco chain. All ToF stuff goes here
 * except the StTofPidTraits and the StTofSoftwareMonitor.
 *
 ***************************************************************************
 *
 * $Log: StTofCollection.h,v $
 * Revision 2.1  2000/12/08 03:52:43  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTofCollection_hh
#define StTofCollection_hh

#include "StObject.h"

class StTofCollection : public StObject {
public:
    StTofCollection();
    ~StTofCollection();
//  StTofCollection(const StTofCollection&) { /* nopt */ }
//  StTofCollection& operator=(const StTofCollection&) {/* use default */}
   
private:
    
    ClassDef(StTofCollection, 1)
};
#endif
