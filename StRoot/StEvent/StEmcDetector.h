/***************************************************************************
 *
 * $Id: StEmcDetector.h,v 2.1 2000/02/23 17:33:59 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcDetector.h,v $
 * Revision 2.1  2000/02/23 17:33:59  ullrich
 * Initial Revision
 *
 * Revision 2.1  2000/02/23 17:33:59  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEmcDetector_hh
#define StEmcDetector_hh

#include "StObject.h"
#include "StEnumerations.h"

class StEmcRawHit;
class StEmcModule;
class StEmcClusterCollection;

class StEmcDetector : public StObject {
public:
    StEmcDetector();
    StEmcDetector(StDetectorId, UInt_t);
    ~StEmcDetector();
    
    StDetectorId  detectorId() const;
    UInt_t  numberOfModules() const;
    
    Bool_t  addHit(StEmcRawHit*);
    UInt_t  numberOfHits() const;
    
    StEmcModule*       module(UInt_t);
    const StEmcModule* module(UInt_t) const;
    
    StEmcClusterCollection*         cluster();
    const StEmcClusterCollection*   cluster() const;

    void setCluster(StEmcClusterCollection*);
    
private:
    StDetectorId            mDetectorId;
    UInt_t                  mNumberOfModules;
    
    StEmcModule             *mModules;
    StEmcClusterCollection  *mClusters;
    
    ClassDef(StEmcDetector,1)
};
#endif
