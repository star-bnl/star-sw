/***************************************************************************
 *
 * $Id: StEmcClusterCollection.h,v 2.1 2000/02/23 17:34:04 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StEmcClusterCollection.h,v $
 * Revision 2.1  2000/02/23 17:34:04  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEmcClusterCollection_hh
#define StEmcClusterCollection_hh

#include "StContainers.h"
#include "StObject.h"
#include "StEnumerations.h"

class StEmcClusterCollection : public StObject {
public: 
    StEmcClusterCollection();
    // StEmcClusterCollection(const StEmcClusterCollection&);            use default
    // StEmcClusterCollection& operator=(const StEmcClusterCollection&); use default
    virtual ~StEmcClusterCollection();
    
    StDetectorId detector() const;
    void setDetector(StDetectorId);
    
    Int_t numberOfClusters() const;
    StSPtrVecEmcCluster& clusters();
    const StSPtrVecEmcCluster& clusters() const;
    void addCluster(StEmcCluster*);
    
    Int_t clusterFinderId() const;
    Int_t clusterFinderParamVersion() const;
    void setClusterFinderId(Int_t);
    void setClusterFinderParamVersion(Int_t);
    
private:
    StDetectorId        mDetector;
    StSPtrVecEmcCluster mClusters;
    
    Int_t        mClusterFinderId;
    Int_t        mClusterFinderParamVersion;
    
    ClassDef(StEmcClusterCollection,1)
};

#endif




