/*!
 * \class StEmcClusterCollection 
 * \author Akio Ogawa, Jan 2000
 */
/***************************************************************************
 *
 * $Id: StEmcClusterCollection.h,v 2.3 2002/02/22 22:56:47 jeromel Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcClusterCollection.h,v $
 * Revision 2.3  2002/02/22 22:56:47  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/04/05 04:00:34  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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
    
    int numberOfClusters() const;
    StSPtrVecEmcCluster&       clusters();
    const StSPtrVecEmcCluster& clusters() const;

    void addCluster(StEmcCluster*);
    
    int  clusterFinderId() const;
    int  clusterFinderParamVersion() const;
    void setClusterFinderId(int);
    void setClusterFinderParamVersion(int);
    
private:
    StDetectorId        mDetector;
    StSPtrVecEmcCluster mClusters;
    
    Int_t        mClusterFinderId;
    Int_t        mClusterFinderParamVersion;
    
    ClassDef(StEmcClusterCollection,1)
};

#endif




