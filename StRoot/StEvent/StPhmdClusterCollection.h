/*!
 * \class StPhmdClusterCollection
 * \author  Subhasis Chattopadhaya
 */
/********************************************************************
 *
 * $Id: StPhmdClusterCollection.h,v 2.1 2002/12/20 22:33:00 ullrich Exp $
 *
 * Author: Subhasis Chattopadhyay, Dec 2002
 ********************************************************************
 *
 * Description: Base class for PMD cluster collection
 *
 ********************************************************************
 *
 * $Log: StPhmdClusterCollection.h,v $
 * Revision 2.1  2002/12/20 22:33:00  ullrich
 * Initial Revision.
 *
 ********************************************************************/
#ifndef STAR_StPhmdClusterCollection
#define STAR_StPhmdClusterCollection

#include "StContainers.h"
#include "StObject.h"
#include "StPhmdDetector.h"
#include "StPhmdCluster.h"


class StPhmdClusterCollection : public StObject {
public: 
    StPhmdClusterCollection();  
    ~StPhmdClusterCollection();

    int                          numberOfclusters() const; 
    StSPtrVecPhmdCluster&        clusters();
    const StSPtrVecPhmdCluster&  clusters() const;
    
    void   addCluster(StPhmdCluster* );
    void   deleteCluster(StPhmdCluster* );
    void   deleteClusters();
    int    clusterFinderId() const;
    int    clusterFinderParamVersion() const;
    
    void   setClusterFinderId(int);
    void   setClusterFinderParamVersion(int);

private:
    StDetectorId         mDetector;
    StSPtrVecPhmdCluster mClusters;
    
    Int_t        mClusterFinderId;
    Int_t        mClusterFinderParamVersion;
    
    ClassDef(StPhmdClusterCollection,1)
};

#endif




