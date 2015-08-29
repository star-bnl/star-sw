/***************************************************************************
 *
 * $Id: StFmsCollection.h,v 2.3 2015/08/26 16:51:59 ullrich Exp $
 *
 * Author: Jingguo Ma, Dec 2009
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFmsCollection.h,v $
 * Revision 2.3  2015/08/26 16:51:59  ullrich
 * Added print out fct and operator.
 *
 * Revision 2.2  2015/02/14 18:57:25  ullrich
 * Big upgrade after adding StFmPoint and StFmsCluster.
 *
 * Revision 2.1  2010/01/08 22:42:31  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StFmsCollection_hh
#define StFmsCollection_hh

#include "Stiostream.h"
#include "StObject.h"
#include "StContainers.h"  // StSPtrVecFmsHit/Cluster/Point definitions

class StFmsHit;
class StFmsCluster;
class StFmsPoint;

/**
 Collection of all hits (towers), clusters and points (photons) in the FMS.
 
 This collection owns all these objects, and is itself owned by StEvent.
 It is therefore vital to *not* delete any of the objects stored in this
 container yourself - the collection will handle freeing memory.
 Similarly, any object added to the collection via an add() method must be
 allocated with new, and not be owned anywhere else.
 */
class StFmsCollection : public StObject {
public:
    StFmsCollection();
    ~StFmsCollection();

    
    void addHit(StFmsHit*);           // Add a hit to the collection
    void addCluster(StFmsCluster*);   // Add a cluster to the collection
    void addPoint(StFmsPoint*);       // Add a point to the collection
    
    unsigned int numberOfHits() const;   // Return the number of hits in the collection
    unsigned int numberOfClusters() const; // Return the number of clusters in the collection
    unsigned int numberOfPoints() const;   // Return the number of points in the collection
    
    StSPtrVecFmsHit& hits();   // Return the hit list
    const StSPtrVecFmsHit& hits() const;
    
    StSPtrVecFmsCluster& clusters();   // Return the cluster list
    const StSPtrVecFmsCluster& clusters() const;
    
    StSPtrVecFmsPoint& points();    // Return the cluster list
    const StSPtrVecFmsPoint& points() const;
  
    void print(Option_t *option="") const;
    
private:
    StSPtrVecFmsHit mHits;  // Owns all FMS hits
    StSPtrVecFmsCluster mClusters;  // Owns all FMS clusters
    StSPtrVecFmsPoint mPoints;  // Owns all FMS points (photons)
    ClassDef(StFmsCollection, 2)
};

#endif
