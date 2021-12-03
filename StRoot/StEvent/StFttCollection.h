/***************************************************************************
 *
 * $Id: StFttCollection.h,v 2.1 2021/01/11 20:25:37 ullrich Exp $
 *
 * Author: Akio Ogawa, 2018 Aug
 ***************************************************************************
 *
 * Description:
 * Collection of all hits (towers), clusters and points (photons) in the FCS
 * This collection owns all these objects, and is itself owned by StEvent.
 * It is therefore vital to *not* delete any of the objects stored in this
 * container yourself - the collection will handle freeing memory.
 * Similarly, any object added to the collection via an add() method must be
 * allocated with new, and not be owned anywhere else.
 *
 ***************************************************************************/
#ifndef StFttCollection_hh
#define StFttCollection_hh

#include "Stiostream.h"
#include "StObject.h"
#include "StEnumerations.h"
#include "StContainers.h"

class StFttRawHit;
class StFttCluster;
class StFttPoint;

class StFttCollection : public StObject {
public:
    StFttCollection();
    ~StFttCollection();
    
    void addRawHit(StFttRawHit*);            // Add a hit 
    StSPtrVecFttRawHit& rawHits();             // Return the hit list
    const StSPtrVecFttRawHit& rawHits() const; // Return the hit list
    unsigned int numberOfRawHits() const;   // Return the number of hits

    void addCluster(StFttCluster*);            // Add a cluster
    StSPtrVecFttCluster& clusters();             // Return the cluster list
    const StSPtrVecFttCluster& clusters() const; // Return the cluster list
    unsigned int numberOfClusters() const;       // Return the number of clusters

    void addPoint(StFttPoint*);              // Add a point
    StSPtrVecFttPoint& points();             // Return the point list
    const StSPtrVecFttPoint& points() const; // Return the point list
    unsigned int numberOfPoints() const;     // Return the number of points

    void print(int option=1);
    
private:
    StSPtrVecFttRawHit  mRawHits;
    StSPtrVecFttCluster mClusters; 
    StSPtrVecFttPoint   mPoints;  

    ClassDef(StFttCollection,1)

};

#endif
