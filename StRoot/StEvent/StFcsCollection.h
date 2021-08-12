/***************************************************************************
 *
 * $Id: StFcsCollection.h,v 2.1 2021/01/11 20:25:37 ullrich Exp $
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
 ***************************************************************************
 *
 * $Log: StFcsCollection.h,v $
 * Revision 2.1  2021/01/11 20:25:37  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StFcsCollection_hh
#define StFcsCollection_hh

#include "Stiostream.h"
#include "StObject.h"
#include "StEnumerations.h"
#include "StContainers.h"

class StFcsHit;
class StFcsCluster;
class StFcsPoint;
//class StFcsPointPair;

class StFcsCollection : public StObject {
public:
    StFcsCollection();
    ~StFcsCollection();
    
    void addHit(unsigned int det, StFcsHit*);            // Add a hit 
    StSPtrVecFcsHit& hits(unsigned int det);             // Return the hit list
    const StSPtrVecFcsHit& hits(unsigned int det) const; // Return the hit list
    unsigned int numberOfHits(unsigned int det) const;   // Return the number of hits

    void addCluster(unsigned int det, StFcsCluster*);            // Add a cluster
    StSPtrVecFcsCluster& clusters(unsigned int det);             // Return the cluster list
    const StSPtrVecFcsCluster& clusters(unsigned int det) const; // Return the cluster list
    unsigned int numberOfClusters(unsigned int det) const;       // Return the number of clusters

    void addPoint(unsigned int det, StFcsPoint*);            // Add a point
    StSPtrVecFcsPoint& points(unsigned int det);             // Return the point list
    const StSPtrVecFcsPoint& points(unsigned int det) const; // Return the point list
    unsigned int numberOfPoints(unsigned int det) const;     // Return the number of points

    int fcsReconstructionFlag()      const;
    void setFcsReconstructionFlag(int v);

    void print(int option=1);
    
private:
    StSPtrVecFcsHit     mHits[kFcsNDet+1];   //+1 for empty channel
    StSPtrVecFcsCluster mClusters[kFcsNDet]; 
    StSPtrVecFcsPoint   mPoints[kFcsNDet];  

    Int_t mFcsReconstructionFlag=0;     // undefined for now

    ClassDef(StFcsCollection,1)

};

inline int StFcsCollection::fcsReconstructionFlag()      const {return mFcsReconstructionFlag;}
inline void StFcsCollection::setFcsReconstructionFlag(int v) {mFcsReconstructionFlag=v;}

#endif
