/***************************************************************************
 *
 * $Id: StFcsCollection.h,v 1.1.2.1 2018/08/24 15:14:36 jwebb Exp $
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
 * Revision 1.1.2.1  2018/08/24 15:14:36  jwebb
 * Forward Calorimeter System (HCAL and WCAL) added to event model.
 *
 *
 **************************************************************************/
#ifndef StFcsCollection_hh
#define StFcsCollection_hh

#include "Stiostream.h"
#include "StObject.h"
#include "StContainers.h"

class StFcsHit;
//class StFcsCluster;
//class StFcsPoint;
//class StFcsPointPair;

class StFcsCollection : public StObject {
public:
    StFcsCollection();
    ~StFcsCollection();
    
    void addHit(int wh, StFcsHit*);            // Add a hit
    StSPtrVecFcsHit& hits(int wh);             // Return the hit list
    const StSPtrVecFcsHit& hits(int wh) const; // Return the hit list
    unsigned int numberOfHits(int wh) const;   // Return the number of hits

    void addHitWcal(StFcsHit*);              // Add a Wcal hit to the collection
    StSPtrVecFcsHit& hitsWcal();             // Return the hit list
    const StSPtrVecFcsHit& hitsWcal() const; // Return the hit list
    unsigned int numberOfHitsWcal() const;   // Return the number of hits in the collection

    void addHitHcal(StFcsHit*);              // Add a Hcal hit to the collection
    StSPtrVecFcsHit& hitsHcal();             // Return the hit list
    const StSPtrVecFcsHit& hitsHcal() const; // Return the hit list
    unsigned int numberOfHitsHcal() const;   // Return the number of hits in the collection

    //void addCluster(StFcsCluster*);              // Add a cluster to the collection
    //StSPtrVecFcsCluster& clusters();             // Return the cluster list
    //const StSPtrVecFcsCluster& clusters() const; // Return the cluster list   
    //unsigned int numberOfClusters() const;       // Return the number of clusters in the collection

    //void addPoint(StFcsPoint*);              // Add a point to the collection    
    //StSPtrVecFcsPoint& points();             // Return the point list
    //const StSPtrVecFcsPoint& points() const; // Return the point list
    //unsigned int numberOfPoints() const;     // Return the number of points in the collection
           
    int fcsReconstructionFlag()      const;
    void setFcsReconstructionFlag(int v);

    void print(int option=1);
    
private:
    StSPtrVecFcsHit     mHitsWcal;        // Owns all FCS Wcal hits
    StSPtrVecFcsHit     mHitsHcal;        // Owns all FCS Ecal hits

    //StSPtrVecFcsCluster mClusters;  // Owns all FCS clusters
    //StSPtrVecFcsPoint   mPoints;    // Owns all FCS points (photons)

    Int_t mFcsReconstructionFlag=0;   // undefined for now

    ClassDef(StFcsCollection, 1)
};

inline int StFcsCollection::fcsReconstructionFlag()      const {return mFcsReconstructionFlag;}
inline void StFcsCollection::setFcsReconstructionFlag(int v) {mFcsReconstructionFlag=v;}

#endif
