/***************************************************************************
 *
 * $Id: StMuFmsCollection.h,v 1.2 2015/08/28 18:36:04 jdb Exp $
 *
 * Author: Jingguo Ma, Jan 2010
 ***************************************************************************
 *
 * Description: FMS data interface to StMuFmsHit, StMuFmsCluster and StMuFmsPoint
 *
 ***************************************************************************
 *
 * $Log: StMuFmsCollection.h,v $
 * Revision 1.2  2015/08/28 18:36:04  jdb
 * Added Akios FMS codes
 *
 * Revision 1.1  2010/01/25 03:57:39  tone421
 * Added FMS and Roman pot arrays
 *
 **************************************************************************/
#ifndef StMuFmsCollection_hh
#define StMuFmsCollection_hh

#include "St_base/StObject.h"
#include "TClonesArray.h"

class StMuFmsHit;
class StMuFmsCluster;
class StMuFmsPoint;

class StMuFmsCollection : public StObject {
public:
    StMuFmsCollection();
    ~StMuFmsCollection();
    
    void          init();
    void          addHit();
    void          addCluster();
    StMuFmsPoint* addPoint();
    unsigned int  numberOfHits() const;
    unsigned int  numberOfClusters() const;
    unsigned int  numberOfPoints() const;
    void          setFmsHitArray(TClonesArray *array) {mHits=array;};
    void          setFmsClusterArray(TClonesArray* array) {mClusters=array;}
    void          setFmsPointArray(TClonesArray* array) {mPoints=array;}

    StMuFmsHit* getHit(int hitId);
    StMuFmsCluster* getCluster(int index);
    StMuFmsPoint* getPoint(int index);
    TClonesArray* getHitArray() { return mHits; };
    TClonesArray* getClusterArray() { return mClusters; }
    TClonesArray* getPointArray() { return mPoints; }
    
private:
    TClonesArray* mHits;
    TClonesArray* mClusters;
    TClonesArray* mPoints;
    
    ClassDef(StMuFmsCollection,2)
};
#endif
