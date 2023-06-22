/***************************************************************************
 *
 * $Id: StMuFcsCollection.h,v 1.0 2021/11/17 16:22:36 jdb Exp $
 *
 * Author: jdb, 2021
 ***************************************************************************
 *
 * Description: Fcs data interface to StMuFcsHit, StMuFcsCluster and StMuFcsPoint
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#ifndef StMuFcsCollection_hh
#define StMuFcsCollection_hh

#include <TObject.h>
#include "TClonesArray.h"

class StMuFcsHit;
class StMuFcsCluster;
class StMuFcsPoint;
class StMuFcsInfo;

class StMuFcsCollection : public TObject {
public:
    StMuFcsCollection();
    ~StMuFcsCollection();
    
    void            init();
    StMuFcsHit*     addHit();
    StMuFcsCluster* addCluster();
    StMuFcsPoint*   addPoint();
    StMuFcsInfo*    addInfo();

    unsigned int    numberOfHits() const;
    unsigned int    numberOfClusters() const;
    unsigned int    numberOfPoints() const;

    void            setFcsHitArray(TClonesArray *array) {mHits=array;};
    void            setFcsClusterArray(TClonesArray* array) {mClusters=array;}
    void            setFcsPointArray(TClonesArray* array) {mPoints=array;}
    void            setFcsInfoArray(TClonesArray* array) {mInfo=array;}

    // helpers to set info directly
    Int_t           fcsReconstructionFlag(); 
    void            setFcsReconstructionFlag( Int_t v );

    StMuFcsHit*     getHit(int index);
    StMuFcsCluster* getCluster(int index);
    StMuFcsPoint*   getPoint(int index);
    StMuFcsInfo*    getInfo();

    unsigned int    indexOfFirstHit( unsigned int idet );
    unsigned int    indexOfFirstCluster( unsigned int idet );
    unsigned int    indexOfFirstPoint( unsigned int idet );

    unsigned int    numberOfHits( unsigned int idet );
    unsigned int    numberOfClusters( unsigned int idet );
    unsigned int    numberOfPoints( unsigned int idet );

    TClonesArray*   getHitArray() { return mHits; };
    TClonesArray*   getClusterArray() { return mClusters; }
    TClonesArray*   getPointArray() { return mPoints; }
    TClonesArray*   getInfoArray() { return mInfo; }

private:
    TClonesArray* mHits=0;
    TClonesArray* mClusters=0;
    TClonesArray* mPoints=0;
    TClonesArray* mInfo=0;

    ClassDef(StMuFcsCollection,1)
};
#endif
