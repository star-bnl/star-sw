/***************************************************************************
 *
 * $Id: StMuFmsCollection.h,v 1.6 2017/08/14 16:22:36 smirnovd Exp $
 *
 * Author: Jingguo Ma, Jan 2010
 ***************************************************************************
 *
 * Description: FMS data interface to StMuFmsHit, StMuFmsCluster and StMuFmsPoint
 *
 ***************************************************************************
 *
 * $Log: StMuFmsCollection.h,v $
 * Revision 1.6  2017/08/14 16:22:36  smirnovd
 * Recover FMS hits using StTriggerData
 *
 * commit 6d7358f4c86a15edd0671326580d291a9843aec9
 * Date:   Tue Aug 8 23:42:41 2017 -0400
 *
 *     StMuFmsUtil: Recover FMS hits using StTriggerData
 *
 * commit 556d07cb8fd87cb62e4ac674226423671c94917d
 * Date:   Tue Aug 8 23:42:34 2017 -0400
 *
 *     StMuFmsUtil: Added func to fill StMuFmsCollection with FMS hits from StTriggerData in StMuEvent
 *
 * commit c355529c1ee401849b2b81d74df8d452886593d1
 * Date:   Tue Aug 8 23:42:19 2017 -0400
 *
 *     [Cosmetic] Changes in whitespace
 *
 * commit 67fdc1b348bebbfbfb137b726ee9c455a7d8be37
 * Date:   Mon Jun 5 12:00:24 2017 -0400
 *
 *     StMuFmsCollection::addHit() Return pointer to just added default FMS hit object
 *
 * Revision 1.5  2015/11/06 17:47:16  jdb
 * Added StMuFmsInfo.{h,cxx} as a new branch for storing event-by-event FMS paramters
 *
 * Revision 1.4  2015/10/23 19:22:49  jdb
 * akio added mFmsReconstructionFlag and related getters and setters. pushed version number of StMuFmsCollection. Corresponding changes for reconstruction flag in StMuFmsUtil.cxx
 *
 * Revision 1.3  2015/10/16 18:13:28  jdb
 * incremented version # for StMuFmsCollection and StMuFmsPoint
 *
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
class StMuFmsInfo;

class StMuFmsCollection : public TObject {
public:
    StMuFmsCollection();
    ~StMuFmsCollection();
    
    void          init();
    StMuFmsHit*   addHit();
    void          addCluster();
    void          addInfo();
    StMuFmsPoint* addPoint();
    unsigned int  numberOfHits() const;
    unsigned int  numberOfClusters() const;
    unsigned int  numberOfPoints() const;
    void          setFmsHitArray(TClonesArray *array) {mHits=array;};
    void          setFmsClusterArray(TClonesArray* array) {mClusters=array;}
    void          setFmsPointArray(TClonesArray* array) {mPoints=array;}
    void          setFmsInfoArray(TClonesArray* array) {mInfo=array;}

    StMuFmsHit* getHit(int hitId);
    StMuFmsCluster* getCluster(int index);
    StMuFmsPoint* getPoint(int index);
    StMuFmsInfo* getInfo();
    TClonesArray* getHitArray() { return mHits; };
    TClonesArray* getClusterArray() { return mClusters; }
    TClonesArray* getPointArray() { return mPoints; }
    

    Int_t fmsReconstructionFlag();
    void setFmsReconstructionFlag(Int_t v);

private:
    TClonesArray* mHits;
    TClonesArray* mClusters;
    TClonesArray* mPoints;
    TClonesArray* mInfo;

    ClassDef(StMuFmsCollection,5)
};
#endif
