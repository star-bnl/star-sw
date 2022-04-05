/*!\class StEmcMicroCollection
\author Alexandre A. P. Suaide

This is the EMC micro collection structure. From here it is possible
to get all EMC information (hits, clusters or points). <br><br>
EmcDet is defined as follow:
  - 1 = bemc
  - 2 = bprs
  - 3 = bsmde
  - 4 = bsmdp
Hit id, cluster id and point id starts from zero.
*/
#ifndef StEmcMicroCollection__h
#define StEmcMicroCollection__h
 
#include "TObject.h"
#include "TObjArray.h"
#include "StEmcMicroHit.h"
#include "StEmcMicroCluster.h"
#include "StEmcMicroPoint.h"

class StEmcMicroCollection: public TObject
{
  public:
                      StEmcMicroCollection();
    virtual           ~StEmcMicroCollection();
    
    StEmcMicroHit*    getHit(Int_t EmcDet,Int_t HitId)    { return (StEmcMicroHit*) mEmcHits[EmcDet-1]->At(HitId); }        ///< Return Hit for a given EMC sub detector
    Int_t             getNHits(Int_t EmcDet)              { return mEmcHits[EmcDet-1]->GetEntries(); }                      ///< Return Number of Hits for a given EMC sub detector

    StEmcMicroCluster*getCluster(Int_t EmcDet,Int_t ClId) { return (StEmcMicroCluster*) mEmcClusters[EmcDet-1]->At(ClId); } ///< Return Cluster for a given EMC sub detector
    Int_t             getNClusters(Int_t EmcDet)          { return mEmcClusters[EmcDet-1]->GetEntries(); }                  ///< Return Number of Cluster for a given EMC sub detector
    
    StEmcMicroPoint*  getPoint(Int_t PtId)                { return (StEmcMicroPoint*)mEmcPoints->At(PtId); }                ///< Return Point for a given EMC sub detector
    Int_t             getNPoints()                        { return mEmcPoints->GetEntries(); }                              ///< Return Number of Points for a given EMC sub detector
    
    void              addHit(Int_t EmcDet,StEmcMicroHit* hit)              { mEmcHits[EmcDet-1]->AddLast(hit); }            
    void              addCluster(Int_t EmcDet,StEmcMicroCluster* cluster)  { mEmcClusters[EmcDet-1]->AddLast(cluster); }
    void              addPoint(StEmcMicroPoint* point)                     { mEmcPoints->AddLast(point); }
    void              clear(Option_t *option="");
        
  private:
    TObjArray*        mEmcHits[4];
    TObjArray*        mEmcClusters[4];
    TObjArray*        mEmcPoints;
    
    ClassDef(StEmcMicroCollection,1)
};
#endif  
    
