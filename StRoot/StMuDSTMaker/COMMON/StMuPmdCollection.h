/*****************************************************************
 * $Id: StMuPmdCollection.h,v 1.5 2005/10/28 05:18:32 mvl Exp $
 *
 * Class : StMuPmdCollection
 * Author: Supriya Das & Subhasis Chattopadhyay
 * ****************************************************************
 *
 * Description: This class holds the PMD clusters for MuDst
 * ****************************************************************
 * $Log: StMuPmdCollection.h,v $
 * Revision 1.5  2005/10/28 05:18:32  mvl
 * Added getters for hit and cluster arrays on requets of Subhasis
 *
 * Revision 1.4  2004/10/19 01:39:35  mvl
 * Changed for splitting on file. Added support for hits
 *
 * Revision 1.3  2004/05/02 04:10:14  perev
 * private => protected
 *
 * Revision 1.2  2004/04/26 00:11:07  perev
 * forward declaration of StMuPmdCluster
 *
 * Revision 1.1  2004/04/02 03:36:21  jeromel
 * New files for PMD
 *
 * ****************************************************************/

#ifndef StMuPmdCollection__h
#define StMuPmdCollection__h
 
#include "TObject.h"
#include "TClonesArray.h"
#include "Stiostream.h"

class StMuPmdCluster;
class StMuPmdHit;

enum {pmd=1, cpv=2};
class StMuPmdCollection: public TObject
{
  public:
                      StMuPmdCollection();
                      StMuPmdCollection(StMuPmdCollection&);
    virtual           ~StMuPmdCollection();
    void              clear(Option_t *option="");     
    void              Clear(Option_t *option=""){clear(option);}     
    void              DeleteThis();
    
    TClonesArray     *getClusterArray(int detector) {return mPmdClusters[detector];}
    int               getNClusters(int detector);
    StMuPmdCluster*   getCluster(int clusterId,int detector);    

    void              addCluster(int detector);
        
    TClonesArray     *getHitArray(int detector) {return mPmdHits[detector];}
    int               getNHits(int detector);
    StMuPmdHit*       getHit(int hitId,int detector);    

    void              addHit(int detector);

    void              setPmdHitArray(TClonesArray *array) {mPmdHits[0]=array;}
    void              setCpvHitArray(TClonesArray *array) {mPmdHits[1]=array;}
    void              setPmdClusterArray(TClonesArray *array) {mPmdClusters[0]=array;}
    void              setCpvClusterArray(TClonesArray *array) {mPmdClusters[1]=array;}
  protected:
    void              init(int detector);
    
    TClonesArray*     mPmdClusters[2];
    TClonesArray*     mPmdHits[2];
    
    ClassDef(StMuPmdCollection,2)
};



#endif  
    
