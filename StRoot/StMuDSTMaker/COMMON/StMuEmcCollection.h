/*!\class StMuEmcCollection
\author Alexandre A. P. Suaide

This is the EMC micro collection structure. From here it is possible
to get all EMC information (hits, clusters or points). <br><br>
EmcDet is defined as follow:
  - 1 = bemc
  - 2 = bprs
  - 3 = bsmde
  - 4 = bsmdp
Hit id, cluster id and point id starts from zero.
Tower id starts from 1 and goes to 4800 to match EMC Id
*/
#ifndef StMuEmcCollection__h
#define StMuEmcCollection__h
 
#include "TObject.h"
#include "TObjArray.h"
#include "StMuEmcHit.h"
#include "StMuEmcCluster.h"
#include "StMuEmcPoint.h"

#include <iostream>

enum {bemc=0, bprs=1, bsmde=2, bsmdp=3};

class StMuEmcCollection: public TObject
{
  public:
                      StMuEmcCollection();
                      StMuEmcCollection(StMuEmcCollection&);
    virtual           ~StMuEmcCollection();
    
    int               getTowerADC(int);    
    int               getNSmdHits(int);
    StMuEmcHit*       getSmdHit(int,int);    
    int               getNClusters(int);
    StMuEmcCluster*   getCluster(int,int);    
    int               getNPoints();
    StMuEmcPoint*     getPoint(int);
        
    void              setTowerADC(int,int);
    void              addSmdHit(int,StMuEmcHit*);
    void              addCluster(int,StMuEmcCluster*);
    void              addPoint(StMuEmcPoint*);
    
    void              clear(Option_t *option="");
     
    void              DeleteThis() { 
	for ( int i=0; i<2; i++) mSmdHits[i]->Delete(); 
	for ( int i=0; i<4; i++) mEmcClusters[i]->Delete(); 
	mEmcPoints->Delete(); 
    }
  private:
    void              packbits(unsigned char*, unsigned int, unsigned int, unsigned int);
    unsigned int      unpackbits(unsigned char*, unsigned int, unsigned int);
    
    unsigned char     mTowerADC[7200];
    short             mNSmdHits[2];
    short             mNClusters[4];
    short             mNPoints;
    
    TObjArray*        mSmdHits[2];
    TObjArray*        mEmcClusters[4];
    TObjArray*        mEmcPoints;
    
    ClassDef(StMuEmcCollection,1)
};



#endif  
    
