/*****************************************************************
 * $Id: StMuPmdCollection.h,v 1.3 2004/05/02 04:10:14 perev Exp $
 *
 * Class : StMuPmdCollection
 * Author: Supriya Das & Subhasis Chattopadhyay
 * ****************************************************************
 *
 * Description: This class holds the PMD clusters for MuDst
 * ****************************************************************
 * $Log: StMuPmdCollection.h,v $
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
    
    int               getNClusters(int detector);
    StMuPmdCluster*   getCluster(int clusterId,int detector);    

    void              addCluster(int detector);
        
  protected:
    void              init(int detector);
    void              packbits(unsigned char*, unsigned int, unsigned int, unsigned int);
    unsigned int      unpackbits(unsigned char*, unsigned int, unsigned int);
    
    TClonesArray*     mPmdClusters[2];
    
    ClassDef(StMuPmdCollection,1)
};



#endif  
    
