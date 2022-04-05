/****************************************************************
 * $Id: StMuEzTree.h,v 1.2 2004/11/29 15:55:07 mvl Exp $
 *
 * Author: Wei-Ming zhang           KSU  Aug. 2004
 *
 *****************************************************************
 * Description:
 * Interface between ezTree and MuDst 
 *
 *****************************************************************/

#ifndef StMuEzTree_h
#define StMuEzTree_h

#include "StObject.h"

class StEvent;
class StEmcRawData;
class EztEventHeader;
class EztTrigBlob;
class EztFpdBlob;
class EztEmcRawData;



class StMuEzTree {
 public:
  enum {mxETowCrate=6}; 

  StMuEzTree();
  virtual ~StMuEzTree();
  
  EztEventHeader*  copyHeader(StEvent* ev);
  EztTrigBlob*     copyTrig(StEvent* ev);
  EztFpdBlob*      copyFpd(StEvent* ev);
  EztEmcRawData*   copy(StEmcRawData *inp, int i1, int i2); // working horse
  EztEmcRawData*   copyETow(StEmcRawData *inp);
  EztEmcRawData*   copyESmd(StEmcRawData *inp);
     
  ClassDef(StMuEzTree,3)
};
    
#endif /* StMuEzTree */
