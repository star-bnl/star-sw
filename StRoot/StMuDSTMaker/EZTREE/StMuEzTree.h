/****************************************************************
 * $Id: StMuEzTree.h,v 1.1 2004/10/28 00:10:20 mvl Exp $
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
class EztEmcRawData;


class StMuEzTree {
 public:
  enum {mxETowCrate=6}; 

  StMuEzTree();
  virtual ~StMuEzTree();
  
  EztEventHeader*  getHeader(StEvent* ev);
  EztTrigBlob*     getTrig(StEvent* ev);
  EztEmcRawData*   copy(StEmcRawData *inp, int i1, int i2); // working horse
  EztEmcRawData*   copyETow(StEmcRawData *inp);
  EztEmcRawData*   copyESmd(StEmcRawData *inp);
     
  ClassDef(StMuEzTree,2)
};
    
#endif /* StMuEzTree */
