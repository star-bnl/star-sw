// \class  EzEEsoloPi0
// \author Jan Balewski

#ifndef EzEEsoloPi0_h
#define EzEEsoloPi0_h
/**************************************************************
 * $Id: EzEEsoloPi0.h,v 1.2 2004/09/03 04:50:52 balewski Exp $
 **************************************************************
 * \class  
 * \author Balewski
 * \date   
 * \brief  
 *
 Access EEMC data & DB  from ezTree, StRoot-free
 Only ezTree data are decoded by this class 
 Uses EEtowers class to do any analysis
 *
 ***********************************************************/

#include "TObject.h"
#include "EEsoloPi0.h"

class TObjArray  ;
class EEstarTrig;
class EEmcEventHeader;
class EEfeeRawEvent;

class EzEEsoloPi0 :public TObject, public  EEsoloPi0{
 private:
  void unpackEzTree();
  void unpackEzTail(); //T,P,Q,R
  void unpackEzSmd(); // U,V
  void unpackEzTrig(); // BBC, CTB

  EEmcEventHeader *eHead;
  EEfeeRawEvent  *eEve;
  EEstarTrig *eTrig;

 public:  
  EzEEsoloPi0();
  virtual ~EzEEsoloPi0();
  void make();
  void set( TObjArray * hL, EEDB *db, EEfeeRawEvent  *eE,EEmcEventHeader *eH, EEstarTrig *eT=0){ HList=hL; eeDb=db;eHead=eH; eEve=eE; eTrig=eT; }; 
    void init();
  ClassDef(EzEEsoloPi0,1) 
};

#endif

/*****************************************************************
 * $Log: EzEEsoloPi0.h,v $
 * Revision 1.2  2004/09/03 04:50:52  balewski
 * big clenup
 *
 * Revision 1.1  2004/08/26 04:39:40  balewski
 * towards pi0
 *
 * Revision 1.1  2004/06/12 04:09:24  balewski
 * start
 *
 * Revision 1.1  2004/06/06 04:54:08  balewski
 * dual analyzis
 *
 *
 ********************************************************************/

