// \class  EzEEtowerExample
// \author Jan Balewski

#ifndef EzEEtowerExample_h
#define EzEEtowerExample_h
/*********************************************************************
 * $Id: EzEEtowerExample.h,v 1.1 2004/06/06 04:54:08 balewski Exp $
 *********************************************************************
 * \class  StEEtowerExampleMaker
 * \author Balewski
 * \date   
 * \brief  
 *
 Axample to access EEMC data & DB  from ezTree, StRoot-free
 Only ezTree data are decoded by this class 
 Uses EEtowers class to do any analysis
 *
 *********************************************************************/

#include "TObject.h"
#include "EEtower.h"

class TObjArray  ;
class EEstarTrig;
class EEmcEventHeader;
class EEfeeRawEvent;

class EzEEtowerExample :public TObject, public  EEtower{

 private:
  void unpackEzTree();
  EEmcEventHeader *eHead;
  EEfeeRawEvent  *eEve;
  EEstarTrig *eTrig;

 public:  
  EzEEtowerExample();
  virtual ~EzEEtowerExample();
  void make();
  void set( TObjArray * hL, EEDB *db, EEfeeRawEvent  *eE,EEmcEventHeader *eH, EEstarTrig *eT=0){ HList=hL; eeDb=db;eHead=eH; eEve=eE; eTrig=eT; }; 
    void init();
  ClassDef(EzEEtowerExample,1) 
};

#endif

/*****************************************************************
 * $Log: EzEEtowerExample.h,v $
 * Revision 1.1  2004/06/06 04:54:08  balewski
 * dual analyzis
 *
 *
 ********************************************************************/

