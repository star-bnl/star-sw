// \class  EzEEsmdCal
// \author Jan Balewski

#ifndef EzEEsmdCal_h
#define EzEEsmdCal_h
/*********************************************************************
 * $Id: EzEEsmdCal.h,v 1.1 2004/06/12 04:09:24 balewski Exp $
 *********************************************************************
 * \class  
 * \author Balewski
 * \date   
 * \brief  
 *
 Access EEMC data & DB  from ezTree, StRoot-free
 Only ezTree data are decoded by this class 
 Uses EEtowers class to do any analysis
 *
 *********************************************************************/

#include "TObject.h"
#include "EEsmdCal.h"

class TObjArray  ;
class EEstarTrig;
class EEmcEventHeader;
class EEfeeRawEvent;

class EzEEsmdCal :public TObject, public  EEsmdCal{
 private:
  void unpackEzTree();
  void unpackEzTail(); //T,P,Q,R
  void unpackEzSmd(); // U,V

  EEmcEventHeader *eHead;
  EEfeeRawEvent  *eEve;
  EEstarTrig *eTrig;

 public:  
  EzEEsmdCal(int sect);
  virtual ~EzEEsmdCal();
  void make();
  void set( TObjArray * hL, EEDB *db, EEfeeRawEvent  *eE,EEmcEventHeader *eH, EEstarTrig *eT=0){ HList=hL; eeDb=db;eHead=eH; eEve=eE; eTrig=eT; }; 
    void init();
  ClassDef(EzEEsmdCal,1) 
};

#endif

/*****************************************************************
 * $Log: EzEEsmdCal.h,v $
 * Revision 1.1  2004/06/12 04:09:24  balewski
 * start
 *
 * Revision 1.1  2004/06/06 04:54:08  balewski
 * dual analyzis
 *
 *
 ********************************************************************/

