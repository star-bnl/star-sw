// \class  EzCorrEventLoop
// \author Renee Fatemi

#ifndef EzCorrEventLoop_h
#define EzCorrEventLoop_h
/*********************************************************************
 * $Id: EzCorrEventLoop.h,v 1.2 2004/07/26 22:54:26 rfatemi Exp $
 *********************************************************************
 * \class  EzCorrEventLoop
 * some description
 *
 *********************************************************************/

#include "TObject.h"
#include "CorrAna.h"
class TObjArray  ;
class EEstarTrig;
class EEmcEventHeader;
class EEfeeRawEvent;

class EzCorrEventLoop :public TObject, public  CorrAna{

 private:
  void unpackEzTreeHisto();
  void printCorrupt();
  EEmcEventHeader *eHead;
  EEfeeRawEvent  *eEve;
  EEstarTrig *eTrig;
  int EState;
  int EsmdState;
  int BState;
  int evt;

 public:  
  EzCorrEventLoop();
  virtual ~EzCorrEventLoop();
 
  void make();
  void set( TObjArray * hL, EEfeeRawEvent  *eE,EEmcEventHeader *eH, EEstarTrig *eT=0){ HList=hL; eHead=eH; eEve=eE; eTrig=eT; }; 
  void init();
  void setMode(int hold);
  ClassDef(EzCorrEventLoop,1) 
};


#endif

/*****************************************************************
 * $Log: EzCorrEventLoop.h,v $
 * Revision 1.2  2004/07/26 22:54:26  rfatemi
 * Corruption Update
 *
 * Revision 1.1  2004/07/24 22:51:08  balewski
 * first
 *
 * Revision 1.1  2004/06/06 04:54:08  balewski
 * dual analyzis
 *
 *
 ********************************************************************/

