#ifndef EEfeeRawEvent_h
#define EEfeeRawEvent_h
/*********************************************************************
 * $Id: EEfeeRawEvent.h,v 1.12 2004/09/07 20:32:01 balewski Exp $
 *********************************************************************
 * Descripion:
 * STAR Endcap Electromagnetic Calorimeter Raw FEE Events
 *********************************************************************/
#include "TObject.h"
#include "TClonesArray.h"

class EEfeeDataBlock;

class EEfeeRawEvent :public TObject {
  int ID; // event ID

 public:
  // different content of header data
  enum HeadVer {headVer1,headVer2,headVer3};
  // see ::maskWrongCrates() for explanation
  TClonesArray  *block; 
  
  EEfeeRawEvent();
  virtual ~EEfeeRawEvent();
  void print(int flag=1) const;
  void clear();
  void setID(int i){ ID=i; }
  int  getID() const{return ID;};
  int getNGoodBlock();
  void addFeeDataBlock(EEfeeDataBlock*);
  int maskWrongCrates( long timeStamp,   unsigned token, HeadVer ver=headVer1);
  void maskBEMC(); // tmp
  UShort_t  getValue(int crateID, int channel) const;
  ClassDef(EEfeeRawEvent,1) 
};
#endif

/*
 * $Log: EEfeeRawEvent.h,v $
 * Revision 1.12  2004/09/07 20:32:01  balewski
 * more methods, remove questionable spin bits interpetation
 *
 * Revision 1.11  2004/07/09 02:38:05  balewski
 * BTOW data are not masked out any more but headres are checked as for EEMC
 *
 * Revision 1.10  2004/06/01 16:05:18  balewski
 * forgoten update of data block headers check
 *
 * Revision 1.9  2004/04/02 06:38:52  balewski
 * *** empty log message ***
 *
 * Revision 1.8  2003/12/10 04:43:19  balewski
 * first QA
 *
 * Revision 1.7  2003/12/02 17:22:08  balewski
 * fix after version mixup
 *
 * Revision 1.5  2003/11/24 05:40:55  balewski
 * new stuff for miniDaq
 *
 * Revision 1.4  2003/11/22 05:35:39  balewski
 * *** empty log message ***
 *
 * Revision 1.3  2003/11/20 22:59:40  balewski
 * *** empty log message ***
 *
 * Revision 1.2  2003/11/20 16:01:46  balewski
 * towars run 4
 *
 * Revision 1.1  2003/01/28 23:17:14  balewski
 * start
 *
 * Revision 1.1  2002/11/30 20:04:37  balewski
 * start
 *
 *
 *********************************************************************/


