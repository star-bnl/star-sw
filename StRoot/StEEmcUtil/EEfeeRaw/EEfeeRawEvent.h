#ifndef EEfeeRawEvent_h
#define EEfeeRawEvent_h
/*********************************************************************
 * $Id: EEfeeRawEvent.h,v 1.4 2003/11/22 05:35:39 balewski Exp $
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
  TClonesArray  *block;

  EEfeeRawEvent();
  virtual ~EEfeeRawEvent();
  void print(int flag=1) const;
  void clear();
  void setID(int i){ ID=i; }
  int  getID() const{return ID;};
  void addFeeDataBlock(EEfeeDataBlock*);
  void maskWrongCrates( long timeStamp);
  UShort_t  getValue(int crateID, int channel) const;
  ClassDef(EEfeeRawEvent,1) 
};
#endif

/* $Log: EEfeeRawEvent.h,v $
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


