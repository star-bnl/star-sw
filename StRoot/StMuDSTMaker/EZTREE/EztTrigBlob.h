#ifndef EztTrigBlob_h
#define EztTrigBlob_h
/*********************************************************************
 * $Id: EztTrigBlob.h,v 1.1 2004/10/28 00:10:19 mvl Exp $
 *********************************************************************
 * container for FULL STAR trigger data, requires Akio's calss to unpack it
 */

#include <TObject.h>

class TArrayC ;
class EztTrigBlob : public TObject {
 public:
  
  // data containers
  TArrayC *trgd;  //  trgData  bank
  TArrayC *trgid; //  trgId bank
  time_t   unixTimeStamp; // to chose year-dependent decoder

  // methods
  EztTrigBlob();
  ~EztTrigBlob();
  void     print(int k=0, FILE *fd=stdout) const;
  void     clear();
  time_t   getTimeStamp() const{ return unixTimeStamp;}
  void     setTimeStamp ( time_t  t) { unixTimeStamp  = t;  }

  ClassDef(EztTrigBlob,1) 

};
#endif


/*
 * $Log: EztTrigBlob.h,v $
 * Revision 1.1  2004/10/28 00:10:19  mvl
 * Initial revision of ezTree classes (for EEmc raw data)
 *
 * Revision 1.1  2004/06/18 01:46:35  balewski
 *
 *********************************************************************/
