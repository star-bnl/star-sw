#ifndef EztTrigBlob_h
#define EztTrigBlob_h
/******************************************************************
 * $Id: EztTrigBlob.h,v 1.2 2004/11/29 15:55:07 mvl Exp $
 ******************************************************************
 container for FULL STAR trigger data, requires StTriggerDataMother  to unpack it
*/

#include <TObject.h>

class TArrayC ;
class EztTrigBlob : public TObject {
 public:
  
  // data containers
  TArrayC *trgd;  //  trgData  bank
  TArrayC *trgid; //  trgId bank
  UChar_t version; // version for unpacking

  // methods
  EztTrigBlob();
  ~EztTrigBlob();
  void     print(int k=0, FILE *fd=stdout) const;
  void     clear();
  UChar_t  getVersion() const{ return version;}
  void     setVersion( UChar_t  v) { version=v;  }

  ClassDef(EztTrigBlob,2) 

};
#endif


/*
 * $Log: EztTrigBlob.h,v $
 * Revision 1.2  2004/11/29 15:55:07  mvl
 * Additions by Jan for Fpd ezTree
 *
 * Revision 1.1  2004/10/28 00:10:19  mvl
 * Initial revision of ezTree classes (for EEmc raw data)
 *
 * Revision 1.1  2004/06/18 01:46:35  balewski
 *
 *********************************************************************/
