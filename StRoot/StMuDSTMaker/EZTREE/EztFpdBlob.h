#ifndef EztFpdBlob_h
#define EztFpdBlob_h
/*********************************************************************
 * $Id: EztFpdBlob.h,v 1.1 2004/11/29 15:55:55 mvl Exp $
 *********************************************************************
 * container for FPD raw data
 */

#include <TObject.h>

class TArrayS ;
class EztFpdBlob : public TObject {
 public:
  
  // data containers
  TArrayS *smd;  
  // methods
  EztFpdBlob();
  ~EztFpdBlob();
  void     print(int k=0, FILE *fd=stdout) const;
  void     clear();

  ClassDef(EztFpdBlob,1) 

};
#endif


/*
 * $Log: EztFpdBlob.h,v $
 * Revision 1.1  2004/11/29 15:55:55  mvl
 * New ezTree classes to hold Fpd info
 *
 * Revision 1.1  2004/10/28 00:10:19  mvl
 
 *
 * Revision 1.1  2004/06/18 01:46:35  balewski
 *
 *********************************************************************/
