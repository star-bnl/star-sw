// $Id: StObject.h,v 1.7 2000/07/30 01:40:12 perev Exp $
// $Log: StObject.h,v $
// Revision 1.7  2000/07/30 01:40:12  perev
// StMem class added
//
// Revision 1.6  2000/04/20 14:24:09  perev
// StArray fixes
//
// Revision 1.5  2000/04/18 02:57:26  perev
// StEvent browse
//
// Revision 1.4  1999/11/15 23:09:10  perev
// Streamer for StrArray and auto remove
//
// Revision 1.3  1999/10/30 19:36:02  perev
// Added clone() to StObject
//
// Revision 1.2  1999/06/23 20:31:04  perev
// StArray I/O + browser
//
// Revision 1.1  1999/04/30 13:15:56  fisyak
// Ad StObject, modification StArray for StRootEvent
//

#ifndef STAR_StObject
#define STAR_StObject
 
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StObject class is a base class to implement StEvent                  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TObject.h"
class StObject : public TObject {
  public:
  enum {kStARR = BIT(7), kStRRR = BIT(8)};


  StObject(){}
  virtual ~StObject();
  virtual void Browse(TBrowser *b);
  virtual Bool_t IsFolder();
  virtual TObject *clone() const {return 0;}
          Int_t    IsArr() const; 
protected:
          void     SetArr(Int_t a);  
private:
  ClassDef(StObject,1)  // Base class for StEvent
};
#endif
