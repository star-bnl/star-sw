// $Id: StObject.h,v 1.2 1999/06/23 20:31:04 perev Exp $
// $Log: StObject.h,v $
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
  ClassDef(StObject,1) // Base class for StEvent
  virtual void Browse(TBrowser *b);
  static  int Browse(const TObject *This, TBrowser *b);
  virtual Bool_t IsFolder(){return Browse(this,0);};
};
#endif
