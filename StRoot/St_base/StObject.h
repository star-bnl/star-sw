// $Id: StObject.h,v 1.11 2000/12/09 02:11:02 perev Exp $
// $Log: StObject.h,v $
// Revision 1.11  2000/12/09 02:11:02  perev
// clone() called Clone()
//
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
  virtual ~StObject();
  virtual void Browse(TBrowser *b);
  virtual Bool_t IsFolder() const;
  virtual TObject *clone() const {return ((TObject*)this)->Clone();}
  Int_t   isZombie() const {return IsZombie();}
  virtual void makeZombie(){MakeZombie();}
  ClassDef(StObject,2) // Base class for StEvent
};
#endif
