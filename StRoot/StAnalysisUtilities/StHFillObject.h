// $Id: StHFillObject.h,v 1.1 1999/08/31 20:40:20 genevb Exp $
// $Log: StHFillObject.h,v $
// Revision 1.1  1999/08/31 20:40:20  genevb
// Introduction of library and inclusion of StHFillObject
//
// Revision 1.3  1999/08/10 21:29:21  genevb
// Use formulas, separate headers, use StMessMgr, spaces no longer separate
//
// Revision 1.2  1999/08/03 02:29:36  genevb
// Re-implemented using TMethodCall's
//
// Revision 1.1  1999/07/29 23:27:34  genevb
// Introduction of new class
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StHFillObject allows member functions to be histogrammed             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StHFillObject
#define STAR_StHFillObject

#include "TObject.h"

class StHFillObject : public TObject {
 private:
 protected:
 public:
  StHFillObject();
  virtual ~StHFillObject();
   static void Reset();
  virtual void Draw(Option_t* option);
  virtual void Print(Option_t* option);
  virtual void ls(Option_t* option);
  virtual void Update();
  ClassDef(StHFillObject,1)   //virtual base class for Makers
};

#endif
