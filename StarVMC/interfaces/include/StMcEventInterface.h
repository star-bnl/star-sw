#ifndef STMCEVENTINTERFACE_H
#define STMCEVENTINTERFACE_H

/* $Id: StMcEventInterface.h,v 1.2 2004/07/16 22:57:44 potekhin Exp $ */

////////////////////////////////////////////////////////
//                                                    //
//                                                    //
////////////////////////////////////////////////////////

#include <TObjArray.h>
#include "StMcEvent.hh"


// 
class StMcEventInterface {

 public:
  StMcEventInterface() {};
  virtual ~StMcEventInterface() {};

  static void FinishEventCB(TObjArray* hits_);


 private:
  int foo;
  ClassDef(StMcEventInterface,0)
    };
#endif //STMCEVENTINTERFACE_H
