#ifndef STMCEVENTINTERFACE_H
#define STMCEVENTINTERFACE_H

/* $Id: StMcEventInterface.h,v 1.1 2004/07/13 19:05:41 potekhin Exp $ */

////////////////////////////////////////////////////////
//                                                    //
//                                                    //
////////////////////////////////////////////////////////

#include <TNamed.h>
#include "StMcEvent.hh"


// 
class StMcEventInterface {

 public:
  StMcEventInterface() {};
  virtual ~StMcEventInterface() {};

  static void FinishEventCB(void);


 private:
  int foo;
  ClassDef(StMcEventInterface,0)
    };
#endif //STMCEVENTINTERFACE_H
