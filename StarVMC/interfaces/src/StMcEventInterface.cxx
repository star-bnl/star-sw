// $Id: StMcEventInterface.cxx,v 1.1 2004/07/13 19:08:01 potekhin Exp $
// $Log: StMcEventInterface.cxx,v $
// Revision 1.1  2004/07/13 19:08:01  potekhin
// Interface to StMcEvent
//

#include <TROOT.h>
#include <iostream.h>
#include "StMcEventInterface.h"



ClassImp(StMcEventInterface)


//_______________________________________________________________________
void StMcEventInterface::FinishEventCB(void) {
  cout<<"finish"<<endl;
}
