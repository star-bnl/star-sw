// $Id: StObject.h,v 1.1 1999/04/30 13:15:56 fisyak Exp $
// $Log: StObject.h,v $
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
};
#endif
