//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StChallenger                                                         //
//                                                                      //
// Class enables the interface to the Grand Challenge services.         //
//                                                                      //
// To get access to the services use the static method Challenge().     //
// Challenge() loads the appropriate shared libraries                   //
// that implement the real interface.                                   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StChallenger
#define STAR_StChallenger


#include "StFileI.h"

class StChallenger : public StFileI {

 public:

  virtual ~StChallenger(){}

  static StChallenger *Challenge();
  
  ClassDef(StChallenger,0)

    };
    
#endif
