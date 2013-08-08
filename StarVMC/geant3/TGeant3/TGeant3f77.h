#ifndef ROOT_TGeant3f77
#define ROOT_TGeant3f77 
/* $Id: TGeant3f77.h,v 1.2 2013/08/08 17:20:00 jwebb Exp $ */

//////////////////////////////////////////////////// 
//  C++/f77 interface to Geant3 basic routines    // 
////////////////////////////////////////////////////

#include "TGeant3.h" 
#include <assert.h>

class TGeant3f77 : public TGeant3 { 

public: 
  TGeant3f77(); 
  TGeant3f77(const char *title, Int_t nwgeant=0); 
  virtual ~TGeant3f77() {}
 
  // NOTE:  THe following methods have appeared in the abstract base class
  //        sometime after ROOT 5.22.00.  They indicate that functionality
  //        has been added to TGeant3TGeo which we are lacking.
  //
  //  ***  We need to import current versions from ALICE  ***
  void SetUserParameters( Bool_t ){ assert(0); }
  void SetCollectTracks( Bool_t ){ assert(0); }
  Bool_t IsCollectTracks() const { assert(0); return false; }


private:
  TGeant3f77(const TGeant3f77 &tg3);
  TGeant3f77 & operator=(const TGeant3f77 &tg3);
  
  ClassDef(TGeant3f77,1)  //C++/f77 interface to Geant basic routines 
}; 
#endif //ROOT_TGeant3f77
