#ifndef ROOT_StTGeant3f77
#define ROOT_StTGeant3f77 
/* Copyright(c) 1998-1999, ALICE Experiment at CERN, All rights reserved. *
 * See cxx source for full Copyright notice                               */

/* $Id: StTGeant3f77.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $ */

//////////////////////////////////////////////// 
//  C++/f77 interface to Geant3 basic routines    // 
//////////////////////////////////////////////// 

#include "StTGeant3.h" 

class StTGeant3f77 : public StTGeant3 { 

public: 
  StTGeant3f77(); 
  StTGeant3f77(const char *title, Int_t nwgeant=0); 
  virtual ~StTGeant3f77() {}
 

private:
  StTGeant3f77(const StTGeant3f77 &) {}
  StTGeant3f77 & operator=(const StTGeant3f77&) {return *this;}
  
  ClassDef(StTGeant3f77,1)  //C++/f77 interface to Geant basic routines 
}; 
#endif //ROOT_StTGeant3f77
