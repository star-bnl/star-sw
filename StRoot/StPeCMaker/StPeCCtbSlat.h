//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCCtbSlat.h,v 1.1 2001/02/21 20:44:12 yepes Exp $
// $Log: StPeCCtbSlat.h,v $
// Revision 1.1  2001/02/21 20:44:12  yepes
// Add ctb signals to tree
//
// Revision 1.1  2000/04/21 19:12:25  nystrand
// First Version
//
// Revision 1.1  2000/03/24 22:36:56  nystrand
// First version of StPeCCtbSlat
//
// Revision 1.0  2000/01/20 23:28:51  nystrand
// First Version of StPeCCtbSlat 
//
//////////////////////////////////////////////////////////////////////
//
// StPeCCtbSlat
//
// Pair class for Peripheral Collisions
//
//////////////////////////////////////////////////////////////////////
#ifndef StPeCCtbSlat_h
#define StPeCCtbSlat_h
#include "Rtypes.h"
#include "TObject.h"

class StPeCCtbSlat : public TObject {

public:

                                  StPeCCtbSlat();
                                  StPeCCtbSlat(Byte_t iphi, Byte_t i_eta, Int_t adc);
  virtual                         ~StPeCCtbSlat();

  Byte_t                          i_phi ;
  Byte_t                          i_eta ;
  Int_t                           adc   ;

  ClassDef(StPeCCtbSlat,1)
};

#endif





