//////////////////////////////////////////////////////////////////////
//
// Revision 1.0  2001/2/15           Pablo Yepes: yepes@rice.edu
//
//////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include "StPeCCtbSlat.h"

ClassImp(StPeCCtbSlat)

StPeCCtbSlat::StPeCCtbSlat() {
   i_phi = 0 ;
   i_eta = 0 ;
   adc   = 0 ;
}

StPeCCtbSlat::StPeCCtbSlat( Byte_t _i_phi, Byte_t _i_eta, Int_t _adc ) {
   i_phi = _i_phi ;
   i_eta = _i_eta ;
   adc   = _adc ;
}

StPeCCtbSlat::~StPeCCtbSlat() {
}



