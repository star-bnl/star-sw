//////////////////////////////////////////////////////////////////////
//
// $Id: StEpcConstants.h,v 1.1 2000/05/15 21:18:31 subhasis Exp $
//
// Author: Subhasis Chattopadhyay 
//////////////////////////////////////////////////////////////////////
//
// Description: constants for the Epc maker
//////////////////////////////////////////////////////////////////////
//

#ifndef StEpcConstants_h
#define StEpcConstants_h
#include "Rtypes.h"

class Epc{

 public:

  enum{ nModule =120,
	nPhiBin=10,
	nMaxNoOfClinBin=10,
	nMaxNoOfTrinBin=10};

  ClassDef(Epc,1)  // macro for rootCint
    };
#endif
