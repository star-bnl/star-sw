//////////////////////////////////////////////////////////////////////
//
// $Id: StEpcConstants.h,v 1.2 2001/11/06 23:35:27 suaide Exp $
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

class Epc
{
  public:
  enum
  { 
    nModule =120,
	  nPhiBin=10,
	  nMaxNoOfClinBin=10,
	  nMaxNoOfTrinBin=10
  };

  ClassDef(Epc,1)  // macro for rootCint
};
#endif
