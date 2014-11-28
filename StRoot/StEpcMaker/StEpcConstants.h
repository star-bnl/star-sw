//////////////////////////////////////////////////////////////////////
//
// $Id: StEpcConstants.h,v 1.4 2006/03/24 19:31:15 suaide Exp $
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
        nMaxNoOfClinBin=15,
        nMaxNoOfTrinBin=15
    };

    ClassDef(Epc,1)  // macro for rootCint
};
#endif
