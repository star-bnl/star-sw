//////////////////////////////////////////////////////////////////////
//
// $Id: StEpcCut.cxx,v 1.4 2005/05/23 12:35:14 suaide Exp $
//
// Author: Subhasis Chattopadhyay
//////////////////////////////////////////////////////////////////////
//
// Description: constants for the Epc maker
//////////////////////////////////////////////////////////////////////
//

#include "StEpcCut.h"
//
Float_t StEpcCut::mDeltaEta=0.01;
Float_t StEpcCut::mDeltaPhi=0.01;
Float_t StEpcCut::mRAD_SMD_E=231.23;

// Will be used later to calculate different cut parameters.
//-------------------------------------------------------------------
StEpcCut::StEpcCut()
{
    // Default constructor
}
//--------------------------------------------------------------------
StEpcCut::~StEpcCut()
{
    //Destructor
}
//------------------------------------------------------

