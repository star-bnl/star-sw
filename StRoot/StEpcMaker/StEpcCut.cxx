//////////////////////////////////////////////////////////////////////
//
// $Id: StEpcCut.cxx,v 1.2 2000/12/01 17:08:47 subhasis Exp $
//
// Author: Subhasis Chattopadhyay 
//////////////////////////////////////////////////////////////////////
//
// Description: constants for the Epc maker
//////////////////////////////////////////////////////////////////////
//

#include "StEpcCut.h"
//
Float_t StEpcCut::mDeltaEta=0.1;
Float_t StEpcCut::mDeltaPhi=0.1;
Float_t StEpcCut::mRAD_SMD_E=231.23;

// Will be used later to calculate different cut parameters.
//-------------------------------------------------------------------
StEpcCut::StEpcCut(){
// Default constructor
}
//--------------------------------------------------------------------
StEpcCut::~StEpcCut(){
//Destructor
}
//------------------------------------------------------

