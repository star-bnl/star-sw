//////////////////////////////////////////////////////////////////////
//
// $Id: StEpcCut.cxx,v 1.1 2000/05/15 21:18:32 subhasis Exp $
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
StEpcCut::StEpcCut(){
// Default constructor
}
//--------------------------------------------------------------------
StEpcCut::~StEpcCut(){
//Destructor
}
//------------------------------------------------------

