/******************************************************************
 *
 * $Id: StPmdModule.cxx,v 1.3 2004/09/22 19:24:56 perev Exp $
 *
 * Author: Subhasis Chattopadhyay
 *******************************************************************
 *
 * Description: Class for supermodule
 *
 ********************************************************************
 *
 * $Log: StPmdModule.cxx,v $
 * Revision 1.3  2004/09/22 19:24:56  perev
 * Leak fixed + mess with i,j indexes
 *
 * Revision 1.2  2003/05/12 12:07:13  subhasis
 * Mapping added
 *
 ********************************************************************/
#include "StPmdModule.h"


ClassImp(StPmdModule)

StPmdModule::StPmdModule() { mHits.SetOwner(); }

StPmdModule::~StPmdModule() { /* noop */ }
  
