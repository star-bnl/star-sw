/********************************************************************
 *
 * $Id: StPhmdModule.cxx,v 2.1 2002/12/20 22:33:00 ullrich Exp $
 *
 * Author: Subhasis Chattopadhyay, Dec 2002
 ********************************************************************
 *
 * Description: Class for supermodule 
 *
 ********************************************************************
 *
 * $Log: StPhmdModule.cxx,v $
 * Revision 2.1  2002/12/20 22:33:00  ullrich
 * Initial Revision.
 *
 ********************************************************************/
#include "StPhmdModule.h"


ClassImp(StPhmdModule)

StPhmdModule::StPhmdModule() { /* noop */ }

StPhmdModule::~StPhmdModule() { /* noop */ }

unsigned int
StPhmdModule::numberOfHits() const {return mHits.size();}

const StSPtrVecPhmdHit&
StPhmdModule::hits() const { return mHits; }

StSPtrVecPhmdHit&
StPhmdModule::hits() { return mHits; }



