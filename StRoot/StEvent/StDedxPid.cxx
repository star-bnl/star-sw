/***************************************************************************
 *
 * $Id: StDedxPid.cxx,v 1.1 1999/04/28 22:27:30 fisyak Exp $
 *
 * Author: Craig Ogilvie and Thomas Ullrich, April 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDedxPid.cxx,v $
 * Revision 1.1  1999/04/28 22:27:30  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.1  1999/04/28 22:27:30  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.1  1999/04/08 14:56:31  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StDedxPid.h"
#include "StGlobalTrack.h"

StDedxPid::StDedxPid(const StGlobalTrack *t) : mTrack(t) { /* noop */ }
ClassImp(StDedxPid)

StDedxPid::StDedxPid(StGlobalTrack *t) : mTrack(t) { /* noop */ }

StDedxPid::~StDedxPid() { /* noop */ }
