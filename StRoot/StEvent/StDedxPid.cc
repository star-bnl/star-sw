/***************************************************************************
 *
 * $Id: StDedxPid.cc,v 1.1 1999/04/08 14:56:31 ullrich Exp $
 *
 * Author: Craig Ogilvie and Thomas Ullrich, April 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDedxPid.cc,v $
 * Revision 1.1  1999/04/08 14:56:31  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StDedxPid.hh"
#include "StGlobalTrack.hh"


StDedxPid::StDedxPid(const StGlobalTrack &t) : mTrack(t) { /* noop */ }

StDedxPid::~StDedxPid() { /* noop */ }
