/***************************************************************************
 *
 * $Id: StDedxPid.cc,v 1.2 1999/05/20 16:17:36 ogilvie Exp $
 *
 * Author: Craig Ogilvie and Thomas Ullrich, April 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDedxPid.cc,v $
 * Revision 1.2  1999/05/20 16:17:36  ogilvie
 * added static dedx calibration data members, set, get functions
 *
 * Revision 1.1  1999/04/08 14:56:31  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StDedxPid.hh"
#include "StGlobalTrack.hh"


StDedxPid::StDedxPid(const StGlobalTrack &t) : mTrack(t) { /* noop */ }
StDedxPid::StDedxPid() { /* noop */ }
StDedxPid::~StDedxPid() { /* noop */ }
