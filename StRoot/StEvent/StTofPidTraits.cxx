/***************************************************************************
 *
 * $Id: StTofPidTraits.cxx,v 2.1 2000/12/08 03:52:42 ullrich Exp $
 *
 * Author: Thomas Ullrich, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofPidTraits.cxx,v $
 * Revision 2.1  2000/12/08 03:52:42  ullrich
 * Initial Revision
 *
 ***************************************************************************/
#include "StTofPidTraits.h"

static const char rcsid[] = "$Id: StTofPidTraits.cxx,v 2.1 2000/12/08 03:52:42 ullrich Exp $";

ClassImp(StTofPidTraits)

StTofPidTraits::StTofPidTraits()
    : StTrackPidTraits(kTofPatchId) {/* noop */ }

StTofPidTraits::~StTofPidTraits() { /* noop */ }
