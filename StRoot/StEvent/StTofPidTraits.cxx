/***************************************************************************
 *
 * $Id: StTofPidTraits.cxx,v 2.2 2000/12/08 20:21:08 genevb Exp $
 *
 * Author: Thomas Ullrich, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofPidTraits.cxx,v $
 * Revision 2.2  2000/12/08 20:21:08  genevb
 * Changed kTofPatchId -> kTofId
 *
 * Revision 2.1  2000/12/08 03:52:42  ullrich
 * Initial Revision
 *
 ***************************************************************************/
#include "StTofPidTraits.h"

static const char rcsid[] = "$Id: StTofPidTraits.cxx,v 2.2 2000/12/08 20:21:08 genevb Exp $";

ClassImp(StTofPidTraits)

StTofPidTraits::StTofPidTraits()
    : StTrackPidTraits(kTofId) {/* noop */ }

StTofPidTraits::~StTofPidTraits() { /* noop */ }
