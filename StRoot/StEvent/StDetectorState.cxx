/***************************************************************************
 *
 * $Id: StDetectorState.cxx,v 2.1 2001/12/01 15:34:50 ullrich Exp $
 *
 * Author: Thomas Ullrich, Dec 2001
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDetectorState.cxx,v $
 * Revision 2.1  2001/12/01 15:34:50  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StDetectorState.h"

static const char rcsid[] = "$Id: StDetectorState.cxx,v 2.1 2001/12/01 15:34:50 ullrich Exp $";

ClassImp(StDetectorState)

StDetectorState::StDetectorState()
    : mDetectorId(kUnknownId), mIsGood(true) {/* noop */}

StDetectorState::StDetectorState(StDetectorId det, bool state)
    : mDetectorId(det), mIsGood(state) {/* noop */}

StDetectorState::~StDetectorState() {/* noop */}

StDetectorId
StDetectorState::detector() const {return mDetectorId;}

bool
StDetectorState::good() const {return mIsGood;}

bool
StDetectorState::bad() const {return !good();}

void
StDetectorState::setDetector(StDetectorId det) {mDetectorId = det;}

void
StDetectorState::setGood(bool val) {mIsGood = val;}
