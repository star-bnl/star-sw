/***************************************************************************
 *
 * $Id: StRandom.cc,v 1.1 2000/03/16 16:29:12 ullrich Exp $
 *
 * Author: Thomas Ullrich, Mar 2000
 ***************************************************************************
 *
 * Description:  
 *
 ***************************************************************************
 *
 * $Log: StRandom.cc,v $
 * Revision 1.1  2000/03/16 16:29:12  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StRandom.hh"

RanluxEngine    StRandom::mEngine;
RandFlat        StRandom::mFlat(mEngine);
RandBreitWigner StRandom::mBreitWigner(mEngine);
RandExponential StRandom::mExponential(mEngine);
RandGauss       StRandom::mGauss(mEngine);
RandPoisson     StRandom::mPoisson(mEngine);

StRandom::StRandom() {/* noop */}

StRandom::~StRandom() {/* noop */}

