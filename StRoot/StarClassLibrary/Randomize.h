/***************************************************************************
 *
 * $Id: Randomize.h,v 1.1 1999/01/30 03:59:01 fisyak Exp $
 *
 * Author: Gabriele Cosmo - Created: 5th September 1995
 *         modified for SCL bl
 ***************************************************************************
 *
 * Description:
 *             Randomize.h,v 1.3 1997/12/19 01:35:56
 * -----------------------------------------------------------------------
 *                             HEP Random
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 * This file must be included to make use of the HEP Random module
 *
 ***************************************************************************
 *
 * $Log: Randomize.h,v $
 * Revision 1.1  1999/01/30 03:59:01  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:42  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef Rndmze_h
#define Rndmze_h 1

// Including Engines ...

#include "JamesRandom.h"
#include "DRand48Engine.h"
#include "RandEngine.h"
#include "RanluxEngine.h"
#include "RanecuEngine.h"

// Including distributions ...

#include "RandFlat.h"
#include "RandExponential.h"
#include "RandGauss.h"
#include "RandBreitWigner.h"
#include "RandPoisson.h"

#define HepUniformRand() RandFlat::shoot()

#endif
