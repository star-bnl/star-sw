/***************************************************************************
 *
 * $Id: RandomEngine.cc,v 1.1 1999/01/30 03:59:01 fisyak Exp $
 *
 * Author:  Gabriele Cosmo - Created: 5th September 1995
 *          modified SCL bl
 ***************************************************************************
 *
 * Description:
 *              RandomEngine.cc,v 1.4 1997/04/04 13:22:34
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                       --- HepRandomEngine ---
 *                      class implementation file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 ***************************************************************************
 *
 * $Log: RandomEngine.cc,v $
 * Revision 1.1  1999/01/30 03:59:01  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:29:10  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "RandomEngine.h"

// -----------------------------
// Static members initialisation
// -----------------------------

#include "SeedTable.h"

//------------------------- HepRandomEngine ------------------------------

HepRandomEngine::HepRandomEngine() : theSeeds(&theSeed) {
  theSeed = 19780503;
}

HepRandomEngine::~HepRandomEngine() {}
