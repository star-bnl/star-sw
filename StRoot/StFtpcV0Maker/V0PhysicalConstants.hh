// $Id: V0PhysicalConstants.hh,v 1.3 2000/01/25 15:32:45 fisyak Exp $
//
// $Log: V0PhysicalConstants.hh,v $
// Revision 1.3  2000/01/25 15:32:45  fisyak
// Add namespace for CC5
//
// Revision 1.2  2000/01/03 13:16:21  jcs
// Add CVS Id strings
//
///////////////////////////////////////////////////////////
//  This is an extension of the physical constansts of the 
//    StarClassLibrary.  I try to use the same dotation.
//
//  Written by Mike Heffner 10/30/98
/////////////////////////////////////////////////////////////

#ifndef V0_PHYSICAL_CONSTANTS
#define V0_PHYSICAL_CONSTANTS

#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

static const HepDouble charged_pion_mass_c2 = 139.56995 * MeV;
static const HepDouble neutral_pion_mass_c2 = 134.9764  * MeV;

static const HepDouble charged_kaon_mass_c2 = 493.677 * MeV;
static const HepDouble neutral_kaon_mass_c2 = 497.672 * MeV;

//static const HepDouble lambda_mass_c2 = 1115.683 * MeV;


#endif  // V0_PHYSICAL_CONSTANTS
