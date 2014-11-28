/***************************************************************************
 *
 * $Id: units.cc,v 1.2 2003/09/02 17:59:38 perev Exp $
 *
 * Author: bl, June 1998
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: units.cc,v $
 * Revision 1.2  2003/09/02 17:59:38  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  1999/02/17 12:44:03  ullrich
 * New Revision
 *
 * Revision 1.1  1999/01/23 00:26:53  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <Stiostream.h>
#include "StGlobals.hh"

#define ST_ADD_OLD_CLHEP_SYSTEM_OF_UNITS
#include "SystemOfUnits.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

int main()
{
    cout << "This program illustrates the use of SystemOfUnits and PhysicalContants" << endl;
    cout << "--------------------------------------------------------------------" << endl;

    cout << "1 millimeter = " << (1*millimeter) << endl;
    cout << "1 meter =      " << (1*meter)      << endl;
    cout << "1 centimeter = " << (1*centimeter) << endl;
    cout << "1 fermi =      " << (1*fermi)      << endl;

    cout << "1 barn =       " << (1*barn)       << endl;

    cout << "1 degree =     " << (1*degree)     << endl;

    cout << "1 second =     " << (1*second)     << endl;
    cout << "1 nanosecond = " << (1*nanosecond) << endl;

    cout << "1 kHz =        " << (1*kHz)     << endl;

    cout << "1 newton =    " << (1*newton)     << endl;
    cout << "1 joule =     " << (1*joule)     << endl;
    
    cout << "1 GeV =        " << (1*GeV)     << endl;
    cout << "1 Gigaelectronvolt = " << (1*Gigaelectronvolt)     << endl;


#ifdef ST_ADD_OLD_CLHEP_SYSTEM_OF_UNITS
    cout << "The old CLHEP definitions are also supported:" << endl;
    cout << "---------------------------------------------" << endl;
    cout << "1 mm = " << (1*mm) << endl;
    cout << "1 m  = " << (1*m)  << endl;
    cout << "1 cm = " << (1*cm) << endl;

    cout << "1 s  = " << (1*s)  << endl;
    cout << "1 ns = " << (1*ns) << endl;
#else
    cout << "\nTo use the old CLHEP definitions the flag:" << endl;
    cout << "    ST_ADD_OLD_CLHEP_SYSTEM_OF_UNITS" << endl;
    cout << "must be defined" << endl;
    cout << "**This is an obselete file and should NOT be used**" << endl;
#endif
    
    return 0;
}
