/***************************************************************************
 *
 * $Id: StGlobals.hh,v 1.3 1999/12/21 15:14:03 ullrich Exp $
 *
 * Author:  Thomas Ullrich, 1998
 ***************************************************************************
 *
 * Description:
 * Global Constants and typedefs
 *
 ***************************************************************************
 *
 * $Log: StGlobals.hh,v $
 * Revision 1.3  1999/12/21 15:14:03  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.2  1999/11/09 16:20:08  ullrich
 * Include stdcomp.h only for SUN.
 *
 * Revision 1.1  1999/01/30 03:59:02  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:48  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_GLOBALS_HH
#define ST_GLOBALS_HH

#if defined (__SUNPRO_CC) && __SUNPRO_CC < 0x500
#include <stdcomp.h>
#endif

// **** You should probably not touch anything below this line: ****

//
// Typedefs for numeric types
// for synchronisation with CLHEP
// 
typedef double          HepDouble;
typedef int             HepInt;
typedef float           HepFloat;
typedef bool            HepBoolean;

//
// Typedefs for numeric types
// for synchronisation with STAR
// 
typedef double          StDouble;
typedef float           StFloat;
typedef int             StInt;
typedef bool            StBool;
typedef long            StLong;
typedef unsigned short  StUshort;
typedef unsigned int    StSizeType;

//
//   Global macros
//
#define StNPOS (~(StSizeType)0)

//
//   Global templates
//
template<class T>
inline StInt sign(T a) { return a < 0 ? -1 : 1; }

template<class T>
inline StDouble sqr(T a) { return a*a; }

//
//   Macros for debugging and testing
//
#define PR(x) cout << (#x) << " = " << (x) << endl;

//
//   Include physical constants and system of units
//
//#include "PhysicalConstants.h"

#endif





