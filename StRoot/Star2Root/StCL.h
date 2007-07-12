#ifndef STAR_StCL
#define STAR_StCL
//*-- Author :    Valery Fine(fine@bnl.gov)   25/09/99  
//
// The set of methods to work with the plain matrix / vector
// "derived" from  http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 
//
// $Id: StCL.h,v 1.2 2007/07/12 20:37:48 fisyak Exp $
// $Log: StCL.h,v $
// Revision 1.2  2007/07/12 20:37:48  fisyak
// Add includes for ROOT 5.16
//
// Revision 1.1  2000/02/25 00:48:06  fine
// temporary interface for ROOT 2.23/12 with STAR classes inside
//
// Revision 1.9  1999/12/17 23:28:39  fine
// clean up for the sake of docs + new class St_Table3DPackedPoints introduced

#include "StTypeDefs.h"
#include "RVersion.h"
#if ROOT_VERSION_CODE < 331013
#include <TCL.h>
#else
#include "TCernLib.h"
#endif
#endif
