//*-- Author :    Valeri Fine  08/12/94 begin_html mailto://fine@bnl.gov  end_html

#ifndef STAR_Stypes
#define STAR_Stypes
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Stypes                                                               //
// $Id: Stypes.h,v 1.17 2000/09/15 15:10:31 perev Exp $                                                               
// Basic types used by STAF - ROOT interface.                           //
//                                                                      //
// This header file contains the set of the macro definitions           //
// to generate a ROOT dictionary for "pure" C-strucutre the way ROOT    //
// does it for the "normal" C++ classes                                 //
//                                                                      //
// This header file should be included into the all STAF table wrapper  //
// classes (by stic compiler)                                           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <Ttypes.h>
#include "StTypeDefs.h"
#ifdef __HP_aCC
#include "St_HP_aCC.h"
#endif
enum EReturnCodes { 
  kStOK    = 0, 	// OK
  kStOk    = 0, 	// OK
  kStWarn  = 1, 	// Warning, something wrong but work can be continued
  kStEOF   = 2, 	// End Of File 
  kStErr   = 3, 	// Error, drop this and go to the next event
  kStERR   = 3, 	// Error, drop this and go to the next event
  kStFatal = 4,    	// Fatal error, processing impossible
  kStFATAL = 4,    	// Fatal error, processing impossible
  kStSKIP  = kStErr   + 10,
  kStSkip  = kStSKIP,
  kStSTOP  = kStFATAL + 10,
  kStStop  = kStSTOP
};  

#endif 
