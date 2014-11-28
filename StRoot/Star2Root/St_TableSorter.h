//*-- Author :    Valery Fine   26/01/99  (E-mail: fine@bnl.gov)
#ifndef STAR_St_TableSorter
#define STAR_St_TableSorter

////////////////////////////////////////////////////////////////////////////////////////
//
//  $Id: St_TableSorter.h,v 1.1 2000/02/25 00:48:09 fine Exp $
//
//  St_TableSorter  - Is an "observer" class to sort the St_Table objects
//                    The class provides an interface to the standard "C/C++"
//
// qsort and bsearch subroutine (for further information see your local C/C++ docs)
// =====     =======
//
//  - This class DOESN'T change / touch the "host" table  itself
//    For any St_Table object one can create as many different "sorter"
//    as he/she finds useful for his/her code
//  - Any instance of this class is meaningful as long as the "host" object
//    "St_Table" does exist and is not changed
//  - Any attempt to access this St_TableSorter after the "host" object deleted
//    causes the program abnormal termination
//  - Any attempt to access this St_TableSorter after the "host" object been changed
//    causes an unpredictable result
//  - Any instance (object) of this class is NOT deleted "by automatic" just
//    the "host object "St_Table" deleted. It is the responsibility of the user's code
//    keeping St_TableSorter and the the "host" St_Table objects consistent.
//
////////////////////////////////////////////////////////////////////////////////////////

#include "StTypeDefs.h"
#include <TTableSorter.h>

//______________________________________________________________________
// $Log: St_TableSorter.h,v $
// Revision 1.1  2000/02/25 00:48:09  fine
// temporary interface for ROOT 2.23/12 with STAR classes inside
//
// Revision 1.24  2000/01/12 02:19:22  fine
// new ctor with a pointer to St_Table provided, internal ref to St_Table has been replaced with a regular pointer
//
// Revision 1.23  2000/01/12 01:24:53  fine
// several methods to use St_Table class from the <converted> C program to C++
//
// Revision 1.22  1999/12/05 06:34:17  fine
// Several const methods for St_TableSorter introduced
//
// Revision 1.21  1999/12/04 22:41:37  fine
// clean up sole const methods
//
// Revision 1.20  1999/12/01 14:03:35  fine
// operator[] fixed for mixed types
//
// Revision 1.19  1999/12/01 01:40:04  fine
// new access method with the Long_t parameter has been introduced to avoid the wrong cast from (long) to (double) in CINT
//
// Revision 1.18  1999/08/09 01:38:55  fine
// New method GetKeyAddress has been introduced
//
// Revision 1.17  1999/05/18 17:59:22  fine
// Clean up and some comments
//
// Revision 1.16  1999/05/18 14:41:29  fine
// New methods: CountKey(), CountKeys(), FindFirstKey() have beent introduced
//
// Revision 1.15  1999/05/14 22:20:56  fine
// CountKey and CountKeys methods have been introduced
//
// Revision 1.14  1999/05/14 00:30:38  fine
// GetLastFound method has been introduced
//  
//______________________________________________________________________

#endif
