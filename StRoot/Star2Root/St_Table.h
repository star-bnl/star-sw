//*-- Author :    Valery Fine   24/03/98
// $Id: St_Table.h,v 1.1 2000/02/25 00:48:08 fine Exp $
#ifndef STAF_St_Table
#define STAF_St_Table
  
//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  St_Table                                                            //
//                                                                      //
//  It is a base class to create a "wrapper" class                      //
//  holding the plain C-structure array                                 //
//  (1 element of the structure per element)                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StTypeDefs.h"
#include <TTable.h>

// $Log: St_Table.h,v $
// Revision 1.1  2000/02/25 00:48:08  fine
// temporary interface for ROOT 2.23/12 with STAR classes inside
//
// Revision 1.44  2000/01/28 04:45:28  fine
// new method CopyRows has been introduced
//
// Revision 1.43  2000/01/21 02:09:53  fine
// several parameters were converted to be const
//
// Revision 1.42  2000/01/12 01:24:52  fine
// several methods to use St_Table class from the <converted> C program to C++
//
// Revision 1.41  1999/11/24 00:31:26  fine
// operator[] const has been introduced
//
// Revision 1.40  1999/10/28 16:24:35  fine
// St_DataSet major correction: it may be built with TList (default) or with TObjArray
//
// Revision 1.39  1999/10/28 00:32:55  fine
// method At() has been removed
//
// Revision 1.38  1999/09/24 21:56:08  fisyak
// Add operator [] for particular table (VF)
//
// Revision 1.37  1999/09/07 19:30:29  fine
// table descriptor access has been changed. All tables are affected and must be re-compiled
//
// Revision 1.36  1999/09/04 00:28:02  fine
// St_Table::NaN from VP and gloabl dataset have been introduced
//
// Revision 1.35  1999/08/30 23:15:09  fine
// St_Table::Fit method has been introduced
//
// Revision 1.34  1999/08/20 13:22:25  fine
// new method St_Table::Draw
//.
// Revision 1.33  1999/08/12 16:41:31  fine
// Clean up
//
// Revision 1.32  1999/08/12 16:39:49  fine
// clash between St_Table::GetSize and TArray::GEtSize has been resolved
//
// Revision 1.31  1999/08/11 14:44:39  fine
// name clash with ROOT over enum resolved
//
// Revision 1.30  1999/08/11 00:42:33  fine
// new I/O via St_baseDescriptor table has been implemented
//
// Revision 1.29  1999/07/01 01:45:32  fisyak
// GetRowDescritors => GetRowDescriptors
//
// Revision 1.28  1999/06/26 01:40:56  fisyak
// Add Valery's abstract buffer
//
// Revision 1.27  1999/06/25 01:35:54  fine
// New streamers for St_Tables
//
// Revision 1.26  1999/02/24 17:10:58  fine
//  St_Table  New and Purge method have been introdiced, some clean up for St_module as well
//
// Revision 1.25  1999/01/28 19:13:08  fine
// St_TableSorter has been made up
//
// Revision 1.24  1999/01/19 03:13:32  fine
// St_DataSet::Fine and St_DataSet::FindObject methods have been introduced
//
// Revision 1.23  1999/01/13 20:29:15  fine
// St_DataSet::Pass() method - the option kUp has been introduced
//
// Revision 1.22  1998/12/30 22:30:18  fine
// St_Table::PrintHrader method has been introduced
//
// Revision 1.21  1998/12/30 01:08:02  fisyak
// Add Public SetNRows for used No. of rows
//
// Revision 1.20  1998/12/30 00:52:42  fisyak
// Remove SetfN from public
//
// Revision 1.18  1998/12/29 19:37:40  fine
// St_NodeView:  new class to create refs topology of the "main" St_Node object has been introduced
//
// Revision 1.17  1998/12/17 16:57:57  fine
// St_Table: some extra protections have been established (safe "strncat" have replaced the  unsafe  "strncpy")
//
// Revision 1.16  1998/12/07 20:23:12  fine
// St_Table:: working versions of the Print() and SavePrimitive methods
//
// Revision 1.15  1998/12/06 00:45:49  fisyak
// Add SavePrimitive
//
// Revision 1.14  1998/12/06 00:38:17  fisyak
// Add SavePrimitive
// 

#endif 

