//*CMZ :          13/08/98  18.27.27  by  Valery Fine(fine@bnl.gov)
//*-- Author :    Valery Fine(fine@mail.cern.ch)   13/08/98 

#ifndef ROOT_St_DataSetIter
#define ROOT_St_DataSetIter
 

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// $Id: St_DataSetIter.h,v 1.1 2000/02/25 00:48:06 fine Exp $                             
//
// St_DataSetIter                                                       //
//                                                                      //
// Iterator of St_DataSet lists.                                        //
//                                                                      //
// Provides "standard" features of the TIter class for St_DataSet object//
//                             and                                      //
// allows navigating St_DataSet structure using the custom "directory"  //
//    notation (see St_DataSet::Find(const Char *path) method)          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
#include "StTypeDefs.h"
#include <TDataSetIter.h>
//__________________________________________________
// $Log: St_DataSetIter.h,v $
// Revision 1.1  2000/02/25 00:48:06  fine
// temporary interface for ROOT 2.23/12 with STAR classes inside
//
// Revision 1.20  1999/12/29 02:37:00  fine
// remove warning to make Linux happy
//
// Revision 1.19  1999/12/28 23:32:01  fine
// St_DataSetIter  operator++ removed to avoid a future problem
//
// Revision 1.18  1999/12/28 21:24:07  fine
// St_DataSetIter opeartor * and operator ++ have been implemented
//__________________________________________________
#endif

