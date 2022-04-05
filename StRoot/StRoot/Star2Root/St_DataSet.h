//*CMZ :          13/08/98  18.27.27  by  Valery Fine(fine@bnl.gov)
//*-- Author :    Valery Fine(fine@mail.cern.ch)   13/08/98 
//***********************************************************************
//     C++ class library to create and manipulate hierarchy datasets
// * Copyright(c) 1997~1999  [BNL] Brookhaven National Laboratory, STAR, All rights reserved
// * Author                  Valerie Fine  (fine@bnl.gov)
// * Copyright(c) 1997~1999  Valerie Fine  (fine@bnl.gov)
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
// *
// * Permission to use, copy, modify and distribute this software and its
// * documentation for any purpose is hereby granted without fee,
// * provided that the above copyright notice appear in all copies and
// * that both that copyright notice and this permission notice appear
// * in supporting documentation.  Brookhaven National Laboratory makes no
// * representations about the suitability of this software for any
// * purpose.  It is provided "as is" without express or implied warranty.
// ************************************************************************

// $Id: St_DataSet.h,v 1.1 2000/02/25 00:48:06 fine Exp $
#ifndef ROOT_St_DataSet
#define ROOT_St_DataSet
 
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_DataSet                                                           //
//                                                                      //
// St_DataSet class is a base class to implement the directory-like     //
// data structures and maintain it via St_DataSetIter class iterator    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
#include "StTypeDefs.h"
#include "TDataSet.h"


// $Log: St_DataSet.h,v $
// Revision 1.1  2000/02/25 00:48:06  fine
// temporary interface for ROOT 2.23/12 with STAR classes inside
//
// Revision 1.42  2000/01/23 20:57:50  fine
// methods St_DataSet:Remove have been fixed for the structural members
//
// Revision 1.41  2000/01/12 18:07:23  fine
//  cvs symbols have been added and copyright class introduced
//
// Revision 1.40  1999/11/10 00:49:25  fine
// comment was changed
//
// Revision 1.39  1999/11/06 19:09:45  fisyak
// Clean up for Valery
//
// Revision 1.38  1999/11/04 18:01:19  fine
// Copyright text introduced
//
// Revision 1.37  1999/10/28 16:24:29  fine
// St_DataSet major correction: it may be built with TList (default) or with TObjArray
//
// Revision 1.36  1999/09/04 00:28:01  fine
// St_Table::NaN from VP and gloabl dataset have been introduced
//
// Revision 1.35  1999/08/06 15:24:35  fine
// UnMark method has been introduced
//
// Revision 1.34  1999/07/23 13:26:06  fine
// Several new methods to mark the datasets have been introduced
//
// Revision 1.33  1999/06/26 01:40:55  fisyak
// Add Valery's abstract buffer
//
// Revision 1.32  1999/06/09 22:08:53  fine
// Comment clean up
//
#endif
