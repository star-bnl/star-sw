//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98  
// $Id: St_Node.h,v 1.1 2000/02/25 00:48:06 fine Exp $
// $Log: St_Node.h,v $
// Revision 1.1  2000/02/25 00:48:06  fine
// temporary interface for ROOT 2.23/12 with STAR classes inside
//
// Revision 1.18  1999/11/13 16:59:17  fine
// St_Node and St_NodeView headers adjusted to ROOT 2.23/ not compatible with ROOT 2.22 anymore though!
//
// Revision 1.17  1999/08/03 14:52:23  fine
// make up to make Linux compiler happy
//
// Revision 1.16  1999/07/09 01:56:38  fine
// New method to contrsuct sub views and manage visibilities
//
// Revision 1.15  1999/06/09 22:09:35  fine
// St_PolyLine3D has beed redesigned
//
// Revision 1.14  1999/05/18 20:26:29  fine
// comments clean up
//
// Revision 1.13  1999/05/13 19:57:27  fine
// St_Node  the TShape list has been introduced, St_Node file format has been changed
//
// Revision 1.12  1999/04/19 00:05:15  fine
// New class St_PolylineShape has been introduced
//
// Revision 1.11  1999/04/13 14:26:40  fine
// Geometry-based dataset implementation, next step
//
// Revision 1.10  1999/04/08 16:44:09  fine
// Working version of the NodeView family
//
// Revision 1.9  1999/04/05 03:18:26  fine
// St_Node family steps
//
// Revision 1.8  1999/02/04 19:22:23  fine
// Severak drawing method have been added to draw STAR nodes
//
// Revision 1.7  1999/01/31 02:03:07  fine
// St_DataSetIter::Notify - new method + clean up
//
// Revision 1.6  1999/01/30 04:24:21  fine
// St_Table: Print memory leak fixed
//
// Revision 1.5  1999/01/25 23:36:41  fine
// St_DataSet fixed, St_DataSetIter::operator[] introduced, St_Node with own Hash methdo
//
// Revision 1.4  1998/12/27 04:16:44  fine
// *** empty log message ***
//
// Revision 1.2  1998/12/26 21:40:40  fisyak
// Add Id and Log
//
//Copyright (C) 1998. Brookhaven National Laboratory. Valery Fine (Faine). All right reserved.
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_Node  - TVolume                                                   //
//                                                                      //
// Description of parameters to position a 3-D geometry object          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
#ifndef ROOT_St_Node
#define ROOT_St_Node

#include "StTypeDefs.h"
#include <TVolume.h>

#endif
