//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98   
// $Id: St_NodePosition.h,v 1.1 2000/02/25 00:48:07 fine Exp $
// $Log: St_NodePosition.h,v $
// Revision 1.1  2000/02/25 00:48:07  fine
// temporary interface for ROOT 2.23/12 with STAR classes inside
//
// Revision 1.18  1999/10/28 16:24:31  fine
// St_DataSet major correction: it may be built with TList (default) or with TObjArray
//
// Revision 1.17  1999/09/27 23:45:43  fine
// Several methods to calculate errors were introduced
//
// Revision 1.16  1999/09/22 03:51:50  fine
// New method and RMath class to manage different transformation have been introduced
//
// Revision 1.15  1999/06/09 22:09:35  fine
// St_PolyLine3D has beed redesigned
//
// Revision 1.14  1999/06/05 00:42:32  fine
// SetLineAttribute methods have been introduced
//
// Revision 1.13  1999/04/23 22:47:34  fine
// Node family has been adjusted for St_PolyLineShape class
//
// Revision 1.12  1999/04/13 14:26:40  fine
// Geometry-based dataset implementation, next step
//
// Revision 1.11  1999/04/08 16:44:10  fine
// Working version of the NodeView family
//
// Revision 1.10  1999/04/05 03:18:27  fine
// St_Node family steps
//
// Revision 1.9  1999/04/02 23:36:04  fine
// Collapsed geometry structures has been implemeted
//
// Revision 1.8  1999/03/30 22:30:14  fine
//  Visibility test has been added for Paint method
//
// Revision 1.7  1999/03/27 22:44:59  fine
// Working 3D St_node with X3d and OpenGL
//
// Revision 1.6  1999/03/21 22:38:53  fine
// StDataSetIter make up + new NextDataSet method introced
//
// Revision 1.5  1999/02/04 19:22:23  fine
// Severak drawing method have been added to draw STAR nodes
//
// Revision 1.4  1999/01/31 02:03:07  fine
// St_DataSetIter::Notify - new method + clean up
//
// Revision 1.3  1998/12/27 02:33:17  fine
// St_Node, St_NodePosition - first working versions have been introduced *see macros/STAR_shapes.C for an example)
//
// Revision 1.2  1998/12/26 21:40:40  fisyak
// Add Id and Log
// 
//+SEQ,CopyRight,T=NOINCLUDE.
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_NodePosition   - TVolumePosition                                  //
//                                                                      //
// Description of parameters to position a 3-D geometry object          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
#ifndef ROOT_St_NodePosition
#define ROOT_St_NodePosition

#include "StTypeDefs.h"
#include <TVolumePosition.h>

#endif
