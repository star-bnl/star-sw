//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
// $Id: St_NodeView.h,v 1.1 2000/02/25 00:48:07 fine Exp $
// $Log: St_NodeView.h,v $
// Revision 1.1  2000/02/25 00:48:07  fine
// temporary interface for ROOT 2.23/12 with STAR classes inside
//
// Revision 1.22  1999/11/16 16:29:57  fine
// TObject::GetObjectInfo() implemented
//
// Revision 1.21  1999/11/13 16:59:17  fine
// St_Node and St_NodeView headers adjusted to ROOT 2.23/ not compatible with ROOT 2.22 anymore though!
//
// Revision 1.20  1999/09/22 03:51:50  fine
// New method and RMath class to manage different transformation have been introduced
//
// Revision 1.19  1999/07/09 17:52:56  fine
// New ctors to create sub-view between two nodes
//
// Revision 1.18  1999/07/09 01:56:39  fine
// New method to contrsuct sub views and manage visibilities
//
// Revision 1.17  1999/06/21 22:16:55  fine
// Some clean up
//
// Revision 1.16  1999/06/14 09:30:31  fine
//   default ctor clean
//
// Revision 1.15  1999/06/14 08:45:28  fine
// List of the shapes have been introduced for St_NodeView
//
// Revision 1.14  1999/06/05 00:42:32  fine
// SetLineAttribute methods have been introduced
//
// Revision 1.13  1999/05/29 20:52:33  fine
// Several method to estimat range of 3D object were introduced
//
// Revision 1.12  1999/04/23 22:47:36  fine
// Node family has been adjusted for St_PolyLineShape class
//
// Revision 1.11  1999/04/23 00:09:19  fine
// Working verion of PolyLineShape. Some correction for St_Node family as well
//
// Revision 1.10  1999/04/13 14:26:41  fine
// Geometry-based dataset implementation, next step
//
// Revision 1.9  1999/04/10 15:05:05  fine
// First working version of SavePrimitive. New ctor has been introduced to back SavePrimitive
//
// Revision 1.8  1999/04/09 23:24:08  fine
// St_NodeView::SavePrimitive() - first approach
//
// Revision 1.7  1999/04/08 16:44:10  fine
// Working version of the NodeView family
//
// Revision 1.6  1999/03/30 16:24:02  fine
//  Corrections towards GEANT manager
//
// Revision 1.5  1999/03/29 19:25:26  fine
// Visibility flag have been activated. Some working correction
//
#ifndef STAR_St_NodeView
#define STAR_St_NodeView

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_NodeView - TVolumeView                                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StTypeDefs.h"
#include <TVolumeView.h>

#endif

