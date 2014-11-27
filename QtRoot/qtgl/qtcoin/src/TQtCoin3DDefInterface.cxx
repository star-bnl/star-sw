// @(#)root/gtgl:$Name:  $:$Id: TQtCoin3DDefInterface.cxx,v 1.4 2013/08/30 16:00:16 perev Exp $
// Author: Valery Fine     10/01/07

/****************************************************************************
** TQtCoin3DDefInterface
**
** Copyright (C) 2007 by Valeri Fine.  Brookhaven National Laboratory All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/
#include "TQtCoin3DDefInterface.h"
#include <Inventor/nodes/SoNode.h>

////////////////////////////////////////////////
//
// Class TQtCoin3DNode is an base class
// to create  the "Coin3D portion" of the 
// mixed Root/Coin geometry nodes
//
//  The portion can be defined as
//  ------------------------------
//  1. Text string with the Copin3D class descriptor
//  2. File name of the extranal file in OpenInventor2 format
//  3. Pointer to SoNode* Coin3D object
//
////////////////////////////////////////////////

//_________________________________________________________________________________
void TQtCoin3DNode::SetNode(SoNode *node) 
{ 
   fNode = node; 
   if (fNode) fNode->ref();
   SetType(kMemoryNode); 
}
//_________________________________________________________________________________
TQtCoin3DNode::~TQtCoin3DNode() 
{ 
   if (fNode) fNode->unref(); 
}

