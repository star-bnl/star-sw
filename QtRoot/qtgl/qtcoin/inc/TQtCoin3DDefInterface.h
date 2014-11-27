#ifndef STAR_TQtCoin3DDefInterface
#define STAR_TQtCoin3DDefInterface
// @(#)root/gtgl:$Name:  $:$Id: TQtCoin3DDefInterface.h,v 1.4 2013/08/30 16:00:15 perev Exp $
// Author: Valery Fine     10/01/07

/****************************************************************************
** TObject3DView
**
** Copyright (C) 2007 by Valeri Fine.  Brookhaven National Laboratory All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/
#include "TString.h"

class SoNode;
//////////////////////////////////////////////////////////////////////////
//
// An abstract base class TQtCoin3DDefInterface is 
// an interface  to communicate with the "Coin3D portion"
// of the mixed Root/Coin geomtry nodes
//
//  The portion can be defined as
//  ------------------------------
//  1. Text string with the Copin3D class descriptor
//  2. File name of the extranal file in OpenInventor2 format
//  3. Pointer to SoNode* Coin3D object
//
//////////////////////////////////////////////////////////////////////////
class TQtCoin3DDefInterface {
public:
   enum EECoinNodeType { kUnknown, kStringNode, kFileNode, kMemoryNode };
private:
   EECoinNodeType fType;
 public:
   TQtCoin3DDefInterface(EECoinNodeType type=kUnknown) : fType(type) {}
   TQtCoin3DDefInterface(const TQtCoin3DDefInterface&node) : fType(node.fType) {}
   virtual ~TQtCoin3DDefInterface() {}
   EECoinNodeType GetType() const  { return fType; }
   void SetType(EECoinNodeType type) { fType = type;}
   virtual const char *GetNodeDescriptor() const   = 0;
   virtual const char *GetNodeName()       const   = 0;
   virtual const char *GetFileName()       const   = 0;
   virtual const SoNode *GetNode()         const   = 0;
   virtual SoNode *GetNode()                       = 0;
   virtual void SetFileName(const char *fileName)  = 0;
   virtual void SetNode(SoNode *node)              = 0;
   virtual void SetNodeDescriptor(const char *desc)= 0;
   virtual void SetNodeName(const char *name)      = 0;
};

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

class TQtCoin3DNode : public TQtCoin3DDefInterface {
 private:
    TString fNodeDescriptor; 
    TString fNodeName; 
    SoNode *fNode; //!
 public:
    TQtCoin3DNode(const char *coin3DnodeDesciption="",const char *coin3Dname="") :
       TQtCoin3DDefInterface(kStringNode), fNodeDescriptor(coin3DnodeDesciption),fNodeName(coin3Dname),fNode(0) {}
       TQtCoin3DNode(const TQtCoin3DNode&node):
       TQtCoin3DDefInterface(node), fNodeDescriptor(node.fNodeDescriptor),  fNodeName(node.fNodeName) 
       { SetNode( GetType()==kMemoryNode ? node.fNode : 0); }
       virtual ~TQtCoin3DNode();
       const char *GetNodeDescriptor() const { return fNodeDescriptor;                }
       const char *GetNodeName()       const { return fNodeName;                      }
       const char *GetFileName()       const { return GetType()==kFileNode ? (const char*) GetNodeDescriptor() : 0; }
       const SoNode *GetNode()         const { return GetType()==kMemoryNode ? fNode : 0; }
       SoNode *GetNode()                     { return GetType()==kMemoryNode ? fNode : 0; }
       void SetFileName(const char *fileName)  { SetNodeDescriptor(fileName);SetType(kFileNode);}
       void SetNode(SoNode *node);
       void SetNodeDescriptor(const char *desc)  { fNodeDescriptor = desc;SetType(kStringNode);}
       void SetNodeName(const char *name)        { fNodeName = name;}
    ClassDef(TQtCoin3DNode,1);
};
#endif
