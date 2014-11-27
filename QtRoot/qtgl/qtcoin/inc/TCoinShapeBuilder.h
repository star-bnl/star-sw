// @(#)root/gtgl:$Name:  $:$Id: TCoinShapeBuilder.h,v 1.3 2013/08/30 16:00:14 perev Exp $
// Author: Valery Fine      04/10/06

#ifndef ROOT_TCoinShapeBuilder
#define ROOT_TCoinShapeBuilder

/****************************************************************************
**
** TCoinShapeBuilder
**
** Create a Coin3D shape from the generic he TShape3DPolygonView object
**
** Copyright (C) 2005 by Valeri Fine.  Brookhaven National Laboratory All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/
#include "Rtypes.h"

class TShape3DPolygonView;
class SoGroup;
class SoNormal;
class SoCoordinate3;
class TPolygone3DVertexBindingView;
class TPolygone3DFaceBindingView;
class Coord3D;
class SoFaceSet;
class SoNode;

class TCoinShapeBuilder  {
   friend class TObjectCoinViewFactory;
private:
   SoGroup *fGroup;
   const TShape3DPolygonView &fShapeView;
   const Float_t *fRgba;
        SoNormal *fShapeFaceNormal;    // current normals
   SoCoordinate3 *fShapeFaceVertices;  // current vertices
   SoFaceSet     *fFaceSet;            // face set one per shape is enough
   SoNode        *fMaterial;           // Material property

protected:
   TCoinShapeBuilder(const TShape3DPolygonView &shapeView, const Float_t *rgba);
   virtual ~TCoinShapeBuilder();
   SoGroup *CreateCoinShape();
   void MakeShapeQuads(const TPolygone3DFaceBindingView &shape
                     , Bool_t polygon = false);
   void MakeShapeQuadeStrip(const TPolygone3DFaceBindingView &shape);
   void MakeShape(const TPolygone3DVertexBindingView &shape);
   void MakeShape(const TPolygone3DFaceBindingView &shape);
   inline SoGroup *operator()() { return CreateCoinShape();}
};
#endif
