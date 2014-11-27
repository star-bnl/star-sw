// @(#)root/gtgl:$Name:  $:$Id: TCoinShapeBuilder.cxx,v 1.17 2013/08/30 16:00:15 perev Exp $
// Author: Valery Fine      24/09/06

/****************************************************************************
**
** TCoinShapeBuilder
** An unterface of the class visitor to convert the ROOT 3D objets into 
** the concrete "viewer" representation like OpenGL, OpenInventor, x3d etc
**
** Copyright (C) 2006 by Valeri Fine.  Brookhaven National Laboratory All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#include "TCoinShapeBuilder.h"
#include "TShape3DPolygonView.h"
#include "TObject3DView.h"
#include "TDataSetIter.h"
#include "TStopwatch.h"
#include "TAttMarker.h"
#include <Inventor/nodes/SoBaseColor.h>
#include <Inventor/nodes/SoCoordinate3.h>
#include <Inventor/nodes/SoDrawStyle.h>
#include <Inventor/nodes/SoGroup.h>
#include <Inventor/nodes/SoFaceSet.h>
#include <Inventor/nodes/SoLineSet.h>
#include <Inventor/nodes/SoQuadMesh.h> 
#include <Inventor/fields/SoMFVec3f.h> 
#include <Inventor/nodes/SoNormal.h>
#include <Inventor/nodes/SoNormalBinding.h> 
#include <Inventor/nodes/SoMarkerSet.h> 
#include <Inventor/nodes/SoMaterial.h>
#include <Inventor/nodes/SoPointSet.h>
#include <Inventor/nodes/SoSeparator.h>
#include <Inventor/nodes/SoShapeHints.h>
#include <Inventor/nodes/SoLightModel.h>
#include <Inventor/nodes/SoPickStyle.h>

#include "assert.h"

//______________________________________________________________________________
static inline SoNode *SetCurrentColor(const Float_t *rgba,bool material=false)
{
   SoNode *node = 0;
   if ( material ) {
      if (rgba[3] <= 0.99) {
         SoMaterial * m = new SoMaterial;
         m->diffuseColor.setValue(rgba[0], rgba[1], rgba[2]);
         m->specularColor.setValue(0.7, 0.7, 0.7); 
         m->transparency = rgba[3]; // fFactor;
         m->shininess = .9;
         node = m;
      } else {
         // Use line style for the transparent shapes
         SoGroup *colorGroup = new SoGroup();
         colorGroup->setName("Transparent_Shape");
            SoLightModel *lighting = new SoLightModel;
            lighting->model = SoLightModel::BASE_COLOR;
            
            SoBaseColor * colorNode = new SoBaseColor;
            colorNode->rgb = SbColor(rgba[0], rgba[1], rgba[2]);
            
            SoDrawStyle *ds = new SoDrawStyle();
            ds->style =  SoDrawStyle::LINES;
            if (rgba[3] > 0.9951) {
               // Make the 100% transparent shape "unpickable"
               SoPickStyle *pickStyle = new SoPickStyle();
               pickStyle->style = SoPickStyle::UNPICKABLE;
               colorGroup->addChild(pickStyle);
            }
            colorGroup->addChild(lighting);
            colorGroup->addChild(ds);
            colorGroup->addChild(colorNode);
            node =colorGroup;
      }
   } else {
      SoBaseColor * colorNode = new SoBaseColor;
      colorNode->rgb = SbColor(rgba[0], rgba[1], rgba[2]);
      node = colorNode;
   }
   return node;
}

//____________________________________________________________________________________________________________________
static inline void AddCoordinates(SoCoordinate3 &currentCoordinate
      ,const std::vector<Coord3D> &vertices,const std::vector<Int_t>   &rootIndices)
{
   // Fill the coordinate from indeces
   SoMFVec3f &point = currentCoordinate.point;
   int currentSize = point.getNum();
//   fprintf(stderr," AddCoordinates getNum = %d %p %f %f %f \n",currentSize, point.getValues(0),
//         *(point.getValues(0)->getValue()),*(point.getValues(0)->getValue()+1)
//         ,*(point.getValues(0)->getValue()+2));
   int startIndex  = currentSize == 1 ? currentSize -1 : currentSize;
   int size        = rootIndices.size();
   // fprintf(stderr,"AddCoordinates %d\n", size);
   point.setNum(startIndex+size);
   std::vector <Int_t>::const_iterator index = rootIndices.begin();
   for (;index != rootIndices.end(); ++index ) {
       const Coord3D &vertex = vertices[*index];
       point.set1Value(startIndex++,vertex.fX,vertex.fY,vertex.fZ);
       // fprintf(stderr,"AddCoordinates %f %f %f\n", vertex.fX,vertex.fY,vertex.fZ );
   }
}

//____________________________________________________________________________________________________________________
static inline void AddCoordinates(SoCoordinate3 &currentCoordinate
      ,const std::vector<Coord3D> &vertices)
{
   // Fill the coordinate from indeces
   SoMFVec3f &point = currentCoordinate.point;
   int currentSize = point.getNum(); 
   int startIndex  = currentSize-1;
   int size        = vertices.size();
   // fprintf(stderr,"AddCoordinates %d\n", size);
   point.setNum(startIndex+size);
   std::vector <Coord3D>::const_iterator vertex = vertices.begin();
   for (;vertex != vertices.end(); ++vertex ) {
       point.set1Value(startIndex++,(*vertex).fX,(*vertex).fY,(*vertex).fZ);
       // fprintf(stderr,"AddCoordinates %f %f %f\n", vertex.fX,vertex.fY,vertex.fZ );
   }
}

//____________________________________________________________________________________________________________________
void TCoinShapeBuilder::MakeShapeQuads(const TPolygone3DFaceBindingView &shape, Bool_t polygon)
{
    // Create the Quads / Polygons face set
   if (!fShapeFaceVertices) {
      fGroup->addChild(fShapeFaceVertices = new SoCoordinate3());
      fGroup->addChild(fShapeFaceNormal   = new SoNormal()     );
      fGroup->addChild(fFaceSet           = new SoFaceSet()    );
   }

   const std::vector<Coord3D> &vertices    = fShapeView.fVertex;
   const std::vector<Coord3D> &normals     = fShapeView.fNormals;

   Int_t normIndex = fShapeFaceNormal->vector.getNum();

   fShapeFaceNormal->vector.set1Value(normIndex,normals[shape.fNormalIndex].fX
                                               ,normals[shape.fNormalIndex].fY
                                               ,normals[shape.fNormalIndex].fZ);

   AddCoordinates(*fShapeFaceVertices,vertices, shape.fVertexIndices);

   // fill numVertices attribute
   Int_t nEdges = polygon ? shape.fVertexIndices.size() : 4;
   Int_t size   = shape.fVertexIndices.size()/nEdges;
   SoMFInt32 &numVertices = fFaceSet->numVertices;
   Int_t      currentSize = numVertices.getNum();
   Int_t      index       = numVertices[0] == -1 ? currentSize - 1 : currentSize;
   if (size) numVertices.setNum(currentSize + size - 1);
   else size = 1;
   for (Int_t i=0; i< size; i++) numVertices.set1Value(index++,nEdges);
}
//____________________________________________________________________________________________________________________
void TCoinShapeBuilder::MakeShapeQuadeStrip(const TPolygone3DFaceBindingView &shape)
{
     // Create the QuadeStrip face set
   const std::vector<Coord3D> &vertices = fShapeView.fVertex;
   const std::vector<Int_t>   &vIndices = shape.fVertexIndices;
   const std::vector<Coord3D> &normals  = fShapeView.fNormals;

         SoNormal *shapeMeshNormal  = new SoNormal;      // current normals
   SoCoordinate3 *shapeMeshVertices = new SoCoordinate3; // current vertices
   SoNormalBinding *binding = new SoNormalBinding();
   binding->value = SoNormalBindingElement::OVERALL;

   Int_t size = vIndices.size();
   Float_t thisNormal[3] = 
   {
            normals[shape.fNormalIndex].fX
           ,normals[shape.fNormalIndex].fY
           ,normals[shape.fNormalIndex].fZ
   };
   shapeMeshNormal->vector.set1Value(0,thisNormal);

   Int_t i;
   for (i=0; i< size; i++) {
      Int_t iv = vIndices[i]; const Coord3D &v = vertices[iv];
      shapeMeshVertices->point.set1Value (i, v.fX, v.fY, v.fZ);
   }
   SoQuadMesh * mesh = new SoQuadMesh;
   mesh->verticesPerRow    = 2;
   mesh->verticesPerColumn = (size+1)/2;
   SoSeparator *meshSeparator = new SoSeparator();
   
   meshSeparator->addChild(binding);
   meshSeparator->addChild(shapeMeshNormal);
   meshSeparator->addChild(shapeMeshVertices);
   meshSeparator->addChild(mesh);
   
   if (fGroup) fGroup->addChild(meshSeparator);
}

//____________________________________________________________________________________________________________________
void TCoinShapeBuilder::MakeShape(const TPolygone3DVertexBindingView &shape)
{
    // Create the QuadeStrip vertex set
   const std::vector<Coord3D> &vertices = fShapeView.fVertex;
   const std::vector<Int_t>   &vIndices = shape.fVertexIndices;
         
   const std::vector<Coord3D> &normals  = fShapeView.fNormals;
   const std::vector <Int_t>  &nIndices = shape.fNormalIndices;

        SoNormal *shapeVertexNormal   = new SoNormal;  // current normals
   SoCoordinate3 *shapeVertexVertices = new SoCoordinate3;// current vertices
   Int_t i;
   Int_t size = vIndices.size();
   for (i=0; i< size; i++) {
      Int_t in = nIndices[i]; const Coord3D &n = normals[in];
      shapeVertexNormal  ->vector.set1Value(i, n.fX, n.fY, n.fZ);

      Int_t iv = vIndices[i]; const Coord3D &v = vertices[iv];
      shapeVertexVertices->point.set1Value (i, v.fX, v.fY, v.fZ);
   }
   SoQuadMesh * mesh = new SoQuadMesh;
   mesh->verticesPerRow    = 2;
   mesh->verticesPerColumn = (size+1)/2;
   
   SoNormalBinding *binding = new SoNormalBinding();
   binding->value = SoNormalBindingElement::PER_VERTEX;

   fGroup->addChild(binding);
   fGroup->addChild(shapeVertexNormal);
   fGroup->addChild(shapeVertexVertices);
   fGroup->addChild(mesh);
}


//____________________________________________________________________________________________________________________
void TCoinShapeBuilder::MakeShape(const TPolygone3DFaceBindingView &shape)
{
   // see: http://www.rush3d.com/reference/opengl-redbook-1.1/figures/fig2-6.gif

   switch (shape.fType) {
      case TPolygone3DView::kTriangle:
         assert(0); break;
      case TPolygone3DView::kQuadeStrip:
         MakeShapeQuadeStrip(shape);
         break;
      case TPolygone3DView::kQuade:
         MakeShapeQuads(shape);
         break;
      case TPolygone3DView::kPolygon:
         MakeShapeQuads(shape,false);
         break;
      default: fprintf(stderr, "ERROR **  MakeShape  unknown shape type %x\n",shape.fType);
   }
}

//____________________________________________________________________________________________________________________
TCoinShapeBuilder::TCoinShapeBuilder(const TShape3DPolygonView &shapeView, const Float_t *rgba)
:  fShapeView(shapeView), fRgba(rgba), fShapeFaceNormal (0),fShapeFaceVertices (0), fFaceSet(0)
,  fMaterial(0)
{
   // class ctor 
}

//____________________________________________________________________________________________________________________
TCoinShapeBuilder::~TCoinShapeBuilder()
{ }

//____________________________________________________________________________________________________________________
SoGroup * TCoinShapeBuilder::CreateCoinShape()
{

    SoSeparator *shapeGroup = new SoSeparator;
    shapeGroup->setName("CoinShapeNode");
//    shapeGroup->renderCaching      = SoSeparator::ON;
//    shapeGroup->boundingBoxCaching = SoSeparator::ON;
    fGroup = shapeGroup;
    const std::vector<Coord3D> &vertices = fShapeView.fVertex;

    SoLineSet   *shapeLine  = 0;
    SoPointSet  *shapePoint = 0;
    if (fShapeView.fPolygonsFaceBinding.size()) {
        std::vector <TPolygone3DFaceBindingView>::const_iterator face_binding_iter = fShapeView.fPolygonsFaceBinding.begin();
        for (; face_binding_iter != fShapeView.fPolygonsFaceBinding.end(); ++face_binding_iter) {
            // fprintf(stderr, " TPolygone3DFaceBindingView  %d\n",++testCounter);
            // SetCurrentSize(rgba);
            if ( ( (*face_binding_iter).fType ==  TPolygone3DView::kLines )  && !shapeLine ) { 
               if (!fMaterial) shapeGroup->addChild(fMaterial = SetCurrentColor(fRgba));
               shapeGroup->addChild(fShapeFaceVertices  = new SoCoordinate3());
               shapeLine       = new SoLineSet();
               SoDrawStyle *ds = new SoDrawStyle();
               ds->lineWidth   = fShapeView.GetLineWidth()+0.4;
               shapeGroup->addChild(ds);
              ::AddCoordinates(*fShapeFaceVertices,vertices);
               break;
            } else if ( ( (*face_binding_iter).fType ==  TPolygone3DView::kPoints) && !shapePoint) {
               if (!fMaterial) shapeGroup->addChild(fMaterial = SetCurrentColor(fRgba));
               Style_t style = (Style_t)fShapeView.GetLineStyle();
               if (vertices.size() > 1000) {
                  // Alas, there are too many of them. 
                  // We should not use the simplest  marker style
                  // if the Coin3D in debug mode
                  style =  kDot; 
               }
              /*
               *-*  List of the currently supported markers (screen and PostScript)
               *-*  ===============================================================
               *-*      1 : dot                     kDot
               *-*      2 : +                       kPlus            PLUS_x_x
               *-*      3 : *                       kStar            STAR_x_x
               *-*      4 : o                       kCircle          CIRCLE_LINE_x_x
               *-*      5 : x                       kMultiply        CROSS_x_x
               *-*      6 : small scalable dot      kFullDotSmall
               *-*      7 : medium scalable dot     kFullDotMedium
               *-*      8 : large scalable dot      kFullDotLarge
               *-*      9 -->15 : dot
               *-*     16 : open triangle down      kOpenTriangleDown
               *-*     18 : full cross              kFullCross
               *-*     20 : full circle             kFullCircle       CIRCLE_FILLED_x_x
               *-*     21 : full square             kFullSquare       QUARE_FILLED_x_x
               *-*     22 : full triangle up        kFullTriangleUp   TRIANGLE_LINE_x_x
               *-*     23 : full triangle down      kFullTriangleDown RHOMBUS_LINE_x_x
               *-*     24 : open circle             kOpenCircle       CIRCLE_LINE_x_x
               *-*     25 : open square             kOpenSquare       SQUARE_LINE_x_x
               *-*     26 : open triangle up        kOpenTriangleUp
               *-*     27 : open diamond            kOpenDiamond      DIAMOND_LINE_x_x
               *-*     28 : open cross              kOpenCross
               *-*     29 : open star               kOpenStar
               *-*     30 : full star               kFullStar
               *-*/
               shapeGroup->addChild(fShapeFaceVertices  = new SoCoordinate3());
               float sizeFactor = 2.66;
               static const int makerSize = SoMarkerSet::SHIP_FILLED_5_5+1; // the last marker type
               int markerShift = makerSize*min(2,int(fShapeView.GetLineWidth()/1.66));
               switch (style) {
                  case kDot: default: 
                     sizeFactor = 0.12;
                  case kFullDotSmall : case kFullDotMedium : case kFullDotLarge :
                     shapePoint =  new SoPointSet();
                     break;
                  case kMultiply:
                     shapePoint = new SoMarkerSet();
                    ((SoMarkerSet *)shapePoint)->markerIndex = SoMarkerSet::CROSS_5_5+markerShift; // 5_5 - 7_7 - 9_9 
                     break;
                  case kPlus:
                     shapePoint = new SoMarkerSet();
                    ((SoMarkerSet *)shapePoint)->markerIndex = SoMarkerSet::PLUS_5_5+markerShift; // 5_5 - 7_7 - 9_9 
                     break;
                  case kStar:
                     shapePoint = new SoMarkerSet();
                    ((SoMarkerSet *)shapePoint)->markerIndex = SoMarkerSet::STAR_5_5+markerShift;
                     break;
                  case kCircle:case  kOpenCircle:
                     shapePoint = new SoMarkerSet();
                    ((SoMarkerSet *)shapePoint)->markerIndex = SoMarkerSet::CIRCLE_LINE_5_5+markerShift;
                     break;
                };
               SoDrawStyle *ds = new SoDrawStyle();
               ds->pointSize = max(1.0,(sizeFactor*fShapeView.GetLineWidth() + 0.5));
               shapeGroup->addChild(ds);
               ::AddCoordinates(*fShapeFaceVertices,vertices);
               break;
            } else if ( !(shapeLine || shapePoint || fShapeFaceVertices) ) {
               if (!fMaterial) shapeGroup->addChild(fMaterial = SetCurrentColor(fRgba,true));

               SoNormalBinding *binding = new SoNormalBinding();
               binding->value = SoNormalBindingElement::PER_FACE;
               shapeGroup->addChild(binding);

               //SoShapeHints *hints   = new SoShapeHints;
               //hints->shapeType      = SoShapeHints::SOLID;
               //hints->vertexOrdering.setIgnored(TRUE);
               //shapeGroup->addChild(hints);

            }
            MakeShape(*face_binding_iter);
        }
     }

      if (shapeLine ) shapeGroup->addChild(shapeLine );
      else if (shapePoint) shapeGroup->addChild(shapePoint);
//--
//--   Vertex binding Face Sets
//
      if (fShapeView.fPolygonsVertexBinding.size() ) {
         if (!fMaterial) shapeGroup->addChild(fMaterial = SetCurrentColor(fRgba,true));
         
         // loop over to created the indexed set
         std::vector <TPolygone3DVertexBindingView>::const_iterator vertex_binding_iter = fShapeView.fPolygonsVertexBinding.begin();;
         for (; vertex_binding_iter != fShapeView.fPolygonsVertexBinding.end(); ++vertex_binding_iter) {
             // fprintf(stderr, " fPolygonsVertexBinding  %d\n",++testCounter);
           MakeShape(*vertex_binding_iter);
         }
     }
     return fGroup;
}
