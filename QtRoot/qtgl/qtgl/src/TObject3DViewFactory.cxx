// @(#)root/gtgl:$Name:  $:$Id: TObject3DViewFactory.cxx,v 1.11 2013/08/30 16:00:18 perev Exp $
// Author: Valery Fine      24/04/05

/****************************************************************************
**
** TObject3DViewFactory
** An unterface of the class visitor to convert the ROOT 3D objets into 
** the concrete "viewer" representation like OpenGL, OpenInventor, x3d etc
**
** Copyright (C) 2005 by Valeri Fine.  Brookhaven National Laboratory All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/
#include <cassert>

#include "TObject3DViewFactory.h"
#include "TShape3DPolygonView.h"
#include "TObject3DView.h"
#include "TPoints3DABC.h"

#include "TBRIK.h"
#include "TPARA.h"
#include "TTUBE.h"
#include "TTUBS.h"
#include "TCONE.h"
#include "TCONS.h"
#include "TPCON.h"
#include "TPGON.h"
#include "TELTU.h"
#include "TTRAP.h"
#include "TCTUB.h"
#include "TTRD1.h"
#include "TTRD2.h"
#include "TSPHE.h"
#include "TGTRA.h"
#include "THYPE.h"
#include "TXTRU.h"
//________________________________________________________________
//
//  TGeoShape forward declarations
//________________________________________________________________
#include "TGeoManager.h"
#include "TGeoBBox.h"
#include "TGeoPara.h"
#include "TGeoTube.h"
#include "TGeoCone.h"
#include "TGeoPcon.h"
#include "TGeoPgon.h"
#include "TGeoEltu.h"
#include "TGeoTrd1.h"
#include "TGeoTrd2.h"
#include "TGeoSphere.h"
#include "TGeoArb8.h"

#include "TGeoParaboloid.h"
#include "TGeoTorus.h"
#include "TGeoHype.h"
#include "TGeoXtru.h"

#include "TShape.h"
#include "TGeoShape.h"

//________________________________________________________________
//
//  Misc 3D ROOT classes
//________________________________________________________________

#include "TPolyLine3D.h"
#include "TPolyMarker3D.h"
#include "TPolyLineShape.h"

// shapeview

// 2 list of polygons (binding normal per vertex, normal per face):

// polygon - type of the polygon  (triangle , quad, polygon)
// Polygon hint (right / left hand rules
// Polygon - list of vertex index, list of normal index. "-1" separated
//         The number of the normal index match that of vertex
// Polygon - normal index, and list of  vertex index

// one array of vertex index
// one array of normals 
// one material per shape always (color, fill style, line style) 

//____________________________________________________________________________________________________________________
static inline void AssertNormal(Double_t code) {
   if (code == 0 )  {
    //  assert(code);
   }
}

//____________________________________________________________________________________________________________________
TObject3DViewFactory::TObject3DViewFactory() : TObject3DViewFactoryABC()
{                              }
//____________________________________________________________________________________________________________________
TObject3DViewFactory::~TObject3DViewFactory()
{                                           }
//____________________________________________________________________________________________________________________
//TObject3DView *TObject3DViewFactory::CreateMatrix(const Double_t * /*traslation*/ , const Double_t * /*rotation*/, Bool_t /*isReflection*/ )
//{ return new TObject3DView() ; }
//____________________________________________________________________________________________________________________
void TObject3DViewFactory::AddNormal(TObject3DView *, const Double_t * /* normal */ )
{ }
//____________________________________________________________________________________________________________________
TObject3DView *TObject3DViewFactory::CreateShape(const TObject *shape, const Float_t *rgba)
{ 
   TObject3DView *view =0;
   const TShape *olShape = dynamic_cast<const TShape *>(shape);
   if (olShape) {
        const TPolyLineShape *polyShape = dynamic_cast<const TPolyLineShape *>(shape);
        if (polyShape ) view = MakeShape(polyShape,rgba );
        else            view = MakeShape(olShape,rgba);
   } else {
      const TGeoShape *geoShape = dynamic_cast<const TGeoShape *>(shape);
      if (geoShape)  view = MakeShape(geoShape,rgba);
      else {
         const TPolyLine3D *polyLine = dynamic_cast<const TPolyLine3D *>(shape);
         if (polyLine) view = MakeShape(polyLine,rgba );
         else {
            const TPolyMarker3D *polyMarker = dynamic_cast<const TPolyMarker3D *>(shape);
            if (polyMarker) 
               view = MakeShape(polyMarker,rgba );
            else {
                  fprintf(stderr,"TObject3DViewFactory::CreateShape: Can not draw the %s yet. Sorry!\n", shape->ClassName());
   }   }  }  }
   return view;
}
//____________________________________________________________________________________________________________________
void TObject3DViewFactory::CompileViewLevel(TObject3DView *,ERenderType)
{ }
//____________________________________________________________________________________________________________________
ULong_t TObject3DViewFactory::GetViewerId(TObject3DView *) const 
{return 0; }
//____________________________________________________________________________________________________________________
Bool_t TObject3DViewFactory::NeedCompilation() const
{return kFALSE; }
//____________________________________________________________________________________________________________________
void   TObject3DViewFactory::Release(TObject3DView *)
{ }

//____________________________________________________________________________________________________________________
template <class D> void SetPoints(const TShape *shp,D *points)
{
   class FakeShapePoint : public TShape {
     friend class TObject3DViewFactory;
      private:
                 FakeShapePoint() {};
        virtual ~FakeShapePoint(){};
     public:
        void ShapePoints(Double_t *points) { 
           SetPoints(points) ;
        }
   };
   ((FakeShapePoint *)shp)->ShapePoints(points);
}

template <class D> void SetPoints(const TGeoShape *shp,D *points)
{ 
   shp->SetPoints(points);
}

//________________________________________________________________
//
//   Make the TShapes
//________________________________________________________________

//__________________________________________________________________________________
template <class S> TObject3DView *TObject3DViewFactory::MakeBrikShape(const S &shp, const Float_t *rgba) 
{

   //   points[ 0] = -fDx ; points[ 1] = -fDy ; points[ 2] = -fDz;
   //   points[ 3] = -fDx ; points[ 4] =  fDy ; points[ 5] = -fDz;
   //   points[ 6] =  fDx ; points[ 7] =  fDy ; points[ 8] = -fDz;
   //   points[ 9] =  fDx ; points[10] = -fDy ; points[11] = -fDz;
   //   points[12] = -fDx ; points[13] = -fDy ; points[14] =  fDz;
   //   points[15] = -fDx ; points[16] =  fDy ; points[17] =  fDz;
   //   points[18] =  fDx ; points[19] =  fDy ; points[20] =  fDz;
   //   points[21] =  fDx ; points[22] = -fDy ; points[23] =  fDz;
   //}


   const Int_t nVertices = 8;   const Int_t nNormals  = 6;
   const int topindex    = 4;   const int bottomindex = 5;
   static Int_t sidefaces[6][4]; static int iface=0;  
   if (! iface ) { // build the index at once
      for (iface = 0; iface<4; iface++) {
         sidefaces[topindex][iface]    = 7-iface;
         sidefaces[bottomindex][iface] = iface;
         if (iface < 3) {
            sidefaces[iface][0] = iface+4;
            sidefaces[iface][1] = iface+5;
            sidefaces[iface][2] = iface+1;
            sidefaces[iface][3] = iface+0;
         } else {
            sidefaces[iface][0] = 3;
            sidefaces[iface][1] = 7;
            sidefaces[iface][2] = 4;
            sidefaces[iface][3] = 0;
   }  }  }

   TShape3DPolygonView view(nVertices,nNormals,rgba);
   
   // Get the precomputed vertices from the shape
   view.fVertex.resize(nVertices);
   
   SetPoints(&shp,&view.fVertex[0].fX);
    // Set normals and face indices
    // -x : face=0
    // +y : face=1
    // +x : face=2
    // -y : face=3
    // +z : face=topindex
    // -z : face=bottomindex
    // ---------------------------
    std::vector<Coord3D> &normals = view.fNormals;
    Double_t *vertices = &view.fVertex[0].fX;
    int face;
    int j;
    for (face = 0; face<6; face++,vertices+=3) {
       TPolygone3DFaceBindingView polygon(6);
       Double_t *vertices[4];
       for (j=0;j<4;j++) {
          polygon.fVertexIndices.push_back(sidefaces[face][j]); 
          vertices[j] = &view.fVertex[sidefaces[face][j]].fX;
       }
       Coord3D currentNormal;
       TMath::Normal2Plane(vertices[2],vertices[0],vertices[1],&currentNormal.fX);
       if (!TMath::Normalize(&currentNormal.fX)) {
//         May be this is a triangle !!!
//         Let's try other edges
          TMath::Normal2Plane(vertices[0],vertices[2],vertices[3],&currentNormal.fX);
//           fprintf(stderr," normals: %f %f %f \n", currentNormal.fX ,currentNormal.fY, currentNormal.fZ);
           if (!TMath::Normalize(&currentNormal.fX)) {
//           This is not a surface at all. Any normal is Ok then:
             currentNormal.fX = 1; currentNormal.fY = 0; currentNormal.fZ = 0;
          }
       }
       polygon.fNormalIndex = normals.size();
       normals.push_back(currentNormal);
       view.fPolygonsFaceBinding.push_back(polygon);
    }

    // Complete the transition

    return MakeShape(view,rgba);
}
//__________________________________________________________________________________
template <class D> D  dist(D *x, D *y, int size) {
   D sum;
   for (int i =0; i < size; i++) sum += (x[i]*x[i] - y[i]*y[i]);
   return sum;
}

//__________________________________________________________________________________
template <class S> TObject3DView *TObject3DViewFactory::MakeXtruShape(const S &shp, const Float_t *rgba, int nDiv, int nstacks) 
{
#define vrtndx(i,j)  ((i)+nFaces*(j))
#define vert(i,j) (&vertex[3*vrtndx(i,j)])

   assert(nstacks >= 2);
   Double_t *vertex = 0;

   const Int_t nFaces = nDiv;
   int topindex    = 1;           int bottomindex = 0;
   Int_t i,j;

   // calculate the number of vertices;
   const Int_t nVertices = nDiv*nstacks; 

   // calculate the number of normals;
   Int_t nNormals = nFaces*(nstacks-1)+2;

   TShape3DPolygonView view(nVertices,nNormals,rgba);

   // Get the precomputed vertices from the shape
   view.fVertex.resize(nVertices);
   SetPoints(&shp,&view.fVertex[0].fX);

   std::vector<Coord3D> &normals = view.fNormals; // to be filled yet

   // We need to know the vertices to calculate the normals
   vertex  = &view.fVertex[0].fX;
   view.fPolygonsFaceBinding.reserve(nNormals);

   //
   // Bottom normals 
   // -------------
   Coord3D norm;
   TMath::Normal2Plane( vert(0, 0) ,vert(1, 0),vert(nDiv-1, 0),&norm.fX);
   Int_t bottomNormalIndex = normals.size();
   normals.push_back(norm); 
   //
   // Top normals
   // -------------
   TMath::Normal2Plane( vert(0, nstacks-1),vert(1, nstacks-1),vert(nDiv-1, nstacks-1),&norm.fX);
   Int_t topNormalIndex   = normals.size();
   norm.fX= 0; norm.fY=0; norm.fZ = 1;
   normals.push_back(norm); 
   // All normals have been built
   // Build the shape now.

   // make top and bottom "lids"
   TPolygone3DFaceBindingView topPolygon(nFaces);
   topindex = view.fPolygonsFaceBinding.size();
   topPolygon.fNormalIndex  = topNormalIndex;
   view.fPolygonsFaceBinding.push_back(topPolygon);

   TPolygone3DFaceBindingView bottomPolygon(nFaces);
   bottomindex = view.fPolygonsFaceBinding.size();
   bottomPolygon.fNormalIndex  = bottomNormalIndex;
   view.fPolygonsFaceBinding.push_back(bottomPolygon);

   std::vector<Int_t> &topvertexindex    = view.fPolygonsFaceBinding[topindex]   .fVertexIndices;
   std::vector<Int_t> &bottomvertexindex = view.fPolygonsFaceBinding[bottomindex].fVertexIndices;

   Int_t i1top    = nDiv*(nstacks - 1);
   Int_t i1bottom = nDiv-1;

   view.fPolygonsFaceBinding[topindex]   .fType = TPolygone3DView::kQuadeStrip;
   view.fPolygonsFaceBinding[bottomindex].fType = TPolygone3DView::kQuadeStrip;
   for (i =0;i < nDiv/2; i++)   {
      topvertexindex.push_back(i1top+i);
      topvertexindex.push_back(nDiv*nstacks-i-1);

      bottomvertexindex.push_back(i1bottom-i);
      bottomvertexindex.push_back(i);
   }
   //------------------------------------------ 
   //   make walls
   //------------------------------------------ 
   //Int_t nextStack = nDiv;
   // Int_t currentStack = 0;
   for (j=0;j< nstacks-1; j++) 
   {
      for (i=0;i< nDiv; i++) { 
         TPolygone3DFaceBindingView face(4);
         face.fType = TPolygone3DView::kQuade ;
         Int_t k = (i < nDiv-1) ? i+1 : 0;
         // fill vertex indices
         Int_t v1 = vrtndx( i,j+1); face.fVertexIndices.push_back(v1);
         Int_t v3 = vrtndx( k,j+1); face.fVertexIndices.push_back(v3);
         Int_t v2 = vrtndx( k, j ); face.fVertexIndices.push_back(v2);
         Int_t v0 = vrtndx( i, j ); face.fVertexIndices.push_back(v0);
         // calculate and fill the normal  and its index

         TMath::Normal2Plane( &vertex[3*v0],&vertex[3*v1],&vertex[3*v2],&norm.fX);
         face.fNormalIndex = normals.size();  normals.push_back(norm);        
         // add the entire polygon to the container
         view.fPolygonsFaceBinding.push_back(face);
      }
   }
   // decay the wall if any
#undef vrtndx
#undef vert
   return MakeShape(view,rgba);
}

//__________________________________________________________________________________
template <class S> TObject3DView *TObject3DViewFactory::MakeConeShape(const S &shp, const Float_t *rgba, int n, int nstacks,Int_t plain) 
{
   //  shp - ROOT either TShape or TGeoShape object
   //  n   - number of segments (precision)
   //        < 0 means the shape is segmented
   //        > 0 means the shape is closed
   //  nstacks -  number of stack sections (>=2) 

   //      for (j = 0; j < n; j++) {
   //         points[indx+6*n] = points[indx] = fRmin * fCoTab[j];
   //         indx++;
   //         points[indx+6*n] = points[indx] = fAspectRatio*fRmin * fSiTab[j];
   //         indx++;
   //         points[indx+6*n] = fDz;
   //         points[indx]     =-fDz;
   //         indx++;
   //      }
   //      for (j = 0; j < n; j++) {
   //         points[indx+6*n] = points[indx] = fRmax * fCoTab[j];
   //         indx++;
   //         points[indx+6*n] = points[indx] = fAspectRatio*fRmax * fSiTab[j];
   //         indx++;
   //         points[indx+6*n]= fDz;
   //         points[indx]    =-fDz;
   //         indx++;
   //      }

   //
   //   i - the division number
   //   j - the stack number
   //   k = 0 internal points
   //       1 external points
  
  
   enum {kInternal=0, kExternal};
#define vertindex(i,j,k)  ((i)+nDiv*(2*(j)+(k)))
#define vert(i,j,k) (&vertex[3*vertindex(i,j,k)])
#define vindex(i,j,k) Int_t(((vert(i,j,k) - &vertex[0,0,0])/sizeof(Float_t *)))

   assert(nstacks >= 2);
   // check whether there is some internal "hole"
   Double_t *vertex = 0;
   const Int_t nDiv   = n >= 0 ?  n : -n;
#if 0
   Bool_t hole = (dist<Double_t>(vert(0,0,kInternal),vert(1,0,kInternal),3) != 0 ) ;
#else
   Bool_t hole = kTRUE; // (dist<Double_t>(vert(0,0,kInternal),vert(1,0,kInternal),3) != 0 ) ;
#endif
#if 0
   // Check whether it is realy open
   if (n < 0 && (dist(vert(0,0,kInternal),vert(nDiv,0,kInternal),3) < 
      0.01*dist(vert(0,0,kInternal),vert(0,1,kInternal),3) ) ) {
      n = -n;
   }
#endif
   Bool_t closed = (n >= 0);
   const Int_t nFaces = closed ?  nDiv : nDiv-1;
   int topindex    = 1;           int bottomindex = 0;
   Int_t i,j;

   // calculate the number of vertices;
   const Int_t nVertices = 2*nDiv*nstacks; 

   // calculate the number of normals;
   Int_t nNormals = nDiv;
   if (closed && (plain == kPlain) ) 
      nNormals -= 1;
   nNormals *= (nstacks-1);
   if (hole) 
      nNormals *= 2;
   // add lids and decay walls if any
   nNormals  += 2;
   if (!closed) 
      nNormals +=2;

  //  fprintf(stderr," MakeConeShape %s   nVertices = %d , n = %d , nstacks=%d ,plain=%d,nNormals=%d \n", shp.ClassName(), nVertices, n,  nstacks,plain,nNormals);

   TShape3DPolygonView view(nVertices,nNormals,rgba);

   // Get the precomputed vertices from the shape
   view.fVertex.resize(nVertices);
   SetPoints(&shp,&view.fVertex[0].fX);

   std::vector<Coord3D> &normals = view.fNormals;

   // We need to know the vertices to calculate the normals
   vertex  = &view.fVertex[0].fX;

   if (plain == kPlain)  {
      view.fPolygonsFaceBinding.reserve(nNormals);
   } else {
      Int_t nFlats = 2;          // +2 top and bottom surfaces
      if (!closed) nFlats += 2;  // +2 To render the decay walls
      view.fPolygonsFaceBinding.reserve(nFlats);
      view.fPolygonsVertexBinding.reserve(nNormals-nFlats);
   }


   //
   // Bottom normals 
   // -------------
   Coord3D norm;
   Coord3D norm2;
   TMath::Normal2Plane( vert(0, 0, kExternal)
      ,vert(0, 0, kInternal)
      ,vert(1, 0, kExternal)
      ,&norm.fX);
   Int_t bottomNormalIndex = normals.size();
   normals.push_back(norm); 
   // fprintf(stderr," normal %d %f %f %f\n", bottomNormalIndex, norm.fX, norm.fY, norm.fZ);

   //
   // Top normals
   // -------------
   TMath::Normal2Plane( vert(0, nstacks-1, kInternal)
      ,vert(0, nstacks-1, kExternal)
      ,vert(1, nstacks-1, kExternal)
      ,&norm.fX);
   Int_t topNormalIndex   = normals.size();
   normals.push_back(norm); 
   // fprintf(stderr," normal %d %f %f %f\n", topNormalIndex, norm.fX, norm.fY, norm.fZ);

   Int_t sideDecayNormalIndex = -1;
   if (!closed) {
      sideDecayNormalIndex = normals.size();

      TMath::Normal2Plane( vert(0, 0        , kExternal)
         ,vert(0, nstacks-1, kExternal)
         ,vert(0, 0        , kInternal)
         ,&norm.fX);
      normals.push_back(norm);

      TMath::Normal2Plane( vert(nDiv-1, 0        , kInternal)
         ,vert(nDiv-1, nstacks-1, kExternal)
         ,vert(nDiv-1, 0        , kExternal)
         ,&norm.fX);
      normals.push_back(norm);        
   }

   // All normals have been built
   // Build the shape now.

   // make top and bottom "lids"
   TPolygone3DFaceBindingView topPolygon(nFaces+ (hole ? nFaces:0));
   topindex = view.fPolygonsFaceBinding.size();
   topPolygon.fNormalIndex  = topNormalIndex;
   view.fPolygonsFaceBinding.push_back(topPolygon);

   TPolygone3DFaceBindingView bottomPolygon(nFaces+ (hole ? nFaces:0));
   bottomindex = view.fPolygonsFaceBinding.size();
   bottomPolygon.fNormalIndex  = bottomNormalIndex;
   view.fPolygonsFaceBinding.push_back(bottomPolygon);

   std::vector<Int_t> &topvertexindex    = view.fPolygonsFaceBinding[topindex]   .fVertexIndices;
   std::vector<Int_t> &bottomvertexindex = view.fPolygonsFaceBinding[bottomindex].fVertexIndices;
   // external polygon 
   Int_t i1bottom = 0;
   Int_t i1top = i1bottom + nDiv*(nstacks - 1)* (hole ? 2 : 1);
   i1bottom += nDiv-1;
   if (hole) {
      view.fPolygonsFaceBinding[topindex]   .fType = TPolygone3DView::kQuadeStrip;
      view.fPolygonsFaceBinding[bottomindex].fType = TPolygone3DView::kQuadeStrip;

      for (i = 0; i < nDiv; i++) { 
         topvertexindex.push_back(i1top + i);
         topvertexindex.push_back(i1top+nDiv+i);

         bottomvertexindex.push_back(i1bottom - i);
         bottomvertexindex.push_back(i1bottom+nDiv-i);
      }
      if (closed)  {
         // match the first and last points
         Int_t iFirst = 0;
         topvertexindex.push_back(topvertexindex[2*iFirst]);
         topvertexindex.push_back(topvertexindex[2*iFirst+1]);

         bottomvertexindex.push_back(bottomvertexindex[2*iFirst]);
         bottomvertexindex.push_back(bottomvertexindex[2*iFirst+1]);
      } 
   } else {
      view.fPolygonsFaceBinding[topindex]   .fType = TPolygone3DView::kPolygon;
      view.fPolygonsFaceBinding[bottomindex].fType = TPolygone3DView::kPolygon;
      for (i =0;i < nDiv; i++)   {
         topvertexindex.push_back(i1top+i);
         bottomvertexindex.push_back(i1bottom-i);
      }
      if (closed) {
         topvertexindex.push_back(i1top);
         bottomvertexindex.push_back(i1bottom);
      } else {
         topvertexindex.push_back(i1top+i);
         bottomvertexindex.push_back(i1bottom-i);
      }
   }

   //------------------------------------------ 
   //   make walls
   //------------------------------------------ 
   Int_t nextStack = nDiv;
   Int_t currentStack = 0;
   int nLoop = nDiv;
   if (plain == kPlain && !closed) nLoop--; // I don't know why like this yet :-(((
   for (j=0;j< nstacks-1; j++,nextStack += nDiv, currentStack +=nDiv ) 
   {
      Int_t firstNormalIndex  = -1;
      TPolygone3DVertexBindingView extFaces(nDiv*2,nDiv);
      extFaces.fType=TPolygone3DView::kQuadeStrip;
      for (i=0;i< nLoop; i++) { ///---------
         Int_t nextNormalIndex = normals.size();
         if (i==0 ) firstNormalIndex = nextNormalIndex;
         // external Wall
         if (plain == kPlain) {
            TPolygone3DFaceBindingView face(4);
            face.fType = TPolygone3DView::kQuade ;
            // fill vertex indices
            Int_t v0 = vertindex( i,  j ,kExternal); face.fVertexIndices.push_back(v0);
            Int_t v2 = vertindex(i+1 ,j ,kExternal); face.fVertexIndices.push_back(v2);
            Int_t v3 = vertindex(i+1,j+1,kExternal); face.fVertexIndices.push_back(v3);
            Int_t v1 = vertindex( i ,j+1,kExternal); face.fVertexIndices.push_back(v1);

            // calculate and fill the normal  and its index

            TMath::Normal2Plane( &vertex[3*v0],&vertex[3*v2],&vertex[3*v1],&norm.fX);
            face.fNormalIndex = nextNormalIndex;  normals.push_back(norm);        
            // add the entire polygon to the container
            view.fPolygonsFaceBinding.push_back(face);
         } else {
            // fill vertex indices
            Int_t v0 = vertindex(i,j+1,kExternal);  extFaces.fVertexIndices.push_back(v0);
            Int_t v1 = vertindex(i, j ,kExternal);  extFaces.fVertexIndices.push_back(v1);
            // calculate and fill the normal  and its index
            if (plain == kSphere) {
               norm.fX = vertex[3*v0]; norm.fY = vertex[3*v0+1]; norm.fZ = vertex[3*v0+2];
               AssertNormal(TMath::Normalize(&norm.fX));
               extFaces.fNormalIndices.push_back(nextNormalIndex);
               normals.push_back(norm);        

               norm.fX = vertex[3*v1]; norm.fY = vertex[3*v1+1]; norm.fZ = vertex[3*v1+2];
               AssertNormal(TMath::Normalize(&norm.fX));
               nextNormalIndex++;
            } else {
               if (i == 0 ) {
                  Int_t v0Next = vertindex(i+1,j,kExternal);
                  TMath::Normal2Plane( &vertex[3*v0],&vertex[3*v1],&vertex[3*v0Next],&norm.fX);
               } else if (i == nDiv-1) {
                  Int_t v0Pre = vertindex(i-1,j,kExternal);
                  TMath::Normal2Plane( &vertex[3*v0Pre],&vertex[3*v1], &vertex[3*v0] ,&norm.fX);
               } else {
                  Int_t v0Next = vertindex( ((i == nDiv-1) ? 0 : i+1),j,kExternal);

                  TMath::Normal2Plane( &vertex[3*v0],&vertex[3*v1],&vertex[3*v0Next],&norm.fX);

                  Int_t v0Pre = vertindex(( i==0 ? nDiv-1: i-1),j,kExternal);
                  TMath::Normal2Plane( &vertex[3*v0Pre],&vertex[3*v1], &vertex[3*v0] ,&norm2.fX);
                  norm.fX = (norm.fX + norm2.fX)/2; norm.fY = (norm.fY + norm2.fY)/2; norm.fZ = (norm.fZ + norm2.fZ)/2;
               }
               extFaces.fNormalIndices.push_back(nextNormalIndex); 
            }
            extFaces.fNormalIndices.push_back(nextNormalIndex);
            normals.push_back(norm);    
         }
      }
      if (closed) {
         // close the shape with an extra surface
         if (plain == kPlain) {
            TPolygone3DFaceBindingView face(4);
            // fill vertex indices
            Int_t v0 = vertindex(nDiv-1,j  ,kExternal); face.fVertexIndices.push_back(v0);
            Int_t v2 = vertindex(0     ,j  ,kExternal); face.fVertexIndices.push_back(v2);
            Int_t v3 = vertindex(0     ,j+1,kExternal); face.fVertexIndices.push_back(v3); 
            Int_t v1 = vertindex(nDiv-1,j+1,kExternal); face.fVertexIndices.push_back(v1); 

            // calculate and fill the normal  and its index
            TMath::Normal2Plane( &vertex[3*v0],&vertex[3*v2],&vertex[3*v1],&norm.fX);
            face.fNormalIndex = normals.size();  normals.push_back(norm);        

            // add the entire polygon to the container
            view.fPolygonsFaceBinding.push_back(face);
         } else {
            // fill vertex indices
            Int_t v0 = vertindex(0,j+1,kExternal);  extFaces.fVertexIndices.push_back(v0);
            Int_t v1 = vertindex(0,j  ,kExternal);  extFaces.fVertexIndices.push_back(v1);
            extFaces.fNormalIndices.push_back(firstNormalIndex);
            extFaces.fNormalIndices.push_back(firstNormalIndex);
         }
      }
      if (plain != kPlain) view.fPolygonsVertexBinding.push_back(extFaces);
   }
   if (hole) {
      for (j=0;j< nstacks-1; j++,nextStack += nDiv, currentStack +=nDiv ) 
      {
         Int_t firstNormalIndex  = -1;
         TPolygone3DVertexBindingView intFaces(nDiv*2,nDiv);
         intFaces.fType=TPolygone3DView::kQuadeStrip;
         for (i=0;i< nLoop; i++) { // -----------------
            Int_t nextNormalIndex = normals.size();
            if (i==0 ) firstNormalIndex = nextNormalIndex;

            // internal wall
            if (plain== kPlain) {
               TPolygone3DFaceBindingView face(4);
               // fill vertex indices
               Int_t v0 = vertindex( i , j ,kInternal);  face.fVertexIndices.push_back(v0);
               Int_t v1 = vertindex( i ,j+1,kInternal);  face.fVertexIndices.push_back(v1); 
               Int_t v3 = vertindex(i+1,j+1,kInternal);  face.fVertexIndices.push_back(v3); 
               Int_t v2 = vertindex(i+1, j ,kInternal);  face.fVertexIndices.push_back(v2);
      
               // calculate and fille the normal  and its index

               TMath::Normal2Plane( &vertex[3*v0],&vertex[3*v1],&vertex[3*v2],&norm.fX);
               face.fNormalIndex = normals.size(); normals.push_back(norm);        
               view.fPolygonsFaceBinding.push_back(face);
            } else {
               Int_t v0 = vertindex(i, j ,kInternal); intFaces.fVertexIndices.push_back(v0);
               Int_t v1 = vertindex(i,j+1,kInternal); intFaces.fVertexIndices.push_back(v1);

               // calculate and fill the normal  and its index
               if (plain == kSphere) {

                  norm.fX = -vertex[3*v0]; norm.fY = -vertex[3*v0+1]; norm.fZ = -vertex[3*v0+2];
                  AssertNormal(TMath::Normalize(&norm.fX));
                  intFaces.fNormalIndices.push_back(nextNormalIndex);
                  normals.push_back(norm);        

                  norm.fX = -vertex[3*v1]; norm.fY = -vertex[3*v1+1]; norm.fZ = -vertex[3*v1+2];
                  AssertNormal(TMath::Normalize(&norm.fX));
                  nextNormalIndex++;
               } else {
                  if (i == 0 ) {
                     Int_t v0Next = vertindex(i+1,j,kInternal);
                     TMath::Normal2Plane( &vertex[3*v0],&vertex[3*v1],&vertex[3*v0Next],&norm.fX);
                  } else if (i == nDiv-1) {
                      Int_t v0Pre = vertindex(i-1,j,kInternal);
                      TMath::Normal2Plane( &vertex[3*v0Pre],&vertex[3*v1],&vertex[3*v0] ,&norm.fX);
                  } else {
                     Int_t v0Next = vertindex( ((i == nDiv-1) ? 0 : i+1),j,kInternal);

                     TMath::Normal2Plane( &vertex[3*v0],&vertex[3*v1],&vertex[3*v0Next],&norm.fX);

                     Int_t v0Pre = vertindex( (i==0 ? nDiv-1: i-1),j,kInternal);
                     TMath::Normal2Plane( &vertex[3*v0Pre],&vertex[3*v1],&vertex[3*v0] ,&norm2.fX);
                     norm.fX = (norm.fX + norm2.fX)/2; norm.fY = (norm.fY + norm2.fY)/2; norm.fZ = (norm.fZ + norm2.fZ)/2;
                  }
                  intFaces.fNormalIndices.push_back(nextNormalIndex);
               }
               intFaces.fNormalIndices.push_back(nextNormalIndex);
               normals.push_back(norm);        
            }
         }
         if (closed) {
            // internal wall
            if (plain== kPlain) {
               TPolygone3DFaceBindingView face(4);
               // fill vertex indices
               Int_t v0 = vertindex(nDiv-1, j ,kInternal);  face.fVertexIndices.push_back(v0);
               Int_t v1 = vertindex(nDiv-1,j+1,kInternal);  face.fVertexIndices.push_back(v1); 
               Int_t v3 = vertindex(  0   ,j+1,kInternal);  face.fVertexIndices.push_back(v3); 
               Int_t v2 = vertindex(  0   , j ,kInternal);  face.fVertexIndices.push_back(v2);

               // calculate and fill the normal  and its index

               TMath::Normal2Plane( &vertex[3*v0],&vertex[3*v1],&vertex[3*v2],&norm.fX);
               face.fNormalIndex = normals.size(); normals.push_back(norm);        
               view.fPolygonsFaceBinding.push_back(face);
            } else {
               Int_t v0 = vertindex(0, j ,kInternal); intFaces.fVertexIndices.push_back(v0);
               Int_t v1 = vertindex(0,j+1,kInternal); intFaces.fVertexIndices.push_back(v1);

               intFaces.fNormalIndices.push_back(firstNormalIndex);
               intFaces.fNormalIndices.push_back(firstNormalIndex);
            }
         }
         if (plain != kPlain) 
            view.fPolygonsVertexBinding.push_back(intFaces);
      }
   }
   // decay the wall if any
   if (!closed){
#if 0
      TPolygone3DFaceBindingView face1(2*nstacks);
      face1.fType = TPolygone3DView::kPolygon;
      face1.fNormalIndex = sideDecayNormalIndex;

      TPolygone3DFaceBindingView face2(2*nstacks);
      face2.fNormalIndex = sideDecayNormalIndex+1;
      face2.fType  = TPolygone3DView::kPolygon;
      for (i=0;i<nstacks;i++) {
         Int_t v0 = vertindex( 0, nstacks-i-1 ,kInternal );
         face1.fVertexIndices.push_back(v0);
  
         Int_t u0 = vertindex(nDiv-1, i ,kInternal);
         face2.fVertexIndices.push_back(u0);
      }
      for (i=nstacks-1;i>=0;i--) {
         Int_t v0 = vertindex(   0  , nstacks-i-1 ,kExternal);
         face1.fVertexIndices.push_back(v0);

         Int_t u0 = vertindex(nDiv-1, i ,kExternal);
         face2.fVertexIndices.push_back(u0);
      }
      view.fPolygonsFaceBinding.push_back(face1);
      view.fPolygonsFaceBinding.push_back(face2);
#else
      TPolygone3DFaceBindingView face1(2*nstacks);
      face1.fType = TPolygone3DView::kQuadeStrip;
      face1.fNormalIndex = sideDecayNormalIndex;

      TPolygone3DFaceBindingView face2(2*nstacks);
      face2.fType = TPolygone3DView::kQuadeStrip;
      face2.fNormalIndex = sideDecayNormalIndex+1;

	  for (i=0;i<nstacks;i++) {
         Int_t v0;
         v0 = vertindex( 0, nstacks-i-1 ,kExternal );
         face1.fVertexIndices.push_back(v0);

         v0 = vertindex( 0, nstacks-i-1 ,kInternal );
         face1.fVertexIndices.push_back(v0);

// --
		   v0 = vertindex( nDiv-1, nstacks-i-1 ,kInternal );
         face2.fVertexIndices.push_back(v0);

         v0 = vertindex( nDiv-1, nstacks-i-1 ,kExternal );
         face2.fVertexIndices.push_back(v0);

      }
      view.fPolygonsFaceBinding.push_back(face1);
      view.fPolygonsFaceBinding.push_back(face2);
#endif
   }

#undef vert
   return MakeShape(view,rgba);
}

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TBRIK &shp, const Float_t *rgba) 
{   return MakeBrikShape(shp,rgba); }
//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TPARA &shp, const Float_t *rgba) 
{   return MakeBrikShape(shp,rgba); }

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TTRAP &shp, const Float_t *rgba) 
{   return MakeBrikShape(shp,rgba); }

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TTRD1 &shp, const Float_t *rgba)
{   return MakeBrikShape(shp,rgba); }

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TTRD2 &shp, const Float_t *rgba) 
{   return MakeBrikShape(shp,rgba); }


//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGTRA &shp, const Float_t *rgba) 
{   return MakeBrikShape(shp,rgba); }


//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TTUBE &shp, const Float_t *rgba) {
   return MakeConeShape(shp,rgba,shp.GetNumberOfDivisions(),2,kCyl);
}

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TTUBS &shp, const Float_t *rgba) {
//     gVirtualGL->PaintCone(vertex,-(GetNumberOfDivisions()+1),2);
   return MakeConeShape(shp,rgba,-(shp.GetNumberOfDivisions()+1),2,kCyl);
}

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TCONE &shp, const Float_t *rgba) {
   return MakeConeShape(shp,rgba,shp.GetNumberOfDivisions(),2,kCyl);
}

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TCONS &shp, const Float_t *rgba) {
   return MakeConeShape(shp,rgba,-(shp.GetNumberOfDivisions()+1),2,kCyl);
}

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TPCON &shp, const Float_t *rgba) {
 //   gVirtualGL->PaintCone(vertex,-(GetNumberOfDivisions()+1),fNz)
   return MakeConeShape(shp,rgba,-(shp.GetNumberOfDivisions()+1),shp.GetNz(),kCyl);
}

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TPGON &shp, const Float_t *rgba) {
   return MakeConeShape(shp,rgba,-(shp.GetNumberOfDivisions()+1),shp.GetNz(),kPlain);
}

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TELTU &shp, const Float_t *rgba)
{    return MakeConeShape(shp,rgba,shp.GetNumberOfDivisions(),2,kCyl);          }


//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TCTUB &shp, const Float_t *rgba)
{   return MakeConeShape(shp,rgba,-(shp.GetNumberOfDivisions()+1),2,kCyl);         }

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TSPHE &shp, const Float_t *rgba)
{
   // Alas TSHPE has no method public GetNz()
   //  We have to recompute that value ourselves (see: TSPHE::SetNumberOfDivisions )
    Int_t nZ = Int_t(shp.GetAspectRatio()*shp.GetNumberOfDivisions()*
        (  shp.GetThemax() - shp.GetThemin() )
      / (  shp.GetPhimax() - shp.GetPhimin() ) ) + 1;
   return MakeConeShape(shp,rgba,-(shp.GetNumberOfDivisions()+1),nZ+1,kSphere);
}

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const THYPE & /* shp */ , const Float_t * /* rgb */ ) 
{   return 0;              }

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TXTRU & /* shp */, const Float_t * /* rgb */ )
{   return 0;                                                                               }
//________________________________________________________________
//
//   Make the TGeoShapes
//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoBBox &shp, const Float_t *rgba)
{   return MakeBrikShape(shp,rgba); }

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoPara &shp, const Float_t *rgba)
{   return MakeBrikShape(shp,rgba); }

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoTube &shp, const Float_t *rgba) 
{
  return MakeConeShape(shp,rgba,gGeoManager->GetNsegments(),2,kCyl);
}

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoTubeSeg &shp, const Float_t *rgba) 
{   return MakeConeShape(shp,rgba,-(gGeoManager->GetNsegments()+1),2,kCyl);   }

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoCone &shp, const Float_t *rgba) {
   return MakeConeShape(shp,rgba,gGeoManager->GetNsegments(),2,kCyl);
}

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoConeSeg &shp, const Float_t *rgba) {
   return MakeConeShape(shp,rgba,-(gGeoManager->GetNsegments()+1),2,kCyl);
}

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoPcon &shp, const Float_t *rgba) {
   return MakeConeShape(shp,rgba,-(gGeoManager->GetNsegments()+1),shp.GetNz(),kCyl);
}

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoPgon &shp, const Float_t *rgba) {
	return MakeConeShape(shp,rgba,-(shp.GetNedges()+1),shp.GetNz(),kPlain);
}

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoEltu & shp, const Float_t *rgba) 
{    return MakeConeShape(shp,rgba,-(gGeoManager->GetNsegments()+1),2,kCyl);          }

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoTrap &shp, const Float_t *rgba)
{   return MakeBrikShape(shp,rgba); }

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoCtub &shp, const Float_t *rgba)
{   return MakeConeShape(shp,rgba,-(gGeoManager->GetNsegments()+1),2,kCyl);            }

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoTrd1 &shp, const Float_t *rgba)
{   return MakeBrikShape(shp,rgba); }

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoTrd2 &shp, const Float_t *rgba)
{   return MakeBrikShape(shp,rgba); }

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoSphere & shp, const Float_t *rgba) {
// Int_t specialCase = -1;
// if (TMath::Abs(TMath::Sin(2*(shp.GetPhi2() - shp.GetPhi2()))) <= 0.01) specialCase = -specialCase;
 // Create TSPHE to simplify the calculations
 TSPHE sphere(shp.GetName() // name
	 , "tmp"                 // title
	 ,  0                    // material
    , shp.GetRmin  () 
	 , shp.GetRmax  ()
	 , shp.GetTheta1() 
    , shp.GetTheta2() 
	 , shp.GetPhi1  () 
	 , shp.GetPhi2  () );
	 return MakeShape(sphere,rgba);
 // return MakeConeShape(shp,rgba,specialCase*(gGeoManager->GetNsegments()+2),shp.GetNz()+1,kSphere);
}

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoGtra &shp, const Float_t *rgba) 
{   return MakeBrikShape(shp,rgba); }

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoArb8 &shp, const Float_t *rgba)
{   return MakeBrikShape(shp,rgba); }
//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoParaboloid & /* shp */, const Float_t * /* rgba */ )
{ return 0; }
//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoTorus &/*shp*/, const Float_t * /* rgba */ )
{ return 0; }
//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoHype &/*shp*/, const Float_t * /* rgba */ )
{ return 0; }
//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoXtru &shp, const Float_t *rgba )
{ 	return MakeXtruShape(shp,rgba,shp.GetNvert(),shp.GetNz());               }

//________________________________________________________________
//
//________________________________________________________________
#define GEOSHAPE(shapeCode) \
    ( shape->IsA() == (_NAME2_(T,shapeCode)::Class() ) ) { t=MakeShape(*(_NAME2_(T,shapeCode *))shape,rgba); } 

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TShape *shape, const Float_t *rgba) {
   TObject3DView *t = 0;
   if (shape) {
           if GEOSHAPE(BRIK)
      else if GEOSHAPE(GTRA)
      else if GEOSHAPE(PARA)
      else if GEOSHAPE(TRAP)
      else if GEOSHAPE(TRD1)
      else if GEOSHAPE(TRD2)
      else if GEOSHAPE(PCON)
      else if GEOSHAPE(PGON)
      else if GEOSHAPE(SPHE)
      else if GEOSHAPE(TUBE)
      else if GEOSHAPE(CONE)
      else if GEOSHAPE(ELTU)
      else if GEOSHAPE(HYPE)
      else if GEOSHAPE(TUBS)
      else if GEOSHAPE(CONS)
      else if GEOSHAPE(CTUB)
      else if GEOSHAPE(XTRU);
   }
   if (!t) t= 0; // new TBRIK("DEFAULT","BRIK","void",10,10,10);
   return t;
}
//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TGeoShape *shape,const Float_t *rgba) {
   TObject3DView *t = 0;
   if (shape) {

      if ( shape->TestShapeBit(TGeoShape::kGeoBad           ) ) { printf(" This shape kGeoBad has not been implemented yet %s\n",shape->GetName()); }
      // if ( shape->TestShapeBit(TGeoShape::kGeoRSeg          ) ) { printf(" This shape kGeoRSeg has not been implemented yet %s\n",shape->GetName()); }
      // if ( shape->TestShapeBit(TGeoShape::kGeoPhiSeg        ) ) { printf(" This shape kGeoPhiSeg has not been implemented yet %s\n",shape->GetName()); }
      // if ( shape->TestShapeBit(TGeoShape::kGeoThetaSeg      ) ) { printf(" This shape kGeoThetaSeg has not been implemented yet %s\n",shape->GetName()); }
      if ( shape->TestShapeBit(TGeoShape::kGeoVisX          ) ) { printf(" This shape kGeoVisX has not been implemented yet %s\n",shape->GetName()); }
      if ( shape->TestShapeBit(TGeoShape::kGeoVisY          ) ) { printf(" This shape kGeoVisY has not been implemented yet %s\n",shape->GetName()); }
      if ( shape->TestShapeBit(TGeoShape::kGeoVisZ          ) ) { printf(" This shape kGeoVisZ has not been implemented yet %s\n",shape->GetName()); }
      if ( shape->TestShapeBit(TGeoShape::kGeoRunTimeShape  ) ) { printf(" This shape kGeoRunTimeShape has not been implemented yet %s\n",shape->GetName()); }
      if ( shape->TestShapeBit(TGeoShape::kGeoInvalidShape  ) ) { printf(" This shape kGeoInvalidShape has not been implemented yet %s\n",shape->GetName()); }
      if ( shape->TestShapeBit(TGeoShape::kGeoComb          ) ) { printf(" This shape kGeoComb has not been implemented yet %s\n",shape->GetName()); }
      // if ( shape->TestShapeBit(TGeoShape::kGeoClosedShape   ) ) { printf(" This shape kGeoClosedShape has not been implemented yet %s\n",shape->GetName()); }
 
           if GEOSHAPE(GeoPara)
      else if GEOSHAPE(GeoBBox)
      else if GEOSHAPE(GeoParaboloid)
      else if GEOSHAPE(GeoTorus)
      else if GEOSHAPE(GeoHype)
      else if GEOSHAPE(GeoXtru)
      else if GEOSHAPE(GeoSphere)
      else if GEOSHAPE(GeoTube)
      else if GEOSHAPE(GeoTubeSeg)
      else if GEOSHAPE(GeoCone)
      else if GEOSHAPE(GeoConeSeg)
      else if GEOSHAPE(GeoPcon)
      else if GEOSHAPE(GeoPgon)
      else if GEOSHAPE(GeoArb8)
      else if GEOSHAPE(GeoEltu)
      else if GEOSHAPE(GeoTrap)
      else if GEOSHAPE(GeoCtub)
      else if GEOSHAPE(GeoTrd1)
      else if GEOSHAPE(GeoTrd2)
      else if GEOSHAPE(GeoGtra);

   }
   // if (!t) t= new TBRIK("DEFAULT","BRIK","void",10,10,10);
   return t;
}

//________________________________________________________________
//
//  Misc 3D ROOT classes
//________________________________________________________________

//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TPolyLine3D   *shape, const Float_t *rgba)
{
   TObject3DView *vObj =0;
   Float_t *point  = shape->GetP();
   Int_t    nPoint = shape->GetLastPoint()+1;
   if (point && nPoint) 
   {

      TShape3DPolygonView view(nPoint,0,rgba);
      Int_t theNumberOfLines = 1;
      TPolygone3DFaceBindingView face(theNumberOfLines);
      face.fType = TPolygone3DView::kLines;
      Coord3D::ucopy(view.fVertex,point,nPoint);
      view.fPolygonsFaceBinding.push_back(face);
      
      view.SetLineColor(shape->GetLineColor()); view.SetLineStyle(shape->GetLineStyle());
      view.SetLineWidth(shape->GetLineWidth()); view.SetFillColor(shape->GetLineColor());

      vObj =  MakeShape(view,rgba);
   }
   return vObj;
}
//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TPolyLineShape  *shape, const Float_t *rgba)
{
   class flags : public TPolyLineShape {
      friend class TPolyLineShape;
      private:
            flags(){;}
            virtual ~flags(){;}
      public:
            inline Bool_t GetLineFlag() { return fLineFlag;}
   };
   TObject3DView *vObj =0;
   TPoints3DABC *points = shape->GetPoints();
   UInt_t   nPoint = points ? points->Size():0;
   if (nPoint) 
   {
      TShape3DPolygonView view(nPoint,0,rgba);
      Int_t theNumberOfLines = 1;
      TPolygone3DFaceBindingView face(theNumberOfLines);
      std::vector<Coord3D> &vec = view.fVertex;
      for (UInt_t i = 0; i  <  nPoint; i++) {
          Float_t xyz[3];
          points->GetXYZ(xyz,i);
          vec.push_back(Coord3D(xyz));
      }
      face.fType = ((flags*)shape)->GetLineFlag() ? 
            TPolygone3DView::kLines : TPolygone3DView::kPoints ;
      // fprintf(stderr," %d Lines %s\n",nPoint, ((flags*)shape)->GetLineFlag() ?   "Dots": "Lines" );
      view.fPolygonsFaceBinding.push_back(face);
#if 0
      view.SetLineColor(shape->GetLineColor());          view.SetLineStyle(shape->GetLineStyle());
      view.SetLineWidth(Width_t(shape->GetLineWidth())); view.SetFillColor(shape->GetLineColor());
#else
      view.SetLineColor(shape->GetColorAttribute());         view.SetLineStyle(shape->GetStyleAttribute());
      view.SetFillColor(shape->GetColorAttribute());
      if ( face.fType == TPolygone3DView::kLines) {
         view.SetLineWidth(Width_t(shape->GetSizeAttribute())); 
      } else {
         view.SetLineWidth(Width_t(4*shape->GetSizeAttribute())); 
      }
#endif
      
      vObj =  MakeShape(view,rgba);
   }
   return vObj;
}
//________________________________________________________________
TObject3DView *TObject3DViewFactory::MakeShape(const TPolyMarker3D *shape, const Float_t *rgba)
{
 //   TShape3DPolygonView view(nVertices,nNormals,rgba);
   TObject3DView *vObj =0;
   Float_t *point  = shape->GetP();
   Int_t    nPoint = shape->GetLastPoint()+1;
   if (point && nPoint) 
   {

      TShape3DPolygonView view(nPoint,0,rgba);
      Int_t theNumberOfLines = 1;
      TPolygone3DFaceBindingView face(theNumberOfLines);
      face.fType = TPolygone3DView::kPoints;
      Coord3D::ucopy(view.fVertex,point,nPoint);
      view.fPolygonsFaceBinding.push_back(face);
      
      view.SetLineColor(shape->GetMarkerColor());          view.SetLineStyle(shape->GetMarkerStyle());
      view.SetLineWidth(Width_t(shape->GetMarkerSize()));  view.SetFillColor(shape->GetMarkerColor());
      
     vObj =  MakeShape(view,rgba);
   }
   return vObj;
}
