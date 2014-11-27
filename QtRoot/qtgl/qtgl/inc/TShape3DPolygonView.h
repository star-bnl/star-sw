// @(#)root/gtgl:$Name:  $:$Id: TShape3DPolygonView.h,v 1.8 2013/08/30 16:00:17 perev Exp $
// Author: Valery Fine      27/04/05

#ifndef ROOT_TShape3DPolygonView
#define ROOT_TShape3DPolygonView

/****************************************************************************
** TShape3DPolygonView
**
** Copyright (C) 2005 by Valeri Fine. Brookhaven National Laboratory. All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#include "Riostream.h"
#include "TAttFill.h"
#include "TAttLine.h"
#include "TMath.h"
#include "TMatrixD.h"
#include <vector>

//_______________________________________________________________________________________
class Coord3D {
public:
   Double_t fX;
   Double_t fY;
   Double_t fZ;
   //_____________________________________________________________
   Coord3D():fX(0),fY(0),fZ(0){}
   //_____________________________________________________________
   Coord3D(Double_t x, Double_t y, Double_t z):fX(x),fY(y),fZ(z){}
   //_____________________________________________________________
   Coord3D(const Double_t *xyz)
   {if (xyz) memcpy(&fX,xyz, 3*sizeof(Double_t)); else memset(&fX,0, 3*sizeof(Double_t));}

   //_____________________________________________________________
   Coord3D(const Float_t *xyz)
   {if (xyz) {fX = xyz[0];fY = xyz[1];fZ = xyz[2];} else memset(&fX,0, 3*sizeof(Double_t));}

   //_____________________________________________________________
   inline Double_t xyz(int i) const { return *(&fX+i); }
   //_____________________________________________________________
   inline Double_t X() const { return fX; }
   //_____________________________________________________________
   inline Double_t x() const { return X();}
   //_____________________________________________________________
   inline Double_t Y() const { return fY; }
   //_____________________________________________________________
   inline Double_t y() const { return Y();}
   //_____________________________________________________________
   inline Double_t Z() const { return fZ; }
   //_____________________________________________________________
   inline Double_t z() const { return Z();}
   //_____________________________________________________________
   inline const Coord3D& Invert(){fX=-fX; fY=-fY; fZ=-fZ; return *this;}
   //_____________________________________________________________
   inline const Coord3D& Normal2Plane(const Coord3D &v1,const Coord3D &v2
      , Coord3D &normal) const
   {
      // return the normal at this point to the plane made with two other points:
      // v1 and v2.
      TMath::Normal2Plane((Double_t *) &fX,(Double_t *)&v1.fX,(Double_t *)&v2.fX,&normal.fX);
      return normal;
   }
   //_____________________________________________________________
   inline Double_t &operator [] (int i) { return *(&fX+i);}
   //_____________________________________________________________
   inline const Double_t &operator [] (int i) const { return *(&fX+i);}
   //_____________________________________________________________
   inline Coord3D &operator += (const Coord3D & v) {
      fX += v.x(); fY += v.y(); fZ += v.z();
      return *this;
   }
   //_____________________________________________________________
   inline Coord3D &operator *= (const TMatrixD & m) {
      Double_t x = m(0,0)*X()+m(0,1)*Y()+m(0,2)*Z();
      Double_t y = m(1,0)*X()+m(1,1)*Y()+m(1,2)*Z();
      Double_t z = m(2,0)*X()+m(2,1)*Y()+m(2,2)*Z();
      fX=x; fY=y; fZ=z;
      return *this;
   }
   template <class T> static inline void ucopy(std::vector<Coord3D> &vec, T *array, UInt_t nCoord)
   { 
      // One need this ucopy when the datatype of the array doesn't match one  of Coord3D, namely Double_t
      if (nCoord>0 && array) {
         if (vec.size() !=  nCoord)
            vec.resize(nCoord);
         for (UInt_t i = 0; i  < nCoord; i++) {
            vec[i].fX = array[3*i+0];
            vec[i].fY = array[3*i+1];
            vec[i].fZ = array[3*i+2];
         }
      }
   }
};
//_______________________________________________________________________________________
class  TPolygone3DView
{
 public:
   enum { kTriangle, kQuade, kQuadeStrip, kPolygon,kLines, kPoints };
   Bool_t  fIsRightHand;       // The clockwise or counterclockwise polygon
   UChar_t fType;              // one of kTriangle, kQuade, kPolygon
   std::vector<Int_t>  fVertexIndices;    
   TPolygone3DView(Int_t nVertice)
      : fIsRightHand(kTRUE),fType(kQuade){fVertexIndices.reserve(nVertice);}
};
//_______________________________________________________________________________________
class TPolygone3DVertexBindingView : public TPolygone3DView
{
 public:
   std::vector<Int_t> fNormalIndices;
   TPolygone3DVertexBindingView(Int_t nVertice, Int_t nNormals)
      : TPolygone3DView(nVertice) {fNormalIndices.reserve(nNormals);}
};

//_______________________________________________________________________________________
class  TPolygone3DFaceBindingView : public TPolygone3DView
{
 public:
   UInt_t  fNormalIndex; // -1 means  the fVertexIndices is an array of the end points if any
   TPolygone3DFaceBindingView(Int_t nVertice)
      : TPolygone3DView(nVertice),fNormalIndex(UInt_t(-1)) {}
};

//_______________________________________________________________________________________
class TShape3DPolygonView : public TAttFill, public TAttLine
{
public:
  Float_t fRgba[4];
  ULong_t fShapeID;  //  User-defined shape id
  std::vector<Coord3D>  fNormals;  //  array of the shape normals
  std::vector<Coord3D>  fVertex;   //  array of the shape vertices
  std::vector<TPolygone3DVertexBindingView> fPolygonsVertexBinding;  // collection of the polygon description with the vertex binding
                                          // one normal per vertex 
                                          // the number of the normals as many as the number of the vertices
  std::vector<TPolygone3DFaceBindingView> fPolygonsFaceBinding;    // collection of the polygon description with the face binding
                                          // one normal per face
  TShape3DPolygonView(): fShapeID(0) {}
  TShape3DPolygonView(Int_t nVertice, Int_t nNormals,const Float_t *rgba) : fShapeID(0) 
  { ReserveSizes(nNormals,nVertice); memcpy(fRgba,rgba,sizeof(fRgba)); }
  void Print();
  inline void ReserveSizes(Int_t nVertice, Int_t nNormals) {fVertex.reserve(nVertice); fNormals.reserve(nNormals); }
};


//_______________________________________________________________________________________
inline void TShape3DPolygonView::Print()
{
   fprintf(stderr," -------------------------------\n");
   fprintf(stderr," The number of normals                = %uld \n", fNormals.size());
   fprintf(stderr," The number of vertices               = %uld \n", fVertex.size());
   fprintf(stderr," The number of face binded polygons   = %uld \n", fPolygonsFaceBinding.size());
   fprintf(stderr," The number of vertex binded polygons = %uld \n", fPolygonsVertexBinding.size());

   UInt_t i; UInt_t j;
   if (fPolygonsFaceBinding.size()) {
      fprintf(stderr," -----------fPolygonsFaceBinding----%uld----------------\n",fPolygonsFaceBinding.size());
      for (i = 0; i < fPolygonsFaceBinding.size(); i++) {
         int nIndex = fPolygonsFaceBinding[i].fNormalIndex;
         fprintf(stderr," -- > %d-th face binded polygon\n", i);
         fprintf(stderr," normal %d : coordinate %f %f %f \n"
            ,nIndex
            ,fNormals[nIndex].fX,fNormals[nIndex].fY,fNormals[nIndex].fZ);

         std::vector<Int_t> &nIndices = fPolygonsFaceBinding[i].fVertexIndices;
         for (j = 0; j<nIndices.size();j++) {
            fprintf(stderr," %d. %d vertex %f %f %f \n", j, nIndices[j]
            ,fVertex[nIndices[j]].fX,fVertex[nIndices[j]].fY,fVertex[nIndices[j]].fZ);
         }
      }
   }
   if (fPolygonsVertexBinding.size()) {
      fprintf(stderr," -----------fPolygonsVertexBinding---%uld -----------------\n",fPolygonsVertexBinding.size());

      for (i = 0; i < fPolygonsVertexBinding.size(); i++) {
         {
            std::vector<Int_t> &nIndices = fPolygonsVertexBinding[i].fNormalIndices;
            for (j = 0; j<nIndices.size();j++) {
               fprintf(stderr," %d. %d normals %f %f %f \n", j, nIndices[j]
               ,fNormals[nIndices[j]].fX,fNormals[nIndices[j]].fY,fNormals[nIndices[j]].fZ);
            }
         }
         {
            std::vector<Int_t> &nIndices = fPolygonsVertexBinding[i].fVertexIndices;
            for (j = 0; j<nIndices.size();j++) {
               fprintf(stderr," %d. %d vertex %f %f %f \n", j, nIndices[j]
               ,fVertex[nIndices[j]].fX,fVertex[nIndices[j]].fY,fVertex[nIndices[j]].fZ);
            }
         }
      }
   }
   fprintf(stderr,"  - - - - - - - - - - - - - - - \n\n");
}

#endif
