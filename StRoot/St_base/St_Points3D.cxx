//*CMZ :          29/04/99  16.25.33  by  Valery Fine(fine@mail.cern.ch)
//*-- Author :    Valery Fine(fine@mail.cern.ch)   24/04/99
 
//*KEEP,CopyRight,T=C.
/*************************************************************************
 * Copyright(c) 1995-1998, The ROOT System, All rights reserved.         *
 * Authors: Rene Brun, Nenad Buncic, Valery Fine, Fons Rademakers.       *
 *                                                                       *
 * Permission to use, copy, modify and distribute this software and its  *
 * documentation for non-commercial purposes is hereby granted without   *
 * fee, provided that the above copyright notice appears in all copies   *
 * and that both the copyright notice and this permission notice appear  *
 * in the supporting documentation. The authors make no claims about the *
 * suitability of this software for any purpose.                         *
 * It is provided "as is" without express or implied warranty.           *
 *************************************************************************/
//*KEND.
 
#include <fstream.h>
#include <iostream.h>
 
#include "TClass.h"
#include "St_Points3D.h"
#include "St_PointsArray3D.h"
 
ClassImp(St_Points3D)
 
//______________________________________________________________________________
// St_Points3D is an abstract class of the array of 3-dimensional points.
// It has 4 different constructors.
//
// This class has no implemenatation for Paint, Draw, and SavePrimitive methods
//
//   First one, without any parameters St_Points3D(), we call 'default
// constructor' and it's used in a case that just an initialisation is
// needed (i.e. pointer declaration).
//
//       Example:
//                 St_Points3D *pl1 = new St_Points3D;
//
//
//   Second one is 'normal constructor' with, usually, one parameter
// n (number of points), and it just allocates a space for the points.
//
//       Example:
//                 St_Points3D pl1(150);
//
//
//   Third one allocates a space for the points, and also makes
// initialisation from the given array.
//
//       Example:
//                 St_Points3D pl1(150, pointerToAnArray);
//
//
//   Fourth one is, almost, similar to the constructor above, except
// initialisation is provided with three independent arrays (array of
// x coordinates, y coordinates and z coordinates).
//
//       Example:
//                 St_Points3D pl1(150, xArray, yArray, zArray);
//
 
 
//______________________________________________________________________________
St_Points3D::St_Points3D(St_Points3DABC *points) : fPoints(points)
{
//*-*-*-*-*-*-*-*-*-*-*-*-*3-D PolyLine default constructor*-*-*-*-*-*-*-*-*-*-*
//*-*                      ================================
  DoOwner(kFALSE);
  fPoints = points; 
}
 
 
//______________________________________________________________________________
St_Points3D::St_Points3D(Int_t n, Option_t *option) : fPoints( new St_PointsArray3D(n,option))
{
//*-*-*-*-*-*3-D PolyLine normal constructor without initialisation*-*-*-*-*-*-*
//*-*        ======================================================
//*-*  If n < 0 the default size (2 points) is set
//*-*
   DoOwner();
}
 
//______________________________________________________________________________
St_Points3D::St_Points3D(Int_t n, Float_t *p, Option_t *option) : fPoints(new St_PointsArray3D(n,p,option))
{
//*-*-*-*-*-*-*-*-*-*-*-*-*3-D Point3D normal constructor*-*-*-*-*-*-*-*-*-*-*-*
//*-*                      ===============================
//*-*  If n < 0 the default size (2 points) is set
//*-*
   DoOwner();
}
 
 
//______________________________________________________________________________
St_Points3D::St_Points3D(Int_t n, Float_t *x, Float_t *y, Float_t *z, Option_t *option)
                       : fPoints(new St_PointsArray3D(n,x,y,z,option))
{
//*-*-*-*-*-*-*-*-*-*-*-*-*3-D PolyLine normal constructor*-*-*-*-*-*-*-*-*-*-*-*
//*-*                      ===============================
//*-*  If n < 0 the default size (2 points) is set
//*-*
   DoOwner();
}
 
 
//______________________________________________________________________________
St_Points3D::~St_Points3D()
{
//*-*-*-*-*-*-*-*-*-*-*-*-*3-D PolyLine default destructor*-*-*-*-*-*-*-*-*-*-*-*
//*-*                      ===============================
   Delete();
}
 
 
//______________________________________________________________________________
St_Points3D::St_Points3D(const St_Points3D &point)
{
   ((St_Points3D&)point).Copy(*this);
}
 
 
//______________________________________________________________________________
void St_Points3D::Copy(TObject &obj)
{
//*-*-*-*-*-*-*-*-*-*-*-*-*Copy this St_Points3D to another *-*-*-*-*-*-*-*-*-*-*-*
//*-*                      ==============================
 
   St_Points3DABC::Copy(obj);
   St_Points3D &thatObject = (St_Points3D&)obj;
   thatObject.Delete();
   if (thatObject.IsOwner()) {
      thatObject.fPoints =  new St_Points3D(GetN(),GetP(),GetOption());
     (thatObject.fPoints)->SetLastPosition(GetLastPosition());
   }
   else
     thatObject.fPoints = fPoints;
}
 
//______________________________________________________________________________
void St_Points3D::Delete()
{
  // Delete only own object
  if (fPoints && IsOwner()) delete fPoints;
  fPoints = 0;
}
//______________________________________________________________________________
Bool_t St_Points3D::DoOwner(Bool_t done) { 
  if (done) SetBit(kIsOwner); 
  else ResetBit(kIsOwner);
  return IsOwner();
}
 
//______________________________________________________________________________
void St_Points3D::ExecuteEvent(Int_t event, Int_t px, Int_t py)
{
//*-*-*-*-*-*-*-*-*-*Execute action corresponding to one event*-*-*-*-*-*-*-*-*-*
//*-*                =========================================
  if (fPoints) 
      fPoints->ExecuteEvent(event,px,py);
}
 
//______________________________________________________________________________
void St_Points3D::ls(Option_t *option)
{
//*-*-*-*-*-*-*-*-*-*List this 3-D polyline with its attributes*-*-*-*-*-*-*
//*-*                ==========================================
 
   IndentLevel();
   cout << IsA()->GetName() << " N=" <<GetN()<<" Option="<<option<<endl;
//   IsOwner()?"Owner":"Not owner" << endl;
 
}
//______________________________________________________________________________
void St_Points3D::Print(Option_t *option)
{
//*-*-*-*-*-*-*-*-*-*Dump this 3-D polyline with its attributes*-*-*-*-*-*-*-*-*
//*-*                ==========================================
 
   cout <<"   " << IsA()->GetName() <<" Printing N=" <<GetN()<<" Option="<<option<<endl;
//   IsOwner()?"Owner":"Not owner" << endl;
}

