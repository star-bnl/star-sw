//*CMZ :          29/04/99  16.26.07  by  Valery Fine(fine@mail.cern.ch)
//*-- Author :    Valery Fine     17/08/95
 
#include <fstream.h>
#include <iostream.h>
 
//*KEEP,TROOT.
#include "TROOT.h"
//*KEEP,TClass.
#include "TClass.h"
//*KEEP,St_PolyLine3D.
#include "St_PolyLine3D.h"
//*KEEP,TVirtualPad.
#include "TVirtualPad.h"
//*KEEP,TPostScript.
#include "TPostScript.h"
//*KEEP,TGXW.
#include "TGXW.h"
//*KEEP,TPoint.
#include "TPoint.h"
//*KEEP,TGLKernelABC,T=C++.
#include "TGLKernelABC.h"
//*KEEP,TView.
#include "TView.h"
//*KEEP,TPadView3D,T=C++.
#include "TPadView3D.h"
//*KEND.
 
ClassImp(St_PolyLine3D)
 
//______________________________________________________________________________
// PolyLine3D is a 3-dimensional polyline. It has 5 different constructors.
//
//   First one, without any parameters St_PolyLine3D(), we call 'default
// constructor' and it's used in a case that just an initialisation is
// needed (i.e. pointer declaration).
//
//       Example:
//                 St_PolyLine3D *pl1 = new St_PolyLine3D;
//
//
//   Second one is 'normal constructor' with, usually, one parameter
// n (number of points), and it just allocates a space for the points.
//
//       Example:
//                 St_PolyLine3D pl1(150);
//
//
//   Third one allocates a space for the points, and also makes
// initialisation from the given array.
//
//       Example:
//                 St_PolyLine3D pl1(150, pointerToAnArray);
//
//
//   Fourth one is, almost, similar to the constructor above, except
// initialisation is provided with three independent arrays (array of
// x coordinates, y coordinates and z coordinates).
//
//       Example:
//                 St_PolyLine3D pl1(150, xArray, yArray, zArray);
//
 
 
//______________________________________________________________________________
St_PolyLine3D::St_PolyLine3D(TPoints3DABC *points) : St_Points3D(points)
{
//*-*-*-*-*-*-*-*-*-*-*-*-*3-D PolyLine default constructor*-*-*-*-*-*-*-*-*-*-*
//*-*                      ================================
}
 
//______________________________________________________________________________
St_PolyLine3D::St_PolyLine3D(Int_t n, Option_t *option): St_Points3D(n,option)
{
//*-*-*-*-*-*3-D PolyLine normal constructor without initialisation*-*-*-*-*-*-*
//*-*        ======================================================
//*-*  If n < 0 the default size (2 points) is set
//*-*
//*-*  'normal constructor' with, usually, one parameter
//*-*   n (number of points), and it just allocates a space for the points.
}
 
//______________________________________________________________________________
St_PolyLine3D::St_PolyLine3D(Int_t n, Float_t *p, Option_t *option):St_Points3D(n,p,option)
{
//*-*-*-*-*-*-*-*-*-*-*-*-*3-D PolyLine normal constructor*-*-*-*-*-*-*-*-*-*-*-*
//*-*                      ===============================
//*-*  If n < 0 the default size (2 points) is set
//*-*
//*-*  one allocates a space for the points, and also makes
//*-*  initialisation from the given array.
}
 
//______________________________________________________________________________
St_PolyLine3D::St_PolyLine3D(Int_t n, Float_t *x, Float_t *y, Float_t *z, Option_t *option)
:St_Points3D(n<2?2:n,x,y,z,option)
{
//*-*-*-*-*-*-*-*-*-*-*-*-*3-D PolyLine normal constructor*-*-*-*-*-*-*-*-*-*-*-*
//*-*                      ===============================
//*-*  almost, similar to the constructor above, except
//*-*  initialisation is provided with three independent arrays 
//*-*  (array of x coordinates, y coordinates and z coordinates).
}
 
//______________________________________________________________________________
St_PolyLine3D::~St_PolyLine3D()
{
//*-*-*-*-*-*-*-*-*-*-*-*-*3-D PolyLine default destructor*-*-*-*-*-*-*-*-*-*-*-*
//*-*                      ===============================
}
 
//______________________________________________________________________________
St_PolyLine3D::St_PolyLine3D(const St_PolyLine3D &polyline)
{
   ((St_PolyLine3D&)polyline).Copy(*this);
} 
//______________________________________________________________________________
void St_PolyLine3D::Copy(TObject &obj)
{
//*-*-*-*-*-*-*-*-*-*-*-*-*Copy this polyline to polyline*-*-*-*-*-*-*-*-*-*-*-*
//*-*                      ==============================
   St_Points3D::Copy(obj);
   TAttLine::Copy(((St_PolyLine3D&)obj));
}
  
//______________________________________________________________________________
Int_t St_PolyLine3D::DistancetoPrimitive(Int_t px, Int_t py)
{
//*-*-*-*-*-*-*-*Compute distance from point px,py to a 3-D polyline*-*-*-*-*-*-*
//*-*            ===================================================
//*-*
//*-*  Compute the closest distance of approach from point px,py to each segment
//*-*  of the polyline.
//*-*  Returns when the distance found is below DistanceMaximum.
//*-*  The distance is computed in pixels units.
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
 
   const Int_t inaxis = 7;
   Int_t dist = 9999;

   if (!fPoints) return dist; 

   Int_t puxmin = gPad->XtoAbsPixel(gPad->GetUxmin());
   Int_t puymin = gPad->YtoAbsPixel(gPad->GetUymin());
   Int_t puxmax = gPad->XtoAbsPixel(gPad->GetUxmax());
   Int_t puymax = gPad->YtoAbsPixel(gPad->GetUymax());
 
//*-*- return if point is not in the user area
   if (px < puxmin - inaxis) return dist;
   if (py > puymin + inaxis) return dist;
   if (px > puxmax + inaxis) return dist;
   if (py < puymax - inaxis) return dist;
 
   TView *view = gPad->GetView();
   if (!view) return dist;
 
   Int_t i, dsegment;
   Float_t x1,y1,x2,y2, xndc[3];
   for (i=0;i<Size()-1;i++) {
      Float_t xyz[6];
      fPoints->GetXYZ(xyz,i,2);
      view->WCtoNDC(xyz, xndc);
      x1 = xndc[0];
      y1 = xndc[1];
      view->WCtoNDC(&xyz[3], xndc);
      x2 = xndc[0];
      y2 = xndc[1];
      dsegment =  TPoints3DABC::DistancetoLine(px,py,
                                  gPad->XtoAbsPixel(x1),
                                  gPad->XtoAbsPixel(y1),
                                  gPad->XtoAbsPixel(x2),
                                  gPad->XtoAbsPixel(y2),
                                  GetSizeAttribute()
                                 );
      if (dsegment < dist) dist = dsegment;
   }
   return dist;
}
 
//______________________________________________________________________________
void St_PolyLine3D::Draw(Option_t *option)
{
//*-*-*-*-*-*-*-*Draw this 3-D polyline with its current attributes*-*-*-*-*-*-*
//*-*            ================================================== 
   AppendPad(option);
}
 
//______________________________________________________________________________
void St_PolyLine3D::DrawPolyLine(Int_t n, Float_t *p, Option_t *option)
{
//*-*-*-*-*-*-*-*-*Draw this 3-D polyline with new coordinates*-*-*-*-*-*-*-*-*-*
//*-*              ============================================
 
   St_PolyLine3D *newpolyline = new St_PolyLine3D(Size(),p,GetOption());
   TAttLine::Copy(*newpolyline);
   newpolyline->SetBit(kCanDelete);
   newpolyline->AppendPad(option); 
}
 
//______________________________________________________________________________
Color_t St_PolyLine3D::GetColorAttribute() const
{
  return ((St_PolyLine3D *)this)->GetLineColor();
//  return fAttributes ? fAttributes->GetLineColor():0;
}
//______________________________________________________________________________
Width_t St_PolyLine3D::GetSizeAttribute()  const
{
  return ((St_PolyLine3D *)this)->GetLineWidth();
//   return  fAttributes ? fAttributes->GetLineWidth():0;
}
//______________________________________________________________________________
Style_t St_PolyLine3D::GetStyleAttribute()  const
{
  return ((St_PolyLine3D *)this)->GetLineStyle();
//   return  fAttributes ? fAttributes->GetLineStyle():0;
}
//______________________________________________________________________________
void St_PolyLine3D::Paint(Option_t *option)
{
//*-*-*-*-*-*-*-*-*Paint this 3-D polyline with its current attributes*-*-*-*-*
//*-*              ===================================================
//*-*
//*-* Option could be 'x3d', and it means that output
//*-* will be performed by X3D package.
//*-*
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
 
    if (!fPoints) return;
    //*-* Check whether there is some 3D view class for this TPad
    TPadView3D *view3D = gPad->GetView3D();
    if (view3D) {
        view3D->SetLineAttr(GetColorAttribute(), GetSizeAttribute());
        view3D->PaintPoints3D((const TPoints3DABC *)this, "L");
    }
 
    //*-* Check if option is 'x3d'.      NOTE: This is a simple checking
    //                                         but since there is no other
    //                                         options yet, this works fine.
    Int_t size =Size();
    if ((*option != 'x') && (*option != 'X')) {
        PaintPoints(size);
    }
#ifndef WIN32
    else {
        X3DBuffer *buff = new X3DBuffer;
        if (!buff) return;
        buff->numPoints = size;
        buff->numSegs   = size-1;
        buff->numPolys  = 0;        //NOTE: Because of different structure, our
        buff->polys     = NULL;     //      St_PolyLine3D can't use polygons
        St_Points3D x3dPoints(size);
        buff->points    = fPoints->GetXYZ(x3dPoints.GetP(),0,size);
 
//        Int_t c = (((fAttributes?fAttributes->GetColorAttribute():0) % 8) - 1) * 4;     // Basic colors: 0, 1, ... 8
        Int_t c = ((GetColorAttribute() % 8) - 1) * 4;     // Basic colors: 0, 1, ... 8
        if (c < 0) c = 0;
 
    //*-* Allocate memory for segments *-*
        buff->segs = new Int_t[buff->numSegs*3];
        if (buff->segs) {
            for (Int_t i = 0; i < buff->numSegs; i++) {
                buff->segs[3*i  ] = c;
                buff->segs[3*i+1] = i;
                buff->segs[3*i+2] = i+1;
            }
        }
 
 
        if (buff && buff->points && buff->segs) //If everything seems to be OK ...
            FillX3DBuffer(buff);
        else {                            // ... something very bad was happened
            gSize3D.numPoints -= buff->numPoints;
            gSize3D.numSegs   -= buff->numSegs;
            gSize3D.numPolys  -= buff->numPolys;
        }
 
        if (buff->segs)     delete [] buff->segs;
        if (buff->polys)    delete [] buff->polys;
        if (buff)           delete    buff;
    }
#endif
}
 
//______________________________________________________________________________
void  St_PolyLine3D::PaintPoints(Int_t n, Float_t *, Option_t *)
{
//*-*-*-*-*-*-*-*-*Draw this 3-D polyline with new coordinates*-*-*-*-*-*-*-*-*-*
//*-*              ===========================================
   if (n < 2) return;
 
   TAttLine::Modify();  //Change line attributes only if necessary
 
//*-*- Loop on each individual line
 
   for (Int_t i=1;i<n;i++) {
      Float_t xyz[6];
      fPoints->GetXYZ(xyz,i-1,2);
      gPad->PaintLine3D(xyz, &xyz[3]);
   }
}
 
//______________________________________________________________________________
void St_PolyLine3D::PaintPolyLine(Int_t n, Float_t *p, Option_t *option)
{
//*-*-*-*-*-*-*-*-*Draw this 3-D polyline with new coordinates*-*-*-*-*-*-*-*-*-*
//*-*              ===========================================
   PaintPoints(n,p,option);
}
 
//______________________________________________________________________________
void St_PolyLine3D::SavePrimitive(ofstream &out, Option_t *)
{
    // Save primitive as a C++ statement(s) on output stream out
 
   char quote = '"';
   out<<"   "<<endl;
   if (gROOT->ClassSaved(St_PolyLine3D::Class())) {
       out<<"   ";
   } else {
       out<<"   " << IsA()->GetName()<<" *";
   }
   Int_t size=Size();
   out<<"pline3D = new St_PolyLine3D("<<GetN()<<","<<quote<<GetOption()<<quote<<");"<<endl;
 
   // SaveLineAttributes(out,"pline3D",1,1,1);
 
   if (size > 0) {
    for (Int_t i=0;i<size;i++) {
       out<<"   pline3D->SetPoint("<<i<<","<<GetX(i)<<","<<GetY(i)<<","<<GetZ(i)<<");"<<endl;
    }
   }
   out<<"   pline3D->Draw();"<<endl;
}
 
 
//______________________________________________________________________________
Color_t St_PolyLine3D::SetColorAttribute(Color_t color)
{
  // SetColorAttribute(Color_t color) - set new color for this line
  // Returns:
  //          previous value of the color
  //
  Color_t c = GetColorAttribute();
  SetLineColor(color);
  return c;
}
//______________________________________________________________________________
Width_t St_PolyLine3D::SetSizeAttribute(Width_t  size)
{
  // SetSizeAttribute(Width_t  size) - set new width for this line
  // Returns:
  //          previous value of the cline width
  //
  Width_t s = 0;
#if 1
  s = GetSizeAttribute();
  SetLineWidth(size);
#else
  if (fAttributes) {
    s = fAttributes->GetSizeAttribute();
 //   fAttributes->SetLineWidth(size);
  }
#endif
  return s;
}
//______________________________________________________________________________
Style_t St_PolyLine3D::SetStyleAttribute(Style_t style)
{
  // SetStyleAttribute(Style_t style) - set new style for this line
  // Returns:
  //          previous value of the line style
  //
  Style_t s = 0;
#if 1
   s = GetStyleAttribute();
   SetLineStyle(style);
#else
  Style_t s = 0;
  if (fAttributes) {
    s = fAttributes->GetStyleAttribute();
 //   fAttributes->SetLineStyle(style);
  }
#endif
  return s;
}
 
//______________________________________________________________________________
void St_PolyLine3D::SetPolyLine(Int_t n, Float_t *p, Option_t *option)
{
//*-*-*-*-*-*-*-*-*-*-*Set new values for this 3-D polyline*-*-*-*-*-*-*-*-*-*-*
//*-*                  ====================================
   SetPoints(n,p,option);
}
 
//______________________________________________________________________________
void St_PolyLine3D::Sizeof3D() const
{
//*-*-*-*-*-*Return total X3D size of this shape with its attributes*-*-*-*-*-*-*
//*-*        =======================================================
 
    if (fPoints) {
      gSize3D.numPoints += Size();
      gSize3D.numSegs   += Size()-1;
      gSize3D.numPolys  += 0;
    }
 
}
 
//_______________________________________________________________________
void St_PolyLine3D::Streamer(TBuffer &b)
{
//*-*-*-*-*-*-*-*-*Stream a class object*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*              =========================================
#if 0
   if (b.IsReading()) {
      Version_t v = b.ReadVersion();
      if (v<2){
        // Old version of this object
        TObject::Streamer(b);
 //       TAttLine::Streamer(b);
        b >> fN;
        if (fN) {
           fP = new Float_t[3*fN];
           b.ReadFastArray(fP,3*fN);
        }
        fOption.Streamer(b);
        fLastPoint = fN;
      }
      else {
        St_Points3D::Streamer(b);
        TAttLine::Streamer(b);
      }
   } else {
      b.WriteVersion(St_PolyLine3D::IsA());
      St_Points3D::Streamer(b);
      TAttLine::Streamer(b);
   }
#endif
}

//_______________________________________________________________________
Int_t   St_PolyLine3D::GetAttributeI(const Char_t *attribName) const
{ return 0;}
//_______________________________________________________________________
Float_t St_PolyLine3D::GetAttributeF(const Char_t *attribName) const
{ return 0;}
//_______________________________________________________________________
Double_t St_PolyLine3D::GetAttributeD(const Char_t *attribName) const
{ return 0;}
//_______________________________________________________________________
Int_t  St_PolyLine3D::SetAttribute(const Char_t *attribName,Int_t    attrib)
{ return 0;}
//_______________________________________________________________________
Float_t St_PolyLine3D::SetAttribute(const Char_t *attribName,Float_t  attrib)
{ return 0;}
//_______________________________________________________________________
Double_t St_PolyLine3D::SetAttribute(const Char_t *attribName,Double_t attrib)
{ return 0;}
//_______________________________________________________________________
Int_t   St_PolyLine3D::GetNumberOfAttributes() const
{ return 3;}
//_______________________________________________________________________
Int_t   St_PolyLine3D::SetNumberOfAttributes(Int_t n)
{ return 0;}

