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

#include "St_Points3D.h"

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
St_PolyLine3D::St_PolyLine3D(TPoints3DABC *points)
{
//*-*-*-*-*-*-*-*-*-*-*-*-*3-D PolyLine default constructor*-*-*-*-*-*-*-*-*-*-*
//*-*                      ================================
  m_Points = new St_Points3D(points);
}
 
//______________________________________________________________________________
St_PolyLine3D::St_PolyLine3D(Int_t n, Option_t *option) : St_PolyLineShape(new St_Points3D(n,option),option)
{
//*-*-*-*-*-*3-D PolyLine normal constructor without initialisation*-*-*-*-*-*-*
//*-*        ======================================================
//*-*  If n < 0 the default size (2 points) is set
//*-*
//*-*  'normal constructor' with, usually, one parameter
//*-*   n (number of points), and it just allocates a space for the points.
}
 
//______________________________________________________________________________
St_PolyLine3D::St_PolyLine3D(Int_t n, Float_t *p, Option_t *option): St_PolyLineShape(new St_Points3D(n,p,option),option)
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
: St_PolyLineShape( new St_Points3D(n<2?2:n,x,y,z,option),option)
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
  SafeDelete(m_Points)
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
   m_Points->Copy(obj);
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

   if (!m_Points) return dist; 

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
   for (i=0;i<m_Points->Size()-1;i++) {
      Float_t xyz[6];
      m_Points->GetXYZ(xyz,i,2);
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
   out<<"pline3D = new St_PolyLine3D("<<m_Points->GetN()<<","<<quote<<GetOption()<<quote<<");"<<endl;
 
   // SaveLineAttributes(out,"pline3D",1,1,1);
 
   if (size > 0) {
    for (Int_t i=0;i<size;i++) {
       out<<"   pline3D->SetPoint("<<i<<","<<m_Points->GetX(i)<<","<<m_Points->GetY(i)<<","<<m_Points->GetZ(i)<<");"<<endl;
    }
   }
   out<<"   pline3D->Draw();"<<endl;
}
 
//______________________________________________________________________________
void St_PolyLine3D::SetPolyLine(Int_t n, Float_t *p, Option_t *option)
{
//*-*-*-*-*-*-*-*-*-*-*Set new values for this 3-D polyline*-*-*-*-*-*-*-*-*-*-*
//*-*                  ====================================
   if (m_Points) m_Points->SetPoints(n,p,option);
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

