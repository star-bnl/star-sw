//*CMZ :          28/04/99  20.53.39  by  Valery Fine(fine@mail.cern.ch)
//*-- Author :    Valery Fine(fine@mail.cern.ch)   24/04/99
 
#include "St_Points3DABC.h"
#include "TVirtualPad.h"
#include "TMath.h"
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_Points3DABC                                                       //
//                                                                      //
// Abstract class to define Arrays of 3D points                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
ClassImp(St_Points3DABC)
 
//______________________________________________________________________________
Int_t St_Points3DABC::Add(Float_t x, Float_t y, Float_t z)    { return AddLast(x,y,z);     }
//______________________________________________________________________________
Int_t St_Points3DABC::AddLast(Float_t x, Float_t y, Float_t z){ return SetNextPoint(x,y,z); }
//______________________________________________________________________________
Int_t St_Points3DABC::DistancetoLine(Int_t px, Int_t py, Float_t xp1, Float_t yp1, Float_t xp2, Float_t yp2 )
{
//*-*-*-*-*-*-*-*-*-*-*Compute distance from point px,py to a line*-*-*-*-*-*
//*-*                  ===========================================
//*-*  Compute the closest distance of approach from point px,py to this line.
//*-*  The distance is computed in pixels units.
//*-*
//*-*  Algorithm:
//*-*
//*-*    A(x1,y1)         P                             B(x2,y2)
//*-*    ------------------------------------------------
//*-*                     I
//*-*                     I
//*-*                     I
//*-*                     I
//*-*                    M(x,y)
//*-*
//*-*  Let us call  a = distance AM     A=a**2
//*-*               b = distance BM     B=b**2
//*-*               c = distance AB     C=c**2
//*-*               d = distance PM     D=d**2
//*-*               u = distance AP     U=u**2
//*-*               v = distance BP     V=v**2     c = u + v
//*-*
//*-*  D = A - U
//*-*  D = B - V  = B -(c-u)**2
//*-*     ==> u = (A -B +C)/2c
//*-*
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
   Float_t xl, xt, yl, yt;
   Float_t x     = px;
   Float_t y     = py;
   Float_t x1    = gPad->XtoAbsPixel(xp1);
   Float_t y1    = gPad->YtoAbsPixel(yp1);
   Float_t x2    = gPad->XtoAbsPixel(xp2);
   Float_t y2    = gPad->YtoAbsPixel(yp2);
   if (x1 < x2) {xl = x1; xt = x2;}
   else         {xl = x2; xt = x1;}
   if (y1 < y2) {yl = y1; yt = y2;}
   else         {yl = y2; yt = y1;}
   if (x < xl-2 || x> xt+2) return 9999;  //following algorithm only valid in the box
   if (y < yl-2 || y> yt+2) return 9999;  //surrounding the line
   Float_t xx1   = x  - x1;
   Float_t xx2   = x  - x2;
   Float_t x1x2  = x1 - x2;
   Float_t yy1   = y  - y1;
   Float_t yy2   = y  - y2;
   Float_t y1y2  = y1 - y2;
   Float_t A     = xx1*xx1   + yy1*yy1;
   Float_t B     = xx2*xx2   + yy2*yy2;
   Float_t C     = x1x2*x1x2 + y1y2*y1y2;
   if (C <= 0)  return 9999;
   Float_t c     = TMath::Sqrt(C);
   Float_t u     = (A - B + C)/(2*c);
   Float_t D     = TMath::Abs(A - u*u);
   if (D < 0)   return 9999;
 
//   return Int_t(TMath::Sqrt(D) - 0.5*float(fLineWidth));
   return Int_t(TMath::Sqrt(D));
}
 
//______________________________________________________________________________
Int_t St_Points3DABC::SetNextPoint(Float_t x, Float_t y, Float_t z)
                           { return SetPoint(GetLastPosition()+1,x,y,z);}
