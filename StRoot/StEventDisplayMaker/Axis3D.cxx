//Z,+PATCH,//ROOT/HISTPAINTER.
//Z,+DECK,Axis3D,T=C++..
//*CMZ :          28/11/99  01.06.19  by  Valery Fine(fine@mail.cern.ch)
//*-- Author :    Valery Fine(fine@mail.cern.ch)   27/11/99
// $Id: Axis3D.cxx,v 1.1 1999/11/29 19:49:56 fine Exp $ 
#include <iostream.h>
#include <ctype.h>

#include "TAxis3D.h"
#include "Hoption.h"
#include "TPad.h"
#include "TGaxis.h"
#include "TView.h" 
//______________________________________________________________________________
//   The histogram painter class
//   ===========================
//
 
ClassImp(TAxis3D)

Hoption_t Hopt;
 
//______________________________________________________________________________
TAxis3D::TAxis3D(Option_t *option)
{
 
  fAxis[0].SetName("xaxis");
  fAxis[1].SetName("yaxis");
  fAxis[2].SetName("zaxis");
  fAxis[0].Set(1,0.,1.);
  fAxis[1].Set(1,0.,1.);
  fAxis[2].Set(1,0.,1.);
  UseCurrentStyle(); 
}
 
//______________________________________________________________________________
void TAxis3D::Copy(TObject &obj)
{
//*-*-*-*-*-*-*Copy this histogram structure to newth1*-*-*-*-*-*-*-*-*-*-*-*
//*-*          =======================================
  TNamed::Copy(obj);
  for (Int_t i=0;i<2;i++) fAxis[i].Copy(((TAxis3D&)obj).fAxis[i]);

}
//______________________________________________________________________________
Int_t TAxis3D::DistancetoPrimitive(Int_t px, Int_t py)
{
//*-*-*-*-*-*-*-*-*-*-*Compute distance from point px,py to a line*-*-*-*-*-*
//*-*                  ===========================================
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
 
   return 9999;
}
//______________________________________________________________________________
void TAxis3D::ExecuteEvent(Int_t event, Int_t px, Int_t py)
{
//*-*-*-*-*-*-*-*-*-*-*Execute action corresponding to one event*-*-*-*
//*-*                  =========================================
//*-*  This member function is called when a axis is clicked with the locator
//*-*
//*-*  If Left button clicked on the bin top value, then the content of this bin
//*-*  is modified according to the new position of the mouse when it is released.
//*-*
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
 
}
 
 
//______________________________________________________________________________
Text_t *TAxis3D::GetObjectInfo(Int_t px, Int_t py)
{
//   Redefines TObject::GetObjectInfo.
//   Displays the histogram info (bin number, contents, integral up to bin
//   corresponding to cursor position px,py
//
  return StrDup("axis");
}
 
//______________________________________________________________________________
void TAxis3D::Paint(Option_t *option)
{
 
  //  if (!MakeChopt(option)) return; //check options and fill Hoption structure
  
  TGaxis *axis = new TGaxis();
  PaintLegoAxis(axis, 90);
  delete axis; 
}
 
//______________________________________________________________________________
void TAxis3D::PaintLegoAxis(TGaxis *axis, Float_t ang)
{
//*-*-*-*-*-*-*Draw the axis for legos and surface plots*-*-*-*-*-*-*-*-*-*
//*-*          =========================================
//*-*
 
    static Float_t epsil = 0.001;
 
    Double_t cosa, sina;
    Float_t bmin, bmax;
    Float_t r[24]       /* was [3][8] */;
    Int_t ndiv, i; // ndivx, ndivy, ndivz, i;
    Float_t x1[3], x2[3], y1[3], y2[3], z1[3], z2[3], av[24]  /*  was [3][8] */;
    static char chopax[8]; // , chopay[8], chopaz[8];
    Int_t ix1, ix2, iy1, iy2, iz1, iz2;
    Double_t rad;
 
//*-*-----------------------------------------------------------------------
 
//--    if (Hopt.System != kCARTESIAN) return ;
 
    rad = TMath::ATan(1.) * (float)4. / (float)180.;
    cosa = TMath::Cos(ang*rad);
    sina = TMath::Sin(ang*rad);
 
    TView *view = gPad->GetView();
        if (!view) {
           Error("PaintLegoAxis", "no TView in current pad");
                return;
        }
 
    view->AxisVertex(ang, av, ix1, ix2, iy1, iy2, iz1, iz2);
    for (i = 1; i <= 8; ++i) {
        r[i*3 - 3] = av[i*3 - 3] + av[i*3 - 2]*cosa;
        r[i*3 - 2] = av[i*3 - 2]*sina;
        r[i*3 - 1] = av[i*3 - 1];
    }
 
 
    view->WCtoNDC(&r[ix1*3 - 3], x1);
    view->WCtoNDC(&r[ix2*3 - 3], x2);
    view->WCtoNDC(&r[iy1*3 - 3], y1);
    view->WCtoNDC(&r[iy2*3 - 3], y2);
    view->WCtoNDC(&r[iz1*3 - 3], z1);
    view->WCtoNDC(&r[iz2*3 - 3], z2);
 
    view->SetAxisNDC(x1, x2, y1, y2, z1, z2);
 
    Float_t *rmin = view->GetRmin();
    Float_t *rmax = view->GetRmax();
   
    axis->SetLineWidth(1);
 
    for (i=0;i<3;i++) {
//*-*-          X axis drawing
 
      Float_t ax[2], ay[2];
      Bool_t logAx = kFALSE;
      memset(chopax,0,sizeof(chopax));
      switch (i) {
        case 0 :  ax[0] = x1[0]; ax[1] = x2[0];
                  ay[0] = x1[1]; ay[1] = x2[1];
                  logAx = gPad->GetLogx();
                  break;
        case 1 :  
                  if (TMath::Abs(y1[0] - y2[0]) < epsil)  y2[0] = y1[0];                  
                  ax[0] = y1[0]; ax[1] = y2[0];
                  ay[0] = y1[1]; ay[1] = y2[1];
                  logAx = gPad->GetLogy();
                  break;                
        case 2 :  ax[0] = z1[0]; ax[1] = z2[0];
                  ay[0] = z1[1]; ay[1] = z2[1];
                  strcpy(chopax, "SDH+=");
                  logAx = gPad->GetLogz();
                  break;
        default:
                  continue;
      };
 
       if ( !( TMath::Abs(ax[0] - ax[1]) >= epsil && TMath::Abs(ay[0] - ay[1]) > epsil)) continue;

       if (i < 2) {
         if (ax[0] > ax[1]) strcpy(chopax, "SDHV=+");
         else               strcpy(chopax, "SDHV=-");
       }
       if (i==1 && TMath::Abs(z1[0] - z2[0]) < epsil && TMath::Abs(z1[1] - z2[1]) < epsil) 
                            strcpy(chopax, "SDH+=N");

       //*-*-  Initialize the axis options
       if (logAx) {
          strcat(chopax,"G");
          bmin = TMath::Power(10, rmin[i]);
          bmax = TMath::Power(10, rmax[i]);
       } else {
          bmin = rmin[i];
          bmax = rmax[i];
       }
      
       axis->SetLineColor(fAxis[i].GetAxisColor());
       axis->SetTextFont(fAxis[i].GetLabelFont());
       axis->SetTextColor(fAxis[i].GetLabelColor());
       axis->SetTickSize(fAxis[i].GetTickLength());
       axis->SetLabelFont(fAxis[i].GetLabelFont());
       axis->SetLabelOffset(fAxis[i].GetLabelOffset()+fAxis[i].GetTickLength());
       axis->SetLabelSize(fAxis[i].GetLabelSize());
       axis->SetTitle(fAxis[i].GetTitle());
       axis->SetTitleOffset(fAxis[i].GetTitleOffset());
       axis->SetTitleSize(fAxis[i].GetTitleSize());
       axis->SetBit(TGaxis::kCenterTitle, fAxis[i].TestBit(TGaxis::kCenterTitle));

       //*-*-    Initialize the number of divisions. If the
       //*-*-    number of divisions is negative, option 'N' is required.
       ndiv = fAxis[i].GetNdivisions();
       if (ndiv < 0) {
         ndiv = TMath::Abs(ndiv);
         chopax[6] = 'N';
       }          
      
//*-*-             Option time display is required ?
       if (fAxis[i].GetTimeDisplay()) {
          strcat(chopax,"t");
          if (strlen(fAxis[i].GetTimeFormat()) == 0) {
             axis->SetTimeFormat(fAxis[i].ChooseTimeFormat(bmax-bmin));
          } else {
             axis->SetTimeFormat(fAxis[i].GetTimeFormat());
          }
       }
       axis->SetOption(chopax);
       axis->PaintAxis(ax[0], ay[0], ax[1], ay[1], bmin, bmax, ndiv, chopax);
    }
}
//______________________________________________________________________________
void TAxis3D::SavePrimitive(ofstream &out, Option_t *option)
{
    // Save primitive as a C++ statement(s) on output stream out
 
   char quote = '"';
//   out<<"   "<<endl;
 
   fAxis[0].SaveAttributes(out,GetName(),"->GetXaxis()");
   fAxis[1].SaveAttributes(out,GetName(),"->GetYaxis()");
   fAxis[2].SaveAttributes(out,GetName(),"->GetZaxis()");
 
//   out <<GetName()<<"->Draw("
//      <<quote<<option<<quote<<");"<<endl;
}
 
//______________________________________________________________________________
void TAxis3D::UseCurrentStyle()
{
//*-*-*-*-*-*Replace current attributes by current style*-*-*-*-*
//*-*        ===========================================
 
   fAxis[0].ResetAttAxis("X");
   fAxis[1].ResetAttAxis("Y");
   fAxis[2].ResetAttAxis("Z");
}
 
 
//______________________________________________________________________________
Int_t TAxis3D::AxisChoice( Option_t *axis)
{
   char achoice = toupper(axis[0]);
   if (achoice == 'X') return 0;
   if (achoice == 'Y') return 1;
   if (achoice == 'Z') return 2;
   return -1;
}
 
//______________________________________________________________________________
Int_t TAxis3D::GetNdivisions( Option_t *axis)
{
   Int_t ax = AxisChoice(axis);
   return fAxis[ax].GetNdivisions();
}
 
//______________________________________________________________________________
Color_t TAxis3D::GetAxisColor( Option_t *axis)
{
   Int_t ax = AxisChoice(axis);
   return fAxis[ax].GetAxisColor();
}
 
//______________________________________________________________________________
Color_t TAxis3D::GetLabelColor( Option_t *axis)
{
   Int_t ax = AxisChoice(axis);
   return fAxis[ax].GetLabelColor();
}
 
//______________________________________________________________________________
Style_t TAxis3D::GetLabelFont( Option_t *axis)
{
   Int_t ax = AxisChoice(axis);
   return fAxis[ax].GetLabelFont();
   return 0;
}
 
//______________________________________________________________________________
Float_t TAxis3D::GetLabelOffset( Option_t *axis)
{
   Int_t ax = AxisChoice(axis);
   return fAxis[ax].GetLabelOffset();
}
 
//______________________________________________________________________________
Float_t TAxis3D::GetLabelSize( Option_t *axis)
{
   Int_t ax = AxisChoice(axis);
   return fAxis[ax].GetLabelSize();
}
 
//______________________________________________________________________________
Float_t TAxis3D::GetTickLength( Option_t *axis)
{
   Int_t ax = AxisChoice(axis);
   return fAxis[ax].GetTickLength();
}
 
//______________________________________________________________________________
Float_t TAxis3D::GetTitleOffset( Option_t *axis)
{
   Int_t ax = AxisChoice(axis);
   fAxis[ax].GetTitleOffset();
   return 0;
}
 
//______________________________________________________________________________
void TAxis3D::SetNdivisions(Int_t n, Option_t *axis)
{
   Int_t ax = AxisChoice(axis);
   fAxis[ax].SetNdivisions(n);
}
 
//______________________________________________________________________________
void TAxis3D::SetAxisColor(Color_t color, Option_t *axis)
{
   Int_t ax = AxisChoice(axis);
   fAxis[ax].SetAxisColor(color);
}
 
//______________________________________________________________________________
void TAxis3D::SetAxisRange(Float_t xmin, Float_t xmax, Option_t *axis)
{
   Int_t ax = AxisChoice(axis);
   TAxis *theAxis = &fAxis[ax];
   Int_t bin1 = theAxis->FindBin(xmin);
   Int_t bin2 = theAxis->FindBin(xmax);
   theAxis->SetRange(bin1, bin2);
}
 
//______________________________________________________________________________
void TAxis3D::SetLabelColor(Color_t color, Option_t *axis)
{
   Int_t ax = AxisChoice(axis);
   fAxis[ax].SetLabelColor(color);
}
 
//______________________________________________________________________________
void TAxis3D::SetLabelFont(Style_t font, Option_t *axis)
{
   Int_t ax = AxisChoice(axis);
   fAxis[ax].SetLabelFont(font);
}
 
//______________________________________________________________________________
void TAxis3D::SetLabelOffset(Float_t offset, Option_t *axis)
{
   Int_t ax = AxisChoice(axis);
   fAxis[ax].SetLabelOffset(offset);
}
 
//______________________________________________________________________________
void TAxis3D::SetLabelSize(Float_t size, Option_t *axis)
{
   Int_t ax = AxisChoice(axis);
   fAxis[ax].SetLabelSize(size);
}
 
//______________________________________________________________________________
void TAxis3D::SetTickLength(Float_t length, Option_t *axis)
{
   Int_t ax = AxisChoice(axis);
   fAxis[ax].SetTickLength(length);
}
 
//______________________________________________________________________________
void TAxis3D::SetTitleOffset(Float_t offset, Option_t *axis)
{
   Int_t ax = AxisChoice(axis);
   fAxis[ax].SetTitleOffset(offset);
}

// $Log: Axis3D.cxx,v $
// Revision 1.1  1999/11/29 19:49:56  fine
// ROOT class: TAxis3D. To be moved to ROOT later
//
