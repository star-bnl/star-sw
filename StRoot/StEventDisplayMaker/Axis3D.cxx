//*CMZ :          28/11/99  01.06.19  by  Valery Fine(fine@bnl.gov)
//*-- Author :    Valery Fine(fine@bnl.gov)   27/11/99
// $Id: Axis3D.cxx,v 1.5 1999/11/30 20:09:53 fine Exp $ 
#include <iostream.h>
#include <ctype.h>
#include <assert.h>

#include "TAxis3D.h"
#include "Hoption.h"
#include "TPad.h"
#include "TGaxis.h"
#include "TView.h" 
#include "TVirtualPad.h"

//______________________________________________________________________________
//   The 3D axis painter class
//   ==========================
//
//  To add the 3D rulers to any 3D view one has to create
//  an intance of this class and Draw it.
//
//   TAxis3D rulers;
//   rulers.Draw();
//
// The attributes of the created axice are affected by the current style
// (see TStyle class ) and Set... methods of this class
//
 
ClassImp(TAxis3D)

Hoption_t Hopt;
 
//______________________________________________________________________________
TAxis3D::TAxis3D() : TNamed("axis3druler","ruler"){
  InitSet();
}
//______________________________________________________________________________
TAxis3D::TAxis3D(Option_t *option): TNamed("axis3druler","ruler")
{
  InitSet();
}
 
//______________________________________________________________________________
void TAxis3D::InitSet()
{
 
  fAxis[0].SetName("xaxis");
  fAxis[0].SetTitle("x");
  fAxis[1].SetName("yaxis");
  fAxis[1].SetTitle("y");
  fAxis[2].SetName("zaxis");
  fAxis[2].SetTitle("z");
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
    Int_t ndiv, i;
    Float_t x1[3], x2[3], y1[3], y2[3], z1[3], z2[3], av[24]  /*  was [3][8] */;
    char chopax[8];
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
        default:  assert(0);
                  continue;
      };

      // If the axis is too short - skip it
      if ( ( TMath::Abs(ax[0] - ax[1]) + TMath::Abs(ay[0] - ay[1]))  < epsil  ) continue;
   
      if (i != 2 ) {
        if (ax[0] > ax[1]) strcpy(chopax, "SDHV=+");
        else               strcpy(chopax, "SDHV=-");
      }

      if (i==1 && (TMath::Abs(z1[0] - z2[0]) + TMath::Abs(z1[1] - z2[1])) < epsil) 
                            strcpy(chopax, "SDH+=");
//                            strcpy(chopax, "SDH+=N");

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
       enum { kCenterTitle = BIT(12) }; // to be removed with the last version of ROOT
       axis->SetBit(kCenterTitle, fAxis[i].TestBit(kCenterTitle));

       //*-*-    Initialize the number of divisions. If the
       //*-*-    number of divisions is negative, option 'N' is required.
       ndiv = fAxis[i].GetNdivisions();
       if (ndiv < 0) {
         ndiv = -ndiv;
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
//_______________________________________________________________________________________
void TAxis3D::ToggleRulers(TVirtualPad *pad)
{
  // Turn ON / OFF the "Ruler", TAxis3D object attached
  // to the current pad
  TVirtualPad *thisPad=pad;
  if (!thisPad) thisPad = gPad;
  if (thisPad) {
    // Find axis in the current thisPad 
    TList *l = thisPad->GetListOfPrimitives();
    TObject *o = l->FindObject("axis3druler");
    l->Remove(o);
    if (o)  delete o; 
    else {
      TAxis3D *axis = new TAxis3D;
      axis->SetBit(kCanDelete);
      axis->Draw();
    }          
    thisPad->Modified();
    thisPad->Update();
  }
}

//_______________________________________________________________________________________
//
//   Axis3D.cxx history
//_______________________________________________________________________________________

// $Log: Axis3D.cxx,v $
// Revision 1.5  1999/11/30 20:09:53  fine
// new static method to present rulers
//
// Revision 1.4  1999/11/30 03:01:02  fine
// clean ups
//
// Revision 1.3  1999/11/30 01:44:23  fine
// Z axis fixed
//
// Revision 1.2  1999/11/29 19:57:59  fine
// Missing ROOT constant kCenterTitle hard coded. Should be removed later
//
// Revision 1.1  1999/11/29 19:49:56  fine
// ROOT class: TAxis3D. To be moved to ROOT later
//
