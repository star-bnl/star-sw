#include "TColoredAxis.h"
#include "TStyle.h"
#include "TVirtualPad.h"

ClassImp(TColoredAxis)

//_____________________________________________________________________________
TColoredAxis::TColoredAxis(): TGaxis(), TAttFill(){ fNColors = 0; }

//_____________________________________________________________________________
TColoredAxis::TColoredAxis(Float_t xmin,Float_t ymin,Float_t xmax,Float_t ymax,
               Float_t wmin,Float_t wmax,Int_t ndiv, Option_t *chopt,
               Float_t gridlength, Int_t nColors) :
               TGaxis(xmin,ymin,xmax,ymax,wmin,wmax,ndiv,chopt,gridlength)
{
    fNColors = nColors;
}
//_____________________________________________________________________________
TColoredAxis::TColoredAxis(Float_t xmin,Float_t ymin,Float_t xmax,Float_t ymax,
               const char *funcname, Int_t ndiv=510, Option_t *chopt,
               Float_t gridlength,Int_t nColors) :
               TGaxis(xmin,ymin,xmax,ymax,funcname,ndiv,chopt,gridlength)
{
    fNColors = nColors;
}
//______________________________________________________________________________
void TColoredAxis::Paint(Option_t *)
{
//*-*-*-*-*-*-*-*-*-*-*Draw this axis with its current attributes*-*-*-*-*-*-*
//*-*                  ==========================================
 
   Float_t wmin = fWmin;
   Float_t wmax = fWmax;
   Int_t   ndiv = fNdiv;
   PaintPalette();
   PaintAxis(fX1,fY1,fX2,fY2,wmin,wmax,ndiv,fChopt.Data(),fGridLength);
}

//_____________________________________________________________________________
void TColoredAxis::PaintPalette()
{
//*-*-*-*-*-*-*-*Paint the color palette on the right side of the pad*-*-*-*-*
//*-*            ====================================================
 
   Float_t xup  = GetX1();
   Float_t x2   = GetX2();
   Float_t ymin = GetY1();
   Float_t ymax = GetY2();
   Float_t xr   = 0.05*(x2 - xup);
   Float_t xmin = xup +0.1*xr;
   Float_t xmax = xmin + GetTickSize();

//   if (xmax > x2) xmax = x2-0.01*xr;
   Int_t ncolors = fNColors;
   if (!ncolors) ncolors  = gStyle->GetNumberOfColors();
   Float_t dy = (ymax-ymin)/ncolors;
   for (Int_t i=0;i<ncolors;i++) {
      SetFillColor(gStyle->GetColorPalette(i));
      TAttFill::Modify();
      gPad->PaintBox(xmin,ymin+i*dy,xmax,ymin+(i+1)*dy);
   }   
}
