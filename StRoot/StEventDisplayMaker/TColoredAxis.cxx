#include "TColoredAxis.h"
#include "TStyle.h"
#include "TVirtualPad.h"

ClassImp(TColoredAxis)

//_____________________________________________________________________________
TColoredAxis::TColoredAxis(): TGaxis(), TAttFill(){ 
   fNColors = 0;fLookup  = 0; fNlookup = 0;  
}

//_____________________________________________________________________________
TColoredAxis::TColoredAxis(Float_t xmin,Float_t ymin,Float_t xmax,Float_t ymax,
               Float_t wmin,Float_t wmax,Int_t ndiv, Option_t *chopt,
               Float_t gridlength, Int_t nColors) :
               TGaxis(xmin,ymin,xmax,ymax,wmin,wmax,ndiv,chopt,gridlength)
{
    fNColors = nColors;
    fLookup  = 0;
    fNlookup = 0;
}
//_____________________________________________________________________________
TColoredAxis::TColoredAxis(Float_t xmin,Float_t ymin,Float_t xmax,Float_t ymax,
                Float_t wmin,Float_t wmax, Double_t *wval, Int_t lookupsize,
                Int_t ndiv, Option_t *chopt, Float_t gridlength,Int_t nColors) :
               TGaxis(xmin,ymin,xmax,ymax,wmin,wmax,ndiv,chopt,gridlength)
{
    // fLookup - the array [nColors+1] to map Axis_t values to color
    fNColors = nColors;
    fLookup  = wval;
    fNlookup = lookupsize;
}

//_____________________________________________________________________________
TColoredAxis::TColoredAxis(Float_t xmin,Float_t ymin,Float_t xmax,Float_t ymax,
               const char *funcname, Int_t ndiv, Option_t *chopt,
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
 
   Double_t wmin = fWmin;
   Double_t wmax = fWmax;
   Int_t   ndiv = fNdiv;
   PaintPalette();
   PaintAxis(fX1,fY1,fX2,fY2,wmin,wmax,ndiv,fChopt.Data(),fGridLength);
}

//_____________________________________________________________________________
void TColoredAxis::PaintPalette()
{
//*-*-*-*-*-*-*-*Paint the color palette on the right side of the axis*-*-*-*-*
//*-*            =====================================================
 
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
   Float_t dUp = ymin;
   Float_t dLow = dUp;
   Int_t j = 1; 
   Int_t c = 0;
   Int_t nextColor = 0;
   Int_t iColor = nextColor;
   Double_t deltaColor = 0;
   while (iColor<ncolors) {
      if (fLookup) {
        while((deltaColor = fLookup[j]-fLookup[0]) < (iColor+1.)/ncolors  )  j++;
        if (fLookup[fNlookup-1]-fLookup[j] <= 1./ncolors) {
          // this is the last color slide 
          deltaColor = 1.;
          j =  fNlookup;
        }
        nextColor = Int_t(deltaColor*ncolors+0.5);
        dy = (j-c)*(ymax-ymin)/fNlookup;
        c = j; j++;
      } else {
        nextColor++;
      }
      SetFillColor(gStyle->GetColorPalette(iColor));
      iColor = nextColor;
      TAttFill::Modify();
      dLow = dUp;
      dUp  = dLow + dy;
      if (dUp == dLow) continue;
      gPad->PaintBox(xmin,dLow,xmax,dUp);
   }   
}
