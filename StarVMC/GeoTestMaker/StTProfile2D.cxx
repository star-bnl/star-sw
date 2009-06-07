#include "StTProfile2D.h"
#include "TROOT.h"
#include "TVirtualHistPainter.h"

ClassImp(StTProfile2D)
//______________________________________________________________________________
StTProfile2D::StTProfile2D():TProfile2D()
{
 fPainter = new StTHistPainter();
 fPainter->SetHistogram(this);
}
//______________________________________________________________________________
StTProfile2D::StTProfile2D(const char* name, const char* title
                          ,Int_t nbinsx, Double_t xlow, Double_t xup
			  ,Int_t nbinsy, Double_t ylow, Double_t yup
			  ,double,double, Option_t* option):
  TProfile2D(name,title,nbinsx,xlow,xup,nbinsy,ylow,yup,option)
{
 fPainter = new StTHistPainter();
 fPainter->SetHistogram(this);
}
//______________________________________________________________________________
TProfile *StTProfile2D::ProfileX(const char *name, Int_t firstybin, Int_t lastybin, Option_t *option) const
{
   // *-*-*-*-*Project a 2-D histogram into a profile histogram along X*-*-*-*-*-*
   // *-*      ========================================================
   //
   //   The projection is made from the channels along the Y axis
   //   ranging from firstybin to lastybin included.
   //   By default, bins 1 to ny are included
   //   When all bins are included, the number of entries in the projection
   //   is set to the number of entries of the 2-D histogram, otherwise
   //   the number of entries is incremented by 1 for all non empty cells.
   //
   //   if option "d" is specified, the profile is drawn in the current pad.
   //
   //   Using a TCutG object, it is possible to select a sub-range of a 2-D histogram.
   //   One must create a graphical cut (mouse or C++) and specify the name
   //   of the cut between [] in the option.
   //   For example, with a TCutG named "cutg", one can call:
   //      myhist->ProfileX(" ",firstybin,lastybin,"[cutg]");
   //   To invert the cut, it is enough to put a "-" in front of its name:
   //      myhist->ProfileX(" ",firstybin,lastybin,"[-cutg]");
   //   It is possible to apply several cuts ("," means logical AND):
   //      myhist->ProfileX(" ",firstybin,lastybin,[cutg1,cutg2]");
   //
   //   NOTE that if a TProfile named name exists in the current directory or pad,
   //   the histogram is reset and filled again with the current contents of the TH2.
   //   The X axis attributes of the TH2 are copied to the X axis of the profile.
   //
   //   NOTE2 that the default under- / overflow behavior differs from what ProjectionX
   //   does! Profiles take the bin center into account, so here the under- and overflow
   //   bins are ignored by default.

   return DoProfile(true, name, firstybin, lastybin, option);

}

//______________________________________________________________________________
TProfile *StTProfile2D::ProfileY(const char *name, Int_t firstxbin, Int_t lastxbin, Option_t *option) const
{
   // *-*-*-*-*Project a 2-D histogram into a profile histogram along Y*-*-*-*-*-*
   // *-*      ========================================================
   //
   //   The projection is made from the channels along the X axis
   //   ranging from firstxbin to lastxbin included.
   //   By default, bins 1 to nx are included
   //   When all bins are included, the number of entries in the projection
   //   is set to the number of entries of the 2-D histogram, otherwise
   //   the number of entries is incremented by 1 for all non empty cells.
   //
   //   if option "d" is specified, the profile is drawn in the current pad.
   //
   //   Using a TCutG object, it is possible to select a sub-range of a 2-D histogram.
   //   One must create a graphical cut (mouse or C++) and specify the name
   //   of the cut between [] in the option.
   //   For example, with a TCutG named "cutg", one can call:
   //      myhist->ProfileY(" ",firstybin,lastybin,"[cutg]");
   //   To invert the cut, it is enough to put a "-" in front of its name:
   //      myhist->ProfileY(" ",firstybin,lastybin,"[-cutg]");
   //   It is possible to apply several cuts:
   //      myhist->ProfileY(" ",firstybin,lastybin,[cutg1,cutg2]");
   //
   //   NOTE that if a TProfile named name exists in the current directory or pad,
   //   the histogram is reset and filled again with the current contents of the TH2.
   //   The Y axis attributes of the TH2 are copied to the X axis of the profile.
   //
   //   NOTE2 that the default under- / overflow behavior differs from what ProjectionX
   //   does! Profiles take the bin center into account, so here the under- and overflow
   //   bins are ignored by default.

   return DoProfile(false, name, firstxbin, lastxbin, option);
}
//______________________________________________________________________________
TProfile *StTProfile2D::DoProfile(bool onX, const char *name, Int_t firstbin, Int_t lastbin, Option_t *option) const
{
   TString opt = option;

   const TAxis& outAxis = ( onX ? fXaxis : fYaxis );
   const TAxis&  inAxis = ( onX ? fYaxis : fXaxis );
   Int_t outN = outAxis.GetNbins();
   Int_t  inN = inAxis.GetNbins();
   const char *expectedName = ( onX ? "_pfx" : "_pfy" );

   if (firstbin < 0) firstbin = 1;
   if (lastbin  < 0) lastbin  = inN;
   if (lastbin  > inN+1) lastbin  = inN;

   // Create the profile histogram
   char *pname = (char*)name;
   if (name && strcmp(name, expectedName) == 0) {
      Int_t nch = strlen(GetName()) + 5;
      pname = new char[nch];
      sprintf(pname,"%s%s",GetName(),name);
   }
   TProfile *h1=0;
   //check if a profile with identical name exist
   TObject *h1obj = gROOT->FindObject(pname);
   if (h1obj && h1obj->InheritsFrom("TProfile")) {
      h1 = (TProfile*)h1obj;
      // tb.d. check if number of bin is consistent ?
      h1->Reset();
   }

   Int_t ncuts = 0;
   opt.ToLower();  //must be called after MakeCuts

   if (!h1) {
      const TArrayD *bins = outAxis.GetXbins();
      if (bins->fN == 0) {
         h1 = new TProfile(pname,GetTitle(),outN,outAxis.GetXmin(),outAxis.GetXmax(),option);
      } else {
         h1 = new TProfile(pname,GetTitle(),outN,bins->fArray,option);
      }
   }
   if (pname != name)  delete [] pname;

   // Copy attributes
   h1->GetXaxis()->ImportAttributes( &outAxis);
   h1->SetLineColor(this->GetLineColor());
   h1->SetFillColor(this->GetFillColor());
   h1->SetMarkerColor(this->GetMarkerColor());
   h1->SetMarkerStyle(this->GetMarkerStyle());

   //
   // Fill the profile histogram
   Double_t cont;
   for (Int_t outBin =0;outBin<=outN+1;outBin++) {
      for (Int_t inBin=firstbin;inBin<=lastbin;inBin++) {
         Int_t& binx = (onX ? outBin :  inBin );
         Int_t& biny = (onX ?  inBin : outBin );
         if (ncuts) {
            if (!fPainter->IsInside(binx,biny)) continue;
         }
         cont =  GetCellContent(binx,biny);
	 int bin  = biny*(fXaxis.GetNbins()+2) + binx;
         double ent =  fBinEntries.fArray[bin];
         if (ent <=0.) continue;
         h1->Fill(outAxis.GetBinCenter(outBin),cont/ent,ent );
      }
   }
   if ((firstbin <=1 && lastbin >= inN) && !ncuts) h1->SetEntries(fEntries);

   return h1;
}
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
#include "Riostream.h"
#include "TROOT.h"
#include "TClass.h"
#include "TSystem.h"
#include "THistPainter.h"
#include "TH3.h"
#include "TH2.h"
#include "TF2.h"
#include "TF3.h"
#include "TCutG.h"
#include "TMatrixDBase.h"
#include "TMatrixFBase.h"
#include "TVectorD.h"
#include "TVectorF.h"
#include "TPad.h"
#include "TPaveStats.h"
#include "TFrame.h"
#include "TLatex.h"
#include "TLine.h"
#include "TPolyLine.h"
#include "TPoints.h"
#include "TStyle.h"
#include "TGraph.h"
#include "TPie.h"
#include "TGaxis.h"
#include "TColor.h"
#include "TPainter3dAlgorithms.h"
#include "TGraph2DPainter.h"
#include "TGraphDelaunay.h"
#include "TView.h"
#include "TMath.h"
#include "TRandom.h"
#include "TObjArray.h"
#include "TVectorD.h"
#include "Hoption.h"
#include "Hparam.h"
#include "TPluginManager.h"
#include "TPaletteAxis.h"
#include "TCrown.h"
#include "TVirtualPadEditor.h"
#include "TEnv.h"
#include "TPoint.h"

ClassImp(StTHistPainter)

extern Hoption_t Hoption;
extern Hparam_t  Hparam;
//______________________________________________________________________________
void StTHistPainter::PaintColorLevels(Option_t *)
{
   /* Begin_html
   <a href="#HP14">Control function to draw a 2D histogram as a color plot.</a>
   End_html */

   Double_t z, zc, xk, xstep, yk, ystep, xlow, xup, ylow, yup;

   Double_t zmin = fH->GetMinimum();
   Double_t zmax = fH->GetMaximum();
   if (Hoption.Logz) {
      if (zmin > 0) {
         zmin = TMath::Log10(zmin);
         zmax = TMath::Log10(zmax);
      } else {
         return;
      }
   }

   Double_t dz = zmax - zmin;

   if (dz <= 0) return;

   Style_t fillsav   = fH->GetFillStyle();
   Style_t colsav    = fH->GetFillColor();
   fH->SetFillStyle(1001);
   fH->TAttFill::Modify();

   // Initialize the levels on the Z axis
   Int_t ncolors  = gStyle->GetNumberOfColors();
   Int_t ndiv   = fH->GetContour();
   if (ndiv == 0 ) {
      ndiv = gStyle->GetNumberContours();
      fH->SetContour(ndiv);
   }
   Int_t ndivz  = TMath::Abs(ndiv);
   if (fH->TestBit(TH1::kUserContour) == 0) fH->SetContour(ndiv);
   Double_t scale = ndivz/dz;

   Int_t color;
   for (Int_t j=Hparam.yfirst; j<=Hparam.ylast;j++) {
      yk    = fYaxis->GetBinLowEdge(j);
      ystep = fYaxis->GetBinWidth(j);
      if (Hoption.System == kPOLAR && yk<0) yk= 2*TMath::Pi()+yk;
      for (Int_t i=Hparam.xfirst; i<=Hparam.xlast;i++) {
         Int_t bin  = j*(fXaxis->GetNbins()+2) + i;
         xk    = fXaxis->GetBinLowEdge(i);
         xstep = fXaxis->GetBinWidth(i);
         if (!IsInside(xk+0.5*xstep,yk+0.5*ystep)) continue;
         z     = fH->GetBinContent(bin);
//VP     if (z == 0 && (zmin >= 0 || Hoption.Logz)) continue; // don't draw the empty bins for histograms with positive content
         if (z == 0 && (fH->GetBinError(bin) <=0 || Hoption.Logz)) continue; // don't draw the empty bins for histograms with positive content
         if (Hoption.Logz) {
            if (z > 0) z = TMath::Log10(z);
            else       z = zmin;
         }
         if (z < zmin) continue;
         xup  = xk + xstep;
         xlow = xk;
         if (Hoption.Logx) {
            if (xup > 0)  xup  = TMath::Log10(xup);
            else continue;
            if (xlow > 0) xlow = TMath::Log10(xlow);
            else continue;
         }
         yup  = yk + ystep;
         ylow = yk;
         if (Hoption.System != kPOLAR) {
            if (Hoption.Logy) {
               if (yup > 0)  yup  = TMath::Log10(yup);
               else continue;
               if (ylow > 0) ylow = TMath::Log10(ylow);
               else continue;
            }
            if (xup  < gPad->GetUxmin()) continue;
            if (yup  < gPad->GetUymin()) continue;
            if (xlow > gPad->GetUxmax()) continue;
            if (ylow > gPad->GetUymax()) continue;
            if (xlow < gPad->GetUxmin()) xlow = gPad->GetUxmin();
            if (ylow < gPad->GetUymin()) ylow = gPad->GetUymin();
            if (xup  > gPad->GetUxmax()) xup  = gPad->GetUxmax();
            if (yup  > gPad->GetUymax()) yup  = gPad->GetUymax();
         }

         if (fH->TestBit(TH1::kUserContour)) {
            zc = fH->GetContourLevelPad(0);
            if (z < zc) continue;
            color = -1;
            for (Int_t k=0; k<ndiv; k++) {
               zc = fH->GetContourLevelPad(k);
               if (z < zc) {
                  continue;
               } else {
                  color++;
               }
            }
         } else {
            color = Int_t(0.01+(z-zmin)*scale);
         }

         Int_t theColor = Int_t((color+0.99)*Float_t(ncolors)/Float_t(ndivz));
         if (theColor > ncolors-1) theColor = ncolors-1;
         fH->SetFillColor(gStyle->GetColorPalette(theColor));
         fH->TAttFill::Modify();
         if (Hoption.System != kPOLAR) {
            gPad->PaintBox(xlow, ylow, xup, yup);
         } else  {
            TCrown crown(0,0,xlow,xup,ylow*TMath::RadToDeg(),yup*TMath::RadToDeg());
            crown.SetFillColor(gStyle->GetColorPalette(theColor));
            crown.Paint();
         }
      }
   }

   if (Hoption.Zscale) PaintPalette();

   fH->SetFillStyle(fillsav);
   fH->SetFillColor(colsav);
   fH->TAttFill::Modify();

}
