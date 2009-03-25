#include "StTProfile2D.h"
#include "TROOT.h"
#include "TVirtualHistPainter.h"

ClassImp(StTProfile2D)
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

