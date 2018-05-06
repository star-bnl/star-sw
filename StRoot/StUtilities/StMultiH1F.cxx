//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  StMultiH1F allows multiple similar TH1F histograms to be            //
//  easily plotted on one graph                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifdef __HP_aCC
#include <Stiostream.h>
#else
#include "Stiostream.h"
#endif
#include "StMultiH1F.h"
#include "TString.h"
#include "TLegend.h"
#include "TPad.h"
#include "TDirectory.h"
#include "TMath.h"
ClassImp(StMultiH1F)

StMultiH1F::StMultiH1F() : fMOffset(0.), subHists(0), aHist(0) {}

StMultiH1F::StMultiH1F(const char *name,const char *title,Int_t nbinsx,
		       Axis_t xlow,Axis_t xup ,Int_t nbinsy) :
  TH2F(name,title,nbinsx,xlow,xup,nbinsy,-0.5,-0.5+nbinsy),
  fMOffset(0.), subHists(0), aHist(0) {}

StMultiH1F::StMultiH1F(const char *name,const char *title,Int_t nbinsx,
		       Double_t *xbins,Int_t nbinsy) :
  TH2F(name,title,nbinsx,xbins,nbinsy,-0.5,-0.5+nbinsy),
  fMOffset(0.), subHists(0), aHist(0) {}

StMultiH1F::~StMultiH1F() {
  if (subHists) {
    int ybins = TMath::Min(GetNbinsY(),StMultiH1FMaxBins);
    for (int ybin=0; ybin<ybins; ybin++) delete subHists[ybin];
  }
}

void StMultiH1F::Draw(Option_t *option) {

  int x0 = fXaxis.GetFirst();
  int x1 = fXaxis.GetLast();
  fXaxis.SetRange();
  int ybins = TMath::Min(GetNbinsY(),StMultiH1FMaxBins);

  // dummy histogram pointer(s)
  if (!subHists) { subHists = new TH1F*[ybins]; memset(subHists,0,ybins*sizeof(TH1F*)); }

  if (ybins == 1) {
    delete subHists[0];
    subHists[0] = XProjection(GetName());
    subHists[0]->SetStats((!TestBit(kNoStats)));
    TAxis* taxis = subHists[0]->GetXaxis();
    fXaxis.Copy(*taxis);
    taxis->SetRange(x0,x1);
    fXaxis.SetRange(x0,x1);
    if (fMinimum != -1111)  subHists[0]->SetMinimum(fMinimum);
    if (fMaximum != -1111)  subHists[0]->SetMaximum(fMaximum);
    subHists[0]->Draw();
    return;
  }

  // overlay the y bins of the 2d histogram into a 1d histogram
  // using different line styles

  // make a legend
  TLegend *legend = new TLegend(0.80,0.84,0.98,0.98,"Legend","NDC");
  legend->SetFillColor(0);
  legend->SetFillStyle(0);
  legend->SetMargin(0.25);

  int ybin;
  double maxval = -1e31;
  double minval = 1e31;
  int maxbin = -1;
  int minbin = -1;
  float offset = fMOffset;
  if (fMOffset && gPad->GetLogy()) {
    float max_offset = TMath::Power(
      1.0e10*GetNonZeroMinimum()/GetNonZeroMaximum(),
      1.0/(ybins-1.0));
    if (offset > max_offset) offset = max_offset;
  }

  TString n0;
  for (ybin=0; ybin<ybins; ybin++) {
    if (names[ybin].IsNull()) n0 = GetName();
    else n0 = names[ybin];
    int slice = ybin+1;
    delete subHists[ybin];
    subHists[ybin] = XProjection(n0.Data(),slice);
    subHists[ybin]->SetLineStyle(slice);
    subHists[ybin]->SetStats(kFALSE);
    TAxis* taxis = subHists[ybin]->GetXaxis();
    fXaxis.Copy(*taxis);
    taxis->SetRange(x0,x1);

    if (fMOffset && ybin) {
      subHists[ybin]->SetLineColor(slice);
      if (gPad->GetLogy()) {
        subHists[ybin]->Scale(TMath::Power(offset,ybin));
      } else {
        for (int xbin=0; xbin<GetNbinsX(); xbin++)
          subHists[ybin]->AddBinContent(xbin,offset*ybin);
      }
    }

    double binmax = subHists[ybin]->GetMaximum();
    double binmin = subHists[ybin]->GetMinimum();
    if (binmax > maxval) { maxval = binmax; maxbin = ybin; }
    if (binmin < minval) { minval = binmin; minbin = ybin; }
    legend->AddEntry(subHists[ybin],n0.Data(),"l");
  }

  // can't use the option argument in Draw() since this is called from
  // StHistUtil::DrawHists(), which defaults 2D histograms to a box plot
  if (maxbin == minbin) {
    if (fMinimum != -1111)  subHists[maxbin]->SetMinimum(fMinimum);
    if (fMaximum != -1111)  subHists[maxbin]->SetMaximum(fMaximum);
    subHists[maxbin]->Draw();
  } else {
    delete aHist;
    aHist = new TH1F(*(subHists[maxbin]));
    aHist->SetName(Form("%s_%d",GetName(),ybins+1));
    aHist->SetBinContent(1,maxval);
    aHist->SetBinContent(2,minval);
    aHist->SetMarkerStyle(1); aHist->SetMarkerColor(0);
    if (fMinimum != -1111)  aHist->SetMinimum(fMinimum);
    if (fMaximum != -1111)  aHist->SetMaximum(fMaximum);
    aHist->Draw("p");
    maxbin = -1;
  }
  for (ybin=0; ybin<ybins; ybin++) {
    if (ybin != maxbin) subHists[ybin]->Draw("same");
  }

  // Draw statistics for full set if stats are turned on
  if (!TestBit(kNoStats)) {
    subHists[0] = XProjection(GetName());
    subHists[0]->SetEntries(GetEntries());
    subHists[0]->SetStats(kTRUE);
    subHists[0]->Draw("boxsames");
    legend->SetX1(0.59);
    legend->SetX2(0.77);
  }

  legend->Draw();

  fXaxis.SetRange(x0,x1);
}

TH1F* StMultiH1F::XProjection(const char* name, Int_t ybin) {
  static char buf[256];
  if (ybin<0) sprintf(buf,"%s.",name);
  else sprintf(buf,"%s_%d_%s",GetName(),ybin,name);

  TList* tgList = gDirectory->GetList();
  TH1F* temp = (TH1F*) tgList->FindObject(buf);
  if (temp) tgList->Remove(temp);

  if (ybin<0) temp = (TH1F*) ProjectionX(buf);
  else temp = (TH1F*) ProjectionX(buf,ybin,ybin);
  TAttLine::Copy(*temp);
  TAttFill::Copy(*temp);
  TAttMarker::Copy(*temp);
  return temp; // up to the user of this function to delete
}

void StMultiH1F::SetBarOffset(Float_t offset) {
  if (offset == 0.25) {
    fMOffset = 1.2 * (GetMaximum() - GetMinimum());
    if (!fMOffset) fMOffset = 10.0;
  } else {
    fMOffset = offset;
  }
}

Double_t StMultiH1F::GetNonZeroMinimum() const {
  Double_t value, minimum = GetMinimum();
  if (minimum) return minimum;
  minimum = GetMaximum();
  int bin, binx, biny, binz;
  int xfirst  = fXaxis.GetFirst();
  int xlast   = fXaxis.GetLast();
  int yfirst  = fYaxis.GetFirst();
  int ylast   = TMath::Min(fYaxis.GetLast(),StMultiH1FMaxBins);
  int zfirst  = fZaxis.GetFirst();
  int zlast   = fZaxis.GetLast();
  for (binz=zfirst;binz<=zlast;binz++) {
     for (biny=yfirst;biny<=ylast;biny++) {
        for (binx=xfirst;binx<=xlast;binx++) {
           bin = GetBin(binx,biny,binz);
           value = GetBinContent(bin);
           if (value && value < minimum) minimum = value;
        }
     }
  }
  if (!minimum) minimum = -1.0;
  return minimum;
}

Double_t StMultiH1F::GetNonZeroMaximum() const {
  Double_t value, maximum = GetMaximum();
  if (maximum) return maximum;
  maximum = GetMinimum();
  int bin, binx, biny, binz;
  int xfirst  = fXaxis.GetFirst();
  int xlast   = fXaxis.GetLast();
  int yfirst  = fYaxis.GetFirst();
  int ylast   = TMath::Min(fYaxis.GetLast(),StMultiH1FMaxBins);
  int zfirst  = fZaxis.GetFirst();
  int zlast   = fZaxis.GetLast();
  for (binz=zfirst;binz<=zlast;binz++) {
     for (biny=yfirst;biny<=ylast;biny++) {
        for (binx=xfirst;binx<=xlast;binx++) {
           bin = GetBin(binx,biny,binz);
           value = GetBinContent(bin);
           if (value && value > maximum) maximum = value;
        }
     }
  }
  if (!maximum) maximum = -1.0;
  return maximum;
}

void StMultiH1F::SavePrimitive(ostream& out, Option_t* option) {
  // Save primitive as a C++ statement(s) on output stream out

  bool nonEqiX = kFALSE;
  int i;

  // Check if the histogram has equidistant X bins or not.  If not, we
  // create an array holding the bins.
  if (GetXaxis()->GetXbins()->fN && GetXaxis()->GetXbins()->fArray) {
    nonEqiX = kTRUE;
    out << "   Double_t xAxis[" << GetXaxis()->GetXbins()->fN
        << "] = {";
    for (i = 0; i < GetXaxis()->GetXbins()->fN; i++) {
      if (i != 0) out << ", ";
      out << GetXaxis()->GetXbins()->fArray[i];
    }
    out << "}; " << endl;
  }

  char quote = '"';
  out <<"   "<<endl;
  out <<"   TH1 *" << GetName() << " = new " << ClassName() << "("
      << quote << GetName() << quote << "," << quote << GetTitle() << quote
      << "," << GetXaxis()->GetNbins();
  if (nonEqiX)
    out << ", xAxis";
  else
    out << "," << GetXaxis()->GetXmin()
        << "," << GetXaxis()->GetXmax();
  out << "," << GetYaxis()->GetNbins() << ");" << endl;

  // save bin contents
  int bin;
  for (bin=0;bin<fNcells;bin++) {
    double bc = GetBinContent(bin);
    if (bc) {
      out<<"   "<<GetName()<<"->SetBinContent("<<bin<<","<<bc<<");"<<endl;
    }
  }

  // save bin errors
  if (fSumw2.fN) {
    for (bin=0;bin<fNcells;bin++) {
      double be = GetBinError(bin);
      if (be) {
        out <<"   "<<GetName()<<"->SetBinError("<<bin<<","<<be<<");"<<endl;
      }
    }
  }

  for (bin=0;bin<GetYaxis()->GetNbins();bin++) {
    if (!(names[bin].IsNull()))
      out <<"   "<<GetName()<< "->Rebin(" << bin << ","
          << quote << names[bin] << quote << ");" << endl;
  }
  if (fMOffset != 0)
    out <<"   "<<GetName()<< "->SetBarOffset(" << fMOffset << ");" << endl;

  TH1::SavePrimitiveHelp(out, option);
}


Int_t StMultiH1F::Write(const char *name, Int_t option, Int_t bufsize)
{
  int ybins = TMath::Min(GetNbinsY(),StMultiH1FMaxBins);

  for (int ybin=0; ybin<ybins; ybin++)
  {
    TString projection_name( names[ybin].IsNull() ? GetName() : names[ybin].Data() );
    XProjection(projection_name.Data(), ybin+1)->Write();
  }

  return TH2F::Write();
}

// $Id: StMultiH1F.cxx,v 1.18 2018/05/03 16:04:58 smirnovd Exp $
// $Log: StMultiH1F.cxx,v $
// Revision 1.18  2018/05/03 16:04:58  smirnovd
// Override Write() to save sub histograms in StMultiH1F
//
// Revision 1.17  2016/05/27 18:02:41  genevb
// Garbage collection (Coverity), remove unnecessary ROOT types
//
// Revision 1.16  2015/05/26 15:40:30  genevb
// Handle set min/maxima
//
// Revision 1.15  2013/11/21 22:22:47  genevb
// Protect against array out-of-bounds, use inherited axis handles
//
// Revision 1.14  2008/07/10 21:26:59  genevb
// Allow SavePrimitive of fully drawn TPad to work properly
//
// Revision 1.13  2008/07/09 20:52:38  genevb
// Implement SavePrimitive functions
//
// Revision 1.12  2007/07/12 20:26:03  fisyak
// Add includes for ROOT 5.16
//
// Revision 1.11  2007/04/24 17:45:32  genevb
// Patched for problems with limited axis ranges
//
// Revision 1.10  2007/04/06 20:05:30  genevb
// Allow for lower minima
//
// Revision 1.9  2003/09/02 17:59:20  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.8  2003/01/21 18:33:27  genevb
// Better handling of temporary hists
//
// Revision 1.7  2002/04/23 01:59:16  genevb
// New offset abilities
//
// Revision 1.6  2000/09/15 21:23:36  fisyak
// HP does not have iostream
//
// Revision 1.5  2000/08/28 19:21:05  genevb
// Improved projection code
//
// Revision 1.4  2000/08/28 18:47:50  genevb
// Better handling of 1 y-bin case
//
// Revision 1.3  2000/08/25 22:03:39  genevb
// Fixed entries problem
//
// Revision 1.2  2000/08/25 15:46:42  genevb
// Added stats box, legend names
//
// Revision 1.1  2000/07/26 22:00:27  lansdell
// new multi-hist class for superimposing the x-projections of y-bins (of a TH2F histogram) into one TH1F histogram
//

