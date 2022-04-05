//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  StMultiH2F allows multiple similar TH2F histograms to be            //
//  easily plotted on one graph                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifdef __HP_aCC
#include <Stiostream.h>
#else
#include "Stiostream.h"
#endif
#include "StMultiH2F.h"
#include "TString.h"
#include "TLegend.h"
#include "TPad.h"
#include "TDirectory.h"

ClassImp(StMultiH2F)

StMultiH2F::StMultiH2F() : subHists(0) {}

StMultiH2F::StMultiH2F(const char *name,const char *title,Int_t nbinsx,
		       Axis_t xlow,Axis_t xup,Int_t nbinsy,Axis_t ylow,
                       Axis_t yup,Int_t nbinsz) :
  TH3F(name,title,nbinsx,xlow,xup,nbinsy,ylow,yup,nbinsz,-0.5,-0.5+nbinsz),
  subHists(0) {}

StMultiH2F::~StMultiH2F() {
  if (subHists) {
    int zbins = TMath::Min(GetNbinsZ(),StMultiH2FMaxBins);
    for (int zbin=0; zbin<zbins; zbin++) delete subHists[zbin];
  }
}

void StMultiH2F::Draw(Option_t *option) {
  // Probably only the "box" and "cont" options are reasonable here

  int x0 = fXaxis.GetFirst();
  int x1 = fXaxis.GetLast();
  int y0 = fYaxis.GetFirst();
  int y1 = fYaxis.GetLast();
  fXaxis.SetRange();
  fYaxis.SetRange();
  int zbins = TMath::Min(GetNbinsZ(),StMultiH2FMaxBins);

  // dummy histogram pointer(s)
  if (!subHists) { subHists = new TH2D*[zbins]; memset(subHists,0,zbins*sizeof(TH2D*)); }

  if (zbins == 1) {
    delete subHists[0];
    subHists[0] = XYProjection(GetName());
    subHists[0]->SetStats((!TestBit(kNoStats)));
    TAxis* taxisX = subHists[0]->GetXaxis();
    TAxis* taxisY = subHists[0]->GetYaxis();
    fXaxis.Copy(*taxisX);
    fYaxis.Copy(*taxisY);
    taxisX->SetRange(x0,x1);
    taxisY->SetRange(y0,y1);
    fXaxis.SetRange(x0,x1);
    fYaxis.SetRange(y0,y1);
    subHists[0]->Draw(option);
    return;
  }

  // overlay the z bins of the 3d histogram into a 2d histogram
  // using different box colors

  // make a legend
  TLegend *legend = new TLegend(0.80,0.84,0.98,0.98,"Legend","NDC");
  legend->SetFillColor(0);
  legend->SetFillStyle(0);
  legend->SetMargin(0.25);

  int zbin;
  double maxval = -999999.;
  int maxbin = -1;

  TString n0;
  for (zbin=0; zbin<zbins; zbin++) {
    if ((zbin >= 10) || (names[zbin].IsNull())) n0 = GetName();
    else n0 = names[zbin];
    int slice = zbin+1;
    delete subHists[zbin];
    subHists[zbin] = XYProjection(n0.Data(),slice);
    subHists[zbin]->SetLineColor(60+40*(zbin/(zbins-1)));
    subHists[zbin]->SetStats(kFALSE);
    TAxis* taxisX = subHists[zbin]->GetXaxis();
    TAxis* taxisY = subHists[zbin]->GetYaxis();
    fXaxis.Copy(*taxisX);
    fYaxis.Copy(*taxisY);
    taxisX->SetRange(x0,x1);
    taxisY->SetRange(y0,y1);
  
    double binmax = subHists[zbin]->GetMaximum();
    if (binmax > maxval) {
      maxval = binmax;
      maxbin = zbin;
    }
    legend->AddEntry(subHists[zbin],n0.Data(),"l");
  }

  subHists[maxbin]->SetTitle(GetTitle());
  subHists[maxbin]->Draw(option);
  TString sameoption = option; sameoption += "same";
  for (zbin=0; zbin<zbins; zbin++) {
    if (zbin != maxbin) subHists[zbin]->Draw(sameoption.Data());
  }

  // Draw statistics for full set if stats are turned on
  if (!TestBit(kNoStats)) {
    delete subHists[0];
    subHists[0] = XYProjection(GetName());
    subHists[0]->Reset();
    subHists[0]->SetEntries(GetEntries());
    subHists[0]->SetStats(kTRUE);
    subHists[0]->Draw(sameoption.Data());
    legend->SetX1(0.59);
    legend->SetX2(0.77);
  }

  legend->Draw();

  fXaxis.SetRange(x0,x1);
  fYaxis.SetRange(y0,y1);
}

TH2D* StMultiH2F::XYProjection(const char* name, Int_t zbin) {
  static char buf[256];
  if (zbin<0) sprintf(buf,"%s.",name);
  else sprintf(buf,"%s_%d_%s",GetName(),zbin,name);

  TList* tgList = gDirectory->GetList();
  TH2D* temp = (TH2D*) tgList->FindObject(buf);
  if (temp) tgList->Remove(temp);

  if (zbin<0) fZaxis.SetRange();
  else fZaxis.SetRange(zbin,zbin);
  temp = (TH2D*) Project3D("yx");
  temp->SetName(buf);
  TAttLine::Copy(*temp);
  TAttFill::Copy(*temp);
  TAttMarker::Copy(*temp);
  temp->GetXaxis()->SetRange(fXaxis.GetFirst(),fXaxis.GetLast());
  temp->GetYaxis()->SetRange(fYaxis.GetFirst(),fYaxis.GetLast());
  return temp; // up to the user of this function delete
}

void StMultiH2F::SavePrimitive(ostream& out, Option_t* option) {
  // Save primitive as a C++ statement(s) on output stream out

  bool nonEqiX = kFALSE;
  bool nonEqiY = kFALSE;
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

  // Check if the histogram has equidistant Y bins or not.  If not, we
  // create an array holding the bins.
  if (GetYaxis()->GetXbins()->fN && GetYaxis()->GetXbins()->fArray) {
    nonEqiY = kTRUE;
    out << "   Double_t yAxis[" << GetYaxis()->GetXbins()->fN
        << "] = {";
    for (i = 0; i < GetYaxis()->GetXbins()->fN; i++) {
      if (i != 0) out << ", ";
      out << GetYaxis()->GetXbins()->fArray[i];
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
  if (nonEqiY)
    out << ", yAxis";
  else
    out << "," << GetYaxis()->GetXmin()
        << "," << GetYaxis()->GetXmax();
  out << "," << GetZaxis()->GetNbins() << ");" << endl;

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

  for (bin=0;bin<GetZaxis()->GetNbins();bin++) {
    if (!(names[bin].IsNull()))
      out <<"   "<<GetName()<< "->Rebin(" << bin << ","
          << quote << names[bin] << quote << ");" << endl;
  }

  TH1::SavePrimitiveHelp(out, option);
}

// $Id: StMultiH2F.cxx,v 1.8 2016/05/27 18:02:41 genevb Exp $
// $Log: StMultiH2F.cxx,v $
// Revision 1.8  2016/05/27 18:02:41  genevb
// Garbage collection (Coverity), remove unnecessary ROOT types
//
// Revision 1.7  2015/07/20 18:27:47  genevb
// fix minor bug with SavePrimitive
//
// Revision 1.6  2013/11/21 22:22:48  genevb
// Protect against array out-of-bounds, use inherited axis handles
//
// Revision 1.5  2008/07/10 21:26:59  genevb
// Allow SavePrimitive of fully drawn TPad to work properly
//
// Revision 1.4  2008/07/09 20:52:38  genevb
// Implement SavePrimitive functions
//
// Revision 1.3  2007/04/24 17:45:33  genevb
// Patched for problems with limited axis ranges
//
// Revision 1.2  2007/04/12 22:39:13  genevb
// Remove drawing of underflows
//
// Revision 1.1  2007/03/13 16:22:31  genevb
// Introduce StMultiH2F class
//
//
