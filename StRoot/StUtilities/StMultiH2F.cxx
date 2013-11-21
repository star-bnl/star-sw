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

StMultiH2F::StMultiH2F() {}

StMultiH2F::StMultiH2F(const char *name,const char *title,Int_t nbinsx,
		       Axis_t xlow,Axis_t xup,Int_t nbinsy,Axis_t ylow,
                       Axis_t yup,Int_t nbinsz) :
  TH3F(name,title,nbinsx,xlow,xup,nbinsy,ylow,yup,nbinsz,-0.5,-0.5+nbinsz) {}

void StMultiH2F::Draw(Option_t *option) {
  // Probably only the "box" and "cont" options are reasonable here

  Int_t x0 = fXaxis.GetFirst();
  Int_t x1 = fXaxis.GetLast();
  Int_t y0 = fYaxis.GetFirst();
  Int_t y1 = fYaxis.GetLast();
  fXaxis.SetRange();
  fYaxis.SetRange();
  Int_t zbins = TMath::Min(GetNbinsZ(),StMultiH2FMaxBins);
  if (zbins == 1) {
    TH2D* temp0 = XYProjection(GetName());
    temp0->SetStats((!TestBit(kNoStats)));
    TAxis* taxisX = temp0->GetXaxis();
    TAxis* taxisY = temp0->GetYaxis();
    fXaxis.Copy(*taxisX);
    fYaxis.Copy(*taxisY);
    taxisX->SetRange(x0,x1);
    taxisY->SetRange(y0,y1);
    fXaxis.SetRange(x0,x1);
    fYaxis.SetRange(y0,y1);
    temp0->Draw(option);
    return;
  }

  // overlay the z bins of the 3d histogram into a 2d histogram
  // using different box colors

  // make a legend
  TLegend *legend = new TLegend(0.80,0.84,0.98,0.98,"Legend","NDC");
  legend->SetFillColor(0);
  legend->SetFillStyle(0);
  legend->SetMargin(0.25);

  Int_t zbin;
  Double_t maxval = -999999.;
  Int_t maxbin = -1;

  // dummy histogram pointer
  TH2D** temp = new TH2D*[zbins];

  TString n0;
  for (zbin=0; zbin<zbins; zbin++) {
    if ((zbin >= 10) || (names[zbin].IsNull())) n0 = GetName();
    else n0 = names[zbin];
    Int_t slice = zbin+1;
    temp[zbin] = XYProjection(n0.Data(),slice);
    temp[zbin]->SetLineColor(60+40*(zbin/(zbins-1)));
    temp[zbin]->SetStats(kFALSE);
    TAxis* taxisX = temp[zbin]->GetXaxis();
    TAxis* taxisY = temp[zbin]->GetYaxis();
    fXaxis.Copy(*taxisX);
    fYaxis.Copy(*taxisY);
    taxisX->SetRange(x0,x1);
    taxisY->SetRange(y0,y1);
  
    Double_t binmax = temp[zbin]->GetMaximum();
    if (binmax > maxval) {
      maxval = binmax;
      maxbin = zbin;
    }
    legend->AddEntry(temp[zbin],n0.Data(),"l");
  }

  temp[maxbin]->SetTitle(GetTitle());
  temp[maxbin]->Draw(option);
  TString sameoption = option; sameoption += "same";
  for (zbin=0; zbin<zbins; zbin++) {
    if (zbin != maxbin) temp[zbin]->Draw(sameoption.Data());
  }

  // Draw statistics for full set if stats are turned on
  if (!TestBit(kNoStats)) {
    temp[0] = XYProjection(GetName());
    temp[0]->Reset();
    temp[0]->SetEntries(GetEntries());
    temp[0]->SetStats(kTRUE);
    temp[0]->Draw(sameoption.Data());
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
  return temp;
}

void StMultiH2F::SavePrimitive(ostream& out, Option_t* option) {
  // Save primitive as a C++ statement(s) on output stream out

  Bool_t nonEqiX = kFALSE;
  Bool_t nonEqiY = kFALSE;
  Int_t i;

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
  if (nonEqiX)
    out << ", yAxis";
  else
    out << "," << GetYaxis()->GetXmin()
        << "," << GetYaxis()->GetXmax();
  out << "," << GetZaxis()->GetNbins() << ");" << endl;

  // save bin contents
  Int_t bin;
  for (bin=0;bin<fNcells;bin++) {
    Double_t bc = GetBinContent(bin);
    if (bc) {
      out<<"   "<<GetName()<<"->SetBinContent("<<bin<<","<<bc<<");"<<endl;
    }
  }

  // save bin errors
  if (fSumw2.fN) {
    for (bin=0;bin<fNcells;bin++) {
      Double_t be = GetBinError(bin);
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

// $Id: StMultiH2F.cxx,v 1.6 2013/11/21 22:22:48 genevb Exp $
// $Log: StMultiH2F.cxx,v $
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
