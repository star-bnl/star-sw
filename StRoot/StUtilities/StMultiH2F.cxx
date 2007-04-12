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

  TAxis* axisX = GetXaxis();
  TAxis* axisY = GetYaxis();
  Int_t zbins = GetNbinsZ();
  if (zbins == 1) {
    TH2D* temp0 = XYProjection(GetName());
    temp0->SetStats((!TestBit(kNoStats)));
    axisX->Copy(*(temp0->GetXaxis()));
    axisY->Copy(*(temp0->GetYaxis()));
    temp0->Draw(option);
    return;
  }

  // overlay the z bins of the 3d histogram into a 2d histogram
  // using different box colors

  // make a legend
  TLegend *legend = new TLegend(0.80,0.84,0.98,0.98,"Legend","NDC");
  legend->SetFillColor(0);
  legend->SetFillStyle(4000);
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
    axisX->Copy(*(temp[zbin]->GetXaxis()));
    axisY->Copy(*(temp[zbin]->GetYaxis()));

    Double_t binmax = temp[zbin]->GetMaximum();
    if (binmax > maxval) {
      maxval = binmax;
      maxbin = zbin;
    }
    legend->AddEntry(temp[zbin],n0.Data(),"l");
  }

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
}

TH2D* StMultiH2F::XYProjection(const char* name, Int_t zbin) {
  static char buf[256];
  if (zbin<0) sprintf(buf,"%s.",name);
  else sprintf(buf,"%s.%d.%s",GetName(),zbin,name);

  TList* tgList = gDirectory->GetList();
  TH2D* temp = (TH2D*) tgList->FindObject(buf);
  if (temp) tgList->Remove(temp);

  if (zbin<0) GetZaxis()->SetRange();
  else GetZaxis()->SetRange(zbin,zbin);
  temp = (TH2D*) Project3D("yx");
  temp->SetName(buf);
  TAttLine::Copy(*temp);
  TAttFill::Copy(*temp);
  TAttMarker::Copy(*temp);
  return temp;
}

// $Id: StMultiH2F.cxx,v 1.2 2007/04/12 22:39:13 genevb Exp $
// $Log: StMultiH2F.cxx,v $
// Revision 1.2  2007/04/12 22:39:13  genevb
// Remove drawing of underflows
//
// Revision 1.1  2007/03/13 16:22:31  genevb
// Introduce StMultiH2F class
//
//
