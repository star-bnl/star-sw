// $Id: StMultiH1F.cxx,v 1.1 2000/07/26 22:00:27 lansdell Exp $
// $Log: StMultiH1F.cxx,v $
// Revision 1.1  2000/07/26 22:00:27  lansdell
// new multi-hist class for superimposing the x-projections of y-bins (of a TH2F histogram) into one TH1F histogram
//

#include <iostream>
#include "StMultiH1F.h"
#include "TString.h"
#include "TLegend.h"

ClassImp(StMultiH1F)

StMultiH1F::StMultiH1F() {}

StMultiH1F::StMultiH1F(const char *name,const char *title,Int_t nbinsx,
		       Axis_t xlow,Axis_t xup ,Int_t nbinsy,Axis_t ylow,
		       Axis_t yup) :
  TH2F(name,title,nbinsx,xlow,xup,nbinsy,ylow,yup) {}

StMultiH1F::StMultiH1F(const char *name,const char *title,Int_t nbinsx,
		       Double_t *xbins,Int_t nbinsy,Axis_t ylow,Axis_t yup) :
  TH2F(name,title,nbinsx,xbins,nbinsy,ylow,yup) {}

StMultiH1F::StMultiH1F(const char *name,const char *title,Int_t nbinsx,
		       Axis_t xlow,Axis_t xup,Int_t nbinsy,Double_t *ybins) :
  TH2F(name,title,nbinsx,xlow,xup,nbinsy,ybins) {}

StMultiH1F::StMultiH1F(const char *name,const char *title,Int_t nbinsx,
		       Double_t *xbins,Int_t nbinsy,Double_t *ybins) :
  TH2F(name,title,nbinsx,xbins,nbinsy,ybins) {}

StMultiH1F::StMultiH1F(const char *name,const char *title,Int_t nbinsx,
		       Float_t  *xbins ,Int_t nbinsy,Float_t  *ybins) :
  TH2F(name,title,nbinsx,xbins,nbinsy,ybins) {}

void StMultiH1F::Draw(Option_t *option) {

  // overlay the three bins of the 2d histogram into a 1d histogram
  // using different line styles

  // make a legend
  TLegend *legend = new TLegend(0.70,0.80,0.98,0.98);
  legend->SetFillColor(0);
  legend->SetFillStyle(4000);
  legend->SetHeader("Legend");
  legend->SetMargin(0.25);

  // dummy histogram pointer
  TH1F *temp=0;

  int entries = GetNbinsY();
  for (int entry=0; entry<entries; entry++) {
    TString n0 = GetName();
    if (n0=="QaDedxAllSectors" && entry==0) n0 += "_outer";
    else if (n0=="QaDedxAllSectors" && entry==1) n0 += "_inner";
    else if (entry==0) n0 += "_x";
    else if (entry==1) n0 += "_y";
    else if (entry==2) n0 += "_z";
    temp = (TH1F*)ProjectionX(n0.Data(),entry+1,entry+1);
    temp->SetLineStyle(entry+1);
    temp->SetLineWidth(5);
    temp->SetStats(kFALSE);
    // can't use the option argument in Draw() since this is called from
    // StHistUtil::DrawHists(), which defaults 2D histograms to a box plot
    if (entry==0)
      temp->Draw();
    else
      temp->Draw("same");
    legend->AddEntry(temp,n0,"l");
  }
  legend->Draw();
}
