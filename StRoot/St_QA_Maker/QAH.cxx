// $Id: QAH.cxx,v 2.4 2007/03/13 18:44:07 genevb Exp $ 
// $Log: QAH.cxx,v $
// Revision 2.4  2007/03/13 18:44:07  genevb
// Added StMultiH2F support
//
// Revision 2.3  2004/12/13 15:52:36  genevb
// Numerous updates: PMD, primtrk, FPD, QAShift lists
//
// Revision 2.2  2001/04/28 22:05:12  genevb
// Added EMC histograms
//
// Revision 2.1  2000/08/25 16:04:09  genevb
// Introduction of files
//
//
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  QAH class for instantiating QA Histograms                            //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

#include "StMultiH1F.h"
#include "StMultiH2F.h"
#include "QAH.h"
#include "StMaker.h"

StMaker* QAH::maker = 0;
TString QAH::preString = "";
TString QAH::QAHistName = "";
TString QAH::QAHistTitle = "";

ClassImp(QAH)
//_____________________________________________________________________________
TH1F* QAH::H1F(const Text_t* name, const Text_t* title,
   Int_t nbinsx, Axis_t xlow, Axis_t xup) {

  TH1F* hist = new
    TH1F(NameIt(name),TitleIt(title),nbinsx,xlow,xup);
  if (maker) maker->AddHist(hist);
  return hist;

}
//_____________________________________________________________________________
TH1F* QAH::H1F(const Text_t* name, const Text_t* title,
   Int_t nbinsx, const Double_t* xbins) {

  TH1F* hist = new
    TH1F(NameIt(name),TitleIt(title),nbinsx,xbins);
  if (maker) maker->AddHist(hist);
  return hist;

}
//_____________________________________________________________________________
TH2F* QAH::H2F(const Text_t* name, const Text_t* title,
   Int_t nbinsx, Axis_t xlow, Axis_t xup,
   Int_t nbinsy, Axis_t ylow, Axis_t yup) {

  TH2F* hist = new
    TH2F(NameIt(name),TitleIt(title),nbinsx,xlow,xup,nbinsy,ylow,yup);
  if (maker) maker->AddHist(hist);
  return hist;

}
//_____________________________________________________________________________
TH2F* QAH::H2F(const Text_t* name, const Text_t* title,
   Int_t nbinsx, const Double_t* xbins,
   Int_t nbinsy, Axis_t ylow, Axis_t yup) {

  TH2F* hist = new
    TH2F(NameIt(name),TitleIt(title),nbinsx,xbins,nbinsy,ylow,yup);
  if (maker) maker->AddHist(hist);
  return hist;

}
//_____________________________________________________________________________
TH2F* QAH::MH1F(const Text_t* name, const Text_t* title,
   Int_t nbinsx, Axis_t xlow, Axis_t xup, Int_t nbinsy) {

  TH2F* hist = (TH2F*) new
    StMultiH1F(NameIt(name),TitleIt(title),nbinsx,xlow,xup,nbinsy);
  if (maker) maker->AddHist(hist);
  return hist;

}
//_____________________________________________________________________________
void QAH::MMH1F(TH2F** histp, Int_t nhist, const Text_t* name, const Text_t* title,
	   Int_t nbinsx, Axis_t xlow, Axis_t xup, Int_t nbinsy, Int_t first) {

  for (Int_t i=0; i<nhist; i++) {
    Int_t j = first + (i * nbinsy);
    Int_t k = j + nbinsy - 1;
    histp[i] = QAH::MH1F(Form(name,i),Form(title,j,k),nbinsx,xlow,xup,nbinsy);
    for (Int_t l=0; l<nbinsy; l++) histp[i]->Rebin(l,Form("%d",j+l));
    histp[i]->SetStats(kFALSE);
  }

}
//_____________________________________________________________________________
TH3F* QAH::MH2F(const Text_t* name, const Text_t* title,
   Int_t nbinsx, Axis_t xlow, Axis_t xup, Int_t nbinsy, Axis_t ylow, Axis_t yup,
   Int_t nbinsz) {

  TH3F* hist = (TH3F*) new
    StMultiH2F(NameIt(name),TitleIt(title),nbinsx,xlow,xup,nbinsy,ylow,yup,nbinsz);
  if (maker) maker->AddHist(hist);
  return hist;

}
//_____________________________________________________________________________
const char* QAH::NameIt(const char* name) {

  return ((QAHistName=preString) += name).Data();

}
//_____________________________________________________________________________
const char* QAH::TitleIt(const char* name) {

  return (((QAHistTitle=preString) += " ") += name).Data();

}
//_____________________________________________________________________________
