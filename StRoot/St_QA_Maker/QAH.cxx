// $Id: QAH.cxx,v 2.2 2001/04/28 22:05:12 genevb Exp $ 
// $Log: QAH.cxx,v $
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
const char* QAH::NameIt(const char* name) {

  return ((QAHistName=preString) += name).Data();

}
//_____________________________________________________________________________
const char* QAH::TitleIt(const char* name) {

  return (((QAHistTitle=preString) += " ") += name).Data();

}
//_____________________________________________________________________________
