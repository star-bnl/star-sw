// $Id: QAH.h,v 2.2 2001/04/28 22:05:12 genevb Exp $ 
// $Log: QAH.h,v $
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

#ifndef STAR_StQAH
#define STAR_StQAH

#include "TString.h"
#include "TH1.h"
#include "TH2.h"
class StMaker;

class QAH {

// ************************ Public Functions *****************************
 public:

  QAH() {}
  virtual       ~QAH() {}
  static StMaker* maker;        // maker to which histograms belong
  static TString preString;     // string to prepend to names/titles

  // methods for 1d-hists
  static TH1F* H1F(const Text_t* name, const Text_t* title,
              Int_t nbinsx, Axis_t xlow, Axis_t xup);
  static TH1F* H1F(const Text_t* name, const Text_t* title,
              Int_t nbinsx, const Double_t* xbins);

  // methods for 2d-hists
  static TH2F* H2F(const Text_t* name, const Text_t* title,
              Int_t nbinsx, Axis_t xlow, Axis_t xup,
              Int_t nbinsy, Axis_t ylow, Axis_t yup);
  static TH2F* H2F(const Text_t* name, const Text_t* title,
              Int_t nbinsx, const Double_t* xbins,
              Int_t nbinsy, Axis_t ylow, Axis_t yup);

  // methods for multi 1d-hists
  static TH2F* MH1F(const Text_t* name, const Text_t* title,
              Int_t nbinsx, Axis_t xlow, Axis_t xup, Int_t nbinsy);


// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: QAH.h,v 2.2 2001/04/28 22:05:12 genevb Exp $ built "__DATE__" "__TIME__ ; return cvs;}


 protected:
  static TString QAHistName;    // character string for each hist name
  static TString QAHistTitle;   // character string for each hist title
  static const char* NameIt(const char* name); // method for naming histograms
  static const char* TitleIt(const char* name);// method for titling histograms

  ClassDef(QAH,0)
};


#endif
    







