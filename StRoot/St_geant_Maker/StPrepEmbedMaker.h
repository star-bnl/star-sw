
/*!
 * \class  StPrepEmbedMaker
 * \brief  
 * \author A. Rose LBL, Y. Fisyak BNL, L. Barnby U. Birmingham
 * \date   May 2007
 *
 * $Id: StPrepEmbedMaker.h,v 1.4 2009/07/01 23:21:03 andrewar Exp $
 *
 *
 * -------------------------------------------------------------------------
 * $Log: StPrepEmbedMaker.h,v $
 * Revision 1.4  2009/07/01 23:21:03  andrewar
 * Updated with Strangeness embedding code options, taken from Xianglei's
 * code, Feb 09.
 *
 * Revision 1.3  2008/08/15 15:10:41  lbarnby
 * Flag to skip embedding events without primary vertex with setter (default is to skip)
 *
 * Revision 1.2  2007/08/29 23:00:14  andrewar
 * Added calls for embedding particle parameters, Maker methods
 *
 * Revision 1.1  2007/07/12 20:34:35  fisyak
 * Add StPrepEmbedMaker
 *
 *
 * -------------------------------------------------------------------------
 */

#ifndef StPrepEmbedMaker_hh     
#define StPrepEmbedMaker_hh

#include "StMaker.h"
#include "TGiant3.h"
#include "TString.h"

class StEvent;
class StTrack;
class TFile;
class TGiant3;
class TTree;

class StPrepEmbedMaker : public StMaker {
 public:
  
  StPrepEmbedMaker(const Char_t *name="PrepEmbed");     // constructor
  ~StPrepEmbedMaker();                                 // destructor
  
  Int_t  Init();                      // called once at the beginning of your job
  Int_t  Make();                      // invoked for every event
  Int_t   Finish();
  Int_t  InitRun(int runnum);
  virtual void   Do(const Char_t *option = "dcut cave x 0.1 10 10 0.03 0.03"); // *MENU 
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StPrepEmbedMaker.h,v 1.4 2009/07/01 23:21:03 andrewar Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  void SetPartOpt(Int_t pid, Double_t mult);
  void SetOpt(Double_t ptlow, Double_t pthigh,
	      Double_t etalow, Double_t etahigh, Double_t philow,
	      Double_t phihigh);
  void SetTagFile(const Char_t *file) {mTagFile = file;}
  void SetSkipMode(Bool_t flag=kTRUE) {mSkipMode = flag;}
  void SetSpreadMode(Bool_t flag=kFALSE) {mSpreadMode=flag;}
 private:
  TGiant3 *mGeant3;
  TString mTagFile;
  TString mMoreTagsFile;
  Int_t mEventCounter;
  TFile *mFile;
  TFile *mMoreFile;
  TTree *mTree;
  TTree *mMoreTree;
  Bool_t mSkipMode;
  Bool_t mSpreadMode;
  ClassDef(StPrepEmbedMaker,0)    
};
#endif
