
/*!
 * \class  StPrepEmbedMaker
 * \brief  
 * \author A. Rose LBL, Y. Fisyak BNL, L. Barnby U. Birmingham
 * \date   May 2007
 *
 * $Id: StPrepEmbedMaker.h,v 1.2 2007/08/29 23:00:14 andrewar Exp $
 *
 *
 * -------------------------------------------------------------------------
 * $Log: StPrepEmbedMaker.h,v $
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
    static const char cvs[]="Tag $Name:  $ $Id: StPrepEmbedMaker.h,v 1.2 2007/08/29 23:00:14 andrewar Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  void SetPartOpt(Int_t pid, Double_t mult);
  void SetOpt(Double_t ptlow, Double_t pthigh,
	      Double_t etalow, Double_t etahigh, Double_t philow,
	      Double_t phihigh);
  void SetTagFile(const Char_t *file) {mTagFile = file;}
 private:
  TGiant3 *mGeant3;
  TString mTagFile;
  Int_t mEventCounter;
  TFile *mFile;
  TTree *mTree;
  ClassDef(StPrepEmbedMaker,0)    
};
#endif
