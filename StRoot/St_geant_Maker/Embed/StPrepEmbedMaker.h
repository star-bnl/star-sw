/*!
 * \class  StPrepEmbedMaker
 * \brief  
 * \author A. Rose LBL, Y. Fisyak BNL, L. Barnby U. Birmingham
 * \date   May 2007
 *
 * $Id: StPrepEmbedMaker.h,v 1.2 2010/05/26 03:22:52 hmasui Exp $
 *
 *
 * -------------------------------------------------------------------------
 * $Log: StPrepEmbedMaker.h,v $
 * Revision 1.2  2010/05/26 03:22:52  hmasui
 * Set rapidity +/-10 in gkine/phasespace for spectrum option in order to avoid acceptance cuts
 *
 * Revision 1.1  2010/04/05 20:18:55  jeromel
 * Moved from one level up
 *
 * Revision 1.7  2010/04/02 20:14:50  didenko
 * StPrepEmbedMaker for Hiroshi
 *
 * Revision 1.6  2010/02/09 01:08:38  andrewar
 * Added default value for embedding mode for backward compatibility.
 *
 * Revision 1.5  2010/02/05 23:01:19  andrewar
 * Update with spectra embedding mode.
 *
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
class StEvtHddr ;
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
  Int_t  Finish();
  Int_t  InitRun(const int runnum);
  virtual void   Do(const Char_t *option = "dcut cave x 0.1 10 10 0.03 0.03"); // *MENU 
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StPrepEmbedMaker.h,v 1.2 2010/05/26 03:22:52 hmasui Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  void SetPartOpt(const Int_t pid, const Double_t mult); /// Set geantid(pid) and multiplicity

  /// Set (ptlow, pthigh), (etalow, etahigh), (philow, phihigh), and type
  /// type can be
  ///   flatpt           Flat (pt, y) by 'phasespace'
  ///   flatp            Flat (p, y) by gkine
  ///   strange          Sloped momentum by input temperature (default T is 300 MeV)
  ///
  ///  NOTE: type is case insensitive, FlatPt, flatpt, FLATPT work
  void SetOpt(const Double_t ptlow, const Double_t pthigh,
	      const Double_t etalow, const Double_t etahigh, const Double_t philow,
	      const Double_t phihigh, const TString type="FlatPt");
  void SetTemp(const double t);
  void SetTagFile(const Char_t *file) ;
  void SetSkipMode(const Bool_t flag=kTRUE) ;
  void SetSpreadMode(const Bool_t flag=kFALSE) ;
  void SetTrgOpt(const Int_t TrgId); // Set trigger id cut
  void SetZVertexCut(const Double_t vzlow, const Double_t vzhigh); // Set z-vertex cut

  /// Do phasespace command from input pt, y
  ///   Force to make rapidity distribute within +/- mRapidityMaximumCut for 'spectrum' option
  void phasespace(const Int_t mult) ;

  /// Do gkine command from input p, eta, phi
  ///   Force to make rapidity distribute within +/- mRapidityMaximumCut for 'spectrum' option
  void gkine(const Int_t mult, const Double_t vzmin, const Double_t vzmax) ;

 private:
  static const Double_t mRapidityMaximumCut ; /// Maximum rapidity cut for 'spectrum' option

  /// Get multiplicity used in the embedding
  ///  if input mult = 1  ---> generate 1 particle / event
  ///  if input mult < 1  ---> generate nprimarytracks * mult particle / event (typically 5%)
  Int_t getMultiplicity(const StEvtHddr& EvtHddr, const Int_t nprimarytracks) const ;

  TGiant3 *mGeant3;
  TString mTagFile; /// Tags file name
  TString mMoreTagsFile; /// More tags file name for vertex error (will be removed in future)
  Int_t mEventCounter; /// Number of events
  TFile *mFile; /// 
  TFile *mMoreFile;
  TTree *mTree;
  TTree *mMoreTree;
  Bool_t mSkipMode;
  Bool_t mSpreadMode;

  ClassDef(StPrepEmbedMaker,0)    
};
#endif
