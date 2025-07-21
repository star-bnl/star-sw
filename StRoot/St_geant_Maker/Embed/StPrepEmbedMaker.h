/**
 * @class  StPrepEmbedMaker
 * @brief  Prepares the St_geant_Maker for embedding simulations.
 * @author A. Rose (LBL), Y. Fisyak (BNL), L. Barnby (U. Birmingham)
 * @date   May 2007
 *
 * This maker configures the St_geant_Maker for running simulations embedded
 * into real data events. It ensures that the simulation environment matches
 * the real data by setting parameters such as the primary vertex position,
 * magnetic field, and detector geometry on an event-by-event basis, using
 * information from the corresponding real data event's tags.
 *
 * This maker must be run before St_geant_Maker in the chain to ensure the
 * simulation is correctly configured for each event. It provides options to
 * control particle kinematics (pT, eta, phi), particle type, multiplicity,
 * and event selection criteria based on triggers or vertex properties.
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
  
  virtual Int_t  Init();                      // called once at the beginning of your job
  virtual Int_t  Make();                      // invoked for every event
  virtual Int_t  Finish();
  virtual Int_t  InitRun(const int runnum);
  virtual void   Do(const Char_t *option = "dcut cave x 0.1 10 10 0.03 0.03"); // *MENU 
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StPrepEmbedMaker.h,v 1.9 2014/08/06 11:43:55 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
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
	      const Double_t phihigh, const char* type="FlatPt");
  void SetTemp(const double t);
  void SetTagFile(const Char_t *file) ;
  void SetSkipMode(const Bool_t flag=kTRUE) ;
  void SetSpreadMode(const Bool_t flag=kFALSE) ;
  void SetTrgOpt(const Int_t TrgId); // Set trigger id cut
  void SetZVertexCut(const Double_t vzlow, const Double_t vzhigh); // Set z-vertex cut
  void SetVrCut(const Double_t vr) ; // Set vr = sqrt{vx^2 + vy^2} cut
  void SetVpdVzCut(const Double_t vpdvz) ; // Set |vpdvz-vz| cut
  void SetPVRankCut(const Double_t pvrank) ; // Set cut on P.V. rank > pvrank
  void OpenFzFile() ; /// Switch to enable writing .fz file
  void SetPrimeMode(const Bool_t flag=kFALSE) ; //Switch to prime mode for nucleus (with geantID > 10000) embedding
  void SetVpdVzCutMode(const Bool_t flag=kFALSE) ; //Switch to turn on cut for |VpdVz-Vz|
  void SetPVRankCutMode(const Bool_t flag=kFALSE) ; //Switch to turn on cut for P.V. rank

  void SetRapidityMode(const Bool_t flag=kTRUE) ; //Switch to assigin input kinematic range to rapidity (true) or pseudorapidity(false)

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
  TString mFzFile ; /// .fz file name
  Int_t mEventCounter; /// Number of events
  TFile *mFile; /// 
  TFile *mMoreFile;
  TTree *mTree;
  TTree *mMoreTree;
  Bool_t mSkipMode;
  Bool_t mSpreadMode;
  Bool_t mOpenFzFile; /// Flag to enable/disable writing .fz file (default is false)

  Bool_t mRapidityMode; /// flag to switch between flat in rapdity (true) and flat in pseudo-rapidity (false, default is true).

  Bool_t mPrimeMode;  /// Flag to enable/disable prime mode
  Int_t  mSavePid;
  Bool_t mPrimed;  // 

  Bool_t mVpdVzCutMode;  /// Flag to enable/disable VpdVz cut mode
  Bool_t mPVRankCutMode;  /// Flag to enable/disable PVRank cut mode

  ClassDef(StPrepEmbedMaker,0)    
};
#endif
