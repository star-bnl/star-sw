// $Id: StEEmcSlowMaker.h,v 2.10 2014/08/06 11:43:04 jeromel Exp $

#ifndef STAR_StEEmcSlowMaker
#define STAR_StEEmcSlowMaker

/*!
 *                                                                     
 * \class  StEEmcSlowMaker
 * \author H. Spinka, J. Balewski, J. Webb, P. Nord
 * \date   12/13/04
 * \brief  Slow simulator for EEMC
 *
 * \section WEBPAGES Links to development/documentation web pages
 *
 * <ol>
 * <li><a href="http://web.mit.edu/rhic-spin/public/eemc-bnl/software/slowSimu/">Slow simulator version 1.0</a>
 * <li><a href="http://web.mit.edu/rhic-spin/public/eemc-bnl/software/slowSimu2/">Slow simulator version 2.0</a>
 * </ol> 
 *
 * \section VERSION_1_0 versions 1.0 to 1.4
 *
 * StEEmcSlowMaker is the slow simulator for the EEMC.  At present,
 * it adds poisson statistics to the simulated response of the
 * SMD, pre- and postshower detector chain.  It is also capable of
 * adding realistic pedestal offsets (and widths) based on database
 * information.
 * 
 * Geant energy deposit was converted to an ADC response by the
 * StEEmcFastMaker.  This is inverted using gains stored with the 
 * "sim" flavor in the database, and thre resulting energy deposit
 * is converted to number of mips.  This is converted to the mean
 * number of photoelectrons produced based on QA measurements at
 * ANL (currently a fixed mean of 2.0 pe per mip for SMD, and 
 * 3.9 pe per mip pre/post).  Poisson statistics are applied, and
 * the results smeared with the single pe resolution.
 *
 * Physics parameters are configurable through various "set" methods
 * below.  Defaults should be good for most applications.
 *
 * By default, the ADC values stored in the muDst are overwritten
 * with the values computed in this maker.  i.e. makers in the chain
 * which follow this maker will see more realistic SMD and pre/postshower
 * response than those which come before it in the chain... i.e. unless
 * you know what you're doing, put this before your anlaysis maker!
 *
 * Present status: work ongoing to add more realistic light yield, 1 p.e. 
 * resolution, etc.
 *
 * For the time being, towers remain a straight energy deposit --> adc
 * coversion.  See StEEmcFastMaker.
 *
 * \section VERSION_2_0 version 2.0
 *
 * Starting with version 2.0, improvements have been made to the
 * EEMC simulation:
 *
 * -# A fix has been applied which masks out towers whenever a "fail" bit is set in the database.  Previously this would only take effect if pedestal offsets were added to the tower.
 * -# Pre/post and SMD simulation now use StEmcHit::energy() and StMuEmcHit::getEnergy() to access energy deposited in tiles/strips.
 * -# Corrections to the tower ADC values based on the energy deposited in the pre- and postshower layers are now made.  See below for discussion.
 *
 * The pre- and postshower layers of the endcap differ in construction 
 * from the other layers in the calorimeter stack.  Two wavelength shifting
 * fibers are used to collect the light in each of these layers, routed
 * to the PMT tubes for the tower containing the pre/postshower tile, 
 * and to seperate MAPMT tubes for the preshower and postshower readouts.
 * To correct for nonlinearities introduced by this construction, the
 * preshower layers are 5mm thick (as opposed to 4mm for layers 3-24)
 * and made of brighter scintillator.  
 *
 * The difference in geometry is accounted for in the geant model, but 
 * the brightness difference is not.  Measurements of the mean number
 * photoelectrons per MIP (<npe>/MIP) show that the preshower layers
 * yield 1.86 times as much light per MIP compared to layers 3-23.
 * The postshower layer yields only 0.94 times as much light as layers
 * 3-23.
 *
 * The fast simulator simulates the tower response in the following way.
 * First it sums the GEANT energy deposited in the scintillator in all 24 
 * layers 
 *
 * E = E_pre1 + E_pre2 + E_layer3 + ... + E_layer23 + E_post
 *
 * It then divides by the sampling fraction and multiplies by the ideal
 * gain to obtain the ADC response of the tower.  The ideal gain is set
 * such that a ET=60 GeV photon corresponds to ADC=4096.
 *
 * ADC = E * gain / sampling fraction = E * gain / 0.048
 *
 * But because the preshower (postshower) layers yield more (less)
 * light per unit energy deposit, they are under (over) represented
 * in the energy sum, and thus the ADC response.
 *
 * To correct for this, the slow simulator calculated a "light yield
 * weighted energy deposit"
 *
 * E' = f * E_pre1 + g * E_pre2 + ... + h * E_post
 *
 * with weights f,g,h for the preshower-1, preshower-2 and postshower
 * layers.  Substituting in the energy deposited in all layers, we 
 * can write E' as
 *
 * E' = E + (f-1) * E_pre1 + (g-1) * E_pre2 + (h-1) * E_post
 *
 * where f = g = 1.68 * (4/5), and h = 0.94 * (4/5) are the measured relative 
 * <npe>/MIP for 4mm of preshower 1, preshower 2 and postshower, 
 * relative to the <npe>/MIP for layers 3-23.  The factors of (4/5) are 
 * present because the measured ratios include the increase in brightness
 * due to the increased thickness of pre1,pre2 and post.  This increase
 * is already accounted for by the GEANT model, and hence should be 
 * factored out of the brightness correction.
 *
 * \section VERSION_2_10 version 2.10
 *
 * -# A flag was added to set specific defaults for use in BFC production.
 * -# Default behavior in MuDst analysis chain was changed to _disable_ the tower
 * slow simulator since slow simulator will be run in BFC and energy deposits
 * are not stored in the MuDst for towers.  It is possible to 'redo' the slow
 * simulator in an analysis chain using the event.root files from the simulation
 * -# Pedestal smearing is truncated at N sigma (default is N=3)
 * -# Set thicknesses of pre-shower layers to 4.75 to match fixed geometry
 * 
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif

class TObjArray;
class StEEmcDb;
class StMuEmcCollection;
class StEmcCollection;

#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
#include "StEEmcUtil/EEfeeRaw/EEdims.h"   
#include "StEEmcUtil/database/EEmcDbItem.h"                                                                                            

class StEEmcSlowMaker : public StMaker {

private: 
  // parameters, fixed :                                                                                                                                     
  Float_t mMip2ene; // conversion from mips to  energy in GeV                                                                                                   
  Float_t mSig1pe; // width of the single photoelectron peak (in p.e.)                                                                                         
  Float_t mMip2pe[MaxSmdStrips]; // conversion from mip to p.e. from ANL cosmic ray measurements for SMD strips                                                                                      
  Float_t mPmip2ene[3]; // as above for pre- and post-shower elements                                                                                              
  Float_t mPmip2pe; // as above for pre- and post-shower elements                                                                                               

  Float_t avgNumPePerMip(Int_t stripID); // avg # p.e. per mip                                                                      

  enum Source_t {kMuDst, kStEvent};
  Source_t mSource;

  StEEmcDb *mEeDb;

  Int_t mNInpEve; // private event counter

  enum {maxHist = 32};
  TH1 *mHist[maxHist]; // some global (test) histograms  
  void InitHisto();

  /// Process endcap tower and overwrite ADC values with
  /// new values computed by the slow simulator.
  Int_t MakeTower(StMuEmcCollection *emc);

  /// Process endcap preshower and overwrite ADC values with
  /// new values computed by the slow simulator
  Int_t MakePrePost(StMuEmcCollection *emc);

  /// Process endcap smd strips [UV] and overwrite ADC values 
  /// with new values computed by the slow simulator
  Int_t MakeSMD(StMuEmcCollection *emc);

  /// Process endcap tower and overwrite ADC values with
  /// new values computed by the slow simulator (StEvent version)
  Int_t MakeTower(StEmcCollection* emc);

  /// Process endcap preshower and overwrite ADC values with
  /// new values computed by the slow simulator (StEvent version)
  Int_t MakePrePost(StEmcCollection* emc);

  /// Process endcap smd strips [UV] and overwrite ADC values 
  /// with new values computed by the slow simulator (StEvent version)
  Int_t MakeSMD(StEmcCollection* emc);

  /// Check that ped and sigPed > 0
  Bool_t checkDBped(const EEmcDbItem *x);

  /// Get pedestal smearing from gaussian distribution truncated at N*sigma
  Float_t getPedSmear(Float_t sigPed);

  /// Zero out all ADC
  void setZeroAdc(StMuEmcCollection *emc);

  /// Zero out all ADC (StEvent version)
  void setZeroAdc(StEmcCollection* emc);
  
  Bool_t mEnableSMD;
  Bool_t mEnablePrePost;
  Bool_t mEnableTower;

  // Offset by pedestal (default true)           
  Bool_t mAddPed;
  // Smear the pedestals (default true)         
  Bool_t mSmearPed;
  // Drop bad channels (default false)
  Bool_t mDropBad;
  // Overwrite muDst values(default true)
  Bool_t mOverwrite;

  Bool_t mIsEmbeddingMode;  
  // set separate defaults for use in BFC      
  Bool_t mIsBFC;                                

  enum {kPre1=0, kPre2, kPost, kNumberPrepost};

  Float_t mRelativeLightYield[kNumberPrepost]; /* <N p.e.>/MIP for pre1, pre2 and post   */
  Float_t mSamplingFraction;                   /* sampling fraction from the fast simu   */
  Float_t mSamplingFractionUser;               /* user-specified sampling fraction       */
  Float_t mTowerGains[kEEmcNumSectors];        /* tower gains from the fast simu         */
  Float_t mPrepostGains;                       /* pre/post ..                            */
  Float_t mSmdGains;                           /* smd ..                                 */
  Int_t   mMaxAdc;                             /* max ADC in fast simu                   */
  Bool_t  mDoLightYield;                       /* set false to disable tower pre/post relative light yield correction */
  Float_t mTowerGainFact[kEEmcNumSectors][kEEmcNumSubSectors][kEEmcNumEtas];
  Float_t mSmdGainFact[kEEmcNumSectors][kEEmcNumSmdUVs][kEEmcNumStrips];

  Int_t mTruncatePedSmear; /// Nsigma where ped smearing is truncated                           

  // Left unimplemented
  StEEmcSlowMaker(const StEEmcSlowMaker &);
  StEEmcSlowMaker &operator=(const StEEmcSlowMaker &);
  
public: 

  /// Class constructor
  StEEmcSlowMaker(const Char_t *name = "EEmcSlowSim", const Char_t *muDstMakerName = 0);
  /// Class destructor
  virtual ~StEEmcSlowMaker();

  /// Initialization
  virtual Int_t Init();
  /// Processes a single event
  virtual Int_t Make();
  /// Sets all switches required to perform embedding
  void setEmbeddingMode(Bool_t e = true) {mIsEmbeddingMode = e;}

  /// Disables slow simulator for the towers.  ADC values stored in the
  /// MuDst/StEvent will reflect the values determined by the fast simulator.
  void disableTower() {mEnableTower = false;}
  /// Disables slow simulator for the preshower and postshwoer detectors.
  void disablePrePost() {mEnablePrePost = false;}
  /// Disables slow simulator for the SMD detectors.
  void disableSMD() {mEnableSMD = false;}

  /// Add pedestal offsets from DB
  void setAddPed(Bool_t a = true) {mAddPed = a;}
  /// Smear the pedestal with sigma from DB
  void setSmearPed(Bool_t s = true) {mSmearPed = s;}
  /// Drop bad channels marked as "fail" in DB
  void setDropBad(Bool_t d = true) {mDropBad = true;}

  /// Overwrite the muDst values
  void setOverwrite(Bool_t o = true) {mOverwrite = o;}

  /// Set truncation of pedestal smearing    
  void setTruncatePedSmear(Float_t nSigma) {mTruncatePedSmear = nSigma;}
  
  /// Set the energy lost by one MIP in an 
  /// SMD strip [GeV].
  void setMipElossSmd(Float_t e) {mMip2ene = e;}
  /// Set the number of photoelectrons per 
  /// mip for the specified SMD strip [0..287]
  void setNpePerMipSmd(Int_t strip, Float_t npe) {mMip2pe[strip] = npe;}
  /// Sets the number of photoelectrons per mip 
  /// for all SMD stips to the specified value
  void setNpePerMipSmd(Float_t npe) {for (Int_t i = 0;i < MaxSmdStrips;i++) mMip2pe[i] = npe;}

  /// Set the energy lost by one MIP in a 
  /// pre or postshower layer [GeV].
  void setMipElossPre(Int_t layer, Float_t e) {mPmip2ene[layer] = e;}
  /// Sets the number of photoelectrons per mip 
  /// for the pre/postshower layers
  void setNpePerMipPre(Float_t npe) {mPmip2pe = npe;}

  /// Set the resolution of the single photoelectron 
  /// peak for the MAPMT's
  void setSinglePeResolution(Float_t r) {mSig1pe = r;}

  /// Set the source of ADC. Can be "MuDst" (default) or "StEvent"
  void setSource(const Char_t* name);

  /// Sets the relative light yields for layers 1, 2 and 24 in the
  /// calorimeter stack as measured.  The difference in thickness
  /// between preshower and "normal" layers is accounted for internally
  /// in the code.  Correction may be deactivated by calling setDoLightYield(false).
  /// @param pre1 <N p.e.>/MIP for layer1 / <N p.e.>/MIP for layers 3-23 default=1.86
  /// @param pre2 <N p.e.>/MIP for layer2 / <N p.e.>/MIP for layers 3-23 default=1.86
  /// @param post <N p.e.>/MIP for layer24 / <N p.e.>/MIP for layers 3-23 default=0.94
  void setRelativeLightYield(Float_t pre1, Float_t pre2, Float_t post) {mRelativeLightYield[kPre1] = pre1; mRelativeLightYield[kPre2] = pre2; mRelativeLightYield[kPost] = post;}

  /// Sets control flag which controls whether or not the light-yield
  /// correction for the towers is performed.  Defaults to true.  If
  /// set false, no light yield correction is made.
  void setDoLightYield(Bool_t ly) {mDoLightYield = ly;}

  /// Changes the sampling fraction from the default in the fast simulator
  void setSamplingFraction(Float_t f) {mSamplingFractionUser = f;}

  /// Defines a spread in the tower gains, generated gains will be between zero and mean + 1.0
  void setTowerGainSpread(Float_t s, Float_t mean = 1.0);

  /// Defines a spread in the SMD gains, generated gains will be between zero and mean + 1.0
  void setSmdGainSpread(Float_t s, Int_t sector, Int_t uv, Int_t strip_index);
  void setSmdGainSpread(Float_t s, Int_t strip_index);
  void setSmdGainSpread(Float_t s);

  /// Return MIP dE/dx used for SMD, Pre, Post
  static Float_t getMipdEdx(); 

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StEEmcSlowMaker.h,v 2.10 2014/08/06 11:43:04 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StEEmcSlowMaker, 3)
};

#endif

// $Log: StEEmcSlowMaker.h,v $
// Revision 2.10  2014/08/06 11:43:04  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 2.9  2010/09/07 22:24:52  stevens4
// give access to MIP dE/dx to other makers
//
// Revision 2.8  2010/08/05 21:23:45  stevens4
// Update sampling fraction to 4.8%
//
// Revision 2.7  2010/08/03 02:20:40  stevens4
// final update from peer review
//
// Revision 2.6  2010/07/29 16:12:03  ogrebeny
// Update after the peer review
//
// Revision 2.5  2010/05/03 20:47:23  ogrebeny
// Some code cleanup
//
// Revision 2.4  2010/02/12 23:02:38  ogrebeny
// By the request of the photon group, added an option to shift EEMC gains in the slow simulator.
//
// Revision 2.3  2009/02/05 20:06:53  ogrebeny
// Changed StEEmcDbMaker -> StEEmcDb
//
// Revision 2.2  2008/04/11 14:37:17  jwebb
// Added options to disable operation of individual slow simulaor subsystems.
//
// Revision 2.1  2007/11/28 16:17:34  jwebb
// Added the following features:
//
// 1. User may specify the sampling fraction.
//
// 2. Tower and SMD gain spreads.
//
// Revision 2.0  2007/01/13 00:03:03  jwebb
// Upgrade of the slow simulator.  The following changes have been made:
//
// 1. Towers will always be masked out when a "fail" bit is set in the database.
//    Previously this only happened if pedestals were being added, smeared.
//
// 2. Tower, preshower and postshower ADC values will be simulated using the
//    GEANT energy loss stored in StEmcHit and StMuEmcHit.  Previously, ADC
//    values from the fast simulator were used and energy loss recovered
//    using gains, resulting in roundoff errors.   Note that towers still use
//    the old path for MuDst-based analysis.
//
// 3. Tower simulation now accounts for the different light yields provided
//    by the brighter scintillator and two-fiber readout in the preshower
//    and postshower layers.  Previously, only the difference in thickness
//    was accounted for by the GEANT simulation.
//
// Revision 1.4  2006/12/12 20:29:14  balewski
// added hooks for Endcap embedding
//
// Revision 1.3  2006/08/07 18:50:11  balewski
// added capabilty to run on StEvent, use se-method, see macros/ for example
//
// Revision 1.2  2005/09/23 01:30:11  jwebb
// Tower peds now added  if option is set.
//
// Revision 1.1  2004/12/15 17:02:56  balewski
// try 2
//
