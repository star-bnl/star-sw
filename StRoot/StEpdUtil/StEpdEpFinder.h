#ifndef _StEpdEpFinder
#define _StEpdEpFinder

class TVector3;
//class TH2D;
class TFile;
class TProfile;
class TProfile2D;
class StEpdGeom;
class TClonesArray;

#include "TH3D.h"
#include "TH2D.h"

/*************************************
 * \author Mike Lisa
 * \date 23 June 2018
 *
 * \description:
 * Finds the Event Plane (EP) and EP-related quantities.
 * Also creates correction files and applies them.
 * Also calculates resolution
 *
 * There is a lot of EP-related information.  Raw, phi-weighted, shifted, etc.
 * 1st, 2nd, nth order.  Q-vector and Psi.  Even if the user does not request
 * all of these things, it is convenient and not so wasteful to simply calculate
 * them all at once.  Therefore, the user is presented with a large object of
 * type StEpdEpInfo.  This avoids "calculate-it-on-the-fly" which can be wasteful
 * if the user requests more than one thing, as well as "has-it-already-been-calculated?"
 * ambiguities.
 *
 *   A word about "EventType": the correction factors and other things about the event
 * plane can depend on centrality, vertex position, etc.  This code will apply the
 * corrections separately for different "EventTypes".  It is up to the user to decide
 * what this denotes.  All I care about is that when you send me an event, you tell
 * me the EventTypeId, which is just an integer.  The rest is up to you (as it should be :).
 *   For many (most?) people, this will just be a centrality index.
 *
 * This class will do "phi-weighting" and "shifting" corrections, but it needs
 *  some information to do it.  It will generate such information itself, but
 *  here is what you need to do, if you are starting from scratch:
 * 1) With whatever multiplicity/Vz/whatever event cuts you are going to analyze, and
 *    on whatever dataset, run your code that calls this class.  This will produce
 *    a file called StEpdEpFinderCorrectionHistograms_OUTPUT.root in the cwd.
 *    --> mv StEpdEpFinderCorrectionHistograms_OUTPUT.root StEpdEpFinderCorrectionHistos_INPUT.root
 *    That takes care of the Phi-weighting
 * 2) Repeat the above step (including the rename of the file, overwriting the old one).
 *      That takes care of the shifting weights.
 * 3) You are good to go.
 *
 * Note that, to find the EP, you can weight a hit according to ring (you enter the ring weights)
 *   or by eta (you enter the eta weights) or (default) treat all hits equally.
 *   See SetRingWeights() and SetEtaWeights()
 *
 * ------------------------------------------
 * This class creates some histograms and uses some histograms.  Since I use GetBinContent()
 *  to extract values from the histograms, the binning is important.  I try to keep things
 *  consistent, but let me explain.
 *
 * 1) Phi Weighting.  Used by StEpdEpFinder and created by StEpdEpFinder.
 *    This code creates a histogram with the root name 
 *    Form("PhiWeightEW%d",ew) where ew=0/1 for east/west.
 *    x-axis is PP, with 12 bins running from 0.5 to 12.5.
 *    y-axis is TT, with 31 bins running from 0.5 to 31.5
 *    So, there really ought to be no confusion.  h->GetBinContent(4,16) gets the bin that was filled by h->Fill(4,16)
 *    Anyway, the user has no interaction with this histogram.
 *
 * 2) Shifting correction.  Used by StEpdEpFinder and created by StEpdEpFinder.
 *    This implements equation (6) of arxiv:nucl-ex/9805001
 *    The histogram names are
 *     - Form("EpdShiftEW%dPsi%d_sin",ew,order)   - where ew=0/1 for east/west 
 *     - Form("EpdShiftEW%dPsi%d_cos",ew,order)   - where ew=0/1 for east/west 
 *     - Form("EpdShiftFullEventPsi%d_sin",order)
 *     - Form("EpdShiftFullEventPsi%d_cos",order)
 *    In these histograms, order is "n" (as in n=2 for second-order EP)
 *    x-axis is "i" from equation (6) above.  As in <cos(n*i*Psi_n)>
 *       There are _EpTermsMax bins running from 0.5 to _EPtermsMax+0.5, so there should be no confusion with this axis.
 *    y-axis is EventTypeId, the *user-defined* EventType bin number.
 *--------->>>>>>>>>>>>>>>>>> And at this point I must make a demand of the user <<<<<<<<<<<<<<<<<<<
 *    When the user instantiates an StEpdEpFinder object, he specifies nEventTypeBins, the number of EventType bins he will use.
 *       >>>> The user MUST number these bins 0,1,2,3,4,...(nEventTypeBins-1) when he interacts with this class <<<<
 *       (If he wants to use a different convention in his code, that's fine, but when taling to StEpdEpFinder, use 0..(nEventTypeBins-1)
 *    The y-axis then has nEventTypeBins bins, going from -0.5 to nEventTypeBins-0.5
 *
 * 3) Eta weights. Used (optional) by StEpdEpFinder, but not created by it.
 *    It gives the additional "weight" that a hit gets, and depends on EventType, eta (obviously), and the harmonic order ("n")
 *    x-axis is eta;  y-axis is EventTypeID
 *----->>>>>>>> The user MUST use the same EventType numbering (i.e. integers, starting at zero) as mentioned above.
 *----->>>>>>>> The first (non-underflow) bin on the x-axis MUST be centered at zero, and the last (non-overflow) bin on the x-axis MUST be centered on nEventTypeBins-1
 *
 * 4) Resolution-related histograms.  Not used by StEpdEpFinder, but produced by StEpdEpFinder for user convenience.
 *    These are just the simple <cos(n\Psi_{n,E}-n\Psi_{n,W})> values, as a function of EventType ID
 *
 *************************************/

#define _EpTermsMax 6

#include "StEpdEpInfo.h"

class StEpdEpFinder{
 public:

  /// Constructor.  Initializes values and reads correction file, if it exists.
  /// This file is actually PRODUCED by the code in an earlier run.  The user must rename
  /// the file StEpdEpFinderCorrectionHistograms_OUTPUT.root if he wants to use it.
  /// \param CorrectionFileName     Full name of the .root file with correction histograms.
  /// \param nEventTypeBins        Number of EventType bins that the user is using.  Up to the user to have a consistent usage, here and in analysis.
  StEpdEpFinder(int nEventTypeBins=10, char const* OutFileName="StEpdEpFinderCorrectionHistograms_OUTPUT.root", char const* CorrectionFileName="StEpdEpFinderCorrectionHistograms_INPUT.root");
  ~StEpdEpFinder(){/* no-op */};

  /// sets eta-based weights and sets the flag indicating that eta-based weights will be used
  /// The x-axis of the TH2D is abs(eta).  The y-axis is EventTypeID (according to whatever convention/definition the user is using)
  /// \param order        order of the EP Begins at UNITY! i.e. order=1 means first-order plane, order=2 means 2nd-order plane, ...
  /// \param EtaWeight    histogram with the weights as a function of |eta| and EventType
  void SetEtaWeights(int order, TH2D EtaWeight);

  /// sets ring-based weights and sets the flag indicating that the ring-based weights will be used
  /// \param order        order of the EP Begins at UNITY! i.e. order=1 means first-order plane, order=2 means 2nd-order plane, ...
  /// \param RingWeights  1D array of length 16, containing the RingWeights
  void SetRingWeights(int order, double* RingWeights);

  /// sets the threshold, in units of nMIP, for determining tile weights
  ///   TileWeight = (EpdHit->nMIP()>thresh)?((EpdHit->nMIP()>MAX)?MAX:EpdHit->nMIP()):0;
  /// \param thresh       threshold.  If epdHit->nMIP() is less than thresh, then weight is zero
  void SetnMipThreshold(double thresh);

  /// sets the maximum weight, in units of nMIP, for determining tile weights
  ///   TileWeight = (EpdHit->nMIP()>thresh)?((EpdHit->nMIP()>MAX)?MAX:EpdHit->nMIP()):0;
  /// \param MAX          maximum tile weight.  If epdHit->nMIP()>MAX then weight=MAX
  void SetMaxTileWeight(double MAX);

  /// sets the EpdHit format
  /// 0=StEvent, 1=StMuDst; 2=StPicoDst.  Default is 2
  void SetEpdHitFormat(int format);

  /// call this method at the end of your run to output correction histograms to a file (you can choose to use these or not)
  ///   and to calculate EP resolutions
  void Finish();

  /// returns all information about the EP.  A large object of type StEpdEpInfo is returned, so you don't
  ///  have to call the StEpdEpFinder over and over again for various information
  /// \param EpdHits      Epd Hits in a TClones array.  Will be decoded as StEpdHit, StMuEpdHit, or StPicoEpdHit as dictated by mFormatUsed
  /// \param primVertex   primary vertex position for this event
  /// \param EventTypeID user-defined integer specifying EventType of the event.  User must use same convention in correction histograms and weights
  StEpdEpInfo Results(TClonesArray* EpdHits, TVector3 primVertex, int EventTypeID);

  /// Returns a big string that tells in text what the settings were.
  /// This is for your convenience and is of course optional.  I like
  /// to put a concatenation of such Reports into a text file, so I
  /// "autodocument" what were the settings for a given run
  TString Report();


 private:

  bool OrderOutsideRange(int order);         // just makes sure order is between 1 and _EpOrderMax

  double GetPsiInRange(double Qx, double Qy, int order);
  double RingOrEtaWeight(int ring, double eta, int order, int EventTypeId);
  StEpdGeom* mEpdGeom;

  int mNumberOfEventTypeBins;                // user-defined.  Default is 10.  Used for correction histograms
  int mFormatUsed;                           // 0=StEvent, 1=StMuDst; 2=StPicoDst.  Default is 2

  // tile weight = (0 if ADC< thresh), (MAX if ADC>MAX); (ADC otherwise).
  double mThresh;                            // default is 0.3
  double mMax;                               // default is 2.0

  int mWeightingScheme;                      // 0=none; 1=eta-based weighting; 2=ring-based weighting
  TH2D* mEtaWeights[_EpOrderMax];            // used if mWeightingScheme=1;
  double mRingWeights[16][_EpOrderMax];      // used if mWeightingScheme=2

  TProfile* mAveCosDeltaPsi[_EpOrderMax];        // average of cos(Psi_{East,n}-Psi_{West,n}) using phi-weighted and shifted EPs

  TFile* mCorrectionInputFile;
  TFile* mCorrectionOutputFile;
  TFile* mResolutionOutputFile;

  //  these are shift correction factors that we MAKE now and write out
  TProfile2D* mEpdShiftOutput_sin[3][_EpOrderMax];    // [ewFull][order-1]
  TProfile2D* mEpdShiftOutput_cos[3][_EpOrderMax];    // [ewFull][order-1]
  //  these are shift correction factors that we made before, and USE now
  TProfile2D* mEpdShiftInput_sin[3][_EpOrderMax];     // [ewFull][order-1]
  TProfile2D* mEpdShiftInput_cos[3][_EpOrderMax];     // [ewFull][order-1]
  //   these are the phi weights
  TH3D* mPhiWeightInput[2];      // 12aug2019 - MAL has changed these from TH2D* to TH3D* to include EventType.
  TH3D* mPhiWeightOutput[2];     // the array index is 0/1 for East/West as usual
  TH3D* mPhiAveraged[2];         // the bins are (PP,TT,EventType)

  

  ClassDef(StEpdEpFinder,0)

};

inline void StEpdEpFinder::SetnMipThreshold(double t){mThresh=t;}
inline void StEpdEpFinder::SetMaxTileWeight(double MAX){mMax=MAX;}
inline void StEpdEpFinder::SetEpdHitFormat(int f){mFormatUsed=f;}

#endif
