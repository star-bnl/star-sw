#ifndef StPicoHFMyAnaMaker_h
#define StPicoHFMyAnaMaker_h

#include "StPicoHFMaker/StPicoHFMaker.h"

/* **************************************************
 *  Sample class fo HF picoDST analysis
 * --------------------------------------------------
 * 
 *  For more info look also in the .h files in StPicoHFMaker/
 *     StPicoHFMaker/StPicoHFMaker.h      <-- Base Class for analysis
 *     StPicoHFMaker/StPicoHFEvent.h      <-- Holds candidates for one event (written to Tree)
 *     StPicoHFMaker/StHFCuts.h           <-- Cuts, can be set in run macro
 *     StPicoHFMaker/StHFPair.h           <-- Holds a pair candidate of a two body decay
 *     StPicoHFMaker/StHFTriplet.h        <-- Holds a triplet of a three body decay
 *
 *  Usage:
 *   - Implement
 *        InitHF()
 *        MakeHF()
 *        ClearHF()
 *        FinishHF()
 *
 *  - Do not ovewrite Init, Make, Clear, Finish which are inhertited from StPicoHFMaker via StMaker 

 *  - Set StHFCuts class via setHFBaseCuts(...) in run macro
 *
 *  - Set use mode of StPicoHFMaker class  via setMakerMode(...)
 *     use enum of StPicoHFMaker::eMakerMode
 *      StPicoHFMaker::kAnalyze - don't write candidate trees, just fill histograms
 *      StPicoHFMaker::kWrite   - write candidate trees
 *      StPicoHFMaker::kRead    - read candidate trees and fill histograms
 *
 *  - Set decay mode of analysis via setDecayMode(...)
 *     use enum of StPicoHFEvent::eHFEventMode (see there for more info)
 *      StPicoHFEvent::kTwoParticleDecay,
 *      StPicoHFEvent::kThreeParticleDecay
 *      StPicoHFEvent::kTwoAndTwoParticleDecay
 *
 *  - Implement these track selection methods used to fill vectors for 'good' identified particles
 *      (methods from StHFCuts utility class can/should be used)
 *       isPion
 *       isKaon
 *       isProton
 *
 *  --------------------------------------------------
 *  
 *  Initial Authors:  
 *            Xin Dong        (xdong@lbl.gov)
 *            Mustafa Mustafa (mmustafa@lbl.gov)
 *          **Jochen Thaeder  (jmthader@lbl.gov) 
 * 
 *  ** Code Maintainer
 *
 * **************************************************
 */

class StPicoDst;
class StPicoDstMaker;
class StPicoEvent;
class StPicoTrack;
class StPicoHFEvent;

class StHFPair;
class StHFTriplet;
class StHFCuts;

class StPicoHFMyAnaMaker : public StPicoHFMaker 
{
 public:
  StPicoHFMyAnaMaker(char const* name, StPicoDstMaker* picoMaker, char const* outputBaseFileName,  
		       char const* inputHFListHFtree);
  virtual ~StPicoHFMyAnaMaker();
  
  virtual Int_t InitHF();
  virtual Int_t MakeHF();
  virtual void  ClearHF(Option_t *opt);
  virtual Int_t FinishHF();
  
  // -- ADOPT DECAY CHANNELS, if wished ------------------- 
  void setDecayChannel(unsigned int u) { mDecayChannel = u; }

  enum eDecayChannel {kChannel1, kChannel2, kChannel3};

 protected:
  virtual bool isPion(StPicoTrack const*) const;
  virtual bool isKaon(StPicoTrack const*) const;
  virtual bool isProton(StPicoTrack const*) const;
  
 private:
  int createCandidates();
  int analyzeCandidates();

  // -- private members --------------------------

  unsigned int mDecayChannel;

  // -- ADD USER MEMBERS HERE ------------------- 




  // -- ADD USER MEMBERS HERE -------------------

  ClassDef(StPicoHFMyAnaMaker, 0)
};

#endif
