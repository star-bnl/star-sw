// -*- mode: C++ -*-

//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 19 July 2007
//

#ifndef ST_GAMMA_PYTHIA_EVENT_H
#define ST_GAMMA_PYTHIA_EVENT_H

#include <vector>
using std::vector;

#include "TLorentzVector.h"
#include "StSpinPool/StMCAsymMaker/StPythiaEvent.h"

class StGammaPythiaEvent : public StPythiaEvent {
public:
  StGammaPythiaEvent() {}
  StGammaPythiaEvent(StPythiaEvent* pythia) : StPythiaEvent(*pythia) {}
  ~StGammaPythiaEvent() {}

  void Clear(Option_t* option = "");

  int numberOfPion0() const;
  int numberOfPrompt() const;
  int numberOfDecay() const;
  int numberOfFrag() const;
  int numberOfInitial() const;
  int numberOfFinal() const;

  TLorentzVector& pion0(int i);
  TLorentzVector& prompt(int i);
  TLorentzVector& decay(int i);
  TLorentzVector& frag(int i);
  TLorentzVector& initial(int i);
  TLorentzVector& final(int i);

  vector<TLorentzVector>& pion0();
  vector<TLorentzVector>& prompt();
  vector<TLorentzVector>& decay();
  vector<TLorentzVector>& frag();
  vector<TLorentzVector>& initial();
  vector<TLorentzVector>& final();

private:
  vector<TLorentzVector> mPion0;
  vector<TLorentzVector> mPrompt;
  vector<TLorentzVector> mDecay;
  vector<TLorentzVector> mFrag;
  vector<TLorentzVector> mInitial;
  vector<TLorentzVector> mFinal;

  ClassDef(StGammaPythiaEvent, 1);
};

inline void StGammaPythiaEvent::Clear(Option_t* option)
{
  mPion0.clear();
  mPrompt.clear();
  mDecay.clear();
  mFrag.clear();
  mInitial.clear();
  mFinal.clear();
  StPythiaEvent::Clear(option);
}

inline int StGammaPythiaEvent::numberOfPion0() const { return mPion0.size(); }
inline int StGammaPythiaEvent::numberOfPrompt() const { return mPrompt.size(); }
inline int StGammaPythiaEvent::numberOfDecay() const { return mDecay.size(); }
inline int StGammaPythiaEvent::numberOfFrag() const { return mFrag.size(); }
inline int StGammaPythiaEvent::numberOfInitial() const { return mInitial.size(); }
inline int StGammaPythiaEvent::numberOfFinal() const { return mFinal.size(); }

inline TLorentzVector& StGammaPythiaEvent::pion0(int i) { return mPion0[i]; }
inline TLorentzVector& StGammaPythiaEvent::prompt(int i) { return mPrompt[i]; }
inline TLorentzVector& StGammaPythiaEvent::decay(int i) { return mDecay[i]; }
inline TLorentzVector& StGammaPythiaEvent::frag(int i) { return mFrag[i]; }
inline TLorentzVector& StGammaPythiaEvent::initial(int i) { return mInitial[i]; }
inline TLorentzVector& StGammaPythiaEvent::final(int i) { return mFinal[i]; }

inline vector<TLorentzVector>& StGammaPythiaEvent::pion0() { return mPion0; }
inline vector<TLorentzVector>& StGammaPythiaEvent::prompt() { return mPrompt; }
inline vector<TLorentzVector>& StGammaPythiaEvent::decay() { return mDecay; }
inline vector<TLorentzVector>& StGammaPythiaEvent::frag() { return mFrag; }
inline vector<TLorentzVector>& StGammaPythiaEvent::initial() { return mInitial; }
inline vector<TLorentzVector>& StGammaPythiaEvent::final() { return mFinal; }

#endif
