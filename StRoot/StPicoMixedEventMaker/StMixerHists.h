#ifndef StMixerHists__h
#define StMixerHists__h

/* **************************************************
 *
 * Histogram manager
 *
 * **************************************************
 *
 *  Initial Authors:  
 *         ** Michael Lomnitz (mrlomnitz@lbl.gov)
 *            Mustafa Mustafa (mmustafa@lbl.gov)
 *
 *  ** Code Maintainer
 *
 * **************************************************
 */

#include "StarClassLibrary/StThreeVectorF.hh"
#include "TH2F.h"

#include "StMixerPair.h"

class StMixerHists
{
 public:
  StMixerHists(char* fileBaseName);
  ~StMixerHists();

  void fillSameEvt(const StThreeVectorF& vtx);
  void fillMixedEvt(const StThreeVectorF& vtx);
  void fillSameEvtPair(StMixerPair const* const, int charge);
  void fillMixedEvtPair(StMixerPair const* const, int charge);
  void closeFile();
 private:
  TH2F* mSE_Vtx;
  TH2F* mME_Vtx;
  TH2F* mSE_LS;
  TH2F* mSE_US;
  TH2F* mME_LS;
  TH2F* mME_US;
  
};
inline void StMixerHists::fillSameEvt(const StThreeVectorF& vtx)
{
  mSE_Vtx->Fill(vtx.x(), vtx.y());
  return;
}
inline void StMixerHists::fillMixedEvt(const StThreeVectorF& vtx)
{
  mME_Vtx->Fill(vtx.x(), vtx.y());
  return;
}
inline void StMixerHists::fillSameEvtPair(StMixerPair const* const pair, int charge)
{
  if(charge == 0 )
    mSE_US-> Fill(pair->pt(),pair->m());
  else
    mSE_LS-> Fill(pair->pt(),pair->m());
  return;
}
inline void StMixerHists::fillMixedEvtPair(StMixerPair const* const pair, int charge)
{
  if(charge == 0 )
    mME_US-> Fill(pair->pt(),pair->m());
  else
    mME_LS-> Fill(pair->pt(),pair->m());
  return;
}
#endif
