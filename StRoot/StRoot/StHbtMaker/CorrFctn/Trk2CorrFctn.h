/***************************************************************************
 *
 * $Id: Trk2CorrFctn.h,v 1.1 2001/12/14 23:11:25 fretiere Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description:          
 *
 ***************************************************************************
 *
 * $Log: Trk2CorrFctn.h,v $
 * Revision 1.1  2001/12/14 23:11:25  fretiere
 * Add class HitMergingCut. Add class fabricesPairCut = HitMerginCut + pair purity cuts. Add TpcLocalTransform function which convert to local tpc coord (not pretty). Modify StHbtTrack, StHbtParticle, StHbtHiddenInfo, StHbtPair to handle the hit information and cope with my code
 *
 *
 **************************************************************************/

#ifndef Trk2CorrFctn_hh
#define Trk2CorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"

class Trk2CorrFctn : public StHbtCorrFctn {
public:
  Trk2CorrFctn(char* title);
  virtual ~Trk2CorrFctn();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void Finish();
  virtual void Write();

private:
  StHbt2DHisto* mNumFracRowClosestRow;
  StHbt2DHisto* mDenFracRowClosestRow;
  StHbt2DHisto* mRatFracRowClosestRow;

#ifdef __ROOT__ 
  ClassDef(Trk2CorrFctn, 1)
#endif
};


#endif

