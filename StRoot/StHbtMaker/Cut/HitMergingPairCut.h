/***************************************************************************
 *
 * $Id: HitMergingPairCut.h,v 1.1 2001/12/14 23:11:27 fretiere Exp $
 *
 * Author: Fabrice Retiere
 ***************************************************************************
 *
 * Description: Allow to cut on hit merging      
 * Usage :
 *  HitMergingPairCut* pairCut = new HitMergingPairCut();
 *  pairCut->setDefaultHalfFieldMergingPar();
 *  pairCut->setMaxFracOfMergedRow(MaxMergedHit);   
 *
 ***************************************************************************
 *
 * $Log: HitMergingPairCut.h,v $
 * Revision 1.1  2001/12/14 23:11:27  fretiere
 * Add class HitMergingCut. Add class fabricesPairCut = HitMerginCut + pair purity cuts. Add TpcLocalTransform function which convert to local tpc coord (not pretty). Modify StHbtTrack, StHbtParticle, StHbtHiddenInfo, StHbtPair to handle the hit information and cope with my code
 * 
 *
 **************************************************************************/

#ifndef HitMergingPairCut_hh
#define HitMergingPairCut_hh

// do I need these lines ?
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtPairCut.h"
class ostrstream;

class HitMergingPairCut : public StHbtPairCut{
public:
  HitMergingPairCut();
  HitMergingPairCut(const HitMergingPairCut&);
  //~HitMergingPairCut();

  virtual bool Pass(const StHbtPair*);
  virtual StHbtString Report();
  HitMergingPairCut* Clone();

  void setMaxFracOfMergedRow(double aMaxFracPair);
  void setMergingPar(double aMaxDuInner, double aMaxDzInner,
		     double aMaxDuOuter, double aMaxDzOuter);
  void setDefaultHalfFieldMergingPar();
  void setDefaultFullFieldMergingPar();

  virtual ostrstream* finalReport() const;

protected:
  long mNPairsPassed;
  long mNPairsFailed;

  double mMaxFracPair;
#ifdef __ROOT__
  ClassDef(HitMergingPairCut, 1)
#endif
};

inline HitMergingPairCut::HitMergingPairCut(const HitMergingPairCut& c) : StHbtPairCut(c) {
  mNPairsPassed = 0;
  mNPairsFailed = 0;

}
inline HitMergingPairCut* HitMergingPairCut::Clone() { HitMergingPairCut* c = new HitMergingPairCut(*this); return c;}
inline void HitMergingPairCut::setMaxFracOfMergedRow(double aMaxFracPair){
  mMaxFracPair=aMaxFracPair;
}

#endif
