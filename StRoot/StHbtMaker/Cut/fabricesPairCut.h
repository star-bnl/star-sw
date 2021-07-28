/***************************************************************************
 *
 * $Id: fabricesPairCut.h,v 1.2 2002/12/12 17:03:51 kisiel Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a do-nothing pair cut that simply says "true" to every pair           
 *
 ***************************************************************************
 *
 * $Log: fabricesPairCut.h,v $
 * Revision 1.2  2002/12/12 17:03:51  kisiel
 * Add NDedxHits cut, slight modification for Y cuts and Fabrices probability
 *
 * Revision 1.1  2001/12/14 23:11:27  fretiere
 * Add class HitMergingCut. Add class fabricesPairCut = HitMerginCut + pair purity cuts. Add TpcLocalTransform function which convert to local tpc coord (not pretty). Modify StHbtTrack, StHbtParticle, StHbtHiddenInfo, StHbtPair to handle the hit information and cope with my code
 *
 * Revision 1.5  2000/03/23 22:57:28  laue
 * Clone() function implemented
 *
 * Revision 1.4  2000/01/25 17:35:03  laue
 * I. In order to run the stand alone version of the StHbtMaker the following
 * changes have been done:
 * a) all ClassDefs and ClassImps have been put into #ifdef __ROOT__ statements
 * b) unnecessary includes of StMaker.h have been removed
 * c) the subdirectory StHbtMaker/doc/Make has been created including everything
 * needed for the stand alone version
 *
 * II. To reduce the amount of compiler warning
 * a) some variables have been type casted
 * b) some destructors have been declared as virtual
 *
 * Revision 1.3  1999/10/15 01:57:05  lisa
 * Important enhancement of StHbtMaker - implement Franks CutMonitors
 * ----------------------------------------------------------
 * This means 3 new files in Infrastructure area (CutMonitor),
 * several specific CutMonitor classes in the Cut area
 * and a new base class in the Base area (StHbtCutMonitor).
 * This means also changing all Cut Base class header files from .hh to .h
 * so we have access to CutMonitor methods from Cint command line.
 * This last means
 * 1) files which include these header files are slightly modified
 * 2) a side benefit: the TrackCuts and V0Cuts no longer need
 * a SetMass() implementation in each Cut class, which was stupid.
 * Also:
 * -----
 * Include Franks StHbtAssociationReader
 * ** None of these changes should affect any user **
 *
 * Revision 1.2  1999/07/06 22:33:21  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/


#ifndef fabricesPairCut_hh
#define fabricesPairCut_hh

// do I need these lines ?
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "Cut/HitMergingPairCut.h"
class ostrstream;

class fabricesPairCut : public HitMergingPairCut{
public:
  fabricesPairCut();
  fabricesPairCut(const fabricesPairCut&);
  //~fabricesPairCut();

  virtual bool Pass(const StHbtPair*);
  virtual StHbtString Report();
  fabricesPairCut* Clone();
  void setPiKPairMinProbability(double aPiKPairMinProb);
  void setPiPPairMinProbability(double aPiPPairMinProb);
  void setElPairMaxProbability(double aElPairMaxProb);
  void setPiPiPairMaxProbability(double aPiPiPairMaxProb);
  void setKKPairMaxProbability(double aKKPairMaxProb);
  ostrstream* finalReport() const;

private:
  double mPiKPairMinProb;
  double mPiPPairMinProb;
  double mElPairMaxProb;
  double mPiPiPairMaxProb;
  double mKKPairMaxProb;
#ifdef __ROOT__
  ClassDef(fabricesPairCut, 1)
#endif
};

inline fabricesPairCut::fabricesPairCut(const fabricesPairCut& c) : HitMergingPairCut(c) {
  mNPairsPassed = 0;
  mNPairsFailed = 0;

}
inline fabricesPairCut* fabricesPairCut::Clone() { fabricesPairCut* c = new fabricesPairCut(*this); return c;}

inline void fabricesPairCut::setPiKPairMinProbability(double aPiKPairMinProb){
  mPiKPairMinProb = aPiKPairMinProb;
}
inline void fabricesPairCut::setPiPPairMinProbability(double aPiPPairMinProb){
  mPiPPairMinProb = aPiPPairMinProb;
}
inline void fabricesPairCut::setElPairMaxProbability(double aElPairMaxProb){
  mElPairMaxProb = aElPairMaxProb;
}
inline void fabricesPairCut::setPiPiPairMaxProbability(double aPiPiPairMaxProb){
  mPiPiPairMaxProb = aPiPiPairMaxProb;
}
inline void fabricesPairCut::setKKPairMaxProbability(double aKKPairMaxProb){
  mKKPairMaxProb = aKKPairMaxProb;
}
#endif
