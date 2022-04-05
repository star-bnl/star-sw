/***************************************************************************
 *
 * $Id: StHbtCorrFctn.hh,v 1.11 2013/01/18 16:13:04 yyang Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    base class for a STAR correlation function.  Users should inherit 
 *    from this and must implement constructor, destructor, Report(),
 *    AddMixedPair(), AddRealPair(), Finish()
 *
 ***************************************************************************
 *
 * $Log: StHbtCorrFctn.hh,v $
 * Revision 1.11  2013/01/18 16:13:04  yyang
 * Add PairCut for CFs that share same cuts, but pair cuts
 *
 * Revision 1.10  2001/06/21 19:06:48  laue
 * Some minor structural changes (forward declarations, etc)
 *
 * Revision 1.9  2000/06/29 23:01:10  finch
 * added an extra base class for Parity Computations
 *
 * Revision 1.8  2000/06/15 18:51:32  willson
 * Cuts and Correlation function information moved from StBaseAnalysis
 * to the derived analysis classes.  Global functions installed in
 * Cut and CorrFctn base classes to access analysis pointer.
 *
 * Revision 1.7  2000/05/11 21:16:40  willson
 * myAnalysis pointer changed to type StHbtBaseAnalysis - moved
 * some methods into StHbtBaseAnalysis class
 *
 * Revision 1.6  2000/03/23 22:43:27  laue
 * Clone() function implemented in cuts.
 *
 * Revision 1.5  2000/03/16 01:54:36  laue
 * Copy constructor added to all the cut base classes and to the
 * corrfctn base class
 *
 * Revision 1.4  2000/02/13 17:13:09  laue
 * EventBegin() and EventEnd() functions implemented
 *
 * Revision 1.3  1999/12/03 22:24:33  lisa
 * (1) make Cuts and CorrFctns point back to parent Analysis (as well as other way). (2) Accommodate new PidTraits mechanism
 *
 * Revision 1.2  1999/07/06 22:33:18  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtCorrFctn_hh
#define StHbtCorrFctn_hh

#include "StHbtMaker/Base/StHbtBaseAnalysis.h"
#include "StHbtMaker/Base/StHbtPairCut.h"
#include "StHbtMaker/Infrastructure/StParityTypes.hh"  // can not forward declare typedefs
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Infrastructure/StHbtPair.hh"
#include "StHbtMaker/Infrastructure/StHbtTriplet.hh"
#include "StHbtMaker/Infrastructure/StHbtHisto.hh"

class StHbtCorrFctn{

public:
  StHbtCorrFctn() { mPairCut = 0; }
  StHbtCorrFctn(const StHbtCorrFctn& );
  virtual ~StHbtCorrFctn(){/* no-op */}

  virtual StHbtString Report() = 0;

  virtual void ParityCompute(ParityBuff*, ParityBuff*, int);

  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void AddRealTriplet(const StHbtTriplet*);
  virtual void AddMixedTriplet(const StHbtTriplet*);

  virtual void EventBegin(const StHbtEvent*) { /* no-op */ }
  virtual void EventEnd(const StHbtEvent*) { /* no-op */ }
  virtual void Finish() = 0;

  virtual StHbtCorrFctn* Clone() { return 0;}

  virtual StHbtPairCut* GetPairCut() { return mPairCut; }

  // the following allows "back-pointing" from the CorrFctn to the "parent" Analysis
  friend class StHbtBaseAnalysis;
  StHbtBaseAnalysis* HbtAnalysis(){return myAnalysis;};
  void SetAnalysis(StHbtBaseAnalysis*);

protected:
  StHbtBaseAnalysis* myAnalysis;
  StHbtPairCut* mPairCut;

private:

};

inline void StHbtCorrFctn::ParityCompute(ParityBuff*, ParityBuff*, int) { cout << "Not implemented" << endl; }
inline void StHbtCorrFctn::AddRealPair(const StHbtPair*) { cout << "Not implemented" << endl; }
inline void StHbtCorrFctn::AddMixedPair(const StHbtPair*) { cout << "Not implemented" << endl; }
inline void StHbtCorrFctn::AddRealTriplet(const StHbtTriplet*) { cout << "Not implemented" << endl; }
inline void StHbtCorrFctn::AddMixedTriplet(const StHbtTriplet*) { cout << "Not implemented" << endl; }

inline StHbtCorrFctn::StHbtCorrFctn(const StHbtCorrFctn& c) { myAnalysis =0; mPairCut = c.mPairCut; }
inline void StHbtCorrFctn::SetAnalysis(StHbtBaseAnalysis* analysis) { myAnalysis = analysis; }

#endif
