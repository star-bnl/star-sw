/***************************************************************************
 *
 * $Id: StHbtPairCut.h,v 1.5 2000/03/23 22:43:27 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *         base class for pair-wise cuts
 *         Users inherit from this class and must add their own quantities
 *
 ***************************************************************************
 *
 * $Log: StHbtPairCut.h,v $
 * Revision 1.5  2000/03/23 22:43:27  laue
 * Clone() function implemented in cuts.
 *
 * Revision 1.4  2000/03/16 01:54:37  laue
 * Copy constructor added to all the cut base classes and to the
 * corrfctn base class
 *
 * Revision 1.3  2000/01/07 23:21:17  laue
 * 0.) all 'ClassDef(...)' put inside #ifdef __ROOT__  #endif
 * 1.) unnecessary includes of 'StMaker.h' deleted
 *
 * Revision 1.2  1999/12/03 22:24:34  lisa
 * (1) make Cuts and CorrFctns point back to parent Analysis (as well as other way). (2) Accommodate new PidTraits mechanism
 *
 * Revision 1.1  1999/10/15 01:56:48  lisa
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
 * Revision 1.2  1999/07/06 22:33:19  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtPairCut_hh
#define StHbtPairCut_hh

#include <string>

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtPair.hh"
#include "StHbtMaker/Infrastructure/StHbtCutMonitorHandler.h"

class StHbtPairCut : public StHbtCutMonitorHandler {

public:

  StHbtPairCut(){/* no-op */};   // default constructor. - Users should write their own
  StHbtPairCut(const StHbtPairCut& c); // copy constructor
  virtual ~StHbtPairCut(){/* no-op */};  // destructor

  virtual bool Pass(const StHbtPair* pair) =0;  // true if passes, false if not

  virtual StHbtString Report() =0;    // user-written method to return string describing cuts
  virtual void EventBegin(const StHbtEvent*) { /* no-op */ }
  virtual void EventEnd(const StHbtEvent*) { /* no-op */ }
  virtual StHbtPairCut* Clone() { return 0;}

#ifdef __ROOT__
  ClassDef(StHbtPairCut, 0)
#endif
  // the following allows "back-pointing" from the CorrFctn to the "parent" Analysis
  friend class StHbtAnalysis;
  StHbtAnalysis* HbtAnalysis(){return myAnalysis;};

protected:
  StHbtAnalysis* myAnalysis;

};


inline StHbtPairCut::StHbtPairCut(const StHbtPairCut& c) : StHbtCutMonitorHandler() { myAnalysis = 0; }

#endif
