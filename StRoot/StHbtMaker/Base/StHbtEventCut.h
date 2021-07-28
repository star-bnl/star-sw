/***************************************************************************
 *
 * $Id: StHbtEventCut.h,v 1.9 2001/06/21 19:06:48 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *       base class for Eventwise cuts
 *      Users inherit from this class and must add their own quantities        
 *
 ***************************************************************************
 *
 * $Log: StHbtEventCut.h,v $
 * Revision 1.9  2001/06/21 19:06:48  laue
 * Some minor structural changes (forward declarations, etc)
 *
 * Revision 1.8  2000/06/15 18:51:32  willson
 * Cuts and Correlation function information moved from StBaseAnalysis
 * to the derived analysis classes.  Global functions installed in
 * Cut and CorrFctn base classes to access analysis pointer.
 *
 * Revision 1.7  2000/04/03 16:21:19  laue
 * some include files changed
 *
 * Revision 1.6  2000/03/23 22:43:27  laue
 * Clone() function implemented in cuts.
 *
 * Revision 1.5  2000/03/17 17:18:25  laue
 * Roberts new three particle correlations implemented.
 *
 * Revision 1.4  2000/03/16 01:54:36  laue
 * Copy constructor added to all the cut base classes and to the
 * corrfctn base class
 *
 * Revision 1.3  2000/01/07 23:21:16  laue
 * 0.) all 'ClassDef(...)' put inside #ifdef __ROOT__  #endif
 * 1.) unnecessary includes of 'StMaker.h' deleted
 *
 * Revision 1.2  1999/12/03 22:24:34  lisa
 * (1) make Cuts and CorrFctns point back to parent Analysis (as well as other way). (2) Accommodate new PidTraits mechanism
 *
 * Revision 1.1  1999/10/15 01:56:47  lisa
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

#ifndef StHbtEventCut_hh
#define StHbtEventCut_hh

class StHbtEvent;
class StHbtBaseAnalysis;

#include "StHbtMaker/Infrastructure/StHbtCutMonitorHandler.h"
#include "StHbtMaker/Infrastructure/StHbtString.hh"

class StHbtEventCut : public StHbtCutMonitorHandler {

public:

  StHbtEventCut(){/* no-op */};                // default constructor. - Users should write their own
  StHbtEventCut(const StHbtEventCut& c); // copy constructor
  virtual ~StHbtEventCut(){/* no-op */};       // destructor

  virtual bool Pass(const StHbtEvent* event) =0;  // true if passes, false if not

  virtual StHbtString Report() =0;    // user-written method to return string describing cuts
  virtual StHbtEventCut* Clone() { return 0;}

#ifdef __ROOT__
  ClassDef(StHbtEventCut, 0)
#endif
  // the following allows "back-pointing" from the CorrFctn to the "parent" Analysis
  friend class StHbtBaseAnalysis;
  StHbtBaseAnalysis* HbtAnalysis(){return myAnalysis;};
  void SetAnalysis(StHbtBaseAnalysis*);

protected:
  StHbtBaseAnalysis* myAnalysis;

};

inline StHbtEventCut::StHbtEventCut(const StHbtEventCut& c) : StHbtCutMonitorHandler() { myAnalysis=0;}
inline void StHbtEventCut::SetAnalysis(StHbtBaseAnalysis* analysis) { myAnalysis = analysis; }
#endif
