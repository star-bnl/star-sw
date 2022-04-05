
/***************************************************************************
 *
 * $Id: StHbtParticleCut.h,v 1.9 2009/08/25 20:17:51 fine Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     base class for particle-wise cuts
 * Note:    Users DO NOT inherit from this class!
 *          The base classes StHbtTrackCut and StHbtV0Cut inherit from this,
 *          and users inherit from those
 * 
 ***************************************************************************
 *
 * $Log: StHbtParticleCut.h,v $
 * Revision 1.9  2009/08/25 20:17:51  fine
 * fix the compilation issues under SL5_64_bits  gcc 4.3.2
 *
 * Revision 1.8  2000/06/15 18:51:33  willson
 * Cuts and Correlation function information moved from StBaseAnalysis
 * to the derived analysis classes.  Global functions installed in
 * Cut and CorrFctn base classes to access analysis pointer.
 *
 * Revision 1.7  2000/03/23 22:43:27  laue
 * Clone() function implemented in cuts.
 *
 * Revision 1.6  2000/03/17 17:18:25  laue
 * Roberts new three particle correlations implemented.
 *
 * Revision 1.5  2000/03/16 01:54:37  laue
 * Copy constructor added to all the cut base classes and to the
 * corrfctn base class
 *
 * Revision 1.4  2000/02/13 17:13:15  laue
 * EventBegin() and EventEnd() functions implemented
 *
 * Revision 1.3  2000/01/07 23:21:17  laue
 * 0.) all 'ClassDef(...)' put inside #ifdef __ROOT__  #endif
 * 1.) unnecessary includes of 'StMaker.h' deleted
 *
 * Revision 1.2  1999/12/03 22:24:34  lisa
 * (1) make Cuts and CorrFctns point back to parent Analysis (as well as other way). (2) Accommodate new PidTraits mechanism
 *
 * Revision 1.1  1999/10/15 01:56:50  lisa
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
 * Revision 1.3  1999/09/17 22:37:59  lisa
 * first full integration of V0s into StHbt framework
 *
 * Revision 1.2  1999/07/06 22:33:19  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/


#ifndef StHbtParticleCut_hh
#define StHbtParticleCut_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtCutMonitorHandler.h"

class StHbtBaseAnalysis;

class StHbtParticleCut : public StHbtCutMonitorHandler {

public:

  StHbtParticleCut(){/* no-op */};   // default constructor. - Users should write their own
  StHbtParticleCut(const StHbtParticleCut&); // copy constructor
  virtual ~StHbtParticleCut(){/* no-op */};  // destructor

  virtual StHbtString Report() =0;    // user-written method to return string describing cuts

  double Mass(){return mMass;};       // mass of the particle being selected
  virtual void SetMass(const double& mass) {mMass = mass;};

  virtual void EventBegin(const StHbtEvent*) { /* no-op */ }
  virtual void EventEnd(const StHbtEvent*) { /* no-op */ }
  virtual StHbtParticleCut* Clone() { return 0;}

  virtual StHbtParticleType Type()=0;

  // the following allows "back-pointing" from the CorrFctn to the "parent" Analysis
  friend class StHbtBaseAnalysis;
  StHbtBaseAnalysis* HbtAnalysis(){return myAnalysis;};
  void SetAnalysis(StHbtBaseAnalysis*);

protected:
  double mMass;
  //  StHbtParticleType mType;            // tells whether cut is on Tracks or V0's
  StHbtBaseAnalysis* myAnalysis;

#ifdef __ROOT__
  ClassDef(StHbtParticleCut, 0)
#endif
};

inline StHbtParticleCut::StHbtParticleCut(const StHbtParticleCut& c) : StHbtCutMonitorHandler() { 
  mMass = c.mMass; myAnalysis = 0; 
#ifdef STHBTDEBUG
  cout << " StHbtParticleCut::StHbtParticleCut(const StHbtParticleCut& c) - mMass: " << mMass << endl;
#endif
}
inline void StHbtParticleCut::SetAnalysis(StHbtBaseAnalysis* analysis) { myAnalysis = analysis; }
#endif
