/***************************************************************************
 *
 * $Id: StHbtAnalysis.h,v 1.10 2000/04/03 16:21:50 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *      This is the Class for Analysis objects.  Each of the simultaneous
 *      Analyses running should have one of these instantiated.  They link
 *      into the Manager in an Analysis Collection.
 *
 ***************************************************************************
 *
 * $Log: StHbtAnalysis.h,v $
 * Revision 1.10  2000/04/03 16:21:50  laue
 * some include files changed
 * Multi track cut added
 *
 * Revision 1.9  2000/03/17 17:23:05  laue
 * Roberts new three particle correlations implemented.
 *
 * Revision 1.8  2000/03/16 02:07:04  laue
 * Copy constructor added to StHbtAnalysis (only known cuts, corrfctn).
 *
 * StHbtBinaryReader can now derive filename from StIOMaker and read a list
 * of files.
 *
 * StHbtManager now holds a collection of StHbtEventWriters (multiple writes
 * possible now)
 *
 * Revision 1.7  2000/02/13 17:17:12  laue
 * Calls to the EventBegin() and EventEnd() functions implemented
 * The actual analysis is moved from StHbtManager to StHbtAnalysis
 *
 * Revision 1.6  2000/01/25 17:35:17  laue
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
 * Revision 1.5  1999/12/03 22:24:36  lisa
 * (1) make Cuts and CorrFctns point back to parent Analysis (as well as other way). (2) Accommodate new PidTraits mechanism
 *
 * Revision 1.4  1999/10/15 01:57:23  lisa
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
 * Revision 1.3  1999/10/04 15:38:56  lisa
 * include Franks new accessor methods StHbtAnalysis::CorrFctn and StHbtManager::Analysis as well as McEvent example macro
 *
 * Revision 1.2  1999/07/06 22:33:22  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtAnalysis_hh
#define StHbtAnalysis_hh
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtBaseAnalysis.h"        // base analysis class
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
//#include <string>
#include "StHbtMaker/Base/StHbtEventCut.h"             // base class 
#include "StHbtMaker/Base/StHbtParticleCut.h"          // base class
#include "StHbtMaker/Base/StHbtPairCut.h"              // base class
#include "StHbtMaker/Base/StHbtCorrFctn.hh"             // base class
#include "StHbtMaker/Infrastructure/StHbtCorrFctnCollection.hh"


class StHbtAnalysis : public StHbtBaseAnalysis {

public:

  StHbtAnalysis();
  StHbtAnalysis(const StHbtAnalysis&);  // copy constructor
  virtual ~StHbtAnalysis();

  // Gets and Sets
  StHbtPairCut*       PairCut();
  void SetPairCut(StHbtPairCut*);

  StHbtCorrFctnCollection* CorrFctnCollection();
  StHbtCorrFctn* CorrFctn(int n);    // Access to CFs within the collection

  void AddCorrFctn(StHbtCorrFctn*);

  bool AnalyzeIdenticalParticles();
  virtual StHbtString Report();       //! returns reports of all cuts applied and correlation functions being done

  virtual void ProcessEvent(const StHbtEvent*);
  void EventBegin(const StHbtEvent*); //startup for EbyE
  void EventEnd(const StHbtEvent*);   // cleanup for EbyE

  virtual void Finish();

  friend StHbtLikeSignAnalysis;

private:
  StHbtPairCut*       mPairCut;
  StHbtCorrFctnCollection* mCorrFctnCollection;

#ifdef __ROOT__
  ClassDef(StHbtAnalysis, 0)
#endif

};

// Get's
inline StHbtPairCut*       StHbtAnalysis::PairCut() {return mPairCut;}
inline StHbtCorrFctnCollection* StHbtAnalysis::CorrFctnCollection() {return mCorrFctnCollection;}

// Set's
inline void StHbtAnalysis::AddCorrFctn(StHbtCorrFctn* cf) {mCorrFctnCollection->push_back(cf); cf->myAnalysis=this;}
inline void StHbtAnalysis::SetPairCut(StHbtPairCut* x) { mPairCut = x; x->myAnalysis=this;}
inline bool StHbtAnalysis::AnalyzeIdenticalParticles(){return (mFirstParticleCut==mSecondParticleCut);}

#endif
