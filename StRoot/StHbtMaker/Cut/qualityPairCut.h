/***************************************************************************
 *
 * $Id: qualityPairCut.h,v 1.1 2000/04/05 18:56:18 rcwells Exp $
 *
 * Author: Randy Wells, Ohio State, rcwells@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   A pair which cuts on a topology quality           
 *
 ***************************************************************************
 *
 * $Log: qualityPairCut.h,v $
 * Revision 1.1  2000/04/05 18:56:18  rcwells
 * Adding class qualityPairCut
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


#ifndef qualityPairCut_hh
#define qualityPairCut_hh

// do I need these lines ?
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtPairCut.h"

class qualityPairCut : public StHbtPairCut{
public:
  qualityPairCut();
  qualityPairCut(const qualityPairCut&);
  //~qualityPairCut();

  virtual bool Pass(const StHbtPair*);
  virtual StHbtString Report();
  qualityPairCut* Clone();

  void SetQualityCut(const double& QualCutLo, const double& QualCutHi);

private:
  long mNPairsPassed;
  long mNPairsFailed;
  double mQualCutLo;
  double mQualCutHi;

#ifdef __ROOT__
  ClassDef(qualityPairCut, 1)
#endif
};

inline qualityPairCut::qualityPairCut(const qualityPairCut& c) : StHbtPairCut(c) {
  mNPairsPassed = 0;
  mNPairsFailed = 0;

}
inline qualityPairCut* qualityPairCut::Clone() { qualityPairCut* c = new qualityPairCut(*this); return c;}

#endif
