/***************************************************************************
 *
 * $Id: mikesPairCut.h,v 1.5 2000/03/23 22:57:28 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a do-nothing pair cut that simply says "true" to every pair           
 *
 ***************************************************************************
 *
 * $Log: mikesPairCut.h,v $
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


#ifndef mikesPairCut_hh
#define mikesPairCut_hh

// do I need these lines ?
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtPairCut.h"

class mikesPairCut : public StHbtPairCut{
public:
  mikesPairCut();
  mikesPairCut(const mikesPairCut&);
  //~mikesPairCut();

  virtual bool Pass(const StHbtPair*);
  virtual StHbtString Report();
  mikesPairCut* Clone();


private:
  long mNPairsPassed;
  long mNPairsFailed;

#ifdef __ROOT__
  ClassDef(mikesPairCut, 1)
#endif
};

inline mikesPairCut::mikesPairCut(const mikesPairCut& c) : StHbtPairCut(c) {
  mNPairsPassed = 0;
  mNPairsFailed = 0;

}
inline mikesPairCut* mikesPairCut::Clone() { mikesPairCut* c = new mikesPairCut(*this); return c;}

#endif
