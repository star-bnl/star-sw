/***************************************************************************
 *
 * $Id: StHbtBaseAnalysis.h,v 1.5 2000/06/15 18:51:32 willson Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov
 ***************************************************************************
 * 
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   base class for an HBT analysis.  Users should use one of the
 *   inherited analysis classes with this class.
 *
 ***************************************************************************
 *
 * $Log: StHbtBaseAnalysis.h,v $
 * Revision 1.5  2000/06/15 18:51:32  willson
 * Cuts and Correlation function information moved from StBaseAnalysis
 * to the derived analysis classes.  Global functions installed in
 * Cut and CorrFctn base classes to access analysis pointer.
 *
 * Revision 1.4  2000/05/11 21:16:40  willson
 * myAnalysis pointer changed to type StHbtBaseAnalysis - moved
 * some methods into StHbtBaseAnalysis class
 *
 * Revision 1.3  2000/04/12 01:53:00  willson
 * Initial Installation - Comments Added
 *
 *
 ***************************************************************************/

#ifndef StHbtBaseAnalysis_hh
#define StHbtBaseAnalysis_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

class StHbtEvent;

class StHbtBaseAnalysis{

public:

  StHbtBaseAnalysis() { /* noop */ };
  virtual ~StHbtBaseAnalysis() { /* noop */ };

#ifdef __ROOT__
  ClassDef(StHbtBaseAnalysis, 0)
#endif 
  
  virtual StHbtString Report() = 0;       //! returns reports of all cuts applied and correlation functions being done

  virtual void ProcessEvent(const StHbtEvent*) = 0;

  virtual void Finish() = 0;

};

#endif
