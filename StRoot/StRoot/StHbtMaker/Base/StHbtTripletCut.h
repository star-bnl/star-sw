/***************************************************************************
 *
 * $Id: StHbtTripletCut.h,v 1.5 2009/08/25 20:17:51 fine Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   base class for triplet-wise cuts. Users inherit from 
 *   this class and must add their own quantities.
 *
 ***************************************************************************
 *
 * $Log: StHbtTripletCut.h,v $
 * Revision 1.5  2009/08/25 20:17:51  fine
 * fix the compilation issues under SL5_64_bits  gcc 4.3.2
 *
 * Revision 1.4  2000/06/15 18:51:33  willson
 * Cuts and Correlation function information moved from StBaseAnalysis
 * to the derived analysis classes.  Global functions installed in
 * Cut and CorrFctn base classes to access analysis pointer.
 *
 * Revision 1.3  2000/05/11 21:16:40  willson
 * myAnalysis pointer changed to type StHbtBaseAnalysis - moved
 * some methods into StHbtBaseAnalysis class
 *
 * Revision 1.2  2000/04/12 01:53:00  willson
 * Initial Installation - Comments Added
 *
 * 
 ***************************************************************************/

#ifndef StHbtTripletCut_hh
#define StHbtTripletCut_hh

#include <string>

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTriplet.hh"
#include "StHbtMaker/Infrastructure/StHbtCutMonitorHandler.h"

class StHbtBaseAnalysis;

class StHbtTripletCut : public StHbtCutMonitorHandler {

public:

  StHbtTripletCut(){/* no-op */};   // default constructor. - Users should write their own
  virtual ~StHbtTripletCut(){/* no-op */};  // destructor

  virtual bool Pass(const StHbtTriplet* Triplet) =0;  // true if passes, false if not

  //  virtual string Report() =0;    // user-written method to return string describing cuts
  virtual StHbtString Report() =0;    // user-written method to return string describing cuts

#ifdef __ROOT__
  ClassDef(StHbtTripletCut, 0)
#endif
  // the following allows "back-pointing" from the CorrFctn to the "parent" Analysis
  friend class StHbtBaseAnalysis;
  StHbtBaseAnalysis* HbtAnalysis(){return myAnalysis;};
  void SetAnalysis(StHbtBaseAnalysis*);

protected:
  StHbtBaseAnalysis* myAnalysis;


};

inline void StHbtTripletCut::SetAnalysis(StHbtBaseAnalysis* analysis) { myAnalysis = analysis; }

#endif
