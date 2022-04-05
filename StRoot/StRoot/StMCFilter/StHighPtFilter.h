// @(#)STAR/eg:$Id: StHighPtFilter.h,v 1.1 2011/01/25 20:06:52 jwebb Exp $
// Author: V.Perev  Mar/2009
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StExampleFilter: base filter class for EvGen and Geant                		//
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StHighPtFilter
#define STAR_StHighPtFilter
#include <string>
#include "StMCFilter.h"
class StHepParticleMaster;
class StG3ParticleMaster;
class StGenParticleMaster;
/// Example of implementation of StMCFilter class
/// more info in http://www.star.bnl.gov/~perev/SIM/mcFi;lter.ppt
class StHighPtFilter: public StMCFilter {


 public:
                                // ****** constructors and destructor
  StHighPtFilter();
   virtual ~StHighPtFilter(){;}

   /// user reject function called immediately after Event Generator.
   /// Currently Pythia Vertex in zero. EG==EventGenerator
   int RejectEG(const StGenParticleMaster &ptl) const;

   /// user reject function called before GEANT tracking 
   /// Vertex is already generated. GT == GeantTracker
   int RejectGT(const StGenParticleMaster &ptl) const;

   /// user reject function called after GEANT tracking 
   /// Vertex and tracks are already generated. GE==GeantEnd
   int RejectGE(const StGenParticleMaster &ptl) const;

   void readConfig();
   void parseConfig( std::string key, double value );

 public:
 protected:

   double mPtCut;
   double mEtaCut;

};

#endif

