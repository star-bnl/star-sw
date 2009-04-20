// @(#)STAR/eg:$Id: StExampleFilter.h,v 1.2 2009/04/20 20:44:17 perev Exp $
// Author: V.Perev  Mar/2009
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StExampleFilter: base filter class for EvGen and Geant                		//
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StExampleFilter
#define STAR_StExampleFilter
#include <string>
#include "StMCFilter.h"
class StHepParticles;
class StG3Particles;
class StGenParticles;
/// more info in http://www.star.bnl.gov/~perev/SIM/mcFi;lter.ppt
class StExampleFilter : public StMCFilter {


public:
                                // ****** constructors and destructor
   StExampleFilter():StMCFilter("example"){};
   virtual ~StExampleFilter(){;}

/// user reject function called immediately after Event Generator.
/// Currently Pythia Vertex in zero. EG==EventGenerator
   int RejectEG(const StGenParticles &ptl) const;

/// user reject function called before GEANT tracking 
/// Vertex is already generated. GT == GeantTracker

   int RejectGT(const StGenParticles &ptl) const;

/// user reject function called after GEANT tracking 
/// Vertex and tracks are already generated. GE==GeantEnd
   int RejectGE(const StGenParticles &ptl) const;
public:
//	static methods
protected:
};

#endif

