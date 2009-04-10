// @(#)STAR/eg:$Id: StExampleFilter.h,v 1.1 2009/04/10 19:59:20 perev Exp $
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

class StExampleFilter : public StMCFilter {


public:
                                // ****** constructors and destructor
   StExampleFilter():StMCFilter("example"){};
   virtual ~StExampleFilter(){;}

   int RejectEG(const StGenParticles &ptl) const;
   int RejectGT(const StGenParticles &ptl) const;
   int RejectGE(const StGenParticles &ptl) const;
public:
//	static methods
protected:
};

#endif

