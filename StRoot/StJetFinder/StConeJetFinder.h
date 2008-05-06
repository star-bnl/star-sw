// -*- mode: c++;-*-
// $Id: StConeJetFinder.h,v 1.35 2008/05/06 19:15:17 tai Exp $
#ifndef StConeJetFinder_HH
#define StConeJetFinder_HH

#include "StConeJetFinderBase.h"

class StConeJetFinder : public StConeJetFinderBase {

public:
	
  StConeJetFinder(const StConePars& pars);
  virtual ~StConeJetFinder();
	
protected:
		
  friend struct PreJetInitializer; 
	
  StConeJetFinder();

private:

};

#endif

