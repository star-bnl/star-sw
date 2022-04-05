// @(#)STAR/eg:$Id: StFmsJetFilter.h,v 1.1 2017/03/01 14:58:23 jwebb Exp $
// Author: V.Perev  Mar/2009
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFmsJetFilter: base filter class for EvGen and Geant                		//
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StFmsJetFilter
#define STAR_StFmsJetFilter
#include <string>
#include "StMCFilter.h"
class StHepParticleMaster;
class StG3ParticleMaster;
class StGenParticleMaster;
/// Example of implementation of StMCFilter class
/// more info in http://www.star.bnl.gov/~perev/SIM/mcFi;lter.ppt
class StFmsJetFilter : public StMCFilter {


public:
                                // ****** constructors and destructor
   StFmsJetFilter():StMCFilter("exampleFmsJet"){};
   virtual ~StFmsJetFilter(){;}

/// user reject function called immediately after Event Generator.
/// Currently Pythia Vertex in zero. EG==EventGenerator
   Int_t RejectEG(const StGenParticleMaster &ptl) const;

   //float DPhi(float phim, float phi, float &dphi);
   Float_t DPhi(float phim, float phi);
/// user reject function called before GEANT tracking 
/// Vertex is already generated. GT == GeantTracker
   
   Int_t RejectGT(const StGenParticleMaster &ptl) const;
   
/// user reject function called after GEANT tracking 
/// Vertex and tracks are already generated. GE==GeantEnd
   Int_t RejectGE(const StGenParticleMaster &ptl) const;
 public:
   //	static methods
 protected:
};

#endif

