#ifndef __StarGenStats_h__
#define __StarGenStats_h__

/**
   \class StarGenStats
   \brief End of run statistics for event generators
   
   Collects end of run statistics for event generators, e.g. number of trials, number of accepted
   events, total cross section, etc...

   The meaning of the following variables should follow that documented here--
   
   http://home.thep.lu.se/~torbjorn/pythia81html/EventInformation.html

   \author Jason C. Webb <jwebb@bnl.gov>

 */

#include "TNamed.h"

class StarGenStats : public TNamed
{
 public:

  StarGenStats(const Char_t *name="stats", const Char_t *title="" );
  /*
    StarGenStats( const StarGenStats &other );
  */

  ~StarGenStats(){ /* nada */ };

  Int_t    nTried;        //*< Number of event generator trials        
  Int_t    nSelected;     //*< Number of event generator selected events 
  Int_t    nAccepted;     //*< Number of event generator accepted events 

  Double_t sigmaGen;         //*< Estimated event generator cross section
  Double_t sigmaErr;         //*< Estimated event generator cross section error
  Double_t sumWeightGen;     //*< Sum of event generator weights, equal to nAccepted if generator is unbiased.  pythia 8's info.weightSum()

  //
  // Luminosity is a derived quantity... nAccepted = luminosity * sigmaGen
  //
  Double_t luminosity(){ return sumWeightGen / sigmaGen; }    //*< Event generator luminosity
  Double_t luminosityErr(){ return (sigmaErr/sigmaGen)*luminosity(); }

  //
  // Filter statistics
  //
  Int_t    nFilterSeen;   //*< Number of events seen by event filter
  Int_t    nFilterAccept; //*< Number of events accepted by event filter

  Double_t filterAcceptance(){ return Double_t(nFilterAccept)/Double_t(nFilterSeen); }
  Double_t filterRejection() { return 1.0 - filterAcceptance(); }
  

  ClassDef(StarGenStats,1);

};

#endif
