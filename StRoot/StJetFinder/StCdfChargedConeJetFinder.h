//StCdfChargedConeJetFinder.h
//M.L. Miller (Yale Software)
//12/02

/*!
  This is an implemenation of the CDF Charged jet finder (analagous to "Simple UA1 Jet Finder" but
  with charged tracks instead of calorimeter cells.  For efficiency, we still organize the tracks
  into a grid, but we sort the grid by it's leading charged particle.
 */

#ifndef StCdfChargedConeJetFinder_HH
#define StCdfChargedConeJetFinder_HH

#include "StConeJetFinder.h"

class StCdfChargedConeJetFinder : public StConeJetFinder
{
public:

    //cstr-dstr
    StCdfChargedConeJetFinder(StConeJetFinderPars& pars);
    virtual ~StCdfChargedConeJetFinder();
    
    //inherited interface
    virtual void findJets(JetList& protojets);
    //virtual void clear();
    virtual void print();
    
protected:
    //create a StCdfChargedJetEtCell object
    virtual StJetEtCell* makeCell(double etaMin, double etaMax, double phiMin, double phiMax);
    virtual bool acceptSeed(const StJetEtCell* cell);
    virtual bool acceptPair(const StJetEtCell* center, const StJetEtCell* assoc) const;
};

#endif
