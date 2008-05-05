// -*- mode: c++;-*-
// $Id: StCdfChargedConeJetFinder.h,v 1.13 2008/05/05 00:32:47 tai Exp $

/*!
  This is an implemenation of the CDF Charged jet finder (analagous to "Simple UA1 Jet Finder" but
  with charged tracks instead of calorimeter cells.  For efficiency, we still organize the tracks
  into a grid, but we sort the grid by it's leading charged particle.
 */

#ifndef StCdfChargedConeJetFinder_HH
#define StCdfChargedConeJetFinder_HH

#include "StConeJetFinder.h"
#include "StCdfChargedConePars.h"

#include "StCdfChargedJetEtCellFactory.h"

/*!
  \class StCdfChargedConePars
  \author M.L. Miller (MIT Software)
  A simple class to encapsulate the requisite run-time parameters of the cdf-charged cone jet algorithm.
*/
class StCdfChargedConeJetFinder : public StConeJetFinder
{
public:

    //cstr-dstr
    StCdfChargedConeJetFinder(const StCdfChargedConePars& pars);
    virtual ~StCdfChargedConeJetFinder();
    
  void print();

private:    

  void findJets_sub1();
  void findJets_sub2();

  bool shouldNotAddToTheCell(const StEtaPhiCell& theCell, const StEtaPhiCell& otherCell) const;

  bool acceptSeed(const StEtaPhiCell* cell);

  StJetEtCellFactory* makeCellFactory();

};

#endif
