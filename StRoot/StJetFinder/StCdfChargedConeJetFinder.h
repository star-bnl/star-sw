// -*- mode: c++;-*-
// $Id: StCdfChargedConeJetFinder.h,v 1.19 2008/05/08 05:02:13 tai Exp $
#ifndef STCDFCHARGEDCONEJETFINDER_H
#define STCDFCHARGEDCONEJETFINDER_H

/*!
  This is an implemenation of the CDF Charged jet finder (analagous to "Simple UA1 Jet Finder" but
  with charged tracks instead of calorimeter cells.  For efficiency, we still organize the tracks
  into a grid, but we sort the grid by it's leading charged particle.
 */


#include "StConeJetFinderBase.h"
#include "StCdfChargedConePars.h"

#include "StCdfChargedJetEtCellFactory.h"

/*!
  \class StCdfChargedConePars
  \author M.L. Miller (MIT Software)
  A simple class to encapsulate the requisite run-time parameters of the cdf-charged cone jet algorithm.
*/
class StCdfChargedConeJetFinder : public StConeJetFinderBase
{
public:

    StCdfChargedConeJetFinder(const StCdfChargedConePars& pars);
    virtual ~StCdfChargedConeJetFinder();
    
  void findJets(JetList& protoJetList, const FourVecList& particleList);

private:    

  CellList generateLeadingPtOrderedList(JetList& protoJetList);

  void clearPreviousResult();

  void storeTheResultIn(JetList& protoJetList);

  void findProtoJets(CellList& toSearchList);

  void findJetAroundThis(StEtaPhiCell* cell);

  bool shouldNotAddToTheCell(const StEtaPhiCell& theCell, const StEtaPhiCell& otherCell) const;

  bool shouldNotSearchForJetAroundThis(const StEtaPhiCell* cell) const;

  void addToPrejets(StEtaPhiCell& cell);
	
  StJetEtCellFactory* makeCellFactory();

  CellList _preJets;
	
};

#endif // STCDFCHARGEDCONEJETFINDER_H

