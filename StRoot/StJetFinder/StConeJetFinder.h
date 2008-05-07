// -*- mode: c++;-*-
// $Id: StConeJetFinder.h,v 1.39 2008/05/07 21:44:44 tai Exp $
#ifndef StConeJetFinder_HH
#define StConeJetFinder_HH

#include "StConeJetFinderBase.h"

class StConeJetFinder : public StConeJetFinderBase {

public:
	
  StConeJetFinder(const StConePars& pars);
  virtual ~StConeJetFinder();

  void findJets(JetList& protojets);     
	
private:

  void findJetAroundThis(StEtaPhiCell* cell);

  bool shouldNotSearchForJetAroundThis(const StEtaPhiCell* cell) const;

  void findJetWithStableCone();
	
  CellList generateMidpointList();

  void findProtoJetsAroundMidpoints(CellList& midpointList);

  bool isInTheVolume(double eta, double phi);

  bool areTheyInTheSameCell(double eta1, double phi1, double eta2, double phi2);

  StJetSpliterMerger* mMerger;


};

#endif

