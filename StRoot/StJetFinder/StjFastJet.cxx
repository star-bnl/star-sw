//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 31 Aug 2011
//
// $Id: StjFastJet.cxx,v 1.2 2012/03/10 23:09:53 pibero Exp $
//
// $Log: StjFastJet.cxx,v $
// Revision 1.2  2012/03/10 23:09:53  pibero
// Addeed support for fastjet plugins
//
// Revision 1.1  2011/08/31 17:58:01  pibero
// Support for FastJet
//
// http://www.lpthe.jussieu.fr/~salam/fastjet/
// http://www.star.bnl.gov/HyperNews-star/protected/get/starsoft/8521.html
//
//

#include <vector>
#include "fastjet/ClusterSequence.hh"
#include "StjFastJet.h"

void StjFastJet::findJets(JetList& protoJetList, const FourVecList& particleList)
{
  // Read in input particles
  std::vector<fastjet::PseudoJet> inputParticles;
  for (size_t i = 0; i < particleList.size(); ++i) {
    fastjet::PseudoJet pseudojet(particleList[i]->px(),particleList[i]->py(),particleList[i]->pz(),particleList[i]->e());
    pseudojet.set_user_index(i);
    inputParticles.push_back(pseudojet);
  }

  // Run the jet clustering with the above jet definition
  fastjet::ClusterSequence clusterSequence(inputParticles,jetDefinition());

  // Extract inclusive jets with pt > ptmin
  std::vector<fastjet::PseudoJet> inclusiveJets = clusterSequence.inclusive_jets(mPars.ptMin());
  for (size_t i = 0; i < inclusiveJets.size(); ++i) {
    StProtoJet protojet;
    std::vector<fastjet::PseudoJet> constituents = clusterSequence.constituents(inclusiveJets[i]);
    for (size_t j = 0; j < constituents.size(); ++j) {
      const_cast<FourVecList&>(protojet.list()).push_back(particleList[constituents[j].user_index()]);
    }
    protojet.update();
    protoJetList.push_back(protojet);
  }
}

fastjet::JetDefinition StjFastJet::jetDefinition() const
{
  return (mPars.jetAlgorithm() == fastjet::plugin_algorithm)
    ? fastjet::JetDefinition(static_cast<fastjet::JetDefinition::Plugin*>(mPars.plugin()))
    : fastjet::JetDefinition(static_cast<fastjet::JetAlgorithm>(mPars.jetAlgorithm()),
			     mPars.Rparam(),
			     static_cast<fastjet::RecombinationScheme>(mPars.recombinationScheme()),
			     static_cast<fastjet::Strategy>(mPars.strategy()));
}
