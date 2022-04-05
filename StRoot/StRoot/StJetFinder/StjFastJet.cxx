//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 31 Aug 2011
//
// $Id: StjFastJet.cxx,v 1.3 2016/01/06 22:00:17 gdwebb Exp $
//
// $Log: StjFastJet.cxx,v $
// Revision 1.3  2016/01/06 22:00:17  gdwebb
// This is code to implement the off axis cone underlying event analysis.
//
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
#include "fastjet/ClusterSequenceArea.hh"
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
  if(!mPars.jetAreaFlag()){
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
    
  }else if(mPars.jetAreaFlag()){
    //jet clustering with ghost area
    fastjet::ClusterSequenceArea clusterSequenceArea(inputParticles, jetDefinition(), areaDefinition());
    std::vector<fastjet::PseudoJet> inclusiveJets = clusterSequenceArea.inclusive_jets(mPars.ptMin());
    for (size_t i = 0; i < inclusiveJets.size(); ++i) {
      StProtoJet protojet;
      std::vector<fastjet::PseudoJet> constituents = clusterSequenceArea.constituents(inclusiveJets[i]);
      protojet.setArea(inclusiveJets[i].area());
      protojet.setAreaError(inclusiveJets[i].area_error());
      for (size_t j = 0; j < constituents.size(); ++j) {
	const_cast<FourVecList&>(protojet.list()).push_back(particleList[constituents[j].user_index()]);
      }
      protojet.update();
      protoJetList.push_back(protojet);
    }
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
fastjet::AreaDefinition StjFastJet::areaDefinition() const
{
  fastjet::GhostedAreaSpec area_spec(mPars.jetArea()->ghostMaxRap(), mPars.jetArea()->repeat(), mPars.jetArea()->ghostArea(), mPars.jetArea()->gridScatter(), mPars.jetArea()->ptScatter(), mPars.jetArea()->meanGhostPt());
  return fastjet::AreaDefinition(static_cast<fastjet::AreaType>(mPars.jetArea()->areaType()), area_spec);
}
