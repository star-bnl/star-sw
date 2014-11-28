// -*- mode: c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 31 Aug 2011
//
// $Id: StjFastJet.h,v 1.2 2012/03/10 23:09:53 pibero Exp $
//
// $Log: StjFastJet.h,v $
// Revision 1.2  2012/03/10 23:09:53  pibero
// Addeed support for fastjet plugins
//
// Revision 1.1  2011/08/31 17:57:54  pibero
// Support for FastJet
//
// http://www.lpthe.jussieu.fr/~salam/fastjet/
// http://www.star.bnl.gov/HyperNews-star/protected/get/starsoft/8521.html
//
//

#ifndef STJ_FAST_JET_H
#define STJ_FAST_JET_H

#include "StFastJetPars.h"
#include "StJetFinder.h"

class StjFastJet : public StJetFinder {
public:
  StjFastJet(const StFastJetPars& pars) : mPars(pars) {}

  void Init() {}

  void findJets(JetList& protoJetList, const FourVecList& particleList);

private:
  fastjet::JetDefinition jetDefinition() const;
  StFastJetPars mPars;
};

#endif	// STJ_FAST_JET_H
