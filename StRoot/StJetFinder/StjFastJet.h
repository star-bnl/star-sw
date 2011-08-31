// -*- mode: c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 31 Aug 2011
//
// $Id: StjFastJet.h,v 1.1 2011/08/31 17:57:54 pibero Exp $
//
// $Log: StjFastJet.h,v $
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
  StFastJetPars mPars;
};

#endif	// STJ_FAST_JET_H
