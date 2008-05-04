// -*- mode: c++;-*-
// $Id: StJetFinder.h,v 1.8 2008/05/04 00:43:51 tai Exp $
#ifndef StJetFinder_HH
#define StJetFinder_HH

#include <list>

#include "StProtoJet.h"

class StJetFinder {

public:
  typedef std::list<StProtoJet> JetList;

  typedef StProtoJet::FourVecList FourVecList;

  StJetFinder() { }
  virtual ~StJetFinder() { }

  virtual void Init() = 0;

  virtual void findJets(JetList& protojets) = 0;

protected:

    JetList mJets;

};

#endif

