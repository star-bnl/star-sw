// -*- mode: c++;-*-
// $Id: StJetFinder.h,v 1.7 2008/04/29 00:11:11 tai Exp $
#ifndef StJetFinder_HH
#define StJetFinder_HH

#include <list>

#include "StProtoJet.h"

class StJetFinder {

public:
  typedef std::list<StProtoJet> JetList;

  typedef StProtoJet::FourVecList FourVecList;

  StJetFinder();
  virtual ~StJetFinder();

  virtual void Init() = 0;

  virtual void findJets(JetList& protojets) = 0;
  //  virtual void clear() = 0;
  //  virtual void print() = 0;

protected:
    JetList mJets;

};

#endif

