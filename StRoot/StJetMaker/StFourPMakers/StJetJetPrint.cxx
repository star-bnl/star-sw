// $Id: StJetJetPrint.cxx,v 1.1 2008/07/18 19:20:15 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StJetJetPrint.h"

#include <iostream>

using namespace std;

namespace StSpinJet {

void StJetJetPrint::operator()(const JetList &jetList)
{
  for(JetList::const_iterator it = jetList.begin(); it != jetList.end(); ++it) {
    print(*it);
  }
}

void StJetJetPrint::print(const Jet& jet)
{
  cout 
    << jet.runNumber      << " "
    << jet.eventId        << " "
    << jet.jetId          << " "
    << jet.pt             << " "
    << jet.eta            << " "
    << jet.phi            << " "
    << jet.m              << " "
    << jet.vertexZ        << " "
    << jet.detectorEta    << " "
    << endl;

  for(FourVecList::const_iterator jt = jet.fourVecList.begin(); jt != jet.fourVecList.end(); ++jt) {
    cout 
      << "       "
      << (*jt).runNumber  << " "
      << (*jt).eventId    << " "
      << (*jt).fourvecId  << " "
      << (*jt).type       << " "
      << (*jt).detectorId << " "
      << (*jt).trackId   << " "
      << (*jt).towerId  << " "
      << (*jt).pt      << " "
      << (*jt).eta    << " "
      << (*jt).phi   << " "
      << (*jt).m   << " "
      << endl;
    }

}



}
