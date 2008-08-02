// $Id: StjJetPrint.cxx,v 1.2 2008/08/02 19:22:46 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjJetPrint.h"

#include <iostream>

using namespace std;

namespace StSpinJet {

void StjJetPrint::operator()(const StjJetList &jetList)
{
  for(StjJetList::const_iterator it = jetList.begin(); it != jetList.end(); ++it) {
    print(*it);
  }
}

void StjJetPrint::print(const StjJet& jet)
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

  for(StjFourVecList::const_iterator jt = jet.fourVecList.begin(); jt != jet.fourVecList.end(); ++jt) {
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
