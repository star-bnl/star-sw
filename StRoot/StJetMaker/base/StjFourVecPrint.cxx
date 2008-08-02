// $Id: StjFourVecPrint.cxx,v 1.1 2008/08/02 04:15:22 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjFourVecPrint.h"

#include <iostream>

using namespace std;

namespace StSpinJet {

void StJetFourVecPrint::operator()(const FourVecList &fourList)
{
  for(FourVecList::const_iterator it = fourList.begin(); it != fourList.end(); ++it) {
    print(*it);
  }
}

void StJetFourVecPrint::print(const FourVec& four)
{
  cout 
    << four.runNumber  << " "
    << four.eventId    << " "
    << four.fourvecId  << " "
    << four.type       << " "
    << four.detectorId << " "
    << four.trackId    << " "
    << four.towerId    << " "
    << four.pt         << " "
    << four.eta        << " "
    << four.phi        << " "
    << four.m          << " "
    << four.vertexZ    << " "
    << endl;

}



}
