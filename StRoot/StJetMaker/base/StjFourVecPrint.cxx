// $Id: StjFourVecPrint.cxx,v 1.2 2008/08/02 19:22:45 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjFourVecPrint.h"

#include <iostream>

using namespace std;

namespace StSpinJet {

void StjFourVecPrint::operator()(const StjFourVecList &fourList)
{
  for(StjFourVecList::const_iterator it = fourList.begin(); it != fourList.end(); ++it) {
    print(*it);
  }
}

void StjFourVecPrint::print(const StjFourVec& four)
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
