// $Id: StjFourVecPrint.cxx,v 1.1 2008/11/27 07:29:52 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjFourVecPrint.h"

#include <iostream>
#include <iomanip>

ClassImp(StjFourVecPrint)

using namespace std;

void StjFourVecPrint::operator()(const StjFourVecList &fourList)
{
  for(StjFourVecList::const_iterator it = fourList.begin(); it != fourList.end(); ++it) {
    print(*it);
  }
}

void StjFourVecPrint::print(const StjFourVec& four)
{
  cout 
    << setw(7) << four.runNumber   << ", "
    << setw(7) << four.eventId     << ", "
    << setw(4) << four.fourvecId   << ", "
    << setw(1) << four.type        << ", "
    << setw(1) << four.detectorId  << ", "
    << setw(4) << four.trackId     << ", "
    << setw(4) << four.towerId     << ", "
    << setw(4) << four.mcparticleId << ", "
    << setw(10) << four.pt         << ", "
    << setw(10) << four.eta        << ", "
    << setw(10) << four.phi        << ", "
    << setw(14) << four.m          << ", "
    << setw(10) << four.vertexZ
    << endl;

}
