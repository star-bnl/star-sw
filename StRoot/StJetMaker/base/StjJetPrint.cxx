// $Id: StjJetPrint.cxx,v 1.5 2008/08/10 23:04:40 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjJetPrint.h"

#include "StjFourVecPrint.h"

#include <iostream>
#include <iomanip>

ClassImp(StjJetPrint)

using namespace std;

void StjJetPrint::operator()(const StjJetList &jetList)
{
  for(StjJetList::const_iterator it = jetList.begin(); it != jetList.end(); ++it) {
    print(*it);
  }
}

void StjJetPrint::print(const StjJet& jet)
{
  cout 
    << setw(7)  << jet.runNumber      << ", "
    << setw(7)  << jet.eventId        << ", "
    << setw(4)  << jet.jetId          << ", "
    << setw(10) << jet.pt             << ", "
    << setw(10) << jet.eta            << ", "
    << setw(10) << jet.phi            << ", "
    << setw(14) << jet.m              << ", "
    << setw(10) << jet.vertexZ        << ", "
    << setw(10) << jet.detectorEta    << ", "
    << endl;

  StjFourVecPrint fourprint;

  fourprint(jet.fourVecList);


}
