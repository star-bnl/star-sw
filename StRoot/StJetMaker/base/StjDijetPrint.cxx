// $Id: StjDijetPrint.cxx,v 1.2 2008/08/13 15:34:22 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjDijetPrint.h"

#include "StjJetPrint.h"

#include <iostream>
#include <iomanip>

ClassImp(StjDijetPrint)

using namespace std;

void StjDijetPrint::operator()(const StjDijetList &dijetList)
{
  for(StjDijetList::const_iterator it = dijetList.begin(); it != dijetList.end(); ++it) {
    print(*it);
  }
}

void StjDijetPrint::print(const StjDijet& dijet)
{
  cout 
    << setw(7)  << dijet.runNumber      << ", "
    << setw(7)  << dijet.eventId        << ", "
    << setw(4)  << dijet.dijetId        << ", "
    << setw(14) << dijet.m              << ", "
    << setw(10) << dijet.eta            << ", "
    << setw(10) << dijet.costh          << ", "
    << setw(10) << dijet.dphi           << ", "
    << setw(10) << dijet.vertexZ
    << endl;

  StjJetPrint jetprint;
  StjJetList jetList;
  jetList.push_back(dijet.jet3);
  jetList.push_back(dijet.jet4);
  jetprint(jetList);
}
