// $Id: StjDijetPrint.cxx,v 1.2 2008/09/12 22:32:54 tai Exp $
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
    << "run: "     << setw(7)  << dijet.runNumber      << ", "
    << "event: "   << setw(7)  << dijet.eventId        << ", "
    << "dijet: "   << setw(2)  << dijet.dijetId        << ", "
    << "m: "       << setw(8) << dijet.m              << ", "
    << "eta: "     << setw(8) << dijet.eta            << ", "
    << "costh: "   << setw(8) << dijet.costh          << ", "
    << "dphi: "    << setw(8) << dijet.dphi           << ", "
    << "Rt_s: "    << setw(8) << dijet.neuRtSameSide  << ", "
    << "Rt_a: "    << setw(8) << dijet.neuRtAwaySide  << ", "
    << "vertexZ: " << setw(8) << dijet.vertexZ
    << endl;

  StjJetPrint jetprint;
  StjJetList jetList;
  jetList.push_back(dijet.jet3);
  jetList.push_back(dijet.jet4);
  jetprint(jetList);
}
