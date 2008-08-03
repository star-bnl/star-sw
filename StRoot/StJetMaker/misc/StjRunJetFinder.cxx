// $Id: StjRunJetFinder.cxx,v 1.1 2008/08/03 00:28:59 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjRunJetFinder.h"

#include <StJetFinder/StJetFinder.h>
#include <StJetFinder/StJetPars.h>

#include <StjFourVecForJetFinder.h>

#include <StjEtaToDetectorEta.h>

#include <iostream>

using namespace std;

StjRunJetFinder::StjRunJetFinder(StJetPars* pars)
  : _jetFinder(pars->constructJetFinder())
{ }

void StjRunJetFinder::Init()
{
  _jetFinder->Init();
}

StjJetList StjRunJetFinder::operator()(const StjFourVecList& fourVecList)
{
 typedef std::list<StProtoJet> ProtoJetList;
 typedef std::vector<const AbstractFourVec*> FourList;

 FourList fourList;

 for(StjFourVecList::const_iterator p4 = fourVecList.begin(); p4 != fourVecList.end(); ++p4) {
   fourList.push_back(new StjFourVecForJetFinder(*p4));
 }

  ProtoJetList protoJetList;

  _jetFinder->findJets(protoJetList, fourList);

  StjJetList jetList;

  int jetId(1);
  for(list<StProtoJet>::iterator it = protoJetList.begin(); it != protoJetList.end(); ++it) {
    StProtoJet& protoJet = *it;

    StjJet jet;
    jet.jetId = jetId++;
    jet.pt =  protoJet.pt();
    jet.eta = protoJet.eta();
    jet.phi = protoJet.phi();
    jet.m =   protoJet.mass();
    
    FourList parList = protoJet.list();
    for(FourList::const_iterator it = parList.begin(); it != parList.end(); ++it) {
      StjFourVec fourVec = (dynamic_cast<const StjFourVecForJetFinder*>(*it))->fourVec();
      jet.runNumber = fourVec.runNumber;
      jet.eventId = fourVec.eventId;
      jet.vertexZ = fourVec.vertexZ;
      jet.fourVecList.push_back(fourVec);
    }
    StjEtaToDetectorEta eta2deta;
    jet.detectorEta = eta2deta(jet.eta, jet.vertexZ);
    jetList.push_back(jet);
  }

  for(FourList::iterator it = fourList.begin(); it != fourList.end(); ++it) {
    delete *it;
  }

  return jetList;
}
