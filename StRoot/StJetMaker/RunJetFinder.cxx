// $Id: RunJetFinder.cxx,v 1.4 2008/07/18 04:11:54 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "RunJetFinder.h"

#include <StJetFinder/StJetFinder.h>
#include <StJetFinder/StJetPars.h>

#include <FourVecWithId.h>
#include <FourVecForJetFinder.h>

#include <TLorentzVectorWithId.h>
#include <TLorentzVectorForJet.h>

#include <EtaToDetectorEta.h>

#include <TObjArray.h>

#include <iostream>

using namespace std;

namespace StSpinJet {

RunJetFinder::RunJetFinder(StJetPars* pars)
  : _jetFinder(pars->constructJetFinder())
{ }

void RunJetFinder::Init()
{
  _jetFinder->Init();
}

JetList RunJetFinder::operator()(const FourVecList& fourVecList)
{
 typedef std::list<StProtoJet> ProtoJetList;
 typedef std::vector<const AbstractFourVec*> FourList;

 FourList fourList;

 for(FourVecList::const_iterator p4 = fourVecList.begin(); p4 != fourVecList.end(); ++p4) {
   fourList.push_back(new FourVecForJetFinder(*p4));
 }

  ProtoJetList protoJetList;

  _jetFinder->findJets(protoJetList, fourList);

  JetList jetList;

  int jetId(1);
  for(list<StProtoJet>::iterator it = protoJetList.begin(); it != protoJetList.end(); ++it) {
    StProtoJet& protoJet = *it;

    Jet jet;
    jet.jetId = jetId++;
    jet.pt =  protoJet.pt();
    jet.eta = protoJet.eta();
    jet.phi = protoJet.phi();
    jet.m =   protoJet.mass();
    
    FourList parList = protoJet.list();
    for(FourList::const_iterator it = parList.begin(); it != parList.end(); ++it) {
      FourVec fourVec = (dynamic_cast<const FourVecForJetFinder*>(*it))->fourVec();
      jet.runNumber = fourVec.runNumber;
      jet.eventId = fourVec.eventId;
      jet.vertexZ = fourVec.vertexZ;
      jet.fourVecList.push_back(fourVec);
    }
    EtaToDetectorEta eta2deta;
    jet.detectorEta = eta2deta(jet.eta, jet.vertexZ);
    jetList.push_back(jet);
  }

  for(FourList::iterator it = fourList.begin(); it != fourList.end(); ++it) {
    delete *it;
  }

  return jetList;
}

TObjArray RunJetFinder::operator()(const TObjArray& fourList)
{
  typedef std::list<StProtoJet> ProtoJetList;
  typedef std::vector<const AbstractFourVec*> ParticleList;

  ParticleList particleList;

  for(Int_t j = 0; j < fourList.GetEntries(); ++j) {
    TLorentzVectorWithId* p4 = (TLorentzVectorWithId*)fourList[j];
    particleList.push_back(new FourVecWithId(*p4));
  }

  ProtoJetList protoJetList;

  _jetFinder->findJets(protoJetList, particleList);

  TObjArray ret;

  size_t iJet = 0;
  for(list<StProtoJet>::iterator it = protoJetList.begin(); it != protoJetList.end(); ++it) {
    StProtoJet& protoJet = *it;

    TLorentzVector p;
    p.SetPtEtaPhiM(protoJet.pt(), protoJet.eta(), protoJet.phi(), protoJet.mass());
    ret[iJet] = new TLorentzVectorForJet(p);
    ((TLorentzVectorForJet*)ret[iJet])->jetId = iJet + 1;

    ParticleList parList = protoJet.list();
    size_t iPart = 0;
    for(ParticleList::const_iterator it = parList.begin(); it != parList.end(); ++it) {
      TLorentzVectorWithId part = (dynamic_cast<const FourVecWithId*>(*it))->vec();
      ((TLorentzVectorForJet*)ret[iJet])->runNumber = part.runNumber;
      ((TLorentzVectorForJet*)ret[iJet])->eventId   = part.eventId;
      ((TLorentzVectorForJet*)ret[iJet])->particleList[iPart] = new TLorentzVectorWithId(part);
      ++iPart;
    }


    ++iJet;
  }

  for(ParticleList::iterator it = particleList.begin(); it != particleList.end(); ++it) {
    delete *it;
  }

  return ret;
}

}
