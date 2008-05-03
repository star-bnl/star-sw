// $Id: StTestDataCreator.cxx,v 1.4 2008/05/03 03:13:34 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#include "StTestDataCreator.h"

#include <StJetFinder/AbstractFourVec.h>

#include <TFile.h>
#include <TTree.h>

#include <iostream>
#include <cstring>

using namespace std;

ClassImp(StTestDataCreator)

StTestDataCreator::StTestDataCreator(std::string outFileName)
  : _OutFileName(outFileName)
  , _particleTree(0)
  , _outFile(0)
{

}

StTestDataCreator::~StTestDataCreator()
{

}

void StTestDataCreator::Init()
{

  _outFile = TFile::Open(_OutFileName.c_str(), "RECREATE");
  _particleTree = new TTree("T", "T");
  _particleTree->Branch("ParticleList", &_particleListBranch, "eventID/I:name[128]/C:px/D:py/D:pz/D:phi/D:eta/D:eT/D:e/D:mass/D:charge/D");
  _particleListBranch.eventID = 0;
}

void StTestDataCreator::Finish()
{
  _outFile->Write();
  _outFile->Close();
  delete _outFile; _outFile = 0;
}

void StTestDataCreator::addJetFinder(StFourPMaker* fourPMaker, const std::vector<const AbstractFourVec*>* particleList, std::list<StProtoJet>* protoJetList, const char* name)
{
  DataCtl dataCtl;
  dataCtl._name = name;
  dataCtl._fourPMaker = fourPMaker;
  dataCtl._particleList = particleList;
  dataCtl._protoJetList = protoJetList;

  _dataCtlList.push_back(dataCtl);
}

void StTestDataCreator::fillJetTree()
{
  _particleListBranch.eventID++;

  for(vector<DataCtl>::iterator dataCtl = _dataCtlList.begin(); dataCtl != _dataCtlList.end(); ++dataCtl) {
    const vector<const AbstractFourVec*>& particleList = *((*dataCtl)._particleList) ;
      for(vector<const AbstractFourVec*>::const_iterator particle = particleList.begin(); particle != particleList.end(); ++particle) {
	strncpy(_particleListBranch.name, (*dataCtl)._name.c_str(), 128);
	_particleListBranch.px     = (*particle)->px()    ;
	_particleListBranch.py     = (*particle)->py()    ;
	_particleListBranch.pz     = (*particle)->pz()    ;
	_particleListBranch.phi    = (*particle)->phi()   ;
	_particleListBranch.eta    = (*particle)->eta()   ;
	_particleListBranch.eT     = (*particle)->eT()    ;
	_particleListBranch.e      = (*particle)->e()     ;
	_particleListBranch.mass   = (*particle)->mass()  ;
	_particleListBranch.charge = (*particle)->charge();
	_particleTree->Fill();
      }
  }
}
