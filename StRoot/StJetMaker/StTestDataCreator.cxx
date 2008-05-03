// $Id: StTestDataCreator.cxx,v 1.3 2008/05/03 01:23:03 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#include "StTestDataCreator.h"

#include <StJetFinder/AbstractFourVec.h>

#include <iostream>

using namespace std;

ClassImp(StTestDataCreator)

StTestDataCreator::StTestDataCreator(std::string outFileName)
  : _OutFileName(outFileName)
  , _jetTree(0)
  , _outFile(0)
{

}

StTestDataCreator::~StTestDataCreator()
{

}

void StTestDataCreator::Init()
{

}
 
void StTestDataCreator::Finish()
{

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
  for(vector<DataCtl>::iterator dataCtl = _dataCtlList.begin(); dataCtl != _dataCtlList.end(); ++dataCtl) {
    const vector<const AbstractFourVec*>& particleList = *((*dataCtl)._particleList) ;
      for(vector<const AbstractFourVec*>::const_iterator particle = particleList.begin(); particle != particleList.end(); ++particle) {
	cout << (*dataCtl)._name << " " << (**particle) << endl;
      }
  }
}
