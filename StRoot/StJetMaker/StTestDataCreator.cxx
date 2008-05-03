// $Id: StTestDataCreator.cxx,v 1.2 2008/05/03 01:06:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#include "StTestDataCreator.h"

#include <StJetFinder/AbstractFourVec.h>

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

}

void StTestDataCreator::fillJetTree()
{

}
