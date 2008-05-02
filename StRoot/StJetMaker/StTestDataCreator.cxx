// $Id: StTestDataCreator.cxx,v 1.1 2008/05/02 22:13:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#include "StTestDataCreator.h"

using namespace std;

namespace StSpinJet {

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

void StTestDataCreator::addJetFinder(StFourPMaker* fourPMaker, list<StProtoJet>* protoJetList, const char* name)
{

}

void StTestDataCreator::fillJetTree()
{

}

}
