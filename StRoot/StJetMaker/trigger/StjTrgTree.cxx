// $Id: StjTrgTree.cxx,v 1.1 2008/08/10 23:04:58 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgTree.h"

// #include "StjTrgReader.h"

#include <TTree.h>

#include <iostream>

ClassImp(StjTrgTree)

using namespace std;

StjTrgTree::StjTrgTree(TTree *tree,
		       const Int_t& indexMajor, const Int_t& indexMinor,
		       const char* indexMajorName, const char* indexMinorName
		       )
  : _tree(tree)
  , _indexMajor(indexMajor), _indexMinor(indexMinor)
{
  _tree->BuildIndex(indexMajorName, indexMinorName);
  //  _reader = new StjTrgReader(_tree);
}

int StjTrgTree::id()
{

}

int StjTrgTree::runNumber()
{

}
 
int StjTrgTree::eventId()
{

}

bool StjTrgTree::hard() const
{

}

bool StjTrgTree::soft() const
{

}

bool StjTrgTree::pass()
{

}

double StjTrgTree::prescale()
{

}

double StjTrgTree::vertexZ()
{

}

vector<int> StjTrgTree::towers()
{

}

vector<int> StjTrgTree::jetPatches()
{

}

