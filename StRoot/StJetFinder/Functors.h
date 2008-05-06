//Functors.h
//M.L. Miller (Yale Software)
//07/02

#ifndef Functors_HH
#define Functors_HH

using namespace std;
#include <iostream>

#include "StJetEtCell.h"

#include "StEtGridKey.h"

struct StJetEtCellEtLessThan
{
  bool operator()(StJetEtCell* lhs, StJetEtCell* rhs)
  { return lhs->eT()<rhs->eT(); }
};

class StProtoJet;

struct StProtoJetUpdater
{
  void operator()(StJetEtCell *cell)
  { cell->protoJet().update(); }
};

#endif
