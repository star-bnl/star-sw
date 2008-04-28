//Functors.h
//M.L. Miller (Yale Software)
//07/02

#ifndef Functors_HH
#define Functors_HH

using namespace std;
#include <iostream>

#include "StJetEtCell.h"

struct StJetEtCellEquals
{
  bool operator()(const StJetEtCell* lhs, const StJetEtCell* rhs) const
  { return (*lhs)==(*rhs); }
};

class StEtGridKey {

public:
  StEtGridKey() : iEta(0), iPhi(0) {};
  StEtGridKey(int ie, int ip) : iEta(ie), iPhi(ip) {};

  friend ostream& operator<<(ostream& os, const StEtGridKey& key)
  {
    return os << "iEta:\t" << key.iEta << "\tiPhi:\t" << key.iPhi;
  }

  friend bool operator<(const StEtGridKey& lhs, const StEtGridKey& rhs){
    if (lhs.iEta < rhs.iEta) return true;
    else if (lhs.iEta > rhs.iEta) return false;
    else return lhs.iPhi < rhs.iPhi;
  }

  friend bool operator==(const StEtGridKey& lhs, const StEtGridKey& rhs){
    return !( lhs < rhs ) && !( rhs < lhs);
  }

  friend struct StEtGridKeyLessThan;

  int eta() const { return iEta; }
  int phi() const { return iPhi; }

private:
  int iEta;
  int iPhi;
};

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
