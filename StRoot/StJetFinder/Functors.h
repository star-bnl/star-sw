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
	bool operator()(const StJetEtCell* lhs, const StJetEtCell* rhs) const;
};

struct StEtGridKey {

  StEtGridKey() : iEta(0), iPhi(0) {};
  StEtGridKey(int ie, int ip) : iEta(ie), iPhi(ip) {};

  bool operator==(const StEtGridKey & rhs) {
    return (iEta == rhs.iEta) && (iPhi == rhs.iPhi);
  }
  int iEta;
  int iPhi;
};

struct StEtGridKeyLessThan
{
    bool operator()(const StEtGridKey& lhs, const StEtGridKey& rhs) const;
};

struct StJetEtCellEtLessThan
{
    bool operator()(StJetEtCell* lhs, StJetEtCell* rhs);
};

class StProtoJet;

struct StProtoJetUpdater
{
	void operator()(StJetEtCell*);
};

//inlines --- 

inline ostream& operator<<(ostream& os, const StEtGridKey& key)
{
    return os<<"iEta:\t"<<key.iEta<<"\tiPhi:\t"<<key.iPhi;
}

#endif
