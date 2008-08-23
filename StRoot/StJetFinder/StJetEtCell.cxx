// $Id: StJetEtCell.cxx,v 1.13 2008/08/23 20:47:41 tai Exp $
#include "StJetEtCell.h"

#include <iostream>

using namespace std;

StJetEtCell::StJetEtCell(double etaMin, double etaMax, double phiMin, double phiMax)
  : StEtaPhiCell(etaMin, etaMax, phiMin, phiMax)
{
}

StJetEtCell::StJetEtCell(const StJetEtCell& c)
  : StEtaPhiCell(c)
{
}

StJetEtCell::StJetEtCell()
{
}

StJetEtCell::~StJetEtCell()
{
}

StEtaPhiCell* StJetEtCell::clone() const
{
  return new StJetEtCell(*this);
}

void StJetEtCell::addProtoJet(const StProtoJet& pj)
{
  mEt += pj.eT();
  mProtoJet.add(pj);
  mUpToDate = false;
}

void StJetEtCell::clear()
{
  mEt = 0;
  mNtimesUsed = 0;
  mCells.clear();
  mProtoJet.clear();
  mUpToDate = false;
}

void StJetEtCell::addCell(StEtaPhiCell* cell)
{
  mEt += cell->eT();
  mCells.push_back(cell);
  cell->setNtimesUsed( cell->nTimesUsed() + 1 );
  mUpToDate=false;
}

