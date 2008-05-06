// -*- mode: c++;-*-
// $Id: StEtaPhiCell.cxx,v 1.2 2008/05/06 02:13:15 tai Exp $
#include "StEtaPhiCell.h"

#include <cmath>
#include <algorithm>

StEtaPhiCell::StEtaPhiCell(double etaMin, double etaMax, double phiMin, double phiMax)
  : mEtaMin(etaMin)
  , mEtaMax(etaMax)
  , mPhiMin(phiMin)
  , mPhiMax(phiMax)
  , mEt(0.)
  , mNtimesUsed(0)
  , mUpToDate(false)
{

}

StEtaPhiCell::StEtaPhiCell()
  : mEtaMin(0)
  , mEtaMax(0)
  , mPhiMin(0)
  , mPhiMax(0)
  , mEt(0)
  , mNtimesUsed(0)
  , mUpToDate(false)
{

}

StEtaPhiCell::~StEtaPhiCell()
{

}

// StEtaPhiCell* StEtaPhiCell::clone() const
// {
//   return new StEtaPhiCell(*this);
// }

void StEtaPhiCell::add(const StProtoJet& pj)
{
  mEt += pj.eT();
  mProtoJet.add(pj);
  mUpToDate = false;
}

void StEtaPhiCell::clear()
{
  mEt = 0;
  mNtimesUsed = 0;
  mCells.clear();
  mProtoJet.clear();
  mUpToDate = false;
}

void StEtaPhiCell::add(StEtaPhiCell* cell)
{
  mEt += cell->eT();
  mCells.push_back(cell);
  cell->setNtimesUsed( cell->nTimesUsed() + 1 );
  mUpToDate=false;
}

double StEtaPhiCell::distance(const StEtaPhiCell& rhs) const
{
  double dEta = deltaEta(rhs);
  double dPhi = deltaPhi(rhs);
  return ::sqrt(dEta*dEta + dPhi*dPhi);
}

const StProtoJet& StEtaPhiCell::centroid()
{
  if (!mUpToDate) {

    mUpToDate = true;
    mCentroid.clear();

    //make sure that we're up-to-date
    mProtoJet.update();
    //don't forget to add self-contribution
    mCentroid = mProtoJet;

    //loop on daughter cells:
    for (CellList::iterator it1 = mCells.begin(); it1 != mCells.end(); ++it1) {

      if ( ((*this)==(*it1))==false ) { //don't add to self!
				
	//for each cell, get proto-jet four-list
	StProtoJet& pj = (*it1)->protoJet();
	const StProtoJet::FourVecList& l = pj.list();
				
	//loop on four-list
	for (StProtoJet::FourVecList::const_iterator it2=l.begin(); it2!=l.end(); ++it2) {
	  // add 4-momenta
	  const AbstractFourVec* vec = *it2;
	  mCentroid.add(*vec);
	}
      }
    }
  }
  return mCentroid;
}


