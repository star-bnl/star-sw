//StJetEtCell.cxx
//M.L. Miller (Yale Software)
//07/02

//adapted from Akio Ogawa's code

#include "StJetEtCell.h"

StJetEtCell::StJetEtCell(double etaMin, double etaMax, double phiMin, double phiMax)
  : StEtaPhiCell(etaMin, etaMax, phiMin, phiMax)
{

}

StJetEtCell::StJetEtCell()
{

}

StJetEtCell::~StJetEtCell()
{

}

//
//#include <cmath>
//#include <algorithm>
//
//StJetEtCell::StJetEtCell(double etaMin, double etaMax, double phiMin, double phiMax)
//  : mEtaMin(etaMin)
//  , mEtaMax(etaMax)
//  , mPhiMin(phiMin)
//  , mPhiMax(phiMax)
//  , mEt(0.)
//  , mNtimesUsed(0)
//  , mUpToDate(false)
//{
//
//}
//
//StJetEtCell::StJetEtCell()
//  : mEtaMin(0)
//  , mEtaMax(0)
//  , mPhiMin(0)
//  , mPhiMax(0)
//  , mEt(0)
//  , mNtimesUsed(0)
//  , mUpToDate(false)
//{
//
//}
//
//StJetEtCell::~StJetEtCell()
//{
//
//}
//
//void StJetEtCell::add(const StProtoJet& pj)
//{
//  mEt += pj.eT();
//  mProtoJet.add(pj);
//  mUpToDate = false;
//}
//
//void StJetEtCell::clear()
//{
//  mEt = 0;
//  mNtimesUsed = 0;
//  mCells.clear();
//  mProtoJet.clear();
//  mUpToDate = false;
//}
//
//void StJetEtCell::add(StJetEtCell* cell)
//{
//  mEt += cell->eT();
//  mCells.push_back(cell);
//  cell->setNtimesUsed( cell->nTimesUsed() + 1 );
//  mUpToDate=false;
//}
//
//double StJetEtCell::distance(const StJetEtCell& rhs) const
//{
//  double dEta = deltaEta(rhs);
//  double dPhi = deltaPhi(rhs);
//  return ::sqrt(dEta*dEta + dPhi*dPhi);
//}
//
//const StProtoJet& StJetEtCell::centroid()
//{
//  if (!mUpToDate) {
//
//    mUpToDate = true;
//    mCentroid.clear();
//
//    //make sure that we're up-to-date
//    mProtoJet.update();
//    //don't forget to add self-contribution
//    mCentroid = mProtoJet;
//
//    //loop on daughter cells:
//    for (CellList::iterator it1 = mCells.begin(); it1 != mCells.end(); ++it1) {
//
//      if ( ((*this)==(*it1))==false ) { //don't add to self!
//				
//	//for each cell, get proto-jet four-list
//	StProtoJet& pj = (*it1)->protoJet();
//	const StProtoJet::FourVecList& l = pj.list();
//				
//	//loop on four-list
//	for (StProtoJet::FourVecList::const_iterator it2=l.begin(); it2!=l.end(); ++it2) {
//	  // add 4-momenta
//	  const AbstractFourVec* vec = *it2;
//	  mCentroid.add(*vec);
//	}
//      }
//    }
//  }
//  return mCentroid;
//}
//
