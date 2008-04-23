// $Id: StProtoJet.cxx,v 1.11 2008/04/23 20:00:01 tai Exp $

#include "StProtoJet.h"

using namespace std;

StProtoJet::StProtoJet()
  : mPx(0), mPy(0), mPz(0), mE(0)
  , mCharge(0) 
{

}

StProtoJet::StProtoJet(AbstractFourVec* vec)
  : mPx(vec->px()), mPy(vec->py()), mPz(vec->pz()), mE(vec->e())
  , mCharge(vec->charge())
{
    mList.push_back(vec);
}

StProtoJet::~StProtoJet()
{
}

void StProtoJet::update()
{
    mPx = mPy = mPz = mE = mCharge = 0.;
    // add 4-momenta
    for (FourVecList::const_iterator it2=mList.begin(); it2!=mList.end(); ++it2) {
	AbstractFourVec* vec = *it2;
	mPx += vec->px();
	mPy += vec->py();
	mPz += vec->pz();
	mE  += vec->e();
	mCharge += vec->charge();
    }
}

void StProtoJet::add(const StProtoJet& rhs)
{
  mPx += rhs.mPx;
  mPy += rhs.mPy;
  mPz += rhs.mPz;
  mE  += rhs.mE;
  mCharge += rhs.mCharge;
  for (FourVecList::const_iterator it = rhs.mList.begin(); it!=rhs.mList.end(); ++it) {
    mList.push_back(*it);
  }
}

void StProtoJet::add(const AbstractFourVec& rhs)
{
    mPx += rhs.px();
    mPy += rhs.py();
    mPz += rhs.pz();
    mE  += rhs.e();
    mCharge += rhs.charge();
}

void StProtoJet::merge(const StProtoJet& rhs)
{
  for (FourVecList::const_iterator it = rhs.mList.begin(); it != rhs.mList.end(); ++it) {
    mList.push_back(*it);
    mPx += (*it)->px();
    mPy += (*it)->py();
    mPz += (*it)->pz();
    mE  += (*it)->e();
    mCharge += (*it)->charge();
  }
}

void StProtoJet::clear()
{
  mList.clear();
  mPx = mPy = mPz = mE = mCharge = 0;
}
