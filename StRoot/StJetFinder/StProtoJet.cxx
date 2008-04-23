// $Id: StProtoJet.cxx,v 1.10 2008/04/23 19:24:09 tai Exp $

#include "StProtoJet.h"

using namespace std;

StProtoJet::StProtoJet()
  : mPx(0), mPy(0), mPz(0), mE(0), mCharge(0) 
{

}

StProtoJet::StProtoJet(AbstractFourVec* vec)
{
    mList.push_back(vec);
    mPx = vec->px();
    mPy = vec->py();
    mPz = vec->pz();
    mE  = vec->e();
    mCharge = vec->charge();
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

void StProtoJet::remove(StProtoJet& rhs)
{
    FourVecList& l = rhs.list();

    while (l.empty()==false) {
	FourVecList::iterator where = find(mList.begin(), mList.end(), l.front() );

	if (where==l.end()) {
	    cout <<"StProtoJet::remove(StProtoJet&). ERROR:\t"
		 <<"4-vec not found.  return w/o action"<<endl;
	    return;
	}
	else {
	    l.erase(where);
	}
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
