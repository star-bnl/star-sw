// $Id: StProtoJet.cxx,v 1.7 2008/04/17 20:12:05 tai Exp $

#include "StProtoJet.h"

using namespace std;

StProtoJet::StProtoJet()
{
}

StProtoJet::StProtoJet(AbstractFourVec* vec)
{
    mList.push_back(vec);
    StFourVec::operator=(*vec);
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
	StFourVec::add(*vec);
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
    for (FourVecList::const_iterator it=rhs.mList.begin(); it!=rhs.mList.end(); ++it) {
	mList.push_back(*it);
    }
}

void StProtoJet::merge(const StProtoJet& rhs)
{
    //First copy the Particls from rhs to this:
    for (FourVecList::const_iterator it=rhs.mList.begin(); it!=rhs.mList.end(); ++it) {
	mList.push_back(*it);
	StFourVec::add(**it);
    }
	
    //Now calculate the new jet characteristics 	
    // ala Ellis-Soper
    /*
      double et_k = mEt + rhs.mEt;
      mEta= (mEt*mEta + rhs.mEt*rhs.mEta)/et_k;
      mPhi = (mEt*mPhi + rhs.mEt*rhs.mPhi)/et_k;
      mEt = et_k;
    */
	
}

/*!
  vector<int> StProtoJet::tracks(void)
  {
  vector<int> trackvec;
  for(FourVecList::const_iterator it=mList.begin(); it!=mList.end(); ++it)
  trackvec.push_back(static_cast<StMuTrackFourVec*>(*it)->getIndex());
  return trackvec;
  }
*/
