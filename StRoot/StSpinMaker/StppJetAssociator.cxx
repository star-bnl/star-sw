//StppJetAssociator.cxx
//M.L. Miller (Yale Software)
//11/02

//std
#include <iostream>
#include <algorithm>

//StSpinMaker
#include "StJet.h"

//local
#include "StppJetAssociator.h"

double deltaphi(double p1, double p2);

AssocJetPair::AssocJetPair(StJet* j1, StJet* j2)
    : mJet1(j1), mJet2(j2),
      mDeltaPhi(deltaphi(j1->Phi(), j2->Phi())), mDeltaEta(j1->PseudoRapidity() - j2->PseudoRapidity() ),
    mDistance( sqrt(mDeltaEta*mDeltaEta + mDeltaPhi*mDeltaPhi) )
{
    //cout <<"AssocJetPair::AssocJetPair)()"<<endl;
    //cout <<"j1.eta: "<<j1->eta<<"\tj2.eta: "<<j2->eta<<"\tdEta: "<<mDeltaEta<<"\t"
    //<<"j1.phi: "<<j1->phi<<"\tj2.phi: "<<j2->phi<<"\tdPhi: "<<mDeltaPhi<<"\t"
    //<<"distance: "<<mDistance<<endl;
}

StppJetAssociator::StppJetAssociator() : mAssocDistance(-999.)
{
    clear();
}

void StppJetAssociator::clear()
{
    mClusterKeyedMap.clear();
    mConeKeyedMap.clear();
}

void StppJetAssociator::associate(StJetVec::iterator ktBegin, StJetVec::iterator ktEnd,
				  StJetVec::iterator coneBegin, StJetVec::iterator coneEnd)
{
    if (mAssocDistance == -999.) {
	cout <<"StppJetAssociator::associate(). ERROR:\t"
	     <<"distance measure is not set.  abort."<<endl;
	abort();
    }
    
    for (StJetVec::iterator it1=ktBegin; it1!=ktEnd; ++it1) {
	StJet* ktjet = *it1;
	
	for (StJetVec::iterator it2 = coneBegin; it2!=coneEnd; ++it2) {
	    StJet* conejet = *it2;
	    
	    AssocJetPair ktKeyedPair(ktjet, conejet);
	    if (ktKeyedPair.distance() < mAssocDistance) {
		
		//add to kt-keyed map:
		JetPairMap::value_type ktTemp(ktjet, ktKeyedPair);
		mClusterKeyedMap.insert( ktTemp );

		//add to cone-keyed map:
		AssocJetPair coneKeyedPair(conejet, ktjet);
		JetPairMap::value_type coneTemp(conejet, coneKeyedPair);
		mConeKeyedMap.insert( coneTemp );
	    }
	    
	}
    }
}
