//StiTrackContainer.cxx
//M.L. Miller (Yale Software)
//05/05

//std
#include <iostream>
#include <algorithm>
using namespace std;

//Sti
#include "StiKalmanTrack.h"
#include "StiKalmanTrackNode.h"
#include "StiTrackContainer.h"

StiTrackContainer* StiTrackContainer::sinstance = 0;

StiTrackContainer::StiTrackContainer()
{
    cout <<"StiTrackContainer::StiTrackContainer()"<<endl;
    sinstance = this;
}

StiTrackContainer::~StiTrackContainer()
{
    cout <<"StiTrackContainer::~StiTrackContainer()"<<endl;
}
StiTrackContainer* StiTrackContainer::instance()
{
    return (sinstance) ? sinstance : new StiTrackContainer();
}

void StiTrackContainer::kill()
{
    if (sinstance) {
	delete sinstance;
	sinstance = 0;
    }
}

void StiTrackContainer::push_back(StiKalmanTrack* track)
{
    insert(  KalmanTrackMapValType(track, track) );
}

//non-members

bool StiKalmanTrackLessThan::operator()(const StiKalmanTrack* lhs, const StiKalmanTrack* rhs) const
{
    StiKalmanTrackNode* lhsNode = lhs->getLastNode();
    StiKalmanTrackNode* rhsNode = rhs->getLastNode();

    if (lhsNode->fP3==0. || rhsNode->fP3==0.) {
	cout <<"StiKalmanTrackLessThan::operator()(StiKalmanTrack*, StiKalmanTrack*). ERROR:\t"
	     <<"lhsNode->fP3==0. || rhsNode->fP3==0.  Return false"<<endl;
	return false;
    }
    return (1./lhsNode->fP3 < 1./rhsNode->fP3);
}
