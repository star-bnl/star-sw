//StiTrackContainer.cxx
//M.L. Miller (Yale Software)
//05/05

#include <iostream>
#include <algorithm>
#include "StiTrack.h"
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

void StiTrackContainer::push_back(StiTrack * val)
{
    mvec.push_back(val);
}

void StiTrackContainer::clear()
{
    mvec.clear();
}

//Delete pointers, set to zero
void StiTrackContainer::clearAndDestroy()
{
    for (stitrackvec::iterator it=mvec.begin(); it!=mvec.end(); ++it) {
	delete (*it);
	(*it)=0;
    }
}

unsigned int StiTrackContainer::size() const
{
    return mvec.size();
}

StiTrackContainer::stitrackvec::const_iterator StiTrackContainer::begin() const
{
    return mvec.begin();
}

StiTrackContainer::stitrackvec::const_iterator StiTrackContainer::end() const
{
    return mvec.end();
}

StiTrackContainer::stitrackvec::const_reverse_iterator StiTrackContainer::rbegin() const
{
    return mvec.rbegin();
}

StiTrackContainer::stitrackvec::const_reverse_iterator StiTrackContainer::rend() const
{
    return mvec.rend();
}

