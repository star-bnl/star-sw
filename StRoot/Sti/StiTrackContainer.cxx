//StiTrackContainer.cxx
//M.L. Miller (Yale Software)
//05/05

#include <iostream.h>
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
