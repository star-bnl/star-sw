//StiCompositeSeedFinder.cxx
//M.L. Miller (Yale Software)
//03/01

#include <iostream.h>
#include "StiCompositeSeedFinder.h"

StiCompositeSeedFinder::StiCompositeSeedFinder()
{
    cout <<"StiCompositeSeedFinder::StiCompositeSeedFinder()"<<endl;
}

StiCompositeSeedFinder::~StiCompositeSeedFinder()
{
    cout <<"StiCompositeSeedFinder::~StiCompositeSeedFinder()"<<endl;
}

bool StiCompositeSeedFinder::hasMore()
{
    return false;
}

StiKalmanTrack* StiCompositeSeedFinder::next()
{
    StiKalmanTrack* track=0;
    return track;
}

void StiCompositeSeedFinder::build()
{
    return;
}

void StiCompositeSeedFinder::reset()
{
    return;
}
