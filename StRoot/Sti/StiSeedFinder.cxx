//StiSeedFinder.cxx
//M.L. Miller (Yale Software)
//03/01

//std
#include <iostream.h>

//sti
#include "StiSeedFinder.h"

StiSeedFinder::StiSeedFinder()
    : mFactory(0), mBuildPath("empty"), mBuilt(false),
      mMessenger(*(Messenger::instance(kSeedFinderMessage)))
{
    //cout <<"StiSeedFinder::StiSeedFinder()"<<endl;
}

StiSeedFinder::~StiSeedFinder()
{
    //cout <<"StiSeedFinder::~StiSeedFinder()"<<endl;
}





