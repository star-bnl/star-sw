//StiSeedFinder.cxx
//M.L. Miller (Yale Software)
//03/01

//std
#include <iostream.h>

//sti
#include "StiSeedFinder.h"

StiSeedFinder::StiSeedFinder()
    : mFactory(0), mMessenger(*(Messenger::instance(MessageType::kSeedFinderMessage)))
{
    mMessenger <<"StiSeedFinder::StiSeedFinder()"<<endl;
}

StiSeedFinder::~StiSeedFinder()
{
    mMessenger <<"StiSeedFinder::~StiSeedFinder()"<<endl;
}





