//StiSeedFinder.cxx
//M.L. Miller (Yale Software)
//03/01

//std
#include <iostream.h>

//sti
#include "StiSeedFinder.h"

StiSeedFinder::StiSeedFinder(StiHitContainer* hc)
    : mFactory(0), mMessenger(*(Messenger::instance(MessageType::kSeedFinderMessage))),
      mHitStore(hc)
{
    mMessenger <<"StiSeedFinder::StiSeedFinder()"<<endl;
}

StiSeedFinder::~StiSeedFinder()
{
    mMessenger <<"StiSeedFinder::~StiSeedFinder()"<<endl;
}





