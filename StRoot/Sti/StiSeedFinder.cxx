//StiSeedFinder.cxx
//M.L. Miller (Yale Software)
//03/01
#include <stdexcept>
#include <iostream.h>
#include "StiSeedFinder.h"
#include "Sti/Base/MessageType.h"
#include "Sti/Base/Messenger.h"

StiSeedFinder::StiSeedFinder(const string& name,
			     Factory<StiKalmanTrack>* trackFactory,
			     StiHitContainer* hitContainer,
			     StiDetectorContainer* detectorContainer)
  : Named(name),
    _trackFactory(trackFactory), 
    _hitContainer(hitContainer),
    _detectorContainer(detectorContainer),
    _messenger(*(Messenger::instance(MessageType::kSeedFinderMessage)) )
{
  _messenger <<"StiSeedFinder::StiSeedFinder() - INFO - Started"<<endl;
  _messenger <<"StiSeedFinder::StiSeedFinder() - INFO - Constructing SeedFinder:"<<_name<<endl; 
  if(!_trackFactory)
    throw runtime_error("StiSeedFinder::StiSeedFinder(...) - FATAL - _trackFactory==0");
  if(!_hitContainer)
    throw runtime_error("StiSeedFinder::StiSeedFinder(...) - FATAL - _hitContainer==0");
  if(!_detectorContainer)
    throw runtime_error("StiSeedFinder::StiSeedFinder(...) - FATAL - _detectorContainer==0");
}

StiSeedFinder::~StiSeedFinder()
{
  _messenger <<"StiSeedFinder::~StiSeedFinder() - INFO - Destroying SeedFinder:"<<_name<<endl;
}





