//StiEvaluator.cxx
// A. Rose (WSU)
// 8/01

//STD
#include <iostream.h>

//Sti includes
#include "Sti/StiTrackContainer.h"

//StiMaker includes
#include "StiEvaluator.h"

StiEvaluator* StiEvaluator::sinstance = 0;

StiEvaluator::StiEvaluator()
{
    cout <<"StiEvaluator::StiEvaluator()"<<endl;
    sinstance = this;
}

StiEvaluator::~StiEvaluator()
{
    cout <<"StiEvaluator::~StiEvaluator()"<<endl;
}

StiEvaluator* StiEvaluator::instance()
{
    return (sinstance) ? sinstance : new StiEvaluator();
}

void StiEvaluator::kill()
{
    if (sinstance) {
	delete sinstance;
	sinstance = 0;
    }
}

void StiEvaluator::speak() const
{
    cout <<"StiEvaluator::speak()"<<endl;
}

void StiEvaluator::evaluateForEvent(const StiTrackContainer* trackStore)
{
    cout <<"\nStiEvaluator::evaluateForEvent()"<<endl;
    cout <<"\tNumber of StiTracks:\t"<<trackStore->size()<<endl;
    //stitrackvec is a typedef that is within the public namespace of StiTrackContainer, so it is not a global typedef
    // it looks like this
    //class StiTrackContainer
    //{
    //public:
    //typedef vector<StiTrack*> stitrackvec; ...
    
    for (StiTrackContainer::stitrackvec::const_iterator it=trackStore->begin(); it!=trackStore->end(); ++it) {
	StiTrack* temp = (*it);
	//Now you've got the track, do what you want with it
    }
}
