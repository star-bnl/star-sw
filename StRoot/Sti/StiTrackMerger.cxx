//StiTrackMerger.cxx
//M.L. Miller (Yale Software)
//12/01

//std
#include "Stiostream.h"
#include <cmath>
#include <algorithm>
using namespace std;

//Sti
#include "StiKalmanTrack.h"
#include "StiTrackContainer.h"
#include "StiKalmanTrackNode.h"
#include "StiTrackMerger.h"

StiTrackMerger::StiTrackMerger(StiTrackContainer* store)
  : mTrackStore(store)
{
    cout <<"StiTrackMerger::StiTrackMerger()"<<endl;
}

StiTrackMerger::~StiTrackMerger()
{
  cout <<"StiTrackMerger::~StiTrackMerger()"<<endl;
}
