/// \file StiRootDrawableKalmanTrack.cxx
/// \author Claude A Pruneau, Wayne State University
#include <stdexcept>
#include "StThreeVectorF.hh"
#include "StiGui/StiRootDrawableMcTrack.h"
#include "StMcTrack.hh"
#include "StMcContainers.hh"
#include "StMcTpcHit.hh"

StiRootDrawableMcTrack::StiRootDrawableMcTrack()
  : StiMcTrack(),
    StiRootDrawableTrack()
{}

StiRootDrawableMcTrack::~StiRootDrawableMcTrack()
{}

void StiRootDrawableMcTrack::setStMcTrack(const StMcTrack * mcTrack)
{
  //cout << "StiRootDrawableMcTrack::setStMcTrack(const StMcTrack * mcTrack) -I- Started"<<endl;
  //initialize pointer
  this->StiMcTrack::setStMcTrack(mcTrack);
  //initialize the hits for gui purposes
  const StPtrVecMcTpcHit & tpcHits = mcTrack->tpcHits();
  StMcTpcHitConstIterator hitIter;
  //cout << "StiRootDrawableMcTrack::fillHitsForDrawing() - have tpcHits " << endl;
  for (hitIter= tpcHits.begin();hitIter!=tpcHits.end();hitIter++)
    {
      StThreeVectorF position = (*hitIter)->position();
      add(position.x(), position.y(),position.z());
    }
}

void StiRootDrawableMcTrack::reset()
{
  this->StiMcTrack::reset();
  this->StiRootDrawableTrack::reset();
}

void StiRootDrawableMcTrack::draw()
{
 _line->SetPolyLine((size()/3)-1, &(this->operator[](0)));
 _line->Draw();
}
