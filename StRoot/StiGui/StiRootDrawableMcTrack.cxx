/// \file StiRootDrawableKalmanTrack.cxx
/// \author Claude A Pruneau, Wayne State University
#include <stdexcept>
#include "StThreeVectorF.hh"
#include "StiGui/StiRootDrawableMcTrack.h"
#include "StMcTrack.hh"
#include "StMcContainers.hh"
#include "StMcTpcHit.hh"
#include "Sti/StiHit.h"

StiRootDrawableMcTrack::StiRootDrawableMcTrack()
  : StiMcTrack(),
    StiRootDrawableTrack()
{}

StiRootDrawableMcTrack::~StiRootDrawableMcTrack()
{}
/*
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
}*/

void StiRootDrawableMcTrack::reset()
{
  this->StiMcTrack::reset();
  this->StiRootDrawableTrack::reset();
}

void StiRootDrawableMcTrack::draw()
{
  clear();
  sort(_hits.begin(), _hits.end(), StizHitLessThan());
  for (vector<StiHit*>::const_iterator i=_hits.begin();i!=_hits.end();++i)
    {
    StiHit & hit = **i;    
    push_back(hit.x_g());
    push_back(hit.y_g());
    push_back(hit.z_g());
   // cout << " draw mc track hit:" <<  hit.x_g() <<" " << hit.y_g()<<" " <<hit.z_g()<<endl;
    }
 _line->SetPolyLine((size()/3)-1, &(this->operator[](0)));
 _line->Draw();
}
