/// \file StiRootDrawableKalmanTrack.cxx
/// \author M.L. Miller (Yale Software)
/// \author Claude A Pruneau, Wayne State University
#include <stdexcept>
#include <iostream.h>
#include <algorithm>
using namespace std;
#include "StEventTypes.h"
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "Sti/StiKalmanTrackNode.h"
#include "Sti/StiMapUtilities.h"
#include "Sti/StiKTNIterator.h"
#include "StiRootDisplayManager.h"
#include "StiRootDrawableKalmanTrack.h"
using std::sort;

StiRootDrawableKalmanTrack::StiRootDrawableKalmanTrack()
    : StiKalmanTrack(),
      StiRootDrawableTrack()
{}

StiRootDrawableKalmanTrack::~StiRootDrawableKalmanTrack()
{}

StiKalmanTrackNode * StiRootDrawableKalmanTrack::add(StiHit *h,double alpha, double eta, double curvature, double tanl)
{
  //cout<<"StiRootDrawableKalmanTrack::add(StiHit *h,...) -I- Started"<<endl;
  StiKalmanTrackNode * node = this->StiKalmanTrack::add(h,alpha,eta,curvature,tanl);
  this->StiRootDrawableTrack::add(h->x_g(),h->y_g(),h->z_g());
  return node;
}

StiKalmanTrackNode * StiRootDrawableKalmanTrack::add(StiKalmanTrackNode * node)
{
  //cout<<"StiRootDrawableKalmanTrack::add(StiHit *h,...) -I- Started"<<endl;
  StiKalmanTrackNode * aNode = this->StiKalmanTrack::add(node);
  this->StiRootDrawableTrack::add(node->x_g(), node->y_g(), node->z_g());
  return aNode;
}

void StiRootDrawableKalmanTrack::reset()
{
  this->StiKalmanTrack::reset();
  this->StiRootDrawableTrack::reset();
}
