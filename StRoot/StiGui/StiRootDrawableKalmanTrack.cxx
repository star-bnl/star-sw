/// \file StiRootDrawableKalmanTrack.cxx
/// \author M.L. Miller (Yale Software)
/// \author Claude A Pruneau, Wayne State University
#include <stdexcept>
#include <Stiostream.h>
#include <algorithm>
using namespace std;
#include "StEventTypes.h"
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "Sti/StiKTNIterator.h"
#include "Sti/StiKalmanTrackNode.h"
#include "Sti/StiMapUtilities.h"
#include "Sti/StiKTNIterator.h"
#include "StiRootDrawableKalmanTrack.h"
using std::sort;

StiRootDrawableKalmanTrack::StiRootDrawableKalmanTrack()
    : StiKalmanTrack(),
      StiRootDrawableTrack()
{}

StiRootDrawableKalmanTrack::~StiRootDrawableKalmanTrack()
{}


//StiKalmanTrackNode * StiRootDrawableKalmanTrack::add(StiHit *h,double alpha, double eta, double curvature, double tanl)
//{
  //cout<<"StiRootDrawableKalmanTrack::add(StiHit *h,...) -I- Started"<<endl;
// StiKalmanTrackNode * node = this->StiKalmanTrack::add(h,alpha,eta,curvature,tanl);
  //this->StiRootDrawableTrack::add(h->x_g(),h->y_g(),h->z_g());
  //return node;
//}

//StiKalmanTrackNode * StiRootDrawableKalmanTrack::add(StiKalmanTrackNode * node)
//{
  //cout<<"StiRootDrawableKalmanTrack::add(StiHit *h,...) -I- Started"<<endl;
//  StiKalmanTrackNode * aNode = this->StiKalmanTrack::add(node);
  //this->StiRootDrawableTrack::add(node->x_g(), node->y_g(), node->z_g(),getTrackingDirection());
//  return aNode;
//}

void StiRootDrawableKalmanTrack::reset()
{
  this->StiKalmanTrack::reset();
  this->StiRootDrawableTrack::reset();
}

void StiRootDrawableKalmanTrack::draw()
{
  clear();
  StiKTNForwardIterator iter(lastNode);
  for (;iter!=0;iter++)
    {
      StiKalmanTrackNode & node = *iter;
      push_back(node.x_g());
      push_back(node.y_g());
      push_back(node.z_g());
    } 
  _line->SetPolyLine((size()/3)-1, &(this->operator[](0)));
 _line->Draw();
}
