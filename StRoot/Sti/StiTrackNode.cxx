#include <iostream>
#include "StiHit.h"
#include "StiDetector.h"
#include "StiTrackNode.h"


const StiDetector * StiTrackNode::getDetector() const{
  return( hit==NULL ? detector : hit->detector() );
}
void StiTrackNode::setDetector(const StiDetector *pDetector){
  if(hit==NULL){ detector = pDetector; }
}


//_____________________________________________________________________________
void StiTrackNode::reset()
{ 
  StiDefaultMutableTreeNode::reset();
  hit      = 0;
}


//_____________________________________________________________________________
void StiTrackNode::set(int depth, StiHit * h)
{
  StiDefaultMutableTreeNode::set(depth);
  hit = h;
}

//_____________________________________________________________________________
void StiTrackNode::setAsCopyOf(const StiTrackNode * node)
{
  StiDefaultMutableTreeNode::setAsCopyOf(node);
  hit = node->hit;
}


