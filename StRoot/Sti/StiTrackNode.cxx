#include <iostream>
#include "StiHit.h"
#include "StiDetector.h"
#include "StiTrackNode.h"


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


