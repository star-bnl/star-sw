#ifndef StiTrackNode_H
#define StiTrackNode_H 1

#include <iostream.h>
#include <stdlib.h>

#include "StiDefaultMutableTreeNode.h"
#include "StiHit.h"
class StiDetector;

class StiTrackNode : public StiDefaultMutableTreeNode
{ 
public:
    
  ~StiTrackNode(){};    
  void reset();
  void setAsCopyOf(const StiTrackNode * node);
  StiHit * getHit() const;
  void setHit(StiHit* hit);
  void add(StiTrackNode * node);
  const StiDetector *getDetector() const; 
  void setDetector(const StiDetector *detector);

  //friend ostream& operator<<(ostream& os, const StiTrackNode& n);
 protected:   
  StiTrackNode();    
  StiHit * _hit; 
  const StiDetector * _detector; 
};

inline StiTrackNode::StiTrackNode()
  :  _hit(0),
     _detector(0)
{}
	
inline StiHit * StiTrackNode::getHit() const 
{
  return _hit;
}

inline void StiTrackNode::setHit(StiHit* hit)
{
  _hit = hit;
}

inline void StiTrackNode::setAsCopyOf(const StiTrackNode * node)
{
  StiDefaultMutableTreeNode::setAsCopyOf(node);
  _hit = node->_hit;
}


inline void StiTrackNode::reset()
{ 
  StiDefaultMutableTreeNode::reset();
  _hit      = 0;
  _detector = 0;
}

inline void StiTrackNode::add(StiTrackNode * node)
{	
  node->setParent(this);
  children.push_back(node);
}

inline const StiDetector * StiTrackNode::getDetector() const
{
  return( _hit==0 ? _detector : _hit->detector() );
}

inline void StiTrackNode::setDetector(const StiDetector *detector)
{
  _detector = detector; 
}

#endif



