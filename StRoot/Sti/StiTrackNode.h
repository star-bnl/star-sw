#ifndef StiTrackNode_H
#define StiTrackNode_H 1
#include <iostream.h>
#include <stdlib.h>
#include "StiObjectFactory.h"
#include "StiDefaultMutableTreeNode.h"

class StiHit;
class StiDetector;

class StiTrackNode : public StiDefaultMutableTreeNode
{
 public:

  void reset();
  void set(int depth, StiHit * h);
  void setAsCopyOf(const StiTrackNode * node);
  void     setHit(StiHit * h)  {  
    hit = h;  
    if(hit!=NULL){ detector = NULL; }
  }
  StiHit * getHit() const      {  return hit;}
  friend ostream& operator<<(ostream& os, const StiTrackNode& n);

  StiDetector *getDetector() const;
  void setDetector(StiDetector *pDetector);

 protected:   

  StiHit      * hit;  
  StiDetector * detector; // used if not hit for node
};

typedef StiObjectFactory<StiTrackNode>   StiTrackNodeFactory;



#endif



