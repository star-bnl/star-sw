#ifndef StiTrackNode_H
#define StiTrackNode_H 1

#include <iostream.h>
#include <stdlib.h>

#include "StiDefaultMutableTreeNode.h"
#include "Messenger.h"

class StiHit;
class StiDetector;

class StiTrackNode : public StiDefaultMutableTreeNode
{
public:
    
	void reset();
	void set(StiHit * h);
	void setAsCopyOf(const StiTrackNode * node);
	void setHit(StiHit * h); 
	StiHit * getHit() const;
	
	const StiDetector *getDetector() const;
	void setDetector(const StiDetector *pDetector);
	
	virtual void addChild(StiTrackNode * newChild);
	
	//void setDedx(double e) {dedx=e;}
	//double getDedx() const {return dedx;}

	friend ostream& operator<<(ostream& os, const StiTrackNode& n);

 protected:   
	
	StiTrackNode();    
	double   dedx;
	StiHit * hit;  
	const StiDetector * detector;
};

inline StiTrackNode::StiTrackNode(): hit(0), detector(0)
{
}
	
inline StiHit * StiTrackNode::getHit() const 
{
  return hit;
}

inline void StiTrackNode::setHit(StiHit * h)  
{  
	hit = h;  
	if(hit!=0)
		detector = 0;
}

inline void StiTrackNode::set(StiHit * h)
{
	hit = h;
	if(hit!=0)
		detector = 0;
}

inline void StiTrackNode::setAsCopyOf(const StiTrackNode * node)
{
	StiDefaultMutableTreeNode::setAsCopyOf(node);
	hit = node->hit;
	detector = node->detector;
}
#endif



