#include <iostream>
#include "StiHit.h"
#include "StiDetector.h"
#include "StiTrackNode.h"

ostream& operator<<(ostream& os, const StiDetector& d);

const StiDetector * StiTrackNode::getDetector() const
{
	if (hit==0)
		{
			if (detector==0)
				cout << "StiTrackNode::getDetector() - Fatal Error - Detector improperly set to '0'" << endl;
			else
				cout << "StiTrackNode::getDetector() - Detector  :" << *detector << endl;
		}
	else
		{
			StiDetector * d =  hit->detector();
			if (d==0)
				cout << "StiTrackNode::getDetector() - Fatal Error - Node has hit that has no associated detector" << endl;
			else
				cout << "StiTrackNode::getDetector() - Detector associated with hit :" << *d << endl;
		}
  return( hit==0 ? detector : hit->detector() );
}

void StiTrackNode::setDetector(const StiDetector *pDetector){
  //if(hit==0){ 
	detector = pDetector; 
	if (detector!=0)
		cout << "StiTrackNode::setDetector() - Detector set to :" << *detector << endl;
	else
		cout << "StiTrackNode::setDetector() - Fatal Error - Detector improperly set to '0'" << endl;
}


//_____________________________________________________________________________
void StiTrackNode::reset()
{ 
  StiDefaultMutableTreeNode::reset();
  hit      = 0;
	detector = 0;
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


