#include <iostream>
#include "StiHit.h"
#include "Messenger.h"
#include "StiDetector.h"
#include "StiTrackNode.h"

ostream& operator<<(ostream& os, const StiDetector& d);

const StiDetector * StiTrackNode::getDetector() const
{
	return( hit==0 ? detector : hit->detector() );
}

void StiTrackNode::setDetector(const StiDetector *pDetector)
{
	detector = pDetector; 
	if (detector!=0) 
		*(Messenger::instance(MessageType::kNodeMessage)) 
			<< "StiTrackNode::setDetector() - Detector set to :" << *detector << endl;
	else 
		*(Messenger::instance(MessageType::kNodeMessage)) 
			<< "StiTrackNode::setDetector() - Fatal Error - Detector improperly set to '0'" << endl;
}


//_____________________________________________________________________________
void StiTrackNode::reset()
{ 
    StiDefaultMutableTreeNode::reset();
    hit      = 0;
    detector = 0;
}

//_____________________________________________________________________________
void StiTrackNode::addChild(StiTrackNode * newChild)
{	
	newChild->setParent(this);
	children.push_back(newChild);
}
