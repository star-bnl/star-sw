//Std
#include <stdexcept>
#include <math.h>

#include "StDetectorId.h"
#include "StHit.h"

//Sti
#include "StiTrackFinder.h"
#include "StiHit.h"
#include "StiDefaultMutableTreeNode.h"
#include "StiKalmanTrackNode.h"
#include "StiKalmanTrack.h"
#include "StiKTNIterator.h"
#include "StiDetector.h"
#include "StiPlacement.h"

ostream& operator<<(ostream&, const StiHit&);

Factory<StiKalmanTrackNode>* StiKalmanTrack::trackNodeFactory = 0;
StiKalmanTrackFinderParameters* StiKalmanTrack::pars = 0;

int debugCount=0;

/*! 
   Reset the class members to their default state.
   This method is called by the ctor of the class to initialize the
   members of the class to an "empty" or null track state. The
   method must also be called everytime an instance of this class is
   retrieved from its factory in order to set the first and last
   nodes to "null" thus guaranteeing that the track object is empty
   i.e. does not represent any track and is thus ready for a new
   search and reconstruction.  
 */
void StiKalmanTrack::reset()
{
  firstNode = 0;
  lastNode  = 0;
  trackingDirection = kOutsideIn;
  fittingDirection  = kOutsideIn;
  mSeedHitCount = 0;
  m      = -1.;
  mFlag  = 0;
}

 

/*! 
  Set the factory used for the creation of kalman track nodes.
  \see StiKalmanTrackNodeFactory
*/
void StiKalmanTrack::setKalmanTrackNodeFactory(Factory<StiKalmanTrackNode>* val)
{
  trackNodeFactory = val;
}


/*!
  Add a hit to this track.
  <p>
  If the current lastNode is non null, 
  <ol>
  <li>Insert the given hit in a StiKalmanTrackNode instance.</li>
  <li>Add the new node as a child to the current last node.</li>
  <li>Make the new node the last node of this track.</li>
  </ol>
  else
  <ol>
  <li>Insert the given hit in a StiKalmanTrackNode instance.</li>
  </ol>
*/
StiKalmanTrackNode * StiKalmanTrack::add(StiHit *h,double alpha, double eta, double curvature, double tanl)
{
  TRACKMESSENGER << "StiKalmanTrack::add(...) -I- Started"<<endl;
  StiKalmanTrackNode * n = trackNodeFactory->getInstance();
  TRACKMESSENGER << "StiKalmanTrack::add(...) -I- have n"<<endl;
  if (!n)
    {
      TRACKMESSENGER << "StiKalmanTrack::add(...) -E- n==0"<<endl;
      throw runtime_error("StiKalmanTrack::add(...) -F- n==0");
    }
  TRACKMESSENGER << "StiKalmanTrack::add(...) -I- have valid n"<<endl;
  n->initialize(h,alpha,eta,curvature,tanl);
  if (lastNode!=0)
    lastNode->add(n);
  else 
    firstNode = n;
  lastNode = n;
  TRACKMESSENGER << "StiKalmanTrack::add(...) -I- Done"<<endl;
  return lastNode;
}

// remove the given hit (and node) from this track
// It is assume that the hit has at most one child
void StiKalmanTrack::removeHit(StiHit *h)
{
  StiKalmanTrackNode * n = findHit(h);
  if (n!=0)
    {
      // the hit belongs to this track, let's remove it
      StiKalmanTrackNode * cn = static_cast<StiKalmanTrackNode *> (n->getFirstChild());
      if (cn==0)
	{
	  // no child, this is the last hit
	  StiKalmanTrackNode * pn = static_cast<StiKalmanTrackNode *> (n->getParent());
	  if (pn==0)
	    firstNode = 0;
	  else
	    pn->remove(n);
	}
      else
	{
	  // child exist
	  StiKalmanTrackNode * pn = static_cast<StiKalmanTrackNode *> (n->getParent());
	  if (pn==0)
	    {
	      // no parent, this is the first hit
	      cn->setParent(0); 
	      firstNode = cn;
	    }
	  else
	    {
	      pn->remove(n);
	      pn->add(cn);
	    }
	}
    }
  else
    throw logic_error("StiKalmanTrack::removeHit() - Error - Given hit does not belong to this track");
}

/*!
  Current implementation only considers the first child of each node
  and must therefore be revised.
*/
StiKalmanTrackNode * StiKalmanTrack::findHit(StiHit * h)
{
  if (firstNode==0)
    return 0;
  else
    {
      if (h==firstNode->getHit())
	return firstNode;
      StiKalmanTrackNode * n = firstNode;
      while (n->getChildCount()>0)
	{
	  n = static_cast<StiKalmanTrackNode *> (n->getFirstChild());
	  if (h==n->getHit())
	    return firstNode;
	}
    }
  return 0;
}

/*! Initialization of this kalman track from external parameters.
  <p>
  This track object is initialized on the basis of parameters determined externally. The
  parameters consist of the track curvature, the tangent of pitch angle, the origin of 
  the helix, and a vector of hits already associated with the track.
  
  <h3>Arguments:</h3>
  <TABLE BORDER="0" CELLPADDING="2" CELLSPACING="0" WIDTH="100%">
  <TR> <TD WIDTH="10%">curvature</TD> <TD WIDTH="90%">1/radius of the tack.</TD>  </TR>
  <TR> <TD WIDTH="10%">tanl</TD>      <TD WIDTH="90%">tan(pitch angle)</TD> </TR>
  <TR> <TD WIDTH="10%">origin</TD>    <TD WIDTH="90%">origin of the track in global coordinates.</TD> </TR>
  <TR> <TD WIDTH="10%">v</TD>         <TD WIDTH="90%">vector of hits associated with this track.</TD> </TR>
</TABLE>
<h3>Algorithm:</h3>
<ol>
<li>Verify that a valid node factory exists.</li>
<LI>Use local arrays state and error to add and set all nodes of this track.</LI>
<LI>Use the same curvature, and tanl for all nodes as supplied in argument list.</li>
<li>Use Unit matrix for error matrix.</li>
<li>Loop over all hits of the input hit vector and create a track node for each.</LI>
<li>Paramters of the track node are set according to the y,z of the hits added.</LI>
<li>Hits given are transformed in the local coordinates of their detector.
</ol>
<h3>Notes:</h3>
<OL>
<LI>Throws a logic_error exception if no track node factory is available.</li>
<LI>Throws a logic_error exception if the factory
  is not a castable to a factory of StiKalmanTrackNode.</li>
<li>Throws a logic error exception if hits do not have a valid pointer to a detector object.</li>
</OL>
*/
void StiKalmanTrack::initialize(double curvature,
				double tanl,
				const StThreeVectorD& origin,
				const hitvector & hits)
{
  TRACKMESSENGER << "StiKalmanTrack::initialize() -I- Started -----------------------------"<<endl;
  reset();
  hitvector::const_iterator it;
  StiKalmanTrackNode * node  = 0;
  double eta   =-99999.;
  double alphaP=-99999.;
  double alpha;
  StThreeVectorD temp;
  const StiDetector* detector;
  TRACKMESSENGER << "StiKalmanTrack::initialize() -I-  -1----------------------------"<<endl;
  for (it=hits.begin(); it!=hits.end(); ++it)
    {
      TRACKMESSENGER << "StiKalmanTrack::initialize() -I-  -2----------------------------"<<endl;
      detector = (*it)->detector();
      TRACKMESSENGER << "StiKalmanTrack::initialize() -I-  -3----------------------------"<<endl;

      if (!detector) 
	{
	  cout <<"StiKalmanTrack::initialize() -F- detector==0"<<endl;
	  throw logic_error("StiKalmanTrack::initialize() - FATAL - Hit has null detector.");
	}
      // if alpha is same, avoid recalculating eta
      TRACKMESSENGER << "StiKalmanTrack::initialize() -I-  -4----------------------------"<<endl;
      alpha = detector->getPlacement()->getNormalRefAngle();
      if (alphaP!=alpha)
	{
	  TRACKMESSENGER << "StiKalmanTrack::initialize() -I-  -4a----------------------------"<<endl;

	  temp = origin;
	  temp.rotateZ(-alpha);
	  eta = curvature*temp.x();
	  alphaP=alpha;
	}
      TRACKMESSENGER << "StiKalmanTrack::initialize() -I-  -5----------------------------"<<endl;
      TRACKMESSENGER << *add((*it),alpha,eta,curvature,tanl);
    }
  TRACKMESSENGER << "StiKalmanTrack::initialize() -I- Done -----------------------------"<<endl;
}

StiKalmanTrackNode * StiKalmanTrack::getNodeNear(double x) const
{
  if (firstNode==0) throw logic_error("StiKalmanTrack::getNodeNear() - FATAL - firstNode==0");
  StiDefaultMutableTreeNodeVector* nodes  = firstNode->breadthFirstEnumeration();
  double minDist  = 1.E10;
  double xx, diff;
  StiKalmanTrackNode * bestNode = firstNode;
  StiDefaultMutableTreeNodeIterator it;
  for (it=nodes->begin(); it!=nodes->end(); it++)
    {
      StiKalmanTrackNode * node = static_cast<StiKalmanTrackNode *>(*it);
      xx = node->_x;
      diff = xx-x; if (diff<0) diff = -diff;
      //TRACKMESSENGER << "===> x/diff:" << xx << "\t" << diff << endl;
      if (diff<minDist) 
	{
	  minDist = diff;
	  bestNode = node;
	}
    }
  delete nodes;
  return bestNode;
}

/*!
  Returns the hit position associated with the node nearest to the given "x" value.
*/
StThreeVector<double>
StiKalmanTrack::getHitPositionNear(double x) const
{
  StiKalmanTrackNode * node = getNodeNear(x);
  if (node==0)
    throw logic_error("StiKalmanTrack::getHitPositionNear(double x) - ERROR - node==0");
  StiHit * hit = node->getHit();
  if (hit==0)
    throw runtime_error("StiKalmanTrack::getHitPositionNear(double x) - ERROR - hit==0");
  StThreeVectorF pos = hit->globalPosition();
  return StThreeVector<double>( pos.x(), pos.y(), pos.z() );
}

/// Find and return the nearest track point in the local coordinates
/// of the detector this track lies in.
StThreeVector<double> StiKalmanTrack::getPointNear(double x) const
{
  StiKalmanTrackNode * node = getNodeNear(x);
  if (node==0) throw logic_error("StiKalmanTrack::getPointNear(double x) - ERROR - node==0");
  return node->getPoint();
}

StThreeVector<double> StiKalmanTrack::getGlobalPointNear(double x) const
{
  StiKalmanTrackNode * node = getNodeNear(x);
  if (node==0) throw logic_error("StiKalmanTrack::getGlobalPointNear(double x) - ERROR - node==0");
  return node->getGlobalPoint();
}

// returns a point along the track in global coordinates at radius "x"
// Note that if the track does not pass at or near the given x value
// the null vector is returned.
//
// Algorithm: One first look for the node closest to "x". A "fresh"
// node is obtained from the factory, and its state is set to that
// of the found node. Then the fresh node is "propagated" to the 
// desired "x" position. A null vector (i.e. [0,0,0]) is returned
// if anything fails...
StThreeVector<double> StiKalmanTrack::getGlobalPointAt(double x) const
{
  StiKalmanTrackNode * nearNode = getNodeNear(x);
  if (nearNode==0) throw logic_error("StiKalmanTrack::getGlobalPointAt(double x) - ERROR - nearNode==0");
  Factory<StiKalmanTrackNode> * f = static_cast<Factory<StiKalmanTrackNode>*>(trackNodeFactory);
  if (f==0) throw logic_error("StiKalmanTrack::getGlobalPointAt(double x) - ERROR - no factory f==0");
  StiKalmanTrackNode * n = f->getInstance();
  if (n==0) throw logic_error("StiKalmanTrack::getGlobalPointAt(double x) - ERROR - n==0");
  n->reset();
  n->setState(nearNode);
  int status = n->propagate(x,0);
  if (status<0) throw runtime_error(" StiKalmanTrack::getGlobalPointAt() - WARNING - Position not reachable by this track");
  return n->getGlobalPoint();
}

StThreeVector<double> StiKalmanTrack::getMomentumNear(double x)
{
  StiKalmanTrackNode * node = getNodeNear(x);
  double p[3];
  double e[6];
  node->getMomentum(p,e);
  StThreeVector<double> p3(p[0],p[1],p[2]);
  p3.rotateZ(node->_alpha);
  return p3;
}

StThreeVector<double> StiKalmanTrack::getMomentumAtOrigin() const
{
  double px,py,pz;
  px=py=pz=0;
  StiKalmanTrackNode * inner = getInnerMostNode();
  if (inner==0)throw logic_error("StiKalmanTrack::getMomentumAtOrigin() - ERROR - No node");
  inner->propagate(0.,0);
  double p[3];
  double e[6];
  inner->getMomentum(p,e);
  StThreeVector<double> p3(p[0],p[1],p[2]);
  p3.rotateZ(inner->_alpha);
  return p3;
}

/*! Return the track sign
   <h3>Notes</h3> 
   <ol>
   <li>Use the last node and the field.</li>
   </ol>
*/
int StiKalmanTrack::getCharge() const
{
  return  (lastNode->getCurvature()*pars->field>0)?-1:1;
}

/*! Return the track chi2
   <h3>Notes</h3> 
   <ol>
   <li>Use the chi2 held by the last hit node used in the fit.</li>
   </ol>
*/
double  StiKalmanTrack::getChi2() const
{
  if (fittingDirection==kOutsideIn)
    {
      if (trackingDirection==kOutsideIn)
	return lastNode->_chi2;
      else
	return firstNode->_chi2;
    }
  else // insideOut
    {
      if (trackingDirection==kOutsideIn)
	return firstNode->_chi2;
      else
	return lastNode->_chi2;			
    }
}


/*! 
	Calculate and return the number of hits on this track. 
   <h3>Notes</h3> 
   <ol>
   <li>Iterate through all nodes of this track.</li>
   <li>Count number of hits.</li>
   </ol>
	 \return number of hits.
*/
int StiKalmanTrack::getPointCount() const
{
	int nPts = 0;
	if (firstNode)
		{
		  StiKTNBidirectionalIterator it;
		  for (it=begin();it!=end();it++)
		    {
		      if ((*it).getHit())
						nPts++;
		    }
		}
	return nPts;
}

/*! Calculate and return the maximum possible number of hits on this track. 
  <h3>Notes</h3> 
   <ol>
   <li>Iterate through all nodes of this track.</li>
   <li>Count active layers.</li>
   <li>Use the (y,z) position of the node to determine whether point is on
       active region of the detector i.e. RDO were functional.</li>
   </ol>
	 \return maximum number of points
*/
int StiKalmanTrack::getMaxPointCount() const
{
  int nPts = 0;
  if (firstNode)
    {
      StiKTNBidirectionalIterator it;
      for (it=begin();it!=end();it++)
	{
	  const StiDetector * detector = (*it).getDetector();
	  if (detector)
	    {
	      if (detector->isActive((*it)._p0,(*it)._p1))
		nPts++;
	    }
	  else
	    nPts++; // vertex have no detector...
	}
    }
  return nPts;
}


/*! Return the number of gaps (active layers with no hits) along this track.
  <h3>Notes</h3> 
  <ol>
  <li>A gap consists of one or multiple contiguous active layers through which this track
  passes.</li>
  <li>There can be gaps on the inside or the outside of the track if no hits are found there.</li>
  </ol>
  \returns number of gaps.
*/
int    StiKalmanTrack::getGapCount()    const  
{
  int gaps = 0;
  if (firstNode)
    {
      StiKTNBidirectionalIterator it;
      bool inGap = false;
      for (it=begin();it!=end();it++)
	{
	  const StiDetector * detector = (*it).getDetector();
	  if (detector && detector->isActive())
	    {
	      if ((*it).getHit())
		{
		  if (inGap) 
		    inGap = false;
		}
	      else
		{
		  if (!inGap)
		    {
		      inGap = true;
		      gaps++;
		    }										
		}
	    }
	}
    }
  return gaps;
}

/*! Return the number of hits (points) used in the fit of this track.
  <h3>Notes</h3> 
  <ol>
  <li>Currently no difference is made between points on the track and fit points 
  on the track.</li>
  <li>Call "getPointCount()" to get the count.</li>
  </ol>
  \return number of hits on this track.
*/
int StiKalmanTrack::getFitPointCount()    const  
{
  return getPointCount();
}

/*! Calculate and return the track length.
  <h3>Notes</h3> 
   <ol>
   <li>Using helix track model in global reference frame.</li>
   <li>Using only inner most and outer most hits associated with this track.</li>
   </ol>
   \return tracklength
   \throws runtime_error
*/
double StiKalmanTrack::getTrackLength() const
{
  StiKalmanTrackNode * inNode = getInnerMostHitNode();
  const StThreeVectorF &in  = inNode->getHit()->globalPosition();
  const StThreeVectorF &out = getOuterMostHitNode()->getHit()->globalPosition();
  double dx=out.x()-in.x();
  double dy=out.y()-in.y();
  double dz=out.z()-in.z();
  double curvature = inNode->getCurvature();
  double s = 2*asin(sqrt(dx*dx+dy*dy)*curvature/2.)/curvature;
  return sqrt(dz*dz+s*s);
}

/*! Return the inner most hit associated with this track.
   <h3>Notes</h3>
   <ol>
   <li>Throws logic_error exception if firstNode or lastNode are not defined, or if track has no hit.</li>
   <li>Loop through all nodes from end() to begin() (or vice versa if tracking 
       direction is outside-in) and search for node with hit. Return first hit found.</li>
   </ol>
	 \return inner most hit node on this track
	 \throws logic_error
*/
StiKalmanTrackNode * StiKalmanTrack::getOuterMostHitNode()  const
{
  if (firstNode==0 || lastNode==0)
    throw logic_error("StiKalmanTrack::getOuterMostHitNode() - ERROR - firstNode||lastNode==0");
  StiKTNBidirectionalIterator it;
  
  if (trackingDirection==kOutsideIn)
    {
      for (it=begin();it!=end();it++)
	{
	  if ((*it).getHit())
	    return &*it;
	}
    }
  else
    {	
      for (it=end();it!=begin();it--)
	{
	  if ((*it).getHit())
	    return &*it;
	}
    }
  throw logic_error("StiKalmanTrack::getOuterMostHitNode() - ERROR - Track has no hit");
}


/*! Return the inner most hit associated with this track.
   <h3>Notes</h3>
   <ol>
   <li>Throws logic_error exception if firstNode or lastNode are not defined, or if track has no hit.</li>
   <li>Loop through all nodes from begin() to end() (or vice versa if tracking 
       direction is outside-in) and search for node with hit. Return first hit found.</li>
   </ol>
	 \return outer most hit node on this track
*/
StiKalmanTrackNode * StiKalmanTrack::getInnerMostHitNode()   const
{
  if (firstNode==0 || lastNode==0)
    throw logic_error("StiKalmanTrack::getInnerMostHitNode() - ERROR - firstNode||lastNode==0");
  StiKTNBidirectionalIterator it;
  
  if (trackingDirection==kInsideOut)
    {
      for (it=begin();it!=end();it++)
	{
	  if ((*it).getHit())
	    return &*it;
	}
    }
  else
    {	
      for (it=end();it!=begin();it--)
	{
	  if ((*it).getHit())
	    return &*it;
	}
    }
  throw logic_error("StiKalmanTrack::getInnerMostHitNode() - ERROR - Track has no hit");
}

/*! Return true if inner most hit associated with this track is main vertex.
   <h3>Algorithm</h3>
   <ol>
   <li>Find the inner most hit node associated with this tracks.</li>
   <li>Return true if "x" of inner most hit is less than 2 cm.
   </ol>
	 \return true if "x" of inner most hit is less than 2 cm.
*/
bool  StiKalmanTrack::isPrimary() const
{
  return false;
}

/*! Swap the track node sequence inside-out
   <h3>Algorithm</h3>
   <ol>
   <li>Loop through the node sequence starting with the firstNode and invert the parent child relationships.</li>
   <li>Include removal of all children for each node.</li>
   <li>Include change of parent</li>
   <li>Set parent of last node as "0" to complete swap.</li>
   <li>Change the "trackingDirection" flag to reflect the swap.
   </ol>
 */
void StiKalmanTrack::swap()
{
  StiKalmanTrackNode * parent = 0;
  StiKalmanTrackNode * child  = 0;
  StiKalmanTrackNode * grandChild = 0;
  
  parent = firstNode;
  firstNode = lastNode;
  lastNode = parent; 
  if (parent && parent->getChildCount()>0)
    {
      child  = dynamic_cast<StiKalmanTrackNode *>(parent->getFirstChild());
      parent->removeAllChildren();			
      while (child)
	{
	  if (child->getChildCount()>0)
	    {
	      grandChild = dynamic_cast<StiKalmanTrackNode *>(child->getFirstChild());
	      child->removeAllChildren();
	    }
	  else
	    grandChild = 0;
	  child->add(parent);
	  parent = child;
	  child = grandChild;
	}
      // last parent has no parent
      parent->setParent(0);
    }
  if (trackingDirection==kOutsideIn)
    trackingDirection = kInsideOut;
  else
    trackingDirection = kOutsideIn;
}


///return vector of nodes with hits
vector<StiKalmanTrackNode*> StiKalmanTrack::getNodes(StDetectorId detectorId) const
{
  StiKalmanTrackNode* leaf = getLastNode();
  StiKTNForwardIterator it(leaf);
  StiKTNForwardIterator end = it.end();
  //vector<StHit*> hits;
  vector<StiKalmanTrackNode*> nodeVec;
  while (it!=end) {

    //make sure node has a hit
    /*
    const StiKalmanTrackNode * node = &*it;
    StiHit* hit = node.getHit();
    if (hit && hit->detector() != NULL && hit->getEloss()>0.) {
      if(detectorId==kTpcId && strstr(hit->detector()->getName().c_str(), "Tpc")!=NULL)
	{//Tpc Hit requested and found
	nodeVec.push_back(node);
	}
      else if (detectorId==kSvtId && strstr(hit->detector()->getName().c_str(), "Svt")!=NULL)
	{//Svt hit requested and found
	nodeVec.push_back(node);
	}
    }
    */
    const StiKalmanTrackNode& node = *it;
    StiHit* hit = node.getHit();
    if((&node)->getHit()!=hit) TRACKMESSENGER <<"Danger, Will Robinson! Danger!"<<endl;
    if (hit && hit->detector() != NULL && hit->getEloss()>0.) {
      if(detectorId==kTpcId && strstr(hit->detector()->getName().c_str(), "Tpc")!=NULL)
	{//Tpc Hit requested and found
	nodeVec.push_back(const_cast<StiKalmanTrackNode*>(&node));
	}
      else if (detectorId==kSvtId && strstr(hit->detector()->getName().c_str(), "Svt")!=NULL)
	{//Svt hit requested and found
	nodeVec.push_back(const_cast<StiKalmanTrackNode*>(&node));
	}
    }

    ++it;
  }
  
  return nodeVec;
}

///return hits;
vector<StMeasuredPoint*> StiKalmanTrack::stHits() const
{
  StiKalmanTrackNode* leaf = getLastNode();
  StiKTNForwardIterator it(leaf);
  StiKTNForwardIterator end = it.end();
  //vector<StHit*> hits;
  vector<StMeasuredPoint*> hits;
  while (it!=end) {
    const StiKalmanTrackNode& node = *it;
    StiHit* hit = node.getHit();
    if (hit) {
      StMeasuredPoint * stHit = const_cast<StMeasuredPoint*>( hit->stHit() );
      if (stHit)
				hits.push_back(stHit);
    }
    ++it;
  }
  return hits;
}


/*! Prune the track to select the best branch of the tree identified by given leaf node.
  <p>
  The best brach is assumed to be the one given by the leaf "node".
  All siblings of the given node, are removed, and iteratively
  all siblings of its parent are removed from the parent of the
  parent, etc.
*/
void StiKalmanTrack::prune()
{
  StiKalmanTrackNode * node   = lastNode;
  StiKalmanTrackNode * parent = static_cast<StiKalmanTrackNode *>(node->getParent());
  while (parent)
    {
      parent->removeAllChildrenBut(node);
      node = parent;
      parent = static_cast<StiKalmanTrackNode *>(node->getParent());
    }
}

/*! Declare hits associated with given track as used.
  <p>
  Declare hits on the track ending at "node" as used. 
  This method starts with the last node and seeks the
  parent of each node recursively. The hit associated with each
  node (when there is a hit) is set to "used".
*/	
void StiKalmanTrack::reserveHits()
{
  StiKTNForwardIterator it(lastNode);
  for_each( it, it.end(), SetHitUsed() );
}

/*! Extend track to the given vertex.
  <p>
  Attempt an extension of the track  the given vertex. 
  <p>
  <ol>
  <li>Get node from node factory.</li>
  <li>Reset node.</li>
  <li>Propagate the node from given parent node "sNode", to the given vertex using a 
  call to "propagate".</li>
  <li>Evaluate the chi2 of the extrapolated if the vertex is added to the track. Done
  using a call to "evaluateChi2".</li>
  <li>If chi2 is less than max allowed "maxChi2ForSelection", update track parameters
  using the vertex as a measurement and add the vertex to the track as the last node.</li>
  </ol>
  <h3>Notes</h3>
  <ul>
  <li>Throws logic_error if no node can be obtained from the node factory.</li>
  <li>The methods "propagate", "evaluateChi2", and "updateNode" may throw 
  runtime_error exceptions which are NOT caught here...</li>
  </ul>
*/
bool StiKalmanTrack::extendToVertex(StiHit* vertex)
{
  if (trackingDirection==kInsideOut) 
    throw logic_error("SKT::extendToVertex(const StiHit*) - ERROR - Extension to vtx only allowed for OutsideIn");
  double chi2;
  StiKalmanTrackNode * sNode=0;
  StiKalmanTrackNode * tNode=0;
  bool trackExtended = false;
  sNode = lastNode;
  tNode = trackNodeFactory->getInstance();
  if (tNode==0) throw logic_error("SKTF::extendTrackToVertex() - ERROR - tNode==null");
  tNode->reset();
  tNode->propagate(sNode, vertex);
  chi2 = tNode->evaluateChi2(vertex); 
  if (chi2<pars->maxChi2ForSelection)
    {
      tNode->setHit(vertex);
      tNode->setChi2(chi2);
      add(tNode);
      trackExtended = true;
    }
  return trackExtended;
}

bool StiKalmanTrack::find(int direction)
{
  bool trackExtended=false;
  setFlag(0);
  // invoke tracker to find or extend this track
  TRACKMESSENGER<<"StiKalmanTrack::find(int) -I- Outside-in"<<endl;
  if (trackFinder->find(this,kOutsideIn))
    {
      if (debugCount<5) TRACKMESSENGER<<"/fit(InOut);";
      fit(kInsideOut);
      trackExtended = true;
    }		
  // decide if an outward pass is needed.
  const StiKalmanTrackNode * outerMostNode = getOuterMostHitNode();
	/*
		if (outerMostNode->_x<190. )
    {
      // swap the track inside-out in preparation for the outward search/extension
      TRACKMESSENGER<<"StiKalmanTrack::find(int) -I- Swap track"<<endl;
      swap();      
      try
				{
					if (trackFinder->find(this,kInsideOut))
						{
							if (debugCount<20) TRACKMESSENGER << "fit(OutIn)";
							fit(kOutsideIn);             
							trackExtended = true;
						}
				}
			swap();
			setTrackingDirection(kOutsideIn);
			}*/
  reserveHits();
  setFlag(1);
  return trackExtended;
}

void StiKalmanTrack::setParameters(StiKalmanTrackFinderParameters *parameters)
{
  pars = parameters;
}

