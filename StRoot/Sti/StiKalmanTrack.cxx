//Std
#include <stdexcept>
#include <math.h>

#include "StDetectorId.h"
#include "StHit.h"

//Sti
#include "StiKTNIterator.h"
#include "StiHit.h"
#include "StiDefaultMutableTreeNode.h"
#include "StiKalmanTrackNode.h"
#include "StiKalmanTrack.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiKTNIterator.h"
#include "StiGeometryTransform.h"
#include "StiToolkit.h"
#include "StiMaker/StiDefaultToolkit.h"

ostream& operator<<(ostream&, const StiHit&);

StiObjectFactoryInterface<StiKalmanTrackNode>* StiKalmanTrack::trackNodeFactory = 0;
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
void StiKalmanTrack::setKalmanTrackNodeFactory(StiObjectFactoryInterface<StiKalmanTrackNode>* val)
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
StiKalmanTrackNode * StiKalmanTrack::addHit(StiHit *h)
{
  if (lastNode!=0)
    {
      StiKalmanTrackNode * n = trackNodeFactory->getObject();
      n->reset();
      n->setHit(h);
      n->fX = h->x();
      lastNode->add(n);
			lastNode = n;
      return n;
    }
  else 
    {
      firstNode  = trackNodeFactory->getObject(); 
      firstNode->reset();
      firstNode->setHit(h);
      firstNode->fX = h->x();
			lastNode = firstNode;
      return firstNode;
    }
}


StiKalmanTrackNode * StiKalmanTrack::insertHit(StiHit *hInserted, StiHit * targetParent)
{
  // Add a hit to this track right after the given hit
  // Note that this method is slow because it needs to 
  // find the parent hit first...
  // Note that if the targetParent hit is null, it is assumed
  // the intent is to add the hit before the firstNode
  // It is further assumed that the targetParent has at most
  // one child.
  
  StiKalmanTrackNode * n = trackNodeFactory->getObject();
  n->reset();
  n->setHit(hInserted);
  if (targetParent==0)
    {
      if (firstNode!=0)
	{
	  n->add(firstNode);
	  firstNode = n;
	}
      else
	  firstNode = n;
    }
  else
    {
      StiKalmanTrackNode * pn = findHit(targetParent);
      if (pn==0)
	  throw logic_error("SKT::insertHit() - ERROR - Attempted hit insertion after hit which does not belong to this track");
      else
	{
	  StiKalmanTrackNode * cn = static_cast<StiKalmanTrackNode *> (pn->getFirstChild());
	  if (cn!=0)
	    {
	      pn->remove(cn);
	      n->add(cn);
	      pn->add(n);
	    }
	  else
	    {
	      // hit added as lastChild
	      pn->add(n);
	    }
	}
    }
  return n;
}

void StiKalmanTrack::removeHit(StiHit *h)
{
  // remove the given hit (and node) from this track
  // It is assume that the hit has at most one child
  
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

/*! 
	Remove all references to hits from this track be setting the firstNode and lastNode 
	pointers to "0".
	<h3>Note</h3>
	<ol>
	<li>No need to destroy any object since the memory for the nodes and hits is owned 
	by the factory that supply these.</li>
	</ol>
*/
void StiKalmanTrack::removeAllHits()
{
    firstNode = 0;
		lastNode  = 0;
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
<h3>Internal Track Representation: </h3>

<TABLE BORDER="0" CELLPADDING="2" CELLSPACING="0" WIDTH="80%">
  <TR>
    <TD WIDTH="10%">x</TD>
    <TD WIDTH="10%">fX</TD>
    <TD WIDTH="80%">independent variable</TD>
  </TR>
  <TR>
    <TD WIDTH="10%">state[0]</TD>
    <TD WIDTH="10%">fP0</TD>
    <TD WIDTH="80%">y; ordinate at &quot;x&quot;</TD>
  </TR>
  <TR>
    <TD WIDTH="10%">state[1]</TD>
    <TD WIDTH="10%">fP1</TD>
    <TD WIDTH="80%">z; position along beam axis at &quot;x&quot;</TD>
  </TR>
  <TR>
    <TD WIDTH="10%">state[2]</TD>
    <TD WIDTH="10%">fP2</TD>
    <TD WIDTH="80%">eta=C*x0; C == curvature, x0==position of helix center.</TD>
  </TR>
  <TR>
    <TD WIDTH="10%">state[3]</TD>
    <TD WIDTH="10%">fP3</TD>
    <TD WIDTH="80%">C (local) curvature of the track</TD>
  </TR>
  <TR>
    <TD WIDTH="10%">state[4]</TD>
    <TD WIDTH="10%">fP4</TD>
    <TD WIDTH="80%">tan(l)</TD>
  </TR>
  <TR>
    <TD WIDTH="10%">error[0]</TD>
    <TD WIDTH="10%">fC00</TD>
    <TD WIDTH="80%">Error Matrix - Symmetric e.g. fC20=fC02</TD>
  </TR>
  <TR>
    <TD WIDTH="10%">error[1]</TD>
    <TD WIDTH="10%">fC10</TD>
    <TD WIDTH="80%"></TD>
  </TR>
  <TR>
    <TD WIDTH="10%">error[2]</TD>
    <TD WIDTH="10%">fC11</TD>
    <TD WIDTH="80%"></TD>
  </TR>
  <TR>
    <TD WIDTH="10%">error[3]</TD>
    <TD WIDTH="10%">fC20</TD>
    <TD WIDTH="80%"></TD>
  </TR>
  <TR>
    <TD WIDTH="10%">error[4]</TD>
    <TD WIDTH="10%">fC21</TD>
    <TD WIDTH="80%"></TD>
  </TR>
  <TR>
    <TD WIDTH="10%">error[5]</TD>
    <TD WIDTH="10%">fC22</TD>
    <TD WIDTH="80%"></TD>
  </TR>
  <TR>
    <TD WIDTH="10%">error[6]</TD>
    <TD WIDTH="10%">fC30</TD>
    <TD WIDTH="80%"></TD>
  </TR>
  <TR>
    <TD WIDTH="10%">error[7]</TD>
    <TD WIDTH="10%">fC31</TD>
    <TD WIDTH="80%"></TD>
  </TR>
  <TR>
    <TD WIDTH="10%">error[8]</TD>
    <TD WIDTH="10%">fC32</TD>
    <TD WIDTH="80%"></TD>
  </TR>
  <TR>
    <TD WIDTH="10%">error[9]</TD>
    <TD WIDTH="10%">fC33</TD>
    <TD WIDTH="80%"></TD>
  </TR>
  <TR>
    <TD WIDTH="10%">error[10]</TD>
    <TD WIDTH="10%">fC40</TD>
    <TD WIDTH="80%"></TD>
  </TR>
  <TR>
    <TD WIDTH="10%">error[11]</TD>
    <TD WIDTH="10%">fC41</TD>
    <TD WIDTH="80%"></TD>
  </TR>
  <TR>
    <TD WIDTH="10%">error[12]</TD>
    <TD WIDTH="10%">fC42</TD>
    <TD WIDTH="80%"></TD>
  </TR>
  <TR>
    <TD WIDTH="10%">error[13]</TD>
    <TD WIDTH="10%">fC43</TD>
    <TD WIDTH="80%"></TD>
  </TR>
  <TR>
    <TD WIDTH="10%">error[14]</TD>
    <TD WIDTH="10%">fC44</TD>
    <TD WIDTH="80%"></TD>
  </TR>
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
				const hitvector & v)
{
    if (!trackNodeFactory) 
			throw logic_error("StiKalmanTrack::initialize()\tERROR:\tNo Kalman Track Node Factory-Abort");
    StiObjectFactoryInterface<StiKalmanTrackNode>* fac = trackNodeFactory;
    if (!fac) 
			throw logic_error("StiKalmanTrack::initialize(). ERROR:\tFactory cast failed-Abort");
    //StThreeVectorD stiOrigin;
    double alpha,alphaP,eta;  
    hitvector::const_iterator it;
    double state[5];  
    double error[15];
    // These are constant for all hits
    state[3]=curvature;
    state[4]=tanl;
    // For the time being set a diagonal error matrx
    error[0] = 1.;  
    error[1] = 0.; error[2] = 1.;  
    error[3] = 0.; error[4] = 0.; error[5] = 1.;  
    error[6] = 0.; error[7] = 0.; error[8] = 0.;  error[9]  = 1.;  
    error[10]= 0.; error[11] = 0.;error[12] = 0.; error[13] = 0.;  error[14] = 1.;
    // do the transfer here
    StiKalmanTrackNode * node  = 0;
    StiKalmanTrackNode * pNode = 0;
    eta = 0.;
    for (it=v.begin(); it!=v.end(); ++it)
      {
				const StiDetector* layer = (*it)->detector();
				if (!layer) 
					throw logic_error("StiKalmanTrack::initialize() ERROR:\t Hit has null detector.");
				alpha = layer->getPlacement()->getNormalRefAngle();
				node = fac->getObject();
				if (node==0)
					throw logic_error("StiKalmanTrack::initialize() ERROR:\t Null node returned by Node factory");
				node->reset();
				if (pNode==0)
					alphaP = -99999.; // no parent, set crazy value
				else
					alphaP = pNode->fAlpha; // value of the parent
				if (alphaP!=alpha)
					{
						StThreeVectorD temp = origin;
						temp.rotateZ(-alpha);
						eta = curvature*temp.x();
					}
				state[0] = (*it)->y(); 
				state[1] = (*it)->z(); 
				state[2] = eta;
				node->set((*it), alpha, (*it)->x(), state,error, 0., 0.);
				if (pNode==0) 
					firstNode = node;
				else
						pNode->add(node);
				pNode = node;
				lastNode = node;
      }
}

StiKalmanTrackNode * StiKalmanTrack::getNodeNear(double x) const
{
    if (firstNode==0)  // node in this track, return a null state and error
	return 0;
    //cout << "StiKalmanTrack::getNodeNear(" << x << ") called" << endl;
    //cout << *firstNode << endl;
    StiDefaultMutableTreeNodeVector* nodes  = firstNode->breadthFirstEnumeration();
    double minDist  = 1.E10;
    double xx, diff;
    StiKalmanTrackNode * bestNode = firstNode;
    StiDefaultMutableTreeNodeIterator it;
    for (it=nodes->begin(); it!=nodes->end(); it++)
	{
	    StiKalmanTrackNode * node = static_cast<StiKalmanTrackNode *>(*it);
	    xx = node->fX;
	    diff = xx-x; if (diff<0) diff = -diff;
	    //cout << "===> x/diff:" << xx << "\t" << diff << endl;
	    if (diff<minDist) 
		{
		    minDist = diff;
		    bestNode = node;
		}
	}
    delete nodes;
    return bestNode;
}

StThreeVector<double>
StiKalmanTrack::getHitPositionNear(double x) const
{
	/*
		Returns the hit position associated with the node nearest to the given "x" value.
	*/
	StiKalmanTrackNode * node = getNodeNear(x);
	if (node==0)
		throw logic_error("StiKalmanTrack::getHitPositionNear(double x) - ERROR - node==0");
	StiHit * hit = node->getHit();
	if (hit==0)
		throw runtime_error("StiKalmanTrack::getHitPositionNear(double x) - ERROR - hit==0");
	StThreeVectorF pos = hit->globalPosition();
	return StThreeVector<double>( pos.x(), pos.y(), pos.z() );
}

StThreeVector<double> StiKalmanTrack::getPointNear(double x) const
{
	// returns point in local coordinates
	StiKalmanTrackNode * node = getNodeNear(x);
	if (node==0)
		throw logic_error("StiKalmanTrack::getPointNear(double x) - ERROR - node==0");
	return StThreeVector<double>(node->fX, node->fP0, node->fP1);
}

StThreeVector<double> StiKalmanTrack::getGlobalPointNear(double x) const
{
	// returns point in local coordinates
	double xx,yy,zz;
	StiKalmanTrackNode * node = getNodeNear(x);
	if (node==0)
		throw logic_error("StiKalmanTrack::getGlobalPointNear(double x) - ERROR - node==0");
	xx = node->fX;
	yy = node->fP0;
	zz = node->fP1;
	double alpha = node->fAlpha;
	double ca = cos(alpha);
	double sa = sin(alpha);
	return (StThreeVector<double>(ca*xx-sa*yy, sa*xx+ca*yy, zz));
}


StThreeVector<double> StiKalmanTrack::getGlobalPointAt(double x) const
{
  // returns a point along the track in global coordinates at radius "x"
  // Note that if the track does not pass at or near the given x value
  // the null vector is returned.
  //
  // Algorithm: One first look for the node closest to "x". A "fresh"
  // node is obtained from the factory, and its state is set to that
  // of the found node. Then the fresh node is "propagated" to the 
  // desired "x" position. A null vector (i.e. [0,0,0]) is returned
  // if anything fails...

    double xx,yy,zz;
    StiKalmanTrackNode * nearNode = getNodeNear(x);
    if (nearNode==0)
			throw logic_error("StiKalmanTrack::getGlobalPointAt(double x) - ERROR - nearNode==0");
    StiObjectFactoryInterface<StiKalmanTrackNode> * f 
			= static_cast<StiObjectFactoryInterface<StiKalmanTrackNode>*>(trackNodeFactory);
		if (f==0)
			throw logic_error("StiKalmanTrack::getGlobalPointAt(double x) - ERROR - no factory f==0");
    StiKalmanTrackNode * n = f->getObject();
    if (n==0)
			throw logic_error("StiKalmanTrack::getGlobalPointAt(double x) - ERROR - n==0");
    n->reset();
    n->setState(nearNode);
		n->propagate(x);
		xx = n->fX;
	  yy = n->fP0;
	  zz = n->fP1;
	  double alpha = n->fAlpha;
	  double ca = cos(alpha);
	  double sa = sin(alpha);
	  return (StThreeVector<double>(ca*xx-sa*yy, sa*xx+ca*yy, zz));
}


StThreeVector<double> StiKalmanTrack::getMomentumNear(double x)
{
    StiKalmanTrackNode * node = getNodeNear(x);
    double p[3];
    double e[6];
    node->getMomentum(p,e);
    StThreeVector<double> p3(p[0],p[1],p[2]);
    p3.rotateZ(node->fAlpha);
    return p3;
}

StThreeVector<double> StiKalmanTrack::getMomentumAtOrigin() const
{
    double px,py,pz;
    px = 0;
    py = 0;
    pz = 0;
    StiKalmanTrackNode * inner = getInnerMostNode();
    if (inner==0)
      throw logic_error("StiKalmanTrack::getMomentumAtOrigin() - ERROR - No node");
    inner->propagate(0.);
    double p[3];
    double e[6];
    inner->getMomentum(p,e);
    StThreeVector<double> p3(p[0],p[1],p[2]);
    p3.rotateZ(inner->fAlpha);
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
	return  (lastNode->fP3*StiKalmanTrackNode::getFieldConstant()>0)?-1:1;
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
	return lastNode->fChi2;
      else
	return firstNode->fChi2;
    }
  else // insideOut
    {
      if (trackingDirection==kOutsideIn)
	return firstNode->fChi2;
      else
	return lastNode->fChi2;			
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
	      if (detector->isActive((*it).fP0,(*it).fP1))
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
  double length = 0;
  StiKalmanTrackNode * inNode = getInnerMostHitNode();
  const StThreeVectorF &in  = inNode->getHit()->globalPosition();
  const StThreeVectorF &out = getOuterMostHitNode()->getHit()->globalPosition();
  double dx=out.x()-in.x();
  double dy=out.y()-in.y();
  double dz=out.z()-in.z();
  double curvature = inNode->getCurvature();
  double s = 2*asin(sqrt(dx*dx+dy*dy)*curvature/2.)/curvature;
  double len=sqrt(dz*dz+s*s);
  return length;
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
  //StiKalmanTrackNode * node = getInnerMostHitNode();
  //return (node->fX<2.)?true:false;
  return true;
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
	  child->addChild(parent);
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
    const StiKalmanTrackNode& node = *it;
    //make sure node has a hit
    StiHit* hit = node.getHit();
    if((&node)->getHit()!=hit) cout <<"Danger, Will Robinson! Danger!"<<endl;
    if (hit && hit->detector() != NULL) {
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
    throw logic_error("SKT::extendToVertex(const StiHit*) - LOGIC ERROR - Extension to vtx only allowed for OutsideIn");
  
  //cout << "SKT::extendToVertex(StiHit* vertex) - starting"<<endl;
  double chi2;
  StiKalmanTrackNode * sNode=0;
  StiKalmanTrackNode * tNode=0;
  bool trackExtended = false;

  if (true)
    return true;
  sNode = lastNode;
  tNode = trackNodeFactory->getObject();
  if (tNode==0) 
    throw logic_error("SKTF::extendTrackToVertex()\t- ERROR - tNode==null");
  tNode->reset();
  //cout  << "tNode->propagate(sNode, vertex)"<<endl;
  tNode->propagate(sNode, vertex);
  //cout  << "tNode->setHit(vertex);"<<endl;
  tNode->setHit(vertex);
  //cout << "chi2 = tNode->evaluateChi2()"<<endl;
  chi2 = tNode->evaluateChi2(); 
  //cout << "if (chi2<pars->maxChi2ForSelection)"<<endl;
  if (chi2<pars->maxChi2ForSelection)
    {
      //cout << "	tNode->updateNode();"<<endl;
      tNode->updateNode();
      //cout << "sNode->add(tNode);"<<endl;
      sNode->add(tNode);	
      lastNode = tNode;
      trackExtended = true;
    }
  //cout << "SKT::extendToVertex(StiHit* vertex) - done"<<endl;
  return trackExtended;
}

bool StiKalmanTrack::find(int direction)
{
  bool trackExtended=false;
  setFlag(0);
  //cout << "StiKalmanTrack::find(int direction) called with direction:"<<direction<<endl;
  if (++debugCount<5) 
    {
      cout << "::In-x:" << getInnerMostHitNode()->fX;
      cout << "/Out-x:" << getOuterMostHitNode()->fX;
      cout << "/find(OutIn)";
    }
  // invoke tracker to find or extend this track
  if (trackFinder->find(this,kOutsideIn))
    {
      // prune the undesirable nodes
      //prune();
			
      if (debugCount<5) cout<<"/fit(InOut);";
      fit(kInsideOut);
      trackExtended = true;
    }			
			
  // decide if an outward pass is needed.
  const StiKalmanTrackNode * outerMostNode = getOuterMostHitNode();
  if (debugCount<5) 
    {
      cout << "//In-x:" << getInnerMostHitNode()->fX;
      cout << "//Out-x:" << getOuterMostHitNode()->fX;
    }
  if (outerMostNode->fX<190. )
    {
      // swap the track inside-out in preparation for the outward search/extension
      if (debugCount<5) cout << "/swap";
      swap();      
      setTrackingDirection(kInsideOut); 			if (debugCount<20) cout << "/find(inOut)";
      if (trackFinder->find(this,kInsideOut))
	{
	  if (debugCount<20) cout << "fit(OutIn)";
	  fit(kOutsideIn);                  			if (debugCount<20) cout << "/swap()";
	  trackExtended = true;
	}
      swap();
      setTrackingDirection(kOutsideIn);
    }
  double pp[3];
  if (debugCount<5) 
    {
      getInnerMostHitNode()->getGlobalMomentum(pp);
      cout << "\nIn-x:" << getInnerMostHitNode()->fX<< " p:"<<pp[0]<<" "<<pp[1]<<" "<<pp[2]<<"\n"<<endl;
      getOuterMostHitNode()->getGlobalMomentum(pp);
      cout << "\nOut-x:" << getOuterMostHitNode()->fX<<" p:"<<pp[0]<<" "<<pp[1]<<" "<<pp[2]<<"\n"<<endl;
    }
  reserveHits();  
  setFlag(1);
  return trackExtended;
}

void StiKalmanTrack::setParameters(StiKalmanTrackFinderParameters *parameters)
{
	pars = parameters;
}

