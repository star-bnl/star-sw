//Std
#include <stdexcept>
#include <math.h>

//Sti
#include "StiKTNIterator.h"
#include "StiHit.h"
#include "StiDefaultMutableTreeNode.h"
#include "StiKalmanTrackNode.h"
#include "StiKalmanTrack.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiGeometryTransform.h"
#include "StiKTNIterator.h"

ostream& operator<<(ostream&, const StiHit&);

StiObjectFactoryInterface<StiKalmanTrackNode>* StiKalmanTrack::trackNodeFactory = 0;

void StiKalmanTrack::reset()
{
  firstNode = 0;
	lastNode = 0;
  trackingDirection = kOutsideIn;
  fittingDirection  = kOutsideIn;
}

void StiKalmanTrack::update()
{
  //cout<<"void StiKalmanTrack::update()"<<endl;
  return;
}

void StiKalmanTrack::setKalmanTrackNodeFactory(StiObjectFactoryInterface<StiKalmanTrackNode>* val)
{
  trackNodeFactory = val;
}

void StiKalmanTrack::getMomentum(double p[3], double e[6]) const
{
    // return the momentum of the track at the inner most node held by this track
    // which may (or not) be the primary vertex. 
    // this will need to be refined...
    getInnerMostHitNode()->getMomentum(p,e);
}

double  StiKalmanTrack::getPt()             const
{
    // returns the transverse momentum of the track at the inner most node held by this track
    // which may (or not) be the primary vertex. 
    // this will need to be refined...
    
    return getInnerMostHitNode()->getPt();
}

double StiKalmanTrack::getCurvature()             const
{
  // returns the curvature of the track at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  // this will need to be refined...
  return getInnerMostHitNode()->fP3;
}


double  StiKalmanTrack::getRapidity()       const 
{
  // returns the rapidity of the particle at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  double p[3];
  StiKalmanTrackNode *  inner = getInnerMostHitNode();
  inner->getMomentum(p,0);
  double mass = getMass();
  if (mass<0)
    throw runtime_error("StiKalmanTrack::getRapidity() - particle mass unknown");
  double e = sqrt(mass*mass+p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);
  double nn = e+p[2];
  double dd = e-p[2];
  if (dd>0 && nn>0)
    return 0.5*log(nn/dd);
  else
    return -999.;
}

double  StiKalmanTrack::getPseudoRapidity() const
{
  // Return pseudo rapidity of the particle at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  return -log(tan(M_PI/4.-(getInnerMostHitNode()->getTanL()/2.)));
}

double  StiKalmanTrack::getPhi()            const 
{
  // Return the azimuthal angle of the particle at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  // this will need to be refined...
  double p[3];
  getInnerMostHitNode()->getMomentum(p,0);
  return atan2(p[1],p[0]);
}

double  StiKalmanTrack::getTanL()           const 
{
  // Return tan(lambda) of the particle at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  // this will need to be refined...
  return getInnerMostHitNode()->getTanL();
}



StiKalmanTrackNode * StiKalmanTrack::addHit(StiHit *h)
{
  // Add a hit to this track
  // If the current lastNode is non null
  //   Insert the given hit in a StiKalmanTrackNode instance
  //   Add the new node as a child to the current last node
  //   Make the new node the last node of this track
  // Else 
  //   Insert the given hit in a StiKalmanTrackNode instance
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

double StiKalmanTrack::getTpcDedx() const
{
  return 0.; // to be changed...
}

double StiKalmanTrack::getSvtDedx() const
{
  return 0.; // to be changed...
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
	  throw runtime_error("SKT::insertHit() - ERROR - Attempted hit insertion after hit which does not belong to this track");
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
    throw runtime_error("StiKalmanTrack::removeHit() - Error - Given hit does not belong to this track");
}

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

void StiKalmanTrack::removeAllHits()
{
    firstNode = 0;
		lastNode  = 0;
}

void StiKalmanTrack::initialize(double curvature,
				double tanl,
				const StThreeVectorD& origin,
				const hitvector & v)
{
    // Input parameters_________________________________________
    // origin    : origin of the track in global coordinates
    // curvature : 1/Radius of the track
    // tanl      : tan(pitch angle)
    // v         : vector of hits to be added to this track
    //
    // Generated parameters_____________________________________
    // state[0] = y  ordinate
    // state[1] = z  position along beam axis
    // state[2] = eta=C*x0
    // state[3] = C  (local) curvature of the track
    // state[4] = tan(l) 
    // e[0] = fC00;
    // e[1] = fC10;e[2] = fC11;
    // e[3] = fC20;e[4] = fC21;e[5] = fC22;
    // e[6] = fC30;e[7] = fC31;e[8] = fC32;
    // e[9] = fC33;e[10]= fC40;e[11]= fC41;e[12]= fC42;e[13]= fC43;e[14]= fC44;
    
    //cout <<"StiKalmanTrack::initialize()"<<endl;
    if (!trackNodeFactory) 
	{
	    cout <<"StiKalmanTrack::initialize()\tERROR:\t";
	    cout <<"trackNodeFactory==0.  Abort"<<endl;
	    return;
	}
    StiObjectFactoryInterface<StiKalmanTrackNode>* fac = trackNodeFactory;
    if (!fac) 
	{
	    cout <<"StiKalmanTrack::initialize(). ERROR:\t";
	    cout <<"factory cast failed.  Seg-fault"<<endl;
	}
    
    //StThreeVectorD stiOrigin;
    double alpha,alphaP,eta;  
    hitvector::const_iterator it;
    double state[5];  
    double error[15];
    
    //cout <<"\tSet state[3], state[4]"<<endl;
    // These are constant for all hits
    state[3]=curvature;
    state[4]=tanl;
    //cout <<"\tSet Errors"<<endl;
    // For the time being set a diagonal error matrx
    error[0] = 1.;  
    error[1] = 0.; error[2] = 1.;  
    error[3] = 0.; error[4] = 0.; error[5] = 1.;  
    error[6] = 0.; error[7] = 0.; error[8] = 0.;  error[9]  = 1.;  
    error[10]= 0.; error[11] = 0.;error[12] = 0.; error[13] = 0.;  error[14] = 1.;
    
    // do the transfer here
    StiKalmanTrackNode * node  = 0;
    StiKalmanTrackNode * pNode = 0;
    int i =0;
    eta = 0.;
    //cout <<"\tAdd Hits"<<endl;
    for (it=v.begin(); it!=v.end(); ++it)
      {
	//cout <<"===========Adding Hit: "<<(*(*it))<<endl;
	StiDetector* layer = (*it)->detector();
	if (!layer) 
	  throw runtime_error("StiKalmanTrack::initialize() ERROR:\t Hit has null detector.");
	alpha = layer->getPlacement()->getNormalRefAngle();
	node = fac->getObject();
	if (node==0)
	  throw runtime_error("StiKalmanTrack::initialize() ERROR:\t Null node returned by Node factory");
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
	node->set(i, (*it), alpha, (*it)->x(), state,error, 0., 0.);
	if (pNode==0) 
	  firstNode = node;
	else
	  {
	    pNode->add(node);
	  }
	pNode = node;
	i++;
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
	return StThreeVector<double>(0.,0.,0.);
    else 
	{
	    StiHit * hit = node->getHit();
	    StThreeVectorF pos = hit->globalPosition();
	    return StThreeVector<double>( pos.x(), pos.y(), pos.z() );
	}
}

StThreeVector<double> StiKalmanTrack::getPointNear(double x) const
{
    // returns point in local coordinates
    double xx,yy,zz;
    StiKalmanTrackNode * node = getNodeNear(x);
    if (node==0)
	return StThreeVector<double>(0.,0.,0.);
    else 
	{
	    xx = node->fX;
	    yy = node->fP0;
	    zz = node->fP1;
	    return (StThreeVector<double>(xx, yy,zz));
	}
}

StThreeVector<double> StiKalmanTrack::getGlobalPointNear(double x) const
{
    // returns point in local coordinates
    double xx,yy,zz;
    StiKalmanTrackNode * node = getNodeNear(x);
    if (node==0)
	return StThreeVector<double>(0.,0.,0.);
    else 
	{
	    xx = node->fX;
	    yy = node->fP0;
	    zz = node->fP1;
	    double alpha = node->fAlpha;
	    double ca = cos(alpha);
	    double sa = sin(alpha);
	    double gx = ca*xx-sa*yy;
	    double gy = sa*xx+ca*yy;
	    return (StThreeVector<double>(gx,gy, zz));
	}
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
    StiObjectFactoryInterface<StiKalmanTrackNode> * f 
	= static_cast<StiObjectFactoryInterface<StiKalmanTrackNode>*>(trackNodeFactory);
    StiKalmanTrackNode * n = f->getObject();
    n->reset();
    n->setState(nearNode);
    if (n==0 || nearNode==0)
	return StThreeVector<double>(0.,0.,0.);
    else 
	{
	  try 
	    {
	      n->propagate(x);
	    }
	  catch (runtime_error& rte)
	    {
	      return StThreeVector<double>(0.,0.,0.);
	    }
	  xx = n->fX;
	  yy = n->fP0;
	  zz = n->fP1;
	  double alpha = n->fAlpha;
	  double ca = cos(alpha);
	  double sa = sin(alpha);
	  double gx = ca*xx-sa*yy;
	  double gy = sa*xx+ca*yy;
	  return (StThreeVector<double>(gx,gy, zz));
	}
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
      throw runtime_error("StiKalmanTrack::getMomentumAtOrigin() - ERROR - No node");
    inner->propagate(0.);
    double p[3];
    double e[6];
    inner->getMomentum(p,e);
    StThreeVector<double> p3(p[0],p[1],p[2]);
    p3.rotateZ(inner->fAlpha);
    return p3;
}

double  StiKalmanTrack::getMass() const   
{
  return m;
}

int StiKalmanTrack::getCharge() const
{
  return q;
}
double  StiKalmanTrack::getChi2() const
{
  return chi2;
}

double  StiKalmanTrack::getDca(StiHit * h)    const
{
  // Return the distance of closest approach to given point/hit
  // If no hit is specified assume the primary vertex i.e the last point 
  // on the track
  // set to 0 for now
  return 0;
}

double  StiKalmanTrack::getDca(StiTrack *t)   const
{
  // distance of closest approach to given track
  return 0;
}

double  StiKalmanTrack::getDca2(StiTrack *t)   const
{
    // distance of closest approach to given track - 2D calc
    return 0;
}

double  StiKalmanTrack::getDca3(StiTrack *t)   const
{
    // distance of closest approach to given track - 3D calc
    return 0;
}


int    StiKalmanTrack::getGapCount()    const  
{
  /*
  int gaps = 0;
  if (firstNode)
    {
      StiKTNForwardIterator it(getLastNode());
      bool inGap = false;
      while(it!=it.end())
	{
	  StiKalmanTrackNode * node = it;
	  if (node->getDetector()->isActive())
	    {
	      if (node->getHit())
		{
		  inGap = false;
		}
	      else
		{
		  if (!inGap)
		    {
		      gaps++;
		      inGap = true;
		    }
		  
		}
	    }
	  it++;
	}
      return gaps;
    }
    else*/
    return 0;
}


int    StiKalmanTrack::getFitPointCount()    const  
{
  /*  if (firstNode)
    {
      StiKTNForwardIterator it(getLastNode());
      int nPts = 0;
      while(it!=it.end())
	{
	  StiKalmanTrackNode * node = it;
	  if (node->getDetector()->isActive() && node->getHit())
		nPts++;
	  it++;
	}
      return nPts;
    }
    else*/
    return 0;
}

StiKTNForwardIterator StiKalmanTrack::begin() const 
{
	return StiKTNForwardIterator(firstNode);
}

StiKTNForwardIterator StiKalmanTrack::end() const 
{
	return StiKTNForwardIterator(lastNode);
}


int    StiKalmanTrack::getPointCount() const
{
	StiKTNForwardIterator it;
	int nPts = 0;
	for (it=begin();it!=end();it++)
		{
			if ((*it).getDetector()->isActive() && (*it).getHit())
				nPts++;
			}
	return nPts;
}

double StiKalmanTrack::getPrimaryDca() const
{
  return 0;
}


/// Accessor method returns the outer most node associated with the track.
StiKalmanTrackNode * StiKalmanTrack::getOuterMostNode()  const 
{ 
  if (trackingDirection==kOutsideIn)
    return firstNode;
  else
    return getLastNode();
}

/// Accessor method returns the inner most node associated with the track.
StiKalmanTrackNode * StiKalmanTrack::getInnerMostNode()   const 
{ 
  if (trackingDirection==kInsideOut)
    return firstNode;
  else
    return getLastNode();
}

StiKalmanTrackNode * StiKalmanTrack::getOuterMostHitNode()  const
{
  StiKalmanTrackNode * pNode;
  StiKalmanTrackNode * cNode;
  if (firstNode==0)
    throw runtime_error("StiKalmanTrack::getOuterMostHitNode() - ERROR - firstNode==0");
	StiKTNForwardIterator it(firstNode);
	
  if (trackingDirection==kOutsideIn)
    {
      if ((*it).getHit())
				return &*it;
      else
				{
					pNode = firstNode; // parent
					while (pNode->getChildCount()>0) 
						{
							cNode = static_cast<StiKalmanTrackNode *>(pNode->getFirstChild());
							if (cNode==0)
								throw runtime_error("StiKalmanTrack::getOuterMostHitNode() - ERROR - pNode reports childCount>0 but has no children");
							if (cNode->getHit())
								return cNode;
							pNode = cNode;
						}
				}
    }
  else
    {
      cNode = getLastNode();  
      if (cNode->getHit())
				return cNode;
      else
				{
					pNode = static_cast<StiKalmanTrackNode *>(cNode->getParent());
					while (pNode)
						{
							if (pNode->getHit())
								return pNode;
							cNode = pNode;
							pNode = static_cast<StiKalmanTrackNode *>(cNode->getParent());
						}
				}
    }
  throw runtime_error("StiKalmanTrack::getOuterMostHitNode() - ERROR - Track has no hit");
}

StiKalmanTrackNode * StiKalmanTrack::getInnerMostHitNode()   const
{
  StiKalmanTrackNode * pNode;
  StiKalmanTrackNode * cNode;
  if (firstNode==0)
    throw runtime_error("StiKalmanTrack::getInnerMostHitNode() - ERROR - firstNode==0");
  if (trackingDirection==kInsideOut)
    { 
      if (firstNode->getHit())
	return firstNode;
      else
	{
	  pNode = firstNode; // parent
	  while (pNode->getChildCount()>0) 
	    {
	      cNode = static_cast<StiKalmanTrackNode *>(pNode->getFirstChild());
	      if (cNode==0)
		throw runtime_error("StiKalmanTrack::getInnerMostHitNode() - ERROR - pNode reports childCount>0 but has no children");
	      if (cNode->getHit())
		return cNode;
	      pNode = cNode;
	    }
	}
    }
  else
    {
      cNode = getLastNode();  
      if (cNode->getHit())
	return cNode;
      else
	{
	  pNode = static_cast<StiKalmanTrackNode *>(cNode->getParent());
	  while (pNode)
	    {
	      if (pNode->getHit())
		return pNode;
	      cNode = pNode;
	      pNode = static_cast<StiKalmanTrackNode *>(cNode->getParent());
	    }
	}
    }
  throw runtime_error("StiKalmanTrack::getInnerMostHitNode() - ERROR - Track has no hit");
}


bool  StiKalmanTrack::isPrimary() const
{
  return true; // for now - this must be fixed!!!!!!!!!
}
