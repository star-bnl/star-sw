#include <iostream.h>


//Sti
#include "StiHit.h"
#include "StiDefaultMutableTreeNode.h"
#include "StiKalmanTrackNode.h"
#include "StiKalmanTrack.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiGeometryTransform.h"
//#include "StiKalmanTrackNodeFactory
//#include "StEventTypes.h"

ostream& operator<<(ostream&, const StiHit&);

StiKalmanTrackNodeFactory* StiKalmanTrack::trackNodeFactory = 0;

void StiKalmanTrack::reset()
{
  svtDedx = -1;
  tpcDedx = -1;
  firstNode = 0;
  lastNode  = 0;
}

void StiKalmanTrack::setKalmanTrackNodeFactory(StiKalmanTrackNodeFactory* val)
{
    trackNodeFactory = val;
}

void StiKalmanTrack::getMomentum(double p[3], double e[6]) const
{
  // return the momentum of the track at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  // this will need to be refined...
  lastNode->getMomentum(p,e);
}

double  StiKalmanTrack::getPt()             const
{
  // returns the transverse momentum of the track at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  // this will need to be refined...

  return lastNode->getPt();
}

double  StiKalmanTrack::getRapidity()       const 
{
  // returns the rapidity of the particle at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  // this will need to be refined...
  double p[3];
  lastNode->getMomentum(p,0);
  double mass = getMass();
  if (mass>=0)
    // mass is known, return actual rapidity
    {
      double e = sqrt(mass*mass+p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);
      return 0.5*log(e+p[2])/(e-p[2]);
    }
  else
    return getPseudoRapidity();
}

double  StiKalmanTrack::getPseudoRapidity() const
{
  // Return pseudo rapidity of the particle at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  // this will need to be refined...
  return -log(tan(3.1415927/4.-lastNode->getTanL()));
}
  
double  StiKalmanTrack::getPhi()            const 
{
  // Return the azimuthal angle of the particle at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  // this will need to be refined...
  double p[3];
  lastNode->getMomentum(p,0);
  return atan2(p[1],p[0]);
}

double  StiKalmanTrack::getTanL()           const 
{
  // Return tan(lambda) of the particle at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  // this will need to be refined...
  return lastNode->getTanL();
}

double  StiKalmanTrack::getDca(StiHit * h)    const
{
  // Return the distance of closest approach to given point/hit
  // If no hit is specified assume the primary vertex i.e the last point 
  // on the track
  // set to 0 for now
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


StiKalmanTrackNode * StiKalmanTrack::addHit(StiHit *h)
{
  // Add a hit to this track
  // If the current lastNode is non null
  //   Insert the given hit in a StiKalmanTrackNode instance
  //   Add the new node as a child to the current last node
  //   Make the new node the last node of this track
  // Else 
  //   Insert the given hit in a StiKalmanTrackNode instance
  //   Set firstNode and lastNode equal to the new node

  if (lastNode!=0)
    {
      StiKalmanTrackNodeFactory * f = dynamic_cast<StiKalmanTrackNodeFactory*>(trackNodeFactory);
      StiKalmanTrackNode * n = f->getObject();
			n->reset();
      n->setHit(h);
      n->fX = h->x();
      lastNode->add(n);
      lastNode = n;
      return n;
    }
  else 
    {
      StiKalmanTrackNodeFactory * f = dynamic_cast<StiKalmanTrackNodeFactory*>(trackNodeFactory);
      firstNode  = f->getObject(); 
			firstNode->reset();
      firstNode->setHit(h);
      firstNode->fX = h->x();
      lastNode = firstNode;  // that's the only node on this track
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

  StiKalmanTrackNodeFactory * f = dynamic_cast<StiKalmanTrackNodeFactory*>(trackNodeFactory);
  StiKalmanTrackNode * n = f->getObject();
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
	{
	  firstNode = n;
	  lastNode =firstNode;
	}
    }
  else
    {
      StiKalmanTrackNode * pn = findHit(targetParent);
      if (pn==0)
	{
	  cout << " - StiKalmanTrack::insertHit() - ERROR - Attempting to insert hit after another which" << endl 
	       << " does not belong to this track" << endl;
	}
      else
	{
	  StiKalmanTrackNode * cn = dynamic_cast<StiKalmanTrackNode *> (pn->getFirstChild());
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
	      lastNode = n;
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
      StiKalmanTrackNode * cn = dynamic_cast<StiKalmanTrackNode *> (n->getFirstChild());

      if (cn==0)
	{
	  // no child, this is the last hit
	  StiKalmanTrackNode * pn = dynamic_cast<StiKalmanTrackNode *> (n->getParent());
	  if (pn==0)
	    {
	      // no parent, this is the first hit
	      firstNode = 0;
	      lastNode  = 0;
	    }
	  else
	    {
	      pn->remove(n);
	      lastNode = pn;
	    }
	}
      else
	{
	  // child exist
	  StiKalmanTrackNode * pn = dynamic_cast<StiKalmanTrackNode *> (n->getParent());
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
    {
      // the hit does not belong to this track
      cout << "StiKalmanTrack::removeHit() - Error - Given hit doe not belong to this track" << endl; 
    }
}

StiKalmanTrackNode * StiKalmanTrack::findHit(StiHit * h)
{
  if (firstNode==0)
    return 0;
  else
    {
      if (h==firstNode->getHit())
	{
	  return firstNode;
	}
      StiKalmanTrackNode * n = firstNode;
      while (n->getChildCount()>0)
	{
	  n = dynamic_cast<StiKalmanTrackNode *> (n->getFirstChild());
	  if (h==n->getHit())
	    {
	      return firstNode;
	    }
	}
    }
  return 0;
}

void StiKalmanTrack::removeAllHits()
{
  firstNode = 0;
  lastNode  = 0;
}

int  StiKalmanTrack::getHitCount()
{
  if (lastNode!=0)
    {
      return lastNode->getDepth();
    }
  else
    {
      return 0;
    }
}

StiHit * StiKalmanTrack::getHit(int index)
{
  // not implemented...
  return 0;
}

void StiKalmanTrack::initialize(double curvature,
				double tanl,
				const StThreeVectorD& origin,
				const hitvector & v)
{
  // Input parameters_________________________________________
  // origin    : origin of the track in global coordinates
  // curvature : 1/Radius of the track
  // tanl      : tan(pith angle)
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
      cout <<"StiKalmanTrack::initialize()\tERROR:\ttrackNodeFactory==0.  Abort"<<endl;
      return;
    }
  StiGeometryTransform * t = StiGeometryTransform::instance();
  StiKalmanTrackNodeFactory * fac = dynamic_cast<StiKalmanTrackNodeFactory *>(trackNodeFactory);
  if (!fac) {
      cout <<"StiKalmanTrack::initialize(). ERROR:\tfactory cast failed.  Seg-fault"<<endl;
  }
  
  StThreeVectorD stiOrigin;
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
  //cout <<"\tAdd Hits"<<endl;
  for (it=v.begin(); it!=v.end(); ++it)
    {
			cout <<"===========Adding Hit: "<<(*(*it))<<endl;
      StiDetector* layer = (*it)->detector();
      if (!layer) {
				cout <<"StiKalmanTrack::initialize() ERROR:\tHit has null detector.  Seg-fault"<<endl;
      }
      alpha = layer->getPlacement()->getNormalRefAngle();
      node = fac->getObject();
      node->reset();
      if (node==0)
				{
					cout << "StiKalmanTrack::initialize() - Severe Error - "
							 << "trackNodeFactor returned null object" << endl;
					return;
				}
      if (pNode==0)
				alphaP = -99999.; // no parent, set crazy value
      else
				alphaP = pNode->fAlpha; // value of the parent
      if (alphaP!=alpha)
				{
					stiOrigin = t->operator()(origin, alpha);
					eta = curvature*stiOrigin.x();
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
					cout << " just added a node" << endl;
				}
      pNode = node;
      i++;
    }
  lastNode = node;
	cout << *firstNode;
}

StiKalmanTrackNode * StiKalmanTrack::getNodeNear(double x) const
{
  if (firstNode==0)  // no node in this track, return a null state and error
      return 0;
  StiDefaultMutableTreeNodeVector* nodes  = firstNode->breadthFirstEnumeration();
  double minDist  = 1.E10;
  double xx, diff;
  StiKalmanTrackNode * bestNode = firstNode;
  StiDefaultMutableTreeNodeIterator it;
  for (it=nodes->begin(); it!=nodes->end(); it++)
    {
      StiKalmanTrackNode * node = dynamic_cast<StiKalmanTrackNode *>(*it);
			xx = node->fX;
      diff = xx-x; if (diff<0) diff = -diff;
      cout << "===> x/diff:" << xx << "\t" << diff << endl;
      if (diff<minDist) 
				{
					minDist = diff;
					bestNode = node;
				}
    }
  delete nodes;
  return bestNode;
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
