
//Sti
#include "StiTrackNode.h"

#include "StiKalmanTrack.h"

StiKalmanTrack::StiTrackNodeFactory* StiKalmanTrack::trackNodeFactory = 0;

void StiKalmanTrack::reset()
{
  svtDedx = -1;
  tpcDedx = -1;
  firstNode = 0;
  lastNode  = 0;
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


void StiKalmanTrack::addHit(StiHit *h)
{
  // Add a hit to this track
  // If the current lastNode is non null
  //   Insert the given hit in a StiTrackNode instance
  //   Add the new node as a child to the current last node
  //   Make the new node the last node of this track
  // Else 
  //   Insert the given hit in a StiTrackNode instance
  //   Set firstNode and lastNode equal to the new node

  if (lastNode!=0)
    {
      StiTrackNode * n = trackNodeFactory->getObject();
      n->setHit(h);
      lastNode->add(n);
      lastNode = n;
    }
  else 
    {
      firstNode  = trackNodeFactory->getObject();
      firstNode->setHit(h);
      lastNode = firstNode;  // that's the only node on this track
    }
}

void StiKalmanTrack::insertHit(StiHit *hInserted, StiHit * targetParent)
{
  // Add a hit to this track right after the given hit
  // Note that this method is slow because it needs to 
  // find the parent hit first...
  // Note that if the targetParent hit is null, it is assumed
  // the intent is to add the hit before the firstNode
  // It is further assumed that the targetParent has at most
  // one child.

  StiTrackNode * n = trackNodeFactory->getObject();
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
      StiTrackNode * pn = findHit(targetParent);
      if (pn==0)
	{
	  cout << " - StiKalmanTrack::insertHit() - ERROR - Attempting to insert hit after another which" << endl 
	       << " does not belong to this track" << endl;
	}
      else
	{
	  StiTrackNode * cn = dynamic_cast<StiTrackNode *> (pn->getFirstChild());
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
}

void StiKalmanTrack::removeHit(StiHit *h)
{
  // remove the given hit (and node) from this track
  // It is assume that the hit has at most one child
  
  StiTrackNode * n = findHit(h);
  if (n!=0)
    {
      // the hit belongs to this track, let's remove it
      StiTrackNode * cn = dynamic_cast<StiTrackNode *> (n->getFirstChild());

      if (cn==0)
	{
	  // no child, this is the last hit
	  StiTrackNode * pn = dynamic_cast<StiTrackNode *> (n->getParent());
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
	  StiTrackNode * pn = dynamic_cast<StiTrackNode *> (n->getParent());
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

StiTrackNode * StiKalmanTrack::findHit(StiHit * h)
{
  if (firstNode==0)
    return 0;
  else
    {
      if (h==firstNode->getHit())
	{
	  return firstNode;
	}
      StiTrackNode * n = firstNode;
      while (n->getChildCount()>0)
	{
	  n = dynamic_cast<StiTrackNode *> (n->getFirstChild());
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
