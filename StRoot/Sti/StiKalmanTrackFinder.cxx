/**************************************************************************
 * Copyright(c) 2001, STAR  Experiment at BNL, All rights reserved.       *
 *                                                                        *
 * Author: STAR Integrated Track Task Force                               *
 * Contributors are mentioned in the code where appropriate.              *
 *                                                                        *
 * Permission to use, copy, modify and distribute this software and its   *
 * documentation strictly for non-commercial purposes is hereby granted   *
 * without fee, provided that the above copyright notice appears in all   *
 * copies and that both the copyright notice and this permission notice   *
 * appear in the supporting documentation. The authors make no claims     *
 * about the suitability of this software for any purpose. It is          *
 * provided "as is" without express or implied warranty.                  *
 *                                                                        *
 **************************************************************************/

/**************************************************************************
 *                                                                        *
 * StiKalmanTrackFinder  	                                          *				   
 *                                                                        *
 * Author:  Claude Pruneau, Wayne State University                        *
 * Created: March 2001                                                    *
 *                                                                        *
 * Important Note: The Kalman Filter Code imbedded in this class was given*
 *                 to us gracioulsy by Jouri Belikov from the ALICE       *
 *                 collaboration. i.e. code reproduced with autorization. *
 *                                                                        *
 *                                                                        *
 *                                                                        *
 *                                                                        *
 *                                                                        *
 *                                                                        *
 **************************************************************************/
//Sti
#include "StiHit.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiShape.h"
#include "StiPlanarShape.h"
#include "StiDetectorContainer.h"
#include "StiTrackContainer.h"
#include "StiTrack.h"
#include "StiTrackFinder.h"
#include "StiKalmanTrack.h"
#include "StiTrackSeedFinder.h"
#include "StiEvaluableTrackSeedFinder.h"
#include "StiCompositeSeedFinder.h"
#include "StiTrackFilter.h"
#include "StiKalmanTrackFinder.h"


StiKalmanTrackFinder::StiKalmanTrackFinder()
{
    cout <<"StiKalmanTrackFinder::StiKalmanTrackFinder()"<<endl;    
    reset();
}


StiKalmanTrackFinder::~StiKalmanTrackFinder()
{
    cout <<"StiKalmanTrackFinder::~StiKalmanTrackFinder()"<<endl;
    
}

void StiKalmanTrackFinder::reset()
{
  analyzedTrackSeeds   = 0;
  foundTracks          = 0;
  acceptedTracks       = 0;
  status               = StiConstants::Ok;
  singleNodeDescent    = true;
  singleNodeFrom       = 20;
  mcsCalculated        = false;
  elossCalculated      = false;
  maxChi2ForSelection  = 5.;
}

bool StiKalmanTrackFinder::isValid(bool debug) const
{
    return StiTrackFinder::isValid(debug);
}

//Temporary patch, to test seed finder (MLM, 8/20/01)
void StiKalmanTrackFinder::doNextAction()
{
    //Test composite seed finder
    //StiCompositeSeedFinder* sf = dynamic_cast<StiCompositeSeedFinder*>(trackSeedFinder);
    //if (!sf) {
    //	cout <<"StiKalmanTrackFinder::doNextAction()\tcast to CompositeSF failed"<<endl;
    //	return;
    //}
	
    StiKalmanTrack* track = 0;
    if (trackSeedFinder->hasMore()) {
	track = trackSeedFinder->next();
	if (!track) {
	    cout <<"StiKalmanTrackFinder::doNextAction()\t Track==0. return "<<endl;
	    return;
	}
	else {
	    cout <<"StiKalmanTrackFinder::doNextAction()\t Got Valid track"<<endl;
	    track->update(); //append to display if drawable
	}
	
    }
    else {
	cout <<"\ttrackSeedFinder->hasMore()==false"<<endl;
    }
    return;
}

void StiKalmanTrackFinder::findTracks()
{
    //-----------------------------------------------------------------
    // Find all possible tracks in the given set of hits/points.
    // 
    // Note: The following objects must be set
    // trackSeedFinder  : a helper class object used to find track seeds
    // trackFilter      : a helper class object used to filter tracks 
    //                    before they are added to the track store.
    // trackContainer   : track container
    //-----------------------------------------------------------------
    
    StiTrack * t;
    
    while (trackSeedFinder->hasMore()){

	t = trackSeedFinder->next(); // obtain a pointer to the next track candidate/seed
	if (t!=0) { //check for null pointer
	  try
	    {
	      if (findTrack(t)==StiConstants::Ok) {
		
		if (trackFilter->accept(t)) {
		  trackContainer->push_back(t);
		}
	      }
	    }
	  catch (Exception e)
	    {
	      cout << e << endl;
	    }
	} 
    }
    status = StiConstants::Ok;
}

int StiKalmanTrackFinder::findTrack(StiTrack * t) throw ( Exception)
{
  //-----------------------------------------------------------------
  // Find extension (track) to the given track seed
  // Return Ok      if operation was successful
  // Return Error   if given seed "t" is invalid
  //                or if input data are invalid or if some other 
  //                internal error has occured.
  //-----------------------------------------------------------------
  StiKalmanTrack * tt = dynamic_cast<StiKalmanTrack *> (t);
  StiKalmanTrackNode * lastNode = tt->getLastNode();
  followTrackAtNode(lastNode);
  lastNode = findBestBranch(lastNode);
  pruneNodes(lastNode);
  tt->setLastNode(lastNode);
  extendToMainVertex(lastNode);
  return StiConstants::Ok;
}

void StiKalmanTrackFinder::followTrackAtNode(StiKalmanTrackNode * node)  throw ( Exception)
{
  // swim the current branch of the track down into the detector
  // propagate through inactive volume and stop/return when an active volume 
  // is reached.

  StiKalmanTrackNode * sNode;  // source node
  StiKalmanTrackNode * tNode;  // target node

  // Node Factory 
  StiKalmanTrackNodeFactory * nodeFactory = dynamic_cast<StiKalmanTrackNodeFactory *>(trackNodeFactory);

  // Source node is the given node
  sNode = node;
  tNode = nodeFactory->getObject();
	tNode->reset();

  //const
  StiDetector * sDet = sNode->getHit()->detector();
  geometryContainer->setToDetector(sDet);
  geometryContainer->moveIn();
  StiDetector * tDet = **geometryContainer;

  if (tDet==0)
    throw new Exception("StiKalmanTrackFinder::followTrackAtNode - Fatal - Arg was a null pointer");

  bool searching;
  bool foundDet;

  tNode->setState(sNode);
  int position = propagate(tNode, sDet, tDet);
  int pos;

  // position ==   0  target volume was a hit
  // position ==  -1  target volume was a miss, next search clockwise
  // position ==   1  target volume was a miss, next search anti-clockwise
  // position ==  -2  target volume was a miss, abandon search
  // position ==  +2
  
  switch (position)
    {
    case  0:  foundDet = true; break;
    case -1:
      {
	searching = true;
	while (searching)
	  {
	    geometryContainer->moveMinusPhi();
	    tDet = **geometryContainer;
	    tNode->setState(sNode);
	    pos = propagate(tNode, sDet, tDet);
	    switch (pos)
	      {
	      case   0:  searching = false; foundDet = true;  break; 
	      case  -1:  break;                    
	      case   1:  searching = false; foundDet = false; break; 
	      case  -2:  
	      case   2:  searching = false; foundDet = false; break; 
	      }
	  }
	break;
      }
    case  1:  
      {
	searching = true;
	while (searching)
	  {
	    geometryContainer->movePlusPhi();
	    tDet = **geometryContainer;
	    pos = propagate(tNode, sDet, tDet);
	    switch (pos)
	      {
	      case   0:  searching = false; foundDet = true;  break; 
	      case  -1:  searching = false; foundDet = false; break; 
	      case   1:  break;                    
	      case  -2:  
	      case   2:  searching = false; foundDet = false; break; 
	      }
	  }
	break;
      }
    case -2: 
    case  2: foundDet = false; break;  
    }

  if (foundDet)
    {
      if (!tDet->isActive())
	{
	  followTrackAtNode(tNode);
	}
      else
	{
	  if (singleNodeDescent)
	    followBestNode(sNode,tNode);
	  else
	    exploreNode(sNode,tNode);
	}
    }
}

int StiKalmanTrackFinder::propagate(StiKalmanTrackNode * node, 
				    StiDetector  * sDet,
				    StiDetector  * tDet) throw ( Exception)
{
  //-----------------------------------------------------------------
  //
  // Propagate a track (node) from a given source volume to a given target volume
  //
  // node  : work/target node
  // sDet  : source volume/detector
  // tDet  : target volume/detector
  //
  // Note that at this stage there may not be a hit associated with the 
  // target node. This is why the source detector and target detector
  // must be supplied separately...
  //
  // xk  x coordinate to propagate the track to.
  // x0  radiation length of the material 
  // rho density of the material
  //-----------------------------------------------------------------
  // if the two volumes do not have the same reference frame, a rotation
  // will be needed - do it now.

  int position = 0;
  double sAngle = sDet->getPlacement()->getNormalRefAngle();
  double tAngle = tDet->getPlacement()->getNormalRefAngle();
  if (sAngle!=tAngle)
    rotate(node,tAngle-sAngle);

  double xk = tDet->getPlacement()->getNormalRadius();
  double diff = node->fP3*xk - node->fP2;
  if (diff >= 0.9999999 || diff<0.9999999) 
    {
      return -1;
    }

  // swim the track

  double x1=node->fX, x2=x1+(xk-x1), dx=x2-x1, y1=node->fP0, z1=node->fP1;
  double c1=node->fP3*x1 - node->fP2, r1=sqrt(1.- c1*c1);
  double c2=node->fP3*x2 - node->fP2, r2=sqrt(1.- c2*c2);

  double newY = node->fP0 + dx*(c1+c2)/(r1+r2);
  double newZ = node->fP1 + dx*(c1+c2)/(c1*r2 + c2*r1)*node->fP4; 
 
  StiShape *     tShape = tDet->getShape();
  StiPlacement * tPlace =  tDet->getPlacement();
  double maxY=0., minY=0., maxZ=0., minZ=0.;
  double scale = 1.02;
  
  if (tShape->getShapeCode()==kPlanar)
    {
      StiPlanarShape * tPlanar = dynamic_cast<StiPlanarShape *>(tShape);
      maxY = scale*( tPlace->getNormalXoffset()+tPlanar->getHalfWidth() );
      maxY = scale*( tPlace->getNormalXoffset()-tPlanar->getHalfWidth() );
      maxZ = scale*( tPlace->getZcenter()+tShape->getHalfDepth()       );
      minZ = scale*( tPlace->getZcenter()-tShape->getHalfDepth()       );
    }

  if (newY>maxY)
    {
      if (newZ>maxZ)
	position = 2;
      else if (newZ<=minZ)
	position = 8;
      else
	position = 1;
    }
  else if (newY<minY)
    {
      if (newZ>maxZ)
	position = 4;
      else if (newZ<=minZ)
	position = 6;
      else
	position = 5;
    }
  else
    {
      if (newZ>maxZ)
	position = 3;
      else if (newZ<=minZ)
	position = 7;
      else
	position = 0; // in the right place
    }
 

  node->fP0 = newY;
  node->fP1 = newZ;

  //f = F - 1
  double rr=r1+r2, cc=c1+c2, xx=x1+x2;
  double f02=-dx*(2*rr + cc*(c1/r1 + c2/r2))/(rr*rr);
  double f03= dx*(rr*xx + cc*(c1*x1/r1+c2*x2/r2))/(rr*rr);
  double cr=c1*r2+c2*r1;
  double f12=-dx*node->fP4*(2*cr + cc*(c2*c1/r1-r1 + c1*c2/r2-r2))/(cr*cr);
  double f13=dx*node->fP4*(cr*xx-cc*(r1*x2-c2*c1*x1/r1+r2*x1-c1*c2*x2/r2))/(cr*cr);
  double f14= dx*cc/cr; 

  //b = C*ft
  double b00=f02*node->fC20 + f03*node->fC30;
  double b01=f12*node->fC20 + f13*node->fC30 + f14*node->fC40;
  double b10=f02*node->fC21 + f03*node->fC31;
  double b11=f12*node->fC21 + f13*node->fC31 + f14*node->fC41;
  double b20=f02*node->fC22 + f03*node->fC32;
  double b21=f12*node->fC22 + f13*node->fC32 + f14*node->fC42;
  double b30=f02*node->fC32 + f03*node->fC33;
  double b31=f12*node->fC32 + f13*node->fC33 + f14*node->fC43;
  double b40=f02*node->fC42 + f03*node->fC43;
  double b41=f12*node->fC42 + f13*node->fC43 + f14*node->fC44;
  
  //a = f*b = f*C*ft
  double a00=f02*b20+f03*b30;
  double a01=f02*b21+f03*b31;
  double a11=f12*b21+f13*b31+f14*b41;

  //F*C*Ft = C + (a + b + bt)
  node->fC00 = node->fC00 + a00 + 2*b00;
  node->fC10 = node->fC10 + a01 + b01 + b10; 
  node->fC20 = node->fC20 + b20;
  node->fC30 = node->fC30 + b30;
  node->fC40 = node->fC40 + b40;
  node->fC11 = node->fC11 + a11 + 2*b11;
  node->fC21 = node->fC21 + b21; 
  node->fC31 = node->fC31 + b31; 
  node->fC41 = node->fC41 + b41; 
  node->fX=x2;

  // Multiple scattering


  if (mcsCalculated)
    {
      StiMaterial * mat = sDet->getMaterial();
      StiMaterial * gas = sDet->getGas();
      gas = 0;
      mat = 0; // will need to fix this!!!!!!!
      double x0  = 0.; // needs work!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      double rho = 0.;

      double d=sqrt((x1-node->fX)*(x1-node->fX)
		    +(y1-node->fP0)*(y1-node->fP0)
		    +(z1-node->fP1)*(z1-node->fP1));
      double tanl  = node->getTanL();
      double pt = node->getPt();
      double p2=(1.+tanl*tanl)*pt*pt;
      double beta2=p2/(p2 + massHypothesis*massHypothesis);
      double theta2=14.1*14.1/(beta2*p2*1e6)*d/x0*rho;
      //double theta2=1.0259e-6*10*10/20/(beta2*p2)*d*rho;
      
      double ey=node->fP3*node->fX - node->fP2, ez=node->fP4;
      double xz=node->fP3*ez, zz1=ez*ez+1, xy=node->fP2+ey;
      
      node->fC33 = node->fC33 + xz*xz*theta2;
      node->fC32 = node->fC32 + xz*ez*xy*theta2;
      node->fC43 = node->fC43 + xz*zz1*theta2;
      node->fC22 = node->fC22 + (2*ey*ez*ez*node->fP2+1-ey*ey+ez*ez+
				 node->fP2*node->fP2*ez*ez)*theta2;
      node->fC42 = node->fC42 + ez*zz1*xy*theta2;
      node->fC44 = node->fC44 + zz1*zz1*theta2;

      // Energy losses
      
      if (elossCalculated)
	{
	  double dE=0.153e-3/beta2*(log(5940*beta2/(1-beta2)) - beta2)*d*rho;
	  if (x1 < x2) dE=-dE;
	  cc=node->fP3;
	  node->fP3 = node->fP3 *(1.- sqrt(p2+massHypothesis*massHypothesis)/p2*dE);
	  node->fP2 = node->fP2 + node->fX*(node->fP3-cc);
	}
    }
  return position;
}

void StiKalmanTrackFinder::exploreNode(StiKalmanTrackNode * sNode,
				       StiKalmanTrackNode * tNode)throw (Exception)
{
  // consider all points that could match the current track specified by tNode
  // add/follow  all hits that have a chi2 smaller than maxChi2ForSelection
  //
  // sNode  : parent node
  // tNode  : node being explored. 
  //_______________________________________________________________________
  double yWindow,zWindow;
  double chi2;

  StiKalmanTrackNodeFactory * f = dynamic_cast<StiKalmanTrackNodeFactory * >(trackNodeFactory);
  StiKalmanTrackNode * node;

  StiHit * hit = sNode->getHit();
  yWindow = getYWindow(sNode, hit);
  zWindow = getZWindow(sNode, hit);
  hitContainer->setDeltaD(yWindow);
  hitContainer->setDeltaZ(zWindow);
  //hitContainer->setRefPoint(workHit);  // would like to do by x,y,z rather than a hit... 
  while (hitContainer->hasMore())
    {
      hit = hitContainer->getHit();  /// this may have to change
      chi2 = evaluateChi2(tNode,hit);
      if (chi2<maxChi2ForSelection)
		{
		// add hit as sub node to current node, and follow new node down
		  node = f->getObject();
			node->reset();
		  node->setHit(hit);
		  node->setState(tNode);
		  updateNode(node, chi2);
		  sNode->add(node);
		  followTrackAtNode(node);
		}
    }
}

void StiKalmanTrackFinder::followBestNode(StiKalmanTrackNode * sNode, 
					  StiKalmanTrackNode * tNode)throw (Exception)
{
  double    yWindow,zWindow, chi2, bestChi2;
  StiHit  * hit;
  StiHit  * bestHit;

  StiHit * sHit = sNode->getHit();
  yWindow = getYWindow(sNode, sHit);
  zWindow = getZWindow(sNode, sHit);
  bestHit = 0;
  hitContainer->setDeltaD(yWindow);
  hitContainer->setDeltaZ(zWindow);
  //hitContainer->setRefPoint(workHit);///row, x,y,z, xWindow, yWindow, zWindow);
  bestChi2 = 1e50;
  while (hitContainer->hasMore())
    {
      hit = hitContainer->getHit();
      chi2 = evaluateChi2(tNode,hit);
      if (chi2<maxChi2ForSelection && chi2 < bestChi2)
		{
			bestChi2 = chi2;
			bestHit  = hit;
		}
    }
  if (bestChi2<maxChi2ForSelection && bestHit)
    {
      // add hit to this node, update track info, and chi2 of node
      tNode->setHit(bestHit);
      updateNode(tNode, bestChi2);
      sNode->add(tNode);
      followTrackAtNode(tNode);
    }
}
 
double StiKalmanTrackFinder::evaluateChi2(const StiKalmanTrackNode * node, 
					  const StiHit             * hit   ) const  throw ( Exception)
{
  //-----------------------------------------------------------------
  // This function calculates a predicted chi2 increment. i.e. it 
  // calculates the increment in the chi2 were the given hit added 
  // to the track
  // 
  // node : predicted location of the next hit on the track
  // hit  : hit considered for addition to the track
  // retun : increment in chi2 implied by the node/hit assocition.
  //-----------------------------------------------------------------
  double r00=hit->syy();
  double r01=hit->syz();
  double r11=hit->szz();
  r00+=node->fC00; 
  r01+=node->fC10; 
  r11+=node->fC11;
  double det=r00*r11 - r01*r01;
  if (det< 1.e-10 && det>-1.e-10) 
    {
      cerr <<" KalmanTrack warning: Singular matrix !\n";
      return 1e10;
    }
  double tmp=r00; 
  r00=r11; 
  r11=tmp; 
  r01=-r01;  
  double dy=hit->y() - node->fP0;
  double dz=hit->z() - node->fP1;
  return (dy*r00*dy + 2*r01*dy*dz + dz*r11*dz)/det;
}

/***
  // update the track to add the wNode as a continuation to the track which currently
  // ends at node. Because many hits may be associated with the same wNode, we have
  // to invoke a new instance of trackNode as a copy of wNode, update its track parameters
  // and add it as a child to "node". Return the newNode if all works, null otherwise.


  StiKalmanTrackNode * nNode = f->getObject();
	nNode->reset();
  if (!nNode)
    {
      cout << "StiKalmanTrackFinder::updateTrackAtNode(...) - Severe Error " << endl
	   << "      - trackNodeFactory failed to return a StiKalmanTrackNode" << endl;
      return 0;
    }

  nNode->setAsCopyOf(wNode);
  nNode->setHit(hit);


 ***/


void StiKalmanTrackFinder::updateNode(StiKalmanTrackNode * node, double chisq) throw ( Exception) 
{
  // This method updates the given node with the information 
  // from its associated hit. The given chisq is set as the chi2 
  // of this node.
  //__________________________________________________________________
  // update measurement matrix
  StiHit * hit = node->getHit();
  double r00 = hit->syy();
  double r01 = hit->syz();
  double r11 = hit->szz();  
  r00+=node->fC00; r01+=node->fC10; r11+=node->fC11;
  double det=r00*r11 - r01*r01;
  double tmp=r00; r00=r11/det; r11=tmp/det; r01=-r01/det;
  // update error matrix
  double k00=node->fC00*r00+node->fC10*r01, k01=node->fC00*r01+node->fC10*r11;
  double k10=node->fC10*r00+node->fC11*r01, k11=node->fC10*r01+node->fC11*r11;
  double k20=node->fC20*r00+node->fC21*r01, k21=node->fC20*r01+node->fC21*r11;
  double k30=node->fC30*r00+node->fC31*r01, k31=node->fC30*r01+node->fC31*r11;
  double k40=node->fC40*r00+node->fC41*r01, k41=node->fC40*r01+node->fC41*r11;

  double dy  = hit->y() - node->fP0;
  double dz  = hit->z() - node->fP1;
  double cur = node->fP3 + k30*dy + k31*dz;
  double eta = node->fP2 + k20*dy + k21*dz;
  double ddd = cur*node->fX-eta;
  if (ddd >= 0.99999 || ddd<0.99999) 
      throw new Exception("StiKalmanTrackFinder - Warning - Filtering failed !\n");
  node->fP0 += k00*dy + k01*dz;
  node->fP1 += k10*dy + k11*dz;
  node->fP2  = eta;
  node->fP3  = cur;
  node->fP4 += k40*dy + k41*dz;

  double c01=node->fC10, c02=node->fC20, c03=node->fC30, c04=node->fC40;
  double c12=node->fC21, c13=node->fC31, c14=node->fC41;

  node->fC00-=k00*node->fC00+k01*node->fC10; 
  node->fC10-=k00*c01+k01*node->fC11;
  node->fC20-=k00*c02+k01*c12;   
  node->fC30-=k00*c03+k01*c13;
  node->fC40-=k00*c04+k01*c14; 
  node->fC11-=k10*c01+k11*node->fC11;
  node->fC21-=k10*c02+k11*c12;   
  node->fC31-=k10*c03+k11*c13;
  node->fC41-=k10*c04+k11*c14; 
  node->fC22-=k20*c02+k21*c12;   
  node->fC32-=k20*c03+k21*c13;
  node->fC42-=k20*c04+k21*c14; 
  node->fC33-=k30*c03+k31*c13;
  node->fC43-=k30*c04+k31*c14; 
  node->fC44-=k40*c04+k41*c14; 
  node->fChi2 = chisq + node->fChi2;
}

void StiKalmanTrackFinder::fitInward(StiKalmanTrackNode * node) throw (Exception)
{
	/**
		Perform a track fit from the outside-in.
	 	The track is assumed to consist of a simple node a sequence. 
	    i.e. It is assumed to be pruned. Additionally, since the 
		fit is done from the outside-in the MCS and E-loss are 
		turned off since one would be getting wrong values in 
		this case.
	
		The fit starts from the given node which may (or may not)
		be the first node of the track, the outer most point of the
		track.
	*/
  StiKalmanTrackNode * pNode; // parent node
  StiKalmanTrackNode * cNode; // child node
  StiDetector * pDet;  // parent detector
  StiDetector * cDet;  // child detector
  double chi2;
  
  // no MCS or ELOSS calculated here.
  setMCSCalculated(false);
  setElossCalculated(false);

  pNode = node;
  while (pNode->getChildCount()>0)
    {
		pDet  = pNode->getHit()->detector();
		cNode = dynamic_cast<StiKalmanTrackNode *>(pNode->getFirstChild());
		cDet  = cNode->getHit()->detector();
		cNode->setState(pNode);     // copy state from pNode
		propagate(cNode,pDet,cDet);	// evolve state from pDet to cDet
		chi2 = evaluateChi2(cNode,cNode->getHit());
		updateNode(cNode,chi2);
		pNode = cNode;
    }
}

void StiKalmanTrackFinder::fitOutward(StiKalmanTrackNode * node) throw (Exception)
{
	/**
		Perform a track fit from the outside-in.
	 	The track is assumed to consist of a simple node a sequence. 
	    i.e. It is assumed to be pruned. 
	
		The fit starts from the given node which may (or may not)
		be the last node of the track, the inner most point of the
		track.

		If MCS and ELOSS calculations are required, they must be
		set externally to this call.

		Likewise, the mass hypothesis must also be made externally.
	*/
  StiKalmanTrackNode * pNode; // parent node
  StiKalmanTrackNode * cNode; // child node
  StiDetector * pDet;  // parent detector
  StiDetector * cDet;  // child detector
  double chi2;
  
  cNode = node;
  cDet  = cNode->getHit()->detector();
  pNode = dynamic_cast<StiKalmanTrackNode *>(cNode->getParent());
  while (pNode)
    {
		pDet  = pNode->getHit()->detector();
		pNode->setState(cNode);     // copy state from cNode
		propagate(pNode,cDet,pDet);	// evolve state from cDet to pDet
		chi2 = evaluateChi2(pNode,pNode->getHit());
		updateNode(pNode,chi2);
		cNode = pNode;
		cDet  = pDet;
		pNode = dynamic_cast<StiKalmanTrackNode *>(cNode->getParent());
    }
}


//_____________________________________________________________________________
void StiKalmanTrackFinder::rotate(StiKalmanTrackNode * node, double alpha) throw ( Exception)
{
  //-----------------------------------------------------------------
  // This function rotates by an angle alpha the track representation by alpha
  // held by the given node.
  //-----------------------------------------------------------------
  node->fAlpha += alpha;
  if (node->fAlpha < -3.1415927) node->fAlpha += 2*3.1415927;
  if (node->fAlpha >= 3.1415927) node->fAlpha -= 2*3.1415927;
  
  double x1=node->fX;
  double y1=node->fP0;
  double ca=cos(alpha);
  double sa=sin(alpha);
  double r1=node->fP3*node->fX - node->fP2;
  
  node->fX = x1*ca + y1*sa;
  node->fP0=-x1*sa + y1*ca;
  node->fP2=node->fP2*ca + (node->fP3*y1 + sqrt(1.- r1*r1))*sa;
  
  double r2=node->fP3*node->fX - node->fP2;
  if (r2>= 0.9999999 || r2<0.9999999)
    {
      throw new Exception(" StiKalmanTrackFinder - Warning: Rotation failed - case 1!\n");
    }
  
  double y0=node->fP0 + sqrt(1.- r2*r2)/node->fP3;
  if ((node->fP0-y0)*node->fP3 >= 0.) 
    {
      throw new Exception(" StiKalmanTrackFinder - Warning: Rotation failed - case 2 !\n");
    }

  //f = F - 1
  double f00=ca-1;
  double f23=(y1 - r1*x1/sqrt(1.- r1*r1))*sa;
  double f20=node->fP3*sa;
  double f22=(ca + sa*r1/sqrt(1.- r1*r1))-1;
  
  //b = C*ft
  double b00=node->fC00*f00, b02=node->fC00*f20+node->fC30*f23+node->fC20*f22;
  double b10=node->fC10*f00, b12=node->fC10*f20+node->fC31*f23+node->fC21*f22;
  double b20=node->fC20*f00, b22=node->fC20*f20+node->fC32*f23+node->fC22*f22;
  double b30=node->fC30*f00, b32=node->fC30*f20+node->fC33*f23+node->fC32*f22;
  double b40=node->fC40*f00, b42=node->fC40*f20+node->fC43*f23+node->fC42*f22;

  //a = f*b = f*C*ft
  double a00=f00*b00, a02=f00*b02, a22=f20*b02+f23*b32+f22*b22;

  // *** double dy2=fCyy;

  //F*C*Ft = C + (a + b + bt)
  node->fC00 += a00 + 2*b00;
  node->fC10 += b10;
  node->fC20 += a02+b20+b02;
  node->fC30 += b30;
  node->fC40 += b40;
  node->fC21 += b12;
  node->fC32 += b32;
  node->fC22 += a22 + 2*b22;
  node->fC42 += b42; 

  // *** fCyy+=dy2*sa*sa*r1*r1/(1.- r1*r1);
  // *** fCzz+=d2y*sa*sa*fT*fT/(1.- r1*r1);
}

//_____________________________________________________________________________
void StiKalmanTrackFinder::removeNodeFromTrack(StiKalmanTrackNode * node, StiKalmanTrack* track)
{
  // Remove given node from given track. 
}

void StiKalmanTrackFinder::pruneNodes(StiKalmanTrackNode * node)
{
  // Prune unnecessary nodes on the track starting at given node. 
  // All siblings of the given node, are removed, and iteratively
  // all siblings of its parent are removed from the parent of the
  // parent, etc.

  StiKalmanTrackNode * parent = dynamic_cast<StiKalmanTrackNode *>(node->getParent());
  while (parent)
    {
      parent->removeAllChildrenBut(node);
      node = parent;
      parent = dynamic_cast<StiKalmanTrackNode *>(node->getParent());
    }
}

StiKalmanTrackNode * StiKalmanTrackFinder::findBestBranch(StiKalmanTrackNode * node)throw ( Exception)
{
  // starting at given node, find the best branch and return the end node.
  // Only the leafs are looked at. Basically, the best track is taken as 
  // that being the longest and the lowest chi-square. Return the last node 
  // of the best branch.
  float chi2;
  float bestChi2;
  StiKalmanTrackNode * bestNode;
  StiKalmanTrackNode * leaf;
  
  leaf = dynamic_cast<StiKalmanTrackNode *>(node->getFirstLeaf());
  bestChi2 = leaf->fChi2;
  bestNode = leaf;
  leaf =  dynamic_cast<StiKalmanTrackNode *>(leaf->getNextLeaf());
  while (leaf)
    {
      chi2 = leaf->fChi2;
      if (chi2<bestChi2)
	{
	  bestChi2 = chi2;
	  bestNode = leaf;
	}
      leaf = dynamic_cast<StiKalmanTrackNode *>(leaf->getNextLeaf());
    }
  return bestNode;
}

//_____________________________________________________________________________
bool StiKalmanTrackFinder::extendToMainVertex(StiKalmanTrackNode * node)
{
  //-----------------------------------------------------------------
  // This function propagates tracks to the "vertex".
  //-----------------------------------------------------------------
  double c=node->fP3*node->fX - node->fP2;
  double tgf=-node->fP2/(node->fP3*node->fP0 + sqrt(1-c*c));
  double snf=tgf/sqrt(1.+ tgf*tgf);
  double xv=(node->fP2+snf)/node->fP3;

  xv = 0;//return propagateTrackAtNodeTo(xv,x0,rho,pm);
  return true; // requires work here...
}

double StiKalmanTrackFinder::getYWindow(StiKalmanTrackNode * n, StiHit * h) const 
{
  double rv, sy2a, sy2b;
  sy2a = n->fC00;  // syy of the track at this node
  sy2b = h->syy(); // measured error of the hit at this node
  rv = 4*sqrt(sy2a+sy2b);
  if (rv<0.2)
    rv = 0.2;
  else if (rv>5.)
    rv = 5.;
  return rv;
}

double StiKalmanTrackFinder::getZWindow(StiKalmanTrackNode * n, StiHit * h) const 
{
  double rv, sz2a, sz2b;
  sz2a = n->fC11;  // szz of the track at this node
  sz2b = h->szz(); // measured error of the hit at this node
  rv = 4*sqrt(sz2a+sz2b);
  if (rv<0.2)
    rv = 0.2;
  else if (rv>5.)
    rv = 5.;
  return rv;
}
