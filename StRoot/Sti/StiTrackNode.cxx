#include <iostream>
#include "StiTrackNode.h"

//_____________________________________________________________________________
void StiTrackNode::set(const double xx[5],
	 const double cc[15], 
	 double xref, 
	 double alpha)
{
  // Set the state of the track and covariance matrix
  // x[0] : "y"
  // x[1] : "z"
  // x[2] : sin(phi)
  // x[3] : tan(lambda)
  // x[4] : 1/pt

  fX      = xref;
  fAlpha  = alpha;
  fdEdx   = 0;
  fChi2   = 0;
  mDepth  = 0;

  fP0=xx[0]; 
  fP1=xx[1]; 
  fP2=xx[2]; 
  fP3=xx[3]; 
  fP4=xx[4];
  // covariance error matrix
  fC00=cc[0];
  fC10=cc[1];  
  fC11=cc[2];
  fC20=cc[3];  
  fC21=cc[4];  
  fC22=cc[5];
  fC30=cc[6];  
  fC31=cc[7];  
  fC32=cc[8];  
  fC33=cc[9];
  fC40=cc[10]; 
  fC41=cc[11]; 
  fC42=cc[12]; 
  fC43=cc[13]; 
  fC44=cc[14];
}

//_____________________________________________________________________________
void StiTrackNode::setAsCopyOf(StiTrackNode * node)
{
  fX    = node->fX;
  fAlpha= node->fAlpha;
  fdEdx = node->fdEdx;
  fChi2  = node->fChi2;
  mDepth = node->mDepth;

  fP0   = node->fP0;
  fP1   = node->fP1;
  fP2   = node->fP2;
  fP3   = node->fP3;
  fP4   = node->fP4;
  // covariance error matrix
  fC00  = node->fC00;
  fC10  = node->fC10;
  fC11  = node->fC11;
  fC20  = node->fC20;
  fC21  = node->fC21;
  fC22  = node->fC22;
  fC30  = node->fC30;
  fC31  = node->fC31;
  fC32  = node->fC32;
  fC33  = node->fC33;
  fC40  = node->fC40;
  fC41  = node->fC41;
  fC42  = node->fC42;
  fC43  = node->fC43;
  fC44  = node->fC44;
}

ostream& operator<<(ostream& os, const StiTrackNode& n)
{
  return os << n.fX  <<"\t"
  << n.fAlpha<<"\t"
  << n.fdEdx <<"\t"
  << n.fChi2 <<"\t"
  << n.mDepth<<"\t"
  << n.fP0 <<"\t"
  << n.fP1 <<"\t"
  << n.fP2 <<"\t"
  << n.fP3 <<"\t"
  << n.fP4 <<"\t"
  << n.fC00<<"\t"
  << n.fC10<<"\t"
  << n.fC11<<"\t"
  << n.fC20<<"\t"
  << n.fC21<<"\t"
  << n.fC22<<"\t"
  << n.fC30<<"\t"
  << n.fC31<<"\t"
  << n.fC32<<"\t"
  << n.fC33<<"\t"
  << n.fC40<<"\t"
  << n.fC41<<"\t"
  << n.fC42<<"\t"
  << n.fC43<<"\t"
  << n.fC44;
}
