/*!
 * $Id: StiHitErrorCalculator.cxx,v 2.0 2002/12/10 21:59:13 pruneau Exp $  
 *
 * Author: A. Rose, WSU, Jan 2002
 *
 * The Hit Error Calculator class is responsible for returning
 * errors for a hit from an arbitrary detector acording to a 
 * parameterization established by Mike Lisa et. al.,
 *
 * http://www.star.bnl.gov/~lisa/HitErrors (Mar 14, 2001)
 *
 *
 * $Log: StiHitErrorCalculator.cxx,v $
 * Revision 2.0  2002/12/10 21:59:13  pruneau
 * Introducing version 2.0
 *
 *
 */


//Sti inlcudes
#include "StiKalmanTrackNode.h"
#include "StiHitErrorCalculator.h"

StiDefaultHitErrorCalculator::StiDefaultHitErrorCalculator()
{
  set(0.,0.,0.,0.,0.,0.); 
}

StiDefaultHitErrorCalculator::~StiDefaultHitErrorCalculator()
{}

void StiDefaultHitErrorCalculator::set(double intrinsicZ, double driftZ,
				    double crossZ, double intrinsicX,
				    double driftX, double crossX)
{
  coeff[0]= intrinsicZ;
  coeff[1]= driftZ;
  coeff[2]= crossZ;
  coeff[3]= intrinsicX;
  coeff[4]= driftX;
  coeff[5]= crossX;
}

void StiDefaultHitErrorCalculator::calculateError(StiKalmanTrackNode & node) const
{  
  StiHit * hit = node.getHit();
  double dz = (fabs(hit->z())-200.)/100.;
  double cosCA = node._cosCA;
  double sinCA = node._sinCA;
  double tanCA = sinCA/cosCA;
  double ecross=coeff[0]-coeff[1]*dz/cosCA +coeff[2]*tanCA*tanCA;
  double tanDip=node.getTanL();
  double cosDipInv=sqrt(1+tanDip*tanDip);
  double edip=coeff[3]-coeff[4]*dz*cosDipInv+coeff[5]*tanDip*tanDip;
  node.eyy = 10000.*ecross*ecross;
  node.ezz = 10000.*edip*edip;
}

