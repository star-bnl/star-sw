/*!
 * $Id: StiHitErrorCalculator.cxx,v 2.3 2003/04/04 14:43:44 pruneau Exp $  
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
 * Revision 2.3  2003/04/04 14:43:44  pruneau
 * Fix to the hit error calculator and the getCharge methods.
 *
 * Revision 2.2  2003/04/02 16:45:19  pruneau
 * Fixed error calculation and impose hard coded upper cut on the size of the error.
 *
 * Revision 2.1  2003/01/08 21:17:33  pruneau
 * Addind class StiSortedHitIterator to work in the seed finder
 * and StiDummyVertex finder to provide an StEvent based vertex
 * retrieval mechanism.
 *
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

void StiDefaultHitErrorCalculator::calculateError(StiKalmanTrackNode * node) const
{  
  double dz = (fabs(node->getZ())-200.)/100.;
  double cosCA = node->_cosCA;
  double sinCA = node->_sinCA;
  if (cosCA==0.)
    cosCA=1.e-10;
  double tanCA = sinCA/cosCA;
  double ecross=coeff[0]-coeff[1]*dz/(cosCA*cosCA) +coeff[2]*tanCA*tanCA;
  double tanDip=node->getTanL();
  double cosDipInv=sqrt(1+tanDip*tanDip);
  double edip=coeff[3]-coeff[4]*dz*cosDipInv+coeff[5]*tanDip*tanDip;
  if (ecross>20.e-2) ecross = 20.e-2; // cp 04/02/2003
  if (edip>20.e-2) edip = 20.e-2;     // in meters here
  node->eyy = 10000.*ecross; // in centimeters here...
  node->ezz = 10000.*edip;
  //if (ecross<0 || edip<0 || node->eyy<=0. || node->eyy>10. || node->ezz<=0. || node->ezz>10.)
  //  cout << " Hit Error - ecross:"<<ecross<<" edip:"<<edip
  //<<" node->eyy:"<<node->eyy<<" node->ezz:"<<node->ezz<<endl;
}

