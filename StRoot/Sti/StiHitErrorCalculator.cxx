
/*!
 * $Id: StiHitErrorCalculator.cxx,v 2.31 2006/04/07 18:01:55 perev Exp $  
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
 * Revision 2.31  2006/04/07 18:01:55  perev
 * Back to the latest Sti
 *
 * Revision 2.29  2005/12/18 23:36:53  perev
 * Dependency from StiKalmanTrackNode removed
 *
 * Revision 2.28  2005/12/08 04:14:49  perev
 * new place for StiDebug.h
 *
 * Revision 2.27  2005/12/07 21:24:51  perev
 * Cleanup
 *
 * Revision 2.26  2005/05/31 16:32:21  perev
 * Max & min errors changed to 1e-4,1.
 *
 * Revision 2.25  2005/05/12 17:47:57  perev
 * TPC size 200==>210
 *
 * Revision 2.24  2005/04/11 17:25:29  perev
 * Cleanup
 *
 * Revision 2.23  2005/03/24 17:58:33  perev
 * Do not allow to modify node anymore
 *
 * Revision 2.22  2005/01/17 01:31:25  perev
 * New parameter model
 *
 * Revision 2.21  2004/12/11 04:31:36  perev
 * set of bus fixed
 *
 * Revision 2.20  2004/03/22 23:07:12  pruneau
 * Added comments to operator<<
 *
 * Revision 2.19  2004/02/19 20:40:15  pruneau
 *  Added the notion of loadbale to the builders
 *
 * Revision 2.18  2004/02/02 22:22:54  pruneau
 * Fixed include
 *
 * Revision 2.17  2004/01/30 21:39:38  pruneau
 * Added load method to fetch db data
 *
 * Revision 2.16  2003/10/28 15:58:01  andrewar
 * Added set method to set parameters from input file.
 *
 * Revision 2.15  2003/09/07 03:49:08  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 2.14  2003/08/13 21:04:03  pruneau
 * transfered relevant tracking pars to detector builders
 *
 * Revision 2.13  2003/07/30 19:18:23  pruneau
 * sigh
 *
 * Revision 2.11  2003/07/14 21:12:43  andrewar
 * Revert to old version to eliminate introduced bug
 *
 * Revision 2.9  2003/06/26 20:22:18  andrewar
 * Fixed error in drift calc.
 *
 * Revision 2.8  2003/05/09 22:07:54  pruneau
 * Added protection to avoid 90deg tracks and ill defined eloss
 *
 * Revision 2.7  2003/05/09 14:57:19  pruneau
 * Synching
 *
 * Revision 2.6  2003/05/01 20:46:41  pruneau
 * changed error parametrization
 *
 * Revision 2.5  2003/04/22 21:20:05  pruneau
 * Added hit filter
 * Tuning og finder pars
 * Tuning of KalmanTrackNode
 *
 * Revision 2.4  2003/04/10 12:02:12  pruneau
 * various changes
 *
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
#include "StiTrackNode.h"
#include "StiHitErrorCalculator.h"
#include "tables/St_HitError_Table.h"
#include "StiUtilities/StiDebug.h"

//_____________________________________________________________________________
StiDefaultHitErrorCalculator::StiDefaultHitErrorCalculator()
{
  set(0.,0.,0.,0.,0.,0.); 
}

//_____________________________________________________________________________
StiDefaultHitErrorCalculator::StiDefaultHitErrorCalculator(const StiDefaultHitErrorCalculator & e)
{
  memcpy(coeff,e.coeff,sizeof(coeff));
}

//_____________________________________________________________________________
const StiDefaultHitErrorCalculator & StiDefaultHitErrorCalculator::operator=(const StiDefaultHitErrorCalculator & calc)
{
  memcpy(coeff,calc.coeff,sizeof(coeff));
	return *this;
}

//_____________________________________________________________________________
const StiDefaultHitErrorCalculator & StiDefaultHitErrorCalculator::operator=(const HitError_st & e)
{
  memcpy(coeff,e.coeff,sizeof(coeff));
  return *this;
}

//_____________________________________________________________________________
StiDefaultHitErrorCalculator::~StiDefaultHitErrorCalculator()
{}

//_____________________________________________________________________________
void StiDefaultHitErrorCalculator::set(double intrinsicZ,double driftZ,double crossZ
                                      ,double intrinsicX,double driftX,double crossX)
{
  coeff[0]= intrinsicZ;
  coeff[1]= driftZ;
  coeff[2]= crossZ;
  coeff[3]= intrinsicX;
  coeff[4]= driftX;
  coeff[5]= crossX;
  if (coeff[0]<=0) return;
  cout << *this << endl;
}

//_____________________________________________________________________________
//Load values from given input file stream
void StiDefaultHitErrorCalculator::loadFS(ifstream& iFile)
{
	cout << "StiDefaultHitErrorCalculator::loadFS(ifstream& iFile) -I- Started" <<endl;
  iFile >> coeff[0] >> coeff[1] >> coeff[2];
  iFile >> coeff[3] >> coeff[4] >> coeff[5];
  cout << *this;
  cout << "StiDefaultHitErrorCalculator::loadFS(ifstream& iFile) -I- Done" <<endl;
}

//_____________________________________________________________________________
//Load values from given dataset
void StiDefaultHitErrorCalculator::loadDS(TDataSet & ds)
{
  cout << "StiDefaultHitErrorCalculator::loadDS(TDataSet & ds) -I- Started" <<endl;
	cout << "    Fetching Hit Error for :" << getName() << endl;
  St_HitError * a = dynamic_cast<St_HitError*>(ds.Find(getName().c_str() ));
  assert(a);
  HitError_st * b = a->GetTable();
  assert(b);
  memcpy(coeff,b->coeff,sizeof(coeff));
  cout << *this;
  cout << "StiDefaultHitErrorCalculator::loadDS(TDataSet & ds) -I- Done" <<endl;
}

//_____________________________________________________________________________
ostream& operator<<(ostream& os, const StiDefaultHitErrorCalculator& c)
{
  cout <<"Hit Error Parameters: "
       <<"[0] "<<c.coeff[0] << endl
       <<"[1] "<<c.coeff[1] << endl
       <<"[2] "<<c.coeff[2] << endl
       <<"[3] "<<c.coeff[3] << endl
       <<"[4] "<<c.coeff[4] << endl
       <<"[5] "<<c.coeff[5]<<endl;
  return os;	
}

//_____________________________________________________________________________
void StiDefaultHitErrorCalculator::calculateError(const StiNodePars *pars
                                                 ,double &ecross,double &edip) const 
{  
static const double tenMicrons = 1e-3;
static const double min2Err = tenMicrons*tenMicrons;
static const double max2Err = 1.;

  double dz = (210.-fabs(pars->_z))/100.;
  double cosCA = pars->_cosCA;
  double sinCA = pars->_sinCA;
  if (cosCA<0.01) cosCA=0.01;
  double tanCA = sinCA/cosCA;
  ecross=coeff[0]+coeff[1]*dz/(cosCA*cosCA) +coeff[2]*tanCA*tanCA;
  if (ecross< min2Err) ecross = min2Err;
  if (ecross> max2Err) ecross = max2Err;
  double tanDip=pars->_tanl;
  double cosDipInv2=1+tanDip*tanDip;
         edip=coeff[3]+coeff[4]*dz*cosDipInv2+coeff[5]*tanDip*tanDip;
  if (edip< min2Err) edip = min2Err;
  if (edip> max2Err) edip = max2Err;
}

