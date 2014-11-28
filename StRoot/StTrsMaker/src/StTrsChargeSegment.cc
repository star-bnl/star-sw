/***************************************************************************
 *
 * $Id: StTrsChargeSegment.cc,v 1.43 2011/07/18 21:30:31 genevb Exp $
 *
 * Author: brian May 18, 1998
 *
 ***************************************************************************
 *
 * Description:   Input charge segment...much like a g2t_tpc_hit but
 *              with added functionality
 *
 ***************************************************************************
 *
 *
 * $Log: StTrsChargeSegment.cc,v $
 * Revision 1.43  2011/07/18 21:30:31  genevb
 * Unify GEANT PID discrimination to StarClassLibrary
 *
 * Revision 1.42  2011/01/18 14:40:15  fisyak
 * Clean up TpcDb interfaces and Tpc coordinate transformation
 *
 * Revision 1.41  2009/11/03 14:34:19  fisyak
 * Remove default in zFromTB
 *
 * Revision 1.40  2005/12/12 21:00:12  perev
 * 3 random generators ==> 1
 *
 * Revision 1.39  2004/04/08 20:51:39  perev
 * bug fix low limit must be -10, not 10.
 *
 * Revision 1.38  2004/04/07 18:58:57  perev
 * Cleanup
 *
 * Revision 1.37  2003/12/24 13:44:52  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.36  2003/09/02 17:59:19  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.35  2002/04/26 06:05:26  long
 * add triton and correct charge for he3
 *
 * Revision 1.34  2002/04/25 21:40:23  long
 * add he3,he4,deuteron
 *
 * Revision 1.33  2002/04/25 21:37:03  long
 * *** empty log message ***
 *
 * Revision 1.32  2001/11/21 01:53:42  long
 * adding log message for 3/2001 long;
 *
 * adding:
 *
 *     if(aMiniSegment.position().z()>0)
 *      listOfMiniSegments->push_back(aMiniSegment);  //HL,03/2001 protect against
 *       negative z.
 *
 * Revision 1.31  2001/03/14 23:43:39  long
 * *** empty log message ***
 *
 * Revision 1.30  2001/03/07 20:22:56  long
 * *** empty log message ***
 *
 * Revision 1.29  2000/11/23 02:08:49  lbarnby
 * More protection against Nan
 *
 * Revision 1.28  2000/11/09 19:23:09  lbarnby
 * Cernlib entry point changed (dstlan->dislan) Fix for optimization bug, prevent use of nan. Proper robust solution still required.
 *
 * Revision 1.27  2000/07/30 02:59:23  long
 * *** empty log message ***
 *
 * Revision 1.26  2000/07/30 02:38:36  long
 * comment out numberofLevel
 *
 * Revision 1.25  2000/06/23 00:12:40  snelling
 * Removed dependence on local files now pointed to StDbUtilities
 *
 * Revision 1.24  2000/02/24 16:19:07  long
 * take away straight line model due to changes made by GEANT on de<0 case
 * 
 *Revision 1.24  2000/02/20 16:22:33  long
 * take away straight line model due to changes made by GEANT on de<0 case
 *
 * Revision 1.23  2000/02/10 01:21:49  calderon
 * Switch to use StTpcDb.
 * Coordinates checked for consistency.
 * Fixed problems with StTrsIstream & StTrsOstream.
 *
 * Revision 1.22  2000/01/31 20:38:33  lasiuk
 * namespace for HP (random_shuffle)
 *
 * Revision 1.21  2000/01/10 23:14:30  lasiuk
 * Include MACROS for compatiblity with SUN CC5
 *
 * Revision 1.20  1999/12/08 02:10:42  calderon
 * Modified to eliminate warnings on Linux.
 *
 * Revision 1.19  1999/11/12 01:42:15  long
 * delete "fabs((magDb->at(mSector12Position)).z())>0.01)" because it is not needed
 *
 * Revision 1.18  1999/10/22 15:51:47  calderon
 * Remove ifdefs for erf.  Problem is solved by loading libm at the
 * macro level.
 *
 * Revision 1.17  1999/10/22 00:00:13  calderon
 * -added macro to use Erf instead of erf if we have HP and Root together.
 * -constructor with char* for StTrsDedx so solaris doesn't complain
 * -remove mZeros from StTrsDigitalSector.  This causes several files to
 *  be modified to conform to the new data format, so only mData remains,
 *  access functions change and digitization procedure is different.
 *
 * Revision 1.16  1999/10/04 16:22:33  long
 * straight line model in case of de<0
 *
 * Revision 1.15  1999/09/24 01:23:30  fisyak
 * Reduced Include Path
 *
 * Revision 1.14  1999/07/20 02:18:06  lasiuk
 * remove CVS merge conflicts
 *
 * Revision 1.13  1999/07/19 21:37:42  lasiuk
 * - constructor arguments reordered for increased speed
 * - tssSplit() and associated parameterizations from
 *   tss are included (requires linking with cernlib)
 * - introduce static random number generators
 * - whichGEANTParticle() introduced for g2t input
 *
 * Revision 1.12  1999/06/16 14:26:52  fisyak
 * Add flags for egcs on Solaris
 *
 * Revision 1.11  1999/03/16 02:00:40  lasiuk
 * use STL in ionization generation;
 * do not always use betaGamma as a scale factor;
 * still a problem with the neutrals depositing energy
 *
 * Revision 1.10  1999/03/15 13:49:30  lasiuk
 * field is initialized with Tesla in the data base!
 *
 * Revision 1.9  1999/03/02 17:51:45  lasiuk
 * geant PID
 *
 * Revision 1.8  1999/02/28 20:15:17  lasiuk
 * splitting test/add muon to pid
 *
 * Revision 1.7  1999/02/18 21:18:33  lasiuk
 * rotate() mods to StTpcCoordinateTranform
 *
 * Revision 1.6  1999/02/16 18:15:41  fisyak
 * Check in the latest updates to fix them
 *
 * Revision 1.5  1999/02/12 01:26:37  lasiuk
 * Limit debug output
 *
 * Revision 1.4  1999/02/10 18:02:24  lasiuk
 * verbose output and ostream
 *
 * Revision 1.3  1999/01/28 02:50:28  lasiuk
 * beta gamma for particle mass
 *
 * Revision 1.2  1999/01/15 10:59:11  lasiuk
 * remove g2t pointer
 * add pid member; add systemofunits; mv access fcts to .hh
 * add ostream operator
 *
 **************************************************************************/
#include "StTrsChargeSegment.hh"

#include <algorithm>
#if  defined(__SUNPRO_CC) && __SUNPRO_CC < 0x500
#include <ospace/stl/src/randgen.cpp>
#endif

#ifndef ST_NO_NAMESPACES
using std::random_shuffle;
#endif

#include "StPhysicalHelix.hh"
#include "StParticleTable.hh"
#include "StParticleDefinition.hh"

#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StTrsDeDx.hh"
#include "StTrsRandom.hh"
#include "TMath.h"
// Need a CERNLIB routine for tssSplit
extern "C"  float dislan_(float &x);
float dislan(float x) { return (x<-10.)? 0.:dislan_(x);}
 
HepJamesRandom  StTrsChargeSegment::mEngine;
RandFlat        StTrsChargeSegment::mFlatDistribution(mEngine);

StTrsChargeSegment::StTrsChargeSegment()
    : mPosition(0,0,0),
      mSector12Position(mPosition),
      mMomentum(0,0,0),
      mId(0),
      mDE(0),
      mDs(0),
      mPid(0),
      mNumberOfElectrons(-1),
      mSectorOfOrigin(0)

{ /* nopt */ }

StTrsChargeSegment::StTrsChargeSegment(StThreeVector<double>& pos,
				       StThreeVector<double>& mom,
				       int    id,
				       double de,
				       double ds,
				       int    pid,
				       double ne)
    : mPosition(pos),
      mSector12Position(mPosition),
      mMomentum(mom),
      mId(id),
      mDE(de),
      mDs(ds),
      mPid(pid),  // default is -1
      mNumberOfElectrons(ne), //default is -1
      mSectorOfOrigin(0)
{ /* nopt */ }

StTrsChargeSegment::~StTrsChargeSegment() {/* nopt */ }


void StTrsChargeSegment::whichGEANTParticle(double& particleMass, int& charge)
{
    StParticleDefinition* pDef = StParticleTable::instance()->findParticleByGeantId(mPid);
    if (pDef) {
      particleMass = pDef->mass();
      charge = (int) (pDef->charge());
    } else {
      // Probably uncharged, but DO NOT BREAK IT!
      cout << "Mass is Undefined (" << mPid << ")" << endl;
      particleMass = 0*GeV;
      charge = 0;
      //subSegments = 1;
    }
}

#ifndef ST_NO_TEMPLATE_DEF_ARGS
void StTrsChargeSegment::split(StTrsDeDx*       gasDb,
			       StMagneticField* magDb,
			       int subSegments, 
			       list<StTrsMiniChargeSegment>* listOfMiniSegments)
#else
void StTrsChargeSegment::split(StTrsDeDx*       gasDb,
			       StMagneticField* magDb,
			       int subSegments,
			       list<StTrsMiniChargeSegment,allocator<StTrsMiniChargeSegment> >* listOfMiniSegments)
#endif
{
#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<int>   ionization(StTrsDeDx::numberOfElectrons);
    vector<float> theIonization;

#else
    vector<int, allocator<int> >     ionization(StTrsDeDx::numberOfElectrons);
    vector<float, allocator<float> > theIonization;
#endif


    //
    // Calculate the number of electrons in complete segment
    //
    if (mNumberOfElectrons<0)
	mNumberOfElectrons = (mDE)/(gasDb->W());
    
//     PR(mDE/eV);
//     PR(gasDb->W());
//     PR(mNumberOfElectrons);
    //
    // Only do costly initialization if you
    // break into more than one subsegment
    //
    double particleMass;
    int    charge;

    if(subSegments>1) {
	//
	// PID are GEANT3 Conventions
	// Must get from pid structures
	whichGEANTParticle(particleMass, charge);

    }

    //
    // Double Check
    if(subSegments > 1) {  // if an uncharged particle deposits energy, do not split it

	// what is the subsegment length?
	 
	double deltaS = mDs/static_cast<double>(subSegments);

	// set the segment length in the gasDb:
	gasDb->setPadLength(deltaS*centimeter);

	//
	// theIonization
	//
	theIonization.clear();

	// number of segments to split given by command line argument (default 1):
	//  should be related to mNumberOfElectrons
	 
	double ionizationLeft = mNumberOfElectrons;

	for(int ii=0; ii<(subSegments-1); ii++) {
	    //
	    // generate electrons
	    double betaGamma = abs(mMomentum)/particleMass;
	    // 	     PR(betaGamma);
	    if(betaGamma>3)
		gasDb->electrons(ionization, betaGamma);
	    else
		gasDb->electrons(ionization);  // betaGamma = 3
	    
	    // Don't generate too much ionization
	    if(ionization[StTrsDeDx::total] > ionizationLeft) {
		theIonization.push_back(ionizationLeft);
	    }
	    else {
		theIonization.push_back(ionization[StTrsDeDx::total]);
	    }
	
	    ionizationLeft -= theIonization.back();
// 	     PR(ionizationLeft);
	     
	} // loop over subsegments
	theIonization.push_back(ionizationLeft);
	 
	//copy(theIonization.begin(),theIonization.end(), ostream_iterator<float>(cout,","));
	random_shuffle(theIonization.begin(), theIonization.end(),StTrsRandom::inst());
	//cout << endl;
	//copy(theIonization.begin(), theIonization.end(), ostream_iterator<float>(cout,","));
     
	//To decompose track use the helix parameterization.
	//StPhysicalHelix(p,x,B,+/-)
	// Need some track info from pid:

	StPhysicalHelix
	    track(mMomentum,
		  mSector12Position,
		  (magDb->at(mSector12Position)).z(), //*tesla NOT now!
		  charge);
	
//  	PR(particleMass/GeV);
// 	PR(mMomentum.mag());

	//
	// calculate the offset to the start position
	//
	double newPosition = -mDs/2. + deltaS/2.;
	//PR(newPosition);
	
	//
	// loop over all subSegments and distribute charge
	//
    
	//cout << "ionization.size() " << (ionization.size()) << endl;
// 	PR(subSegments);
	for(int i=0; i<subSegments; i++) {
	    // Take the electrons from the vector
	    
	    // If there is no Ionization, take the next
	    if(!theIonization[i]) {
		newPosition += deltaS;
		continue;
	    }
	    
 	    //PR(newPosition);
 	    //PR(track.at(newPosition));
	    
	    StTrsMiniChargeSegment aMiniSegment(track.at(newPosition),
						theIonization[i],
						deltaS, mId);
	    listOfMiniSegments->push_back(aMiniSegment);
	    
	    newPosition += deltaS;
	    // 	PR(newPosition);
	} // loop over subsegments
	
    } // if (subsegments > 1)  ---> allows us to skip the helix construction
    else if(subSegments == 1) {
	StTrsMiniChargeSegment aSingleMiniSegment(mPosition,
						  mNumberOfElectrons,
						  mDs, mId);
        
	if(aSingleMiniSegment.position().z()>0)listOfMiniSegments->push_back(aSingleMiniSegment);
// 	PR(mPosition);
// 	PR(mNumberOfElectrons);
    }

}
#ifndef ST_NO_TEMPLATE_DEF_ARGS
void StTrsChargeSegment::tssSplit(StTrsDeDx*       gasDb,
				  StMagneticField* magDb,
				  int  numberOfLevels  ,
				  list<StTrsMiniChargeSegment>* listOfMiniSegments)
#else
void StTrsChargeSegment::tssSplit(StTrsDeDx*       gasDb,
				  StMagneticField* magDb,
				  int  numberOfLevels  ,
				  list<StTrsMiniChargeSegment,allocator<StTrsMiniChargeSegment> >* listOfMiniSegments)
#endif
{
#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif

    //
    // Calculate the numser of electrons in complete segment
    //
  
    if (mNumberOfElectrons<0)
      //	mNumberOfElectrons = (mDE)/(gasDb->W());

                                      
      mNumberOfElectrons = fabs(mDE)/(gasDb->W());//HL,9/10/99
    
 
  
                                      

    //PR(mDE/eV);
    //PR(gasDb->W());
    //PR(mNumberOfElectrons);

    //  int numberOfLevels =
    //	static_cast<int>(::log(static_cast<double>(subSegments))/M_LN2 + .999);
	
    // numberOfLevels++;  // take care of the zero!

    double totalNumberOfSubSegments = ::pow(2.,(numberOfLevels-1));
	
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<double>          tmp;
    vector<vector<double> > ionizationSegments;

#else
    vector<double, allocator<double> >     tmp;
    typedef vector<double, allocator<double> > iSegs;
    vector<iSegs , allocator<iSegs> > ionizationSegments;
#endif

    //
    // Minimum ioinizing is a 3GeV proton!
    //
    
    double minimumIonizingdEdx =
	gasDb->betheBlochTSS(3.*GeV,1,.938*GeV);
    //PR(minimumIonizingdEdx/(keV/centimeter));
    int ii,jj;  // cntrs
    for(ii=0; ii<numberOfLevels; ii++)
	ionizationSegments.push_back(tmp);
	
    //
    // Fill dE/dx of complete segment
    //
    ionizationSegments[0].push_back(this->numberOfElectrons());
	
    //
    // Must Determine the particle mass!
    double particleMass;
    int    charge;
    whichGEANTParticle(particleMass, charge);

    //PR(this->momentum());
    //PR(particleMass/MeV);
    

    if(charge != 0) {
	double numberOfSubSegmentsInCurrentLevel;
	double numberOfSubSegmentsInPreviousLevel;
	    
	double dEdxBethe =
	    gasDb->betheBlochTSS(abs(this->momentum()),charge,particleMass);
	//PR(dEdxBethe/(MeV/centimeter));
	    
	double mip = dEdxBethe/minimumIonizingdEdx;
	    
	double xBinary;
      	
	for(ii=1; ii<numberOfLevels; ii++) {
	    numberOfSubSegmentsInCurrentLevel = 1<<(ii);
	    float dL = mDs/numberOfSubSegmentsInCurrentLevel;
	    //PR(dL/centimeter);
		
	    // In this level you must create "numberOfSubSegments" subsegments
	    // Loop over the existing ones...
	      numberOfSubSegmentsInPreviousLevel = 1<<(ii-1); 
	    for(jj=0; jj<numberOfSubSegmentsInPreviousLevel; jj++) {
		double parentdEdx = ionizationSegments[(ii-1)][jj];
		
		double lengthOfSegmentToBeSplit = dL*2.;
		double dEAverage = dEdxBethe*(lengthOfSegmentToBeSplit);
		    
		double dErelave =
		    (parentdEdx*gasDb->W())/dEAverage;
		    
		//PR(lengthOfSegmentToBeSplit);
		//PR(dErelave);
		//PR(mip);
		    
		// Must pass the length of segment in the units of centimeters!
		
		xBinary = binaryPartition((lengthOfSegmentToBeSplit/centimeter),
					  dErelave,mip);
		//PR(xBinary);
		ionizationSegments[ii].push_back(xBinary*parentdEdx);
		ionizationSegments[ii].push_back((1.-xBinary)*parentdEdx);
	    }
	} // levels
	    
	// --> Added here!
	    
	//
	// Distribution on a helix
	//
	
	double deltaS =
	    mDs/static_cast<double>(totalNumberOfSubSegments);
	    
	//To decompose track use the helix parameterization.
	//StPhysicalHelix(p,x,B,+/-)
	// Need some track info from pid:
     
	 
	StPhysicalHelix
	    track(mMomentum,
		  mSector12Position,
		  (magDb->at(mSector12Position)).z(), //*tesla NOT now!
		  charge);
	
	//PR(particleMass/GeV);
	//PR(mMomentum.mag());
	
	//
	// calculate the offset to the start position
	//
	double newPosition = -mDs/2. + deltaS/2.;
	
	//
	// loop over all subSegments and distribute charge
	//
	
	for(unsigned int i=0; i<ionizationSegments[(numberOfLevels-1)].size(); i++) {
	    // Take the electrons from the vector
	    StTrsMiniChargeSegment aMiniSegment(track.at(newPosition),
						ionizationSegments[(numberOfLevels-1)][i],
					deltaS, mId);
            if(aMiniSegment.position().z()>0)listOfMiniSegments->push_back(aMiniSegment);  //HL,03/2001 protect against negative z.	
	   
	    newPosition += deltaS;
	} // loop over subsegments

  
   
          
    } // charge !=0
    else {
	//   
	// Not sure what to do with this?
	//
	StTrsMiniChargeSegment aBadMiniSegment(mPosition,
					       mNumberOfElectrons,
					       mDs, mId);
	
    }
}

// Add member functions necessary for tssSplit
double StTrsChargeSegment::sigmaParameter(double l, double derelave, double xmip, int index) const
{
    double beta[2]  = {1.0, 2.0};
    double b[2]     = {0.0, -0.02};
    double gamma[2] = {.0041, .0052};
    double c[2]     = {-1.02, -1.02};
    double delta[2] = {-.022,-.0058};
    double d[2]     = {-1.88, -1.92};
    
    double a     = ::pow(xmip,delta[index])+d[index]+b[index]*derelave;
    double alpha = (::pow(xmip,gamma[index])+c[index])/(::pow(derelave,beta[index]));
    return (::pow(l,alpha)+a);
}

double StTrsChargeSegment::meanParameter(double l, double derelave, double xmip, int index) const
{
    double a[2]     = {0.367, 0.267};
    double alpha[2] = {.05, .1};
    double beta[2]  = {.2, 1.0};
    double gamma[2] = {.0423, .0852};

    return ( (a[index]*::pow(l,gamma[index])*::pow(xmip,alpha[index]))/(::pow(derelave,beta[index])) );
}

double StTrsChargeSegment::xReflectedGauss(double x0, double sig) const
{
    double granularity = .001;
    double root2Sigma  = M_SQRT2*sig;
    double denom = TMath::Erf((1.-x0)/root2Sigma) + TMath::Erf(x0/root2Sigma);
    double testValue = mFlatDistribution.shoot();
    
    double xlo =0.;
    double xhi = 1.;

    double x,p;
    do {
	x = .5*(xlo+xhi);
 	p =.5*(1. + (TMath::Erf((x-x0)/root2Sigma) + TMath::Erf((x+x0-1)/root2Sigma))/denom);
	if( (fabs(testValue-p)<granularity) || (fabs(xhi-xlo)<1.e-7)) {
	    return x;
	}
	else if (testValue>p) {
	    xlo = x;
	}
	else {
	    xhi = x;
	}
    } while(true);
}

double StTrsChargeSegment::xReflectedLandau(double x0, double sig) const
{
    // uses dislan_() from CERNLIB
    double granularity = .001;

    float arg3 = (1.-x0)/sig;
    float arg4 = -x0/sig;

    float dlan3 = dislan(arg3);
    float dlan4 = dislan(arg4);

    double denom = dlan3 - dlan4;

    double testValue = mFlatDistribution.shoot();
    
    double xlo =0.;
    double xhi = 1.;

    float arg1, arg2;
    float dlan1, dlan2;

    int counter=0;
    double x,p;
    do {
	x = .5*(xlo+xhi);
	arg1 = (x-x0)/sig;
	arg2 = (1.-x-x0)/sig;
	dlan1 = dislan(arg1);
	dlan2 = dislan(arg2);
	if (isnan(dlan1)) dlan1=0;
	p = .5*(1. + (dlan1 - dlan2)/denom);
	if( (fabs(testValue-p)<granularity) ) {
	    return x;
	}
	else if (testValue>p) {
	    xlo = x;
	}
	else {
	    xhi = x;
	}
	if(counter++ == 100){
	  cout << "Probable race condition in loop in StTrsChargeSegement::xReflectedLandau" << endl;
	  PR(arg1); PR(arg2); PR(arg3); PR(arg4);
          return x;
	}
    } while(true);
}

double StTrsChargeSegment::binaryPartition(double l, double derelave, double xmip) const
{
    int index;
    if(derelave<1.)
	index = 0;   // f2c indices
    else
	index = 1;

    double x0  = fabs(meanParameter(l,derelave,xmip,index));
    double sig = fabs(sigmaParameter(l,derelave,xmip,index));

    if(x0>1)  x0  =  .001;
    if(sig>1) sig = 1.0;

    double value;
    if(index == 0) {
	value = xReflectedGauss(x0,sig);
    }
    else {
	value = xReflectedLandau(x0,sig);
    }
    return value;
}


// Non-member
ostream& operator<<(ostream& os, const StTrsChargeSegment& seg)
{
    return os << "(Pos:" << seg.position() << ", mon:" << seg.momentum() 
	      << ", id:" << seg.id() 
	      << ", dE:" << seg.dE() 
	      << ", dS:" << seg.ds() 
	      << ", pid:" << seg.pid() 
	      << ")";
}
