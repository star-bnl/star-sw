/***************************************************************************
 *
 * $Id: StTrsChargeSegment.cc,v 1.13 1999/07/19 21:37:42 lasiuk Exp $
 *
 * Author: brian May 18, 1998
 *
 ***************************************************************************
 *
 * Description: Input charge segment...much like a g2t_tpc_hit but
 *              with added functionality
 *
 ***************************************************************************
 *
 * $Log: StTrsChargeSegment.cc,v $
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
#if  defined(__sun) && ! defined(__GNUG__)
#include "ospace/stl/src/randgen.cpp"
#endif

#include "StPhysicalHelix.hh"

#include "StTpcCoordinateTransform.hh"
#include "StTrsDeDx.hh"

// Need a CERNLIB routine for tssSplit
extern "C"  float dstlan_(float *);

HepJamesRandom  StTrsChargeSegment::mEngine;
RandFlat        StTrsChargeSegment::mFlatDistribution(mEngine);

StTrsChargeSegment::StTrsChargeSegment()
    : mPosition(0,0,0),
      mMomentum(0,0,0),
      mNumberOfElectrons(-1),
      mDE(0),
      mDs(0),
      mPid(0),
      mSectorOfOrigin(0),
      mSector12Position(mPosition)
{ /* nopt */ }

StTrsChargeSegment::StTrsChargeSegment(StThreeVector<double>& pos,
				       StThreeVector<double>& mom,
				       double de,
				       double ds,
				       int    pid,
				       double ne)
    : mPosition(pos),
      mMomentum(mom),
      mNumberOfElectrons(ne), //default is -1
      mDE(de),
      mDs(ds),
      mPid(pid),  // default is -1
      mSectorOfOrigin(0),
      mSector12Position(mPosition)
{ /* nopt */ }

StTrsChargeSegment::~StTrsChargeSegment() {/* nopt */ }


void StTrsChargeSegment::rotate(StTpcGeometry* geodb, StTpcSlowControl* SCdb, StTpcElectronics* elecDb)
{ // rotate to sector 12 ---use a coordinate transform:
    StTpcCoordinateTransform transformer(geodb, SCdb, elecDb);

    //cout << "rotate() position= " << mPosition << endl;

    mSector12Position =
	transformer.sector12Coordinate(mPosition, &mSectorOfOrigin);

//     PR(mSectorOfOrigin);
//     PR(mSector12Position);
}

void StTrsChargeSegment::whichGEANTParticle(double& particleMass, int& charge)
{
    //
    // This should really use the StParticle classes in the SCL
    //
    
    switch (mPid) {
    case 2:    // e+
	particleMass = .00051099906*GeV;
	charge = 1;
	break;
    case 3:    // e-
	particleMass = .00051099906*GeV;
	charge = -1;	      
	break;
    case 5:    // muon+
	particleMass = .105658389*GeV;
	charge = 1;
	break;
    case 6:    // muon-
	particleMass = .105658389*GeV;
	charge = -1;
	break;
    case 8:    // pion+
	particleMass = .1395700*GeV;
	charge = 1;
	break;
    case 9:    // pion-
	particleMass = .1395700*GeV;
	charge = -1;
	break;
    case 11:    // kaon+
	particleMass = .493677*GeV;
	charge = 1;
	break;
    case 12:    // kaon-
	particleMass = .493677*GeV;
	charge = -1;
	break;
    case 14:    // proton
	particleMass = .93827231*GeV;
	charge = 1;
	break;
    case 15:    // anti-proton
	particleMass = .93827231*GeV;
	charge = -1;
	break;
    default: // Probably uncharged, but DO NOT BREAK IT!
	//cout << "Mass is Undefined (" << mPid << ")" << endl;
	particleMass = 0*GeV;
	charge = 0;
	//subSegments = 1;
	break;
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
	random_shuffle(theIonization.begin(), theIonization.end());
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
	for(ii=0; ii<subSegments; ii++) {
	    // Take the electrons from the vector
	    
	    // If there is no Ionization, take the next
	    if(!theIonization[ii]) {
		newPosition += deltaS;
		continue;
	    }
	    
 	    //PR(newPosition);
 	    //PR(track.at(newPosition));
	    
	    StTrsMiniChargeSegment aMiniSegment(track.at(newPosition),
						theIonization[ii],
						deltaS);
	    listOfMiniSegments->push_back(aMiniSegment);
	    
	    newPosition += deltaS;
	    // 	PR(newPosition);
	} // loop over subsegments
	
    } // if (subsegments > 1)  ---> allows us to skip the helix construction
    else if(subSegments == 1) {
	StTrsMiniChargeSegment aSingleMiniSegment(mPosition,
						  mNumberOfElectrons,
						  mDs);
	listOfMiniSegments->push_back(aSingleMiniSegment);
// 	PR(mPosition);
// 	PR(mNumberOfElectrons);
    }

}
#ifndef ST_NO_TEMPLATE_DEF_ARGS
void StTrsChargeSegment::tssSplit(StTrsDeDx*       gasDb,
				  StMagneticField* magDb,
				  int subSegments, 
				  list<StTrsMiniChargeSegment>* listOfMiniSegments)
#else
void StTrsChargeSegment::tssSplit(StTrsDeDx*       gasDb,
				  StMagneticField* magDb,
				  int subSegments,
				  list<StTrsMiniChargeSegment,allocator<StTrsMiniChargeSegment> >* listOfMiniSegments)
#endif
{
#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif

    //
    // Calculate the number of electrons in complete segment
    //
    if (mNumberOfElectrons<0)
	mNumberOfElectrons = (mDE)/(gasDb->W());

    //PR(mDE/eV);
    //PR(gasDb->W());
    //PR(mNumberOfElectrons);

    int numberOfLevels =
	static_cast<int>(log(static_cast<double>(subSegments))/M_LN2 + .999);
	
    numberOfLevels++;  // take care of the zero!

    double totalNumberOfSubSegments = pow(2.,(numberOfLevels-1));
	
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
    //PR(charge);

    if(charge != 0) {
	double numberOfSubSegmentsInCurrentLevel;
	double numberOfSubSegmentsInPreviousLevel;
	    
	double dEdxBethe =
	    gasDb->betheBlochTSS(abs(this->momentum()),charge,particleMass);
	//PR(dEdxBethe/(MeV/centimeter));
	    
	double mip = dEdxBethe/minimumIonizingdEdx;
	    
	double xBinary;	
	for(ii=1; ii<numberOfLevels; ii++) {
	    numberOfSubSegmentsInCurrentLevel = pow(2.,ii);
	    float dL = mDs/numberOfSubSegmentsInCurrentLevel;
	    //PR(dL/centimeter);
		
	    // In this level you must create "numberOfSubSegments" subsegments
	    // Loop over the existing ones...
	    numberOfSubSegmentsInPreviousLevel = pow(2.,(ii-1));
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
	
	for(ii=0; ii<ionizationSegments[(numberOfLevels-1)].size(); ii++) {
	    // Take the electrons from the vector
	    StTrsMiniChargeSegment aMiniSegment(track.at(newPosition),
						ionizationSegments[(numberOfLevels-1)][ii],
						deltaS);
	    listOfMiniSegments->push_back(aMiniSegment);
	    
	    newPosition += deltaS;
	} // loop over subsegments
    } // charge !=0
    else {
	//
	// Not sure what to do with this?
	//
	StTrsMiniChargeSegment aBadMiniSegment(mPosition,
					       mNumberOfElectrons,
					       mDs);
	listOfMiniSegments->push_back(aBadMiniSegment);
    }
}

// Add member functions necessary for tssSplit
double StTrsChargeSegment::sigmaParameter(double& l, double& derelave, double& xmip, int& index) const
{
    double beta[2]  = {1.0, 2.0};
    double b[2]     = {0.0, -0.02};
    double gamma[2] = {.0041, .0052};
    double c[2]     = {-1.02, -1.02};
    double delta[2] = {-.022,-.0058};
    double d[2]     = {-1.88, -1.92};
    
    double a     = pow(xmip,delta[index])+d[index]+b[index]*derelave;
    double alpha = (pow(xmip,gamma[index])+c[index])/(pow(derelave,beta[index]));
    return (pow(l,alpha)+a);
}

double StTrsChargeSegment::meanParameter(double& l, double& derelave, double& xmip, int& index) const
{
    double a[2]     = {0.367, 0.267};
    double alpha[2] = {.05, .1};
    double beta[2]  = {.2, 1.0};
    double gamma[2] = {.0423, .0852};

    return ( (a[index]*pow(l,gamma[index])*pow(xmip,alpha[index]))/(pow(derelave,beta[index])) );
}

double StTrsChargeSegment::xReflectedGauss(double& x0, double& sig) const
{
    double granularity = .001;
    double root2Sigma  = M_SQRT2*sig;

    double denom = erf((1.-x0)/root2Sigma) + erf(x0/root2Sigma);

    double testValue = mFlatDistribution.shoot();
    
    double xlo =0.;
    double xhi = 1.;

    double x,p;
    do {
	x = .5*(xlo+xhi);
	p =.5*(1. + (erf((x-x0)/root2Sigma) + erf((x+x0-1)/root2Sigma))/denom);
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

double StTrsChargeSegment::xReflectedLandau(double& x0, double& sig) const
{
    // uses dstlan_() from CERNLIB
    double granularity = .001;

    float arg3 = (1.-x0)/sig;
    float arg4 = -x0/sig;
    
    double denom = dstlan_(&arg3) - dstlan_(&arg4);

    double testValue = mFlatDistribution.shoot();
    
    double xlo =0.;
    double xhi = 1.;

    float arg1, arg2;
    
    double x,p;
    do {
	x = .5*(xlo+xhi);
	arg1 = (x-x0)/sig;
	arg2 = (1.-x-x0)/sig;
	p = .5*(1. + (dstlan_(&arg1) - dstlan_(&arg2))/denom);
	if( (fabs(testValue-p)<granularity) ) {
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

double StTrsChargeSegment::binaryPartition(double l, double& derelave, double& xmip) const
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
    return os << '(' << seg.position() << ", " << seg.momentum() << ", " << seg.dE() << ", " << seg.ds() << ')';
}
