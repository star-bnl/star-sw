/*********************************************************************
 * $Id: StRichIonization.cxx,v 1.9 2000/04/05 16:00:38 lasiuk Exp $
 *
 * Description:
 *  StRichIonization simulates the charged particle track through the gas.
 *  The main parameters of the process are the average number of
 *  ionization clusters per length unit and the distribution of
 *  ionized electrons in each of those clusters.
 *
 *  The function first randomly (poisson) choses a number of
 *  interaction clusters on the electrons path through the gas
 *  and then determines also randomly (e_distribut) how
 *  many electrons were ionized at each cluster. Each electron
 *  is then passed to the InduceSignal algorithm.
 *
 *  e_distribut: p is chosen and then compared to the Nth
 *  column of e_distribut. If p fails the "greater than" test
 *  then the number of electrons ionized is given by N. If not
 *  the same check is done with the N+1 column.
 *
 * Implementation of the StRichIonization function object.
 *
 *********************************************************************
 * $Log: StRichIonization.cxx,v $
 * Revision 1.9  2000/04/05 16:00:38  lasiuk
 * viewer, gid, update
 *
 * Revision 1.8  2000/03/17 14:54:44  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.7  2000/03/12 23:56:34  lasiuk
 * new coordinate system
 * exchange MyRound with inline templated funtion
 *
 * Revision 1.6  2000/02/14 01:14:06  lasiuk
 * add track_p to GHit c'tor
 *
 * Revision 1.5  2000/02/11 21:11:37  lasiuk
 * use new name in physics db interface
 *
 * Revision 1.4  2000/02/08 23:51:13  lasiuk
 * removal of rrs macro---CC4.2 cannot handle it!
 *
 * Revision 1.3  2000/02/08 16:27:34  lasiuk
 * change to class.  Put dbs and random generators into
 * class as data members
 *
 * Revision 1.2  2000/01/25 22:02:21  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:02  lasiuk
 * Initial Revision
 *********************************************************************/
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif
#include "StRichIonization.h"
#include "StRichPhysicsDb.h"

#ifdef RICH_WITH_VIEWER
#include "StRichViewer.h"
#endif

#include "SystemOfUnits.h"
#include "PhysicalConstants.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

StRichIonization::StRichIonization()
{
    mPhysicsDb = StRichPhysicsDb::getDb();
    mAverageNumberOfInteractions      = mPhysicsDb->averageNumberOfInteractions();
    mMaximumElectronEnergyProbability = mPhysicsDb->maximumElectronEnergyProbability();

    //
    // initialization of bethe-bloch parameters for CH4
    //
    // mKonstant = constant in Bethe-Bloch curve formalism
    // mAlfat    = (alpha)(t)
    // mZa       = Z/A for CH4
    // mIonize   = ionization potential for CH4
    // mDensity  = density of CH4
    // mSaturationValue = low enery cutoff
    //                    in 1/beta**2 part of the curve
    //
    // *** SEE paper for description
    // mFirstCorner = x0
    // mPlateau     = x1
    // mShapeOfRise = m
    // interSectionOfRiseAndPlateau = xa
    // bigx           = log(bg) in base 10
    // saturationTerm = d

    mKonstant = 0.1536*MeV*centimeter2/gram;
    mZa       = 10./16.;
    mAlfat    = mKonstant*gram/MeV/centimeter2*mZa;
    mIonize   = 13.6*eV;
    mDensity  = 0.000670*gram/centimeter3;
    mSaturationValue = 8.;
    
    
    // first corner -- x0
    mFirstCorner = .426266;

    // plateau -- x1
    mPlateau = 4.;
    //double  = 2.; makes minimium more peaked

    // shape of rise
    //double m = 2.5;
    mShapeOfRise = 1.5;

    // rise and plateau intersection -- xa
    mIntersectionOfRiseAndPlateau =
	log(1.649*mIonize/eV/(28.8*sqrt(mDensity*centimeter3/gram*mZa)));

    mF = 4.606*(mIntersectionOfRiseAndPlateau-mFirstCorner)/(pow((mPlateau-mFirstCorner),mShapeOfRise));   
}

StRichIonization::~StRichIonization() { /* nopt */ }

void StRichIonization::splitSegment(const StRichGHit* hit, list<StRichMiniHit*>& aList) const
{
    // locals
    double t, x, y, z;
    double p;
    unsigned int n;


    double betheBlochScaleFactor = 1;
//      if(hit->mass()) {
//  	double bg = abs(hit->momentum())/hit->mass();
//  	PR(bg);
//  	betheBlochScaleFactor = betheBloch(bg);
//      }
    int numberOfInteractions =
	mRandom.Poisson(nearestInteger(hit->ds() * betheBlochScaleFactor*mAverageNumberOfInteractions) );
//     PR(numberOfInteractions);
//     sleep(1);

    for (int i=0; i<numberOfInteractions; i++) {
	//
	// t between -.5 and .5
	t = hit->ds() * ( mRandom.Flat(1.0) - 0.5 );
	//
	// x,y and z of interaction
	x = hit->position().x()  +  t * hit->cosX();
	y = hit->position().y()  +  t * hit->cosY();
	z = hit->position().z()  +  t * hit->cosZ();
	
	//
	// random selector from 0 to e_max
	//
	p = mRandom.Flat(1.0) * mMaximumElectronEnergyProbability;
	
	// number of electrons in this cluster
	//
	n = 1;
	while ( n <= mPhysicsDb->e_distribut.size() && p >= mPhysicsDb->e_distribut[n-1] )
	    n++;
	
#ifdef RICH_WITH_VIEWER
	if (StRichViewer::histograms )
	    StRichViewer::getView()->mClusterElectrons->Fill(n);
#endif
	if(RRS_DEBUG)
	    cout << "StRichIonize::operator() n " <<  n << endl;
	for (unsigned int j=0; j<n; j++ ) {
	    aList.push_back(new StRichMiniHit(StThreeVector<double>(x,y,z),
					      hit->momentum(),
					      hit->trackp(),
					      hit->id(),
					      hit->gid(),
					      hit->mass(),
					      eCharged));
	}
    }
    
}

double StRichIonization::betheBloch(double bg)
{
    //
    // Bethe-Bloch parameterization taken from:
    // A.H Walenta et al. NIM 161 (1979) 45 (see Brian for reference)
    // Gas is taken as CH4
    // nomenclature is as follows:

    // Low energy cutOFF:
    if(log(bg)<-1)
 	return mSaturationValue;

    double bigx = log(bg)/log(10.);
    
    // Saturation term
    //double d;
    double saturationTerm;
    if (bigx<mFirstCorner)
	saturationTerm=0;
    else if ((bigx>mFirstCorner) && (bigx<mPlateau))
	saturationTerm=4.606*(bigx-mIntersectionOfRiseAndPlateau)+mF*(pow((mPlateau-bigx),mShapeOfRise));
    else if(bigx>mPlateau)
	saturationTerm=4.606*(bigx-mIntersectionOfRiseAndPlateau);

    // Bethe-Bloch Parameterization:
    double konstant=.891;

    // Compute the minimum
    double bgMin = 3.;
    double te =
	konstant + 2*log(bgMin) - (sqr(bgMin)/(sqr(bgMin)+1)) - log(sqr(bgMin)/(sqr(bgMin)+1));
    double Io
	= mAlfat*((sqr(bgMin)+1)/sqr(bgMin))*(log((electron_mass_c2/MeV*mAlfat)/(sqr(mIonize)))+te);
      
    te = konstant + 2*log(bg) - (sqr(bg)/(sqr(bg)+1)) - log(sqr(bg)/(sqr(bg)+1)) - saturationTerm;
    double I = mAlfat*((sqr(bg)+1)/sqr(bg))*(log((electron_mass_c2/MeV*mAlfat)/(sqr(mIonize)))+te);

    return (I/Io);
}
