/***************************************************************************
 *
 * $Id: randomTest1.cc,v 1.2 2003/09/02 17:59:38 perev Exp $
 *
 * Author: Brian Lasiuk, May 1998 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: randomTest1.cc,v $
 * Revision 1.2  2003/09/02 17:59:38  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  1999/02/17 12:44:01  ullrich
 * New Revision
 *
 * Revision 1.1  1999/01/23 00:26:50  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <Stiostream.h>
#include <string>
#include "StGlobals.hh"
#include "StPrompt.hh"
#include "Random.h"

// the random engines
#include "JamesRandom.h"
#include "RanecuEngine.h"
#include "RanluxEngine.h"
#include "DRand48Engine.h"
#include "RandEngine.h"

// the different distributions
#include "RandFlat.h"
#include "RandPoisson.h"
#include "RandExponential.h"
#include "RandGauss.h"
#include "RandBreitWigner.h"

//         --------------------- MAIN --------------------------       //
int main()
{    
    HepJamesRandom  engine;       // default engine
    RanluxEngine    engine2;

    RandFlat        flatDistribution(engine);
    RandGauss       gaussDistribution(engine);
    RandExponential exponentialDistribution(engine);
    RandPoisson     poissonDistribution(engine);
    RandBreitWigner breitWignerDistribution(engine);
    
    const int size=5;                 // array size
    const int numberOfNumbers = 10;   // # of numbers to generate

    int    *vecI = new int[size];     // StInt    vecI[size]
    long   *vecL = new long[size];
    double *vec  = new double[size];  // StDouble vec[size];

    StInt distribution=1;

    // counters
    int i, jj;
    
    do {
	cout << "(1) Flat"         << "\n"
	     << "(2) Gaussian"     << "\n"
	     << "(3) Exponential"  << "\n"
	     << "(4) Poissonian"   << "\n"
	     << "(5) Breit-Wigner" << endl;
	
	StPrompt("Which Distribution?", distribution);

    if(distribution>0 && distribution<6)
	break;
    
    } while(true);

    
    double mean  = 2;
    double width = 10;
    double cut   = 4;

    // Single numbers:

    StDouble flatNumber;
    StInt    flatINumber;
    StDouble gaussNumber;
    StDouble exponentialNumber;
    StInt    poissonNumber;
    StDouble bwNumber;
    
    switch(distribution) {
    case 1:
	cout << "Flat Distribution:" << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    flatNumber = flatDistribution.shoot();
	    PR(flatNumber);
	}
	cout << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    flatNumber  = flatDistribution.shoot(width);
	    PR(flatNumber);
	}
	cout << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    flatNumber  = flatDistribution.shoot(width,width+5);
	    PR(flatNumber);
	}
	cout << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    flatINumber =
		flatDistribution.shootInt(width);
	    PR(flatINumber);
	}
	cout << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    flatINumber  = (int) flatDistribution.shoot(width,width+5);
	    PR(flatINumber);
	}
	cout << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    flatINumber  = flatDistribution.shootBit();
	    PR(flatINumber);
	}
	cout << "Specify an Engine" << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    flatNumber   = flatDistribution.shoot(&engine2);
	    PR(flatNumber);
	}
	cout << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    flatNumber   = flatDistribution.shoot(&engine2, width);
	    PR(flatNumber);
	}
	cout << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    flatNumber   = flatDistribution.shoot(&engine2, width,width+5);
	    PR(flatNumber);
	}
	cout << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    flatINumber  = flatDistribution.shootInt(&engine2, width);
	    PR(flatINumber);
	}
	cout << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    flatINumber  = (int) flatDistribution.shoot(&engine2, width,width+5);
	    PR(flatINumber);
	}
	cout << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    flatINumber  = flatDistribution.shootBit(&engine2);
	    PR(flatINumber);
	}
	cout << "ARRAYs of numbers" << endl;
	flatDistribution.shootArray(size,vec);
	for(i=0; i<size; i++)
	    cout << "i " << *(vec+i) << endl;

	cout << endl;
	flatDistribution.shootArray(size,vec, width, width+10);
	for(i=0; i<size; i++)
	    cout << "i " << *(vec+i) << endl;

	cout << endl;
	flatDistribution.shootArray(&engine2, size,vec);
	for(i=0; i<size; i++)
	    cout << "i " << *(vec+i) << endl;
    
	cout << endl;
	flatDistribution.shootArray(&engine2, size,vec, width, width+10);
	for(i=0; i<size; i++)
	    cout << "i " << *(vec+i) << endl;

	cout << "operator()" << endl;
	for(i=0; i<size; i++) {
	    cout << "flatDistribution() " << flatDistribution() << endl;
	}
	break;
	
    case 2:
	cout << "Gaussian Distribution:" << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    gaussNumber =    gaussDistribution.shoot();
	    PR(gaussNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    gaussNumber =    gaussDistribution.shoot(mean,width);
	    PR(gaussNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    gaussNumber =    gaussDistribution.shoot(&engine2);
	    PR(gaussNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    gaussNumber =    gaussDistribution.shoot(&engine2,mean,width);
	    PR(gaussNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    gaussNumber =    gaussDistribution.fire();
	    PR(gaussNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    gaussNumber =    gaussDistribution.fire(mean,width);
	    PR(gaussNumber);
	}
	cout << "Arrays" << endl;
	gaussDistribution.shootArray(size,vec,mean,width);
	for(i=0; i<size; i++) 
	    cout << "i " << *(vec+i) << endl;
	cout << endl;
	gaussDistribution.shootArray(&engine2,size,vec,mean,width);
	for(i=0; i<size; i++)
	    cout << "i " << *(vec+i) << endl;
	cout << endl;
	gaussDistribution.fireArray(size,vec,mean,width);
	for(i=0; i<size; i++)
	    cout << "i " << *(vec+i) << endl;
	
	break;
    case 3:
	cout << "Exponential Distribution:" << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    exponentialNumber = exponentialDistribution.shoot();
	    PR(exponentialNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    exponentialNumber = exponentialDistribution.shoot(mean);
	    PR(exponentialNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    exponentialNumber = exponentialDistribution.fire();
	    PR(exponentialNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    exponentialNumber = exponentialDistribution.fire(mean);
	    PR(exponentialNumber);
	}
	cout << "Specify Engine" << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    exponentialNumber = exponentialDistribution.shoot(&engine2);
	    PR(exponentialNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    exponentialNumber = exponentialDistribution.shoot(&engine2,mean);
	    PR(exponentialNumber);
	}
	cout << "Arrays" << endl;
	exponentialDistribution.shootArray(size,vec,mean);
	for(i=0; i<size; i++) 
	    cout << "i " << *(vec+i) << endl;
	cout << endl;
	exponentialDistribution.shootArray(&engine2,size,vec,mean);
	for(i=0; i<size; i++) 
	    cout << "i " << *(vec+i) << endl;
	cout << endl;
	exponentialDistribution.fireArray(size,vec,mean);
	for(i=0; i<size; i++) 
	    cout << "i " << *(vec+i) << endl;

	break;
    case 4:
	for(jj=0; jj<numberOfNumbers; jj++) {
	    poissonNumber = poissonDistribution.shoot();
	    PR(poissonNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    poissonNumber = poissonDistribution.shoot(mean);
	    PR(poissonNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    poissonNumber = poissonDistribution.fire();
	    PR(poissonNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    poissonNumber = poissonDistribution.fire(mean);
	    PR(poissonNumber);
	}
	cout << "Specify Engine" << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    poissonNumber = poissonDistribution.shoot(&engine2);
	    PR(poissonNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    poissonNumber = poissonDistribution.shoot(&engine2,mean);
	    PR(poissonNumber);
	}
	cout << "Arrays" << endl;
	poissonDistribution.shootArray(size,vecL,mean);
	for(i=0; i<size; i++) 
	    cout << "i " << *(vecL+i) << endl;
	cout << endl;
	poissonDistribution.shootArray(&engine2,size,vecL,mean);
	for(i=0; i<size; i++) 
	    cout << "i " << *(vecL+i) << endl;
	cout << endl;
	poissonDistribution.fireArray(size,vecL,mean);
	for(i=0; i<size; i++) 
	    cout << "i " << *(vecL+i) << endl;

	break;
    case 5:
	cout << "Breit-Wigner Distribution:" << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    bwNumber = breitWignerDistribution.shoot(mean, width);
	    PR(bwNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    bwNumber = breitWignerDistribution.shoot(mean, width, cut);
	    PR(bwNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    bwNumber = breitWignerDistribution.shootM2(mean, width);
	    PR(bwNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    bwNumber = breitWignerDistribution.shootM2(mean, width, cut);
	    PR(bwNumber);
	}
	cout << "Specify an engine" << endl;
	for(jj=0; jj<numberOfNumbers; jj++) {
	    bwNumber = breitWignerDistribution.shoot(&engine2, mean, width);
	    PR(bwNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    bwNumber = breitWignerDistribution.shoot(&engine2, mean, width, cut);
	    PR(bwNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    bwNumber = breitWignerDistribution.shootM2(&engine2, mean, width);
	    PR(bwNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    bwNumber = breitWignerDistribution.shootM2(&engine2, mean, width, cut);
	    PR(bwNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    bwNumber = breitWignerDistribution.fire(mean, width, cut);
	    PR(bwNumber);
	}
	for(jj=0; jj<numberOfNumbers; jj++) {
	    bwNumber = breitWignerDistribution.fireM2(mean, width, cut);
	    PR(bwNumber);
	}
	cout << endl;
	cout << "ARRAYS:" << endl;
	breitWignerDistribution.shootArray(size, vec,mean, width, cut);
	for(i=0; i<size; i++)
	    cout << "i " << *(vec+i) << endl;
		cout << endl;
	breitWignerDistribution.shootArray(&engine2, size, vec,mean, width, cut);
	for(i=0; i<size; i++)
	    cout << "i " << *(vec+i) << endl;
		cout << endl;
	breitWignerDistribution.fireArray(size, vec,mean, width, cut);
	for(i=0; i<size; i++)
	    cout << "i " << *(vec+i) << endl;
	
	break;
    default:
	break;
    }

    delete [] vecI;
    delete [] vecL;
    delete [] vec;
    
    return 0;
}
