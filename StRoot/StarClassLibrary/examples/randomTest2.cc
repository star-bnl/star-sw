/***************************************************************************
 *
 * $Id: randomTest2.cc,v 1.3 2003/09/02 17:59:38 perev Exp $
 *
 * Author: Brian Lasiuk, May 1998 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: randomTest2.cc,v $
 * Revision 1.3  2003/09/02 17:59:38  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  1999/12/21 15:14:58  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.1  1999/02/17 12:44:02  ullrich
 * New Revision
 *
 * Revision 1.1  1999/01/23 00:26:51  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <Stiostream.h>
#include "StGlobals.hh"
#include "Random.h"

// the random engines
#include "JamesRandom.h"
#include "RanluxEngine.h"

// the different distributions
#include "RandFlat.h"
#include "RandPoisson.h"
#include "RandExponential.h"
#include "RandGauss.h"
#include "RandBreitWigner.h"

//         --------------------- MAIN --------------------------       //
int main()
{
    int i, jj;
    
    const StInt size = 5;
    const StInt numberOfNumbers = 5;
    
    // Generator must be given an engine:
    //   - HepJamesRandom used by default

    HepJamesRandom  engine1;
    RanluxEngine    engine2;
    
    //HepRandom quasiRandom(engine);  // pass engine by reference
    //HepRandom quasiRandom(&engine); // pass engine by pointer

    //engine.showStatus();              // show status of engine
    
    long seed = 7;
    HepRandom quasiRandom;          // or quasiRandom(seed);
    
    StDouble quasiRandomNumber =
	quasiRandom.flat();
    PR(quasiRandomNumber);

    int    *vecI = new int[size];     // StInt    vecI[size]
    double *vec  = new double[size];  // StDouble vec[size];
    
    quasiRandom.flatArray(size,vec);

    cout << "\nPseudo-Random numbers from a flat distribution." << endl;
    for(int ii=0; ii<size; ii++)
	cout << "i " << *(vec+ii) << endl;
        
    PR(quasiRandom.getTheSeed());
    HepJamesRandom jr;

    cout << "All these numbers are the same:" << endl;
    for(int iii=0; iii<size; iii++) {
     	jr.saveStatus();
     	PR(jr.flat());
    	jr.restoreStatus();        // restoring status keeps engine same
    }

    cout << "\nDifferent distributions:" << endl;

    RandFlat        flatDistribution(engine1);
    RandGauss       gaussDistribution(engine2);
    RandExponential exponentialDistribution(engine1);
    RandPoisson     poissonDistribution(engine2);
    RandBreitWigner breitWignerDistribution(engine2);

    double mean  = 2;
    double width = 10;

    cout << "Numbers from a Flat Distribution:" << endl;
    for(jj=0; jj<numberOfNumbers; jj++) {
  	StDouble flatNumber  =
	    flatDistribution.fire(width,width+10);
  	PR(flatNumber);
    }

    cout << "\nNumbers from a Gaussian Distribution:" << endl;
    for(jj=0; jj<numberOfNumbers; jj++) {
 	StDouble gaussNumber =
 	    gaussDistribution.shoot();
 	PR(gaussNumber);
    }
	
    cout << "\nNumbers from an Exponential Distribution:" << endl;
    for(jj=0; jj<numberOfNumbers; jj++) {
	StDouble exponentialNumber =
 	    exponentialDistribution.shoot(&engine2, mean);
 	PR(exponentialNumber);
    }

    cout << "\nNumbers from a Poissonian Distribution:" << endl;
    for(jj=0; jj<numberOfNumbers; jj++) {
	StDouble poissonNumber =
 	    poissonDistribution.shoot();
 	PR(poissonNumber);
    }
    
    cout << "\nAn Array of Numbers from a Breit-Wigner Distribution:" << endl;
    breitWignerDistribution.shootArray(size, vec);

    for(i=0; i<size; i++)
	cout << "(" << i << ") " << *(vec+i) << endl;	
    
    return 0;
}
