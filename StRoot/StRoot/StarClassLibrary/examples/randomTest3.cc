/***************************************************************************
 *
 * $Id: randomTest3.cc,v 1.2 2003/09/02 17:59:38 perev Exp $
 *
 * Author: Brian Lasiuk, July 1998 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: randomTest3.cc,v $
 * Revision 1.2  2003/09/02 17:59:38  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  1999/02/17 12:44:02  ullrich
 * New Revision
 *
 * Revision 1.1  1999/01/23 00:26:52  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <Stiostream.h>
#include <vector>

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

#ifndef ST_NO_TEMPLATE_DEF_ARGS
void printArray(vector<double>& vec)
#else
void printArray(vector<double, allocator<double> >& vec)
#endif
{
    for(int ii=0; ii<vec.size(); ii++)
	cout << "(" << ii << ") " << vec[ii] << endl;
    cout << endl;
}
//         --------------------- MAIN --------------------------       //
int main()
{
    const int size = 5;
    
    // Generator must be given an engine:
    //   - HepJamesRandom used by default
    HepJamesRandom  engine1;
    RanluxEngine    engine2;

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<double> listOfRandoms(size);
#else
    vector<double, allocator<double> > listOfRandoms(size);
#endif
    RandFlat        flatDistribution(engine1);
    RandGauss       gaussDistribution(engine2);
    RandExponential exponentialDistribution(engine1);
    RandPoisson     poissonDistribution(engine2);
    RandBreitWigner breitWignerDistribution(engine2);


    cout << "\nAn Array of Numbers from a Flat Distribution:" << endl;
    flatDistribution.shootArray(listOfRandoms);
    printArray(listOfRandoms);
    
    cout << "\nAn Array of Numbers from a Gauss Distribution:" << endl;
    gaussDistribution.fireArray(listOfRandoms);
    printArray(listOfRandoms);
    
    cout << "\nAn Array of Numbers from an Exponential Distribution:" << endl;
    exponentialDistribution.fireArray(listOfRandoms);
    printArray(listOfRandoms);
    
    cout << "\nAn Array of Numbers from an BW Distribution:" << endl;
    breitWignerDistribution.shootArray(listOfRandoms);
    printArray(listOfRandoms);

    return 0;
}
