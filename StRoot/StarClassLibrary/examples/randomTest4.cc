/***************************************************************************
 *
 * $Id: randomTest4.cc,v 1.2 2003/09/02 17:59:38 perev Exp $
 *
 * Author: Thomas Ullrich, Mar 2000
 ***************************************************************************
 *
 * Description: Test program for StRandom
 *
 ***************************************************************************
 *
 * $Log: randomTest4.cc,v $
 * Revision 1.2  2003/09/02 17:59:38  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2000/03/16 16:30:27  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <Stiostream.h>
#include "StRandom.hh"
#include "StGlobals.hh"

int main()
{
    StRandom  rndm;
    int       i;
    const int n = 10;

    rndm.setSeed(101);
    for (i=0; i<n; i++) 	PR(rndm.flat());
    for (i=0; i<n; i++) 	PR(rndm.flat(2));
    for (i=0; i<n; i++) 	PR(rndm.flatInt(10));
    for (i=0; i<n; i++) 	PR(rndm.flatInt(20,30));
    for (i=0; i<n; i++) 	PR(rndm.flat(1, 2));
    for (i=0; i<n; i++) 	PR(rndm.exponential());
    for (i=0; i<n; i++) 	PR(rndm.exponential(123.));
    for (i=0; i<n; i++) 	PR(rndm.gauss());
    for (i=0; i<n; i++) 	PR(rndm.gauss(10, 1.67));
    for (i=0; i<n; i++) 	PR(rndm.poisson(6.4));
    
    StRandom::setSeed(1001);
    for (i=0; i<n; i++) 	PR(StRandom::flat());
    for (i=0; i<n; i++) 	PR(StRandom::flat(2));
    for (i=0; i<n; i++) 	PR(StRandom::flat(1, 2));
    for (i=0; i<n; i++) 	PR(StRandom::flatInt(10));
    for (i=0; i<n; i++) 	PR(StRandom::flatInt(20,30));
    for (i=0; i<n; i++) 	PR(StRandom::exponential());
    for (i=0; i<n; i++) 	PR(StRandom::exponential(123.));
    for (i=0; i<n; i++) 	PR(StRandom::gauss());
    for (i=0; i<n; i++) 	PR(StRandom::gauss(10, 1.67));
    for (i=0; i<n; i++) 	PR(StRandom::poisson(6.4));
    
    return 0;
}
