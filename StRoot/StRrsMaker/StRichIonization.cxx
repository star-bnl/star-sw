/**********************************************************
 * $Id: StRichIonization.cxx,v 1.2 2000/01/25 22:02:21 lasiuk Exp $
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
 ************************************************************
 * $Log: StRichIonization.cxx,v $
 * Revision 1.2  2000/01/25 22:02:21  lasiuk
 * Second Revision
 *
 * Revision 1.4  2000/02/08 23:51:13  lasiuk
 * removal of rrs macro---CC4.2 cannot handle it!
 *
 * Revision 1.3  2000/02/08 16:27:34  lasiuk
 * change to class.  Put dbs and random generators into
 *
 ************************************************************/

 *
 * Revision 1.2  2000/01/25 22:02:21  lasiuk
 * Second Revision
 *
#include "StRichOtherAlgorithms.h"
#include "PhysicalConstants.h"

#include "StRichIonization.h"
#ifndef ST_NO_NAMESPACES
#endif
#include "StRichInduceSignal.h"
#include "StRichGHit.h"
    void StRichIonization::operator()(const StRichGHit& hit ) 
    {
	static StRichPhysicsDb* mPhysicsDb = StRichPhysicsDb::getDb();
	static Randoms random;
	static MyRound round;
	static double avg_n_inter = mPhysicsDb->avg_n_inter;
	StRichInduceSignal induceSignal;

	double t, x, y, z;                                  // locals
	double p;
	unsigned int n;
	// t between -.5 and .5
	int n_interactions = random.Poisson( round(hit.step * avg_n_inter) );
	p = mRandom.Flat(1.0) * mMaximumElectronEnergyProbability;
	for ( int i = 0; i < n_interactions; i++ )
	    {
		t = hit.step * ( random.Flat(1.0) - 0.5 );         // t between -.5 and .5
		x = hit.x  +  t * hit.cosX;                        // x,y and z of interaction
		y = hit.y  +  t * hit.cosY;
		z = hit.z  +  t * hit.cosZ;
		
		//
		// random selector from 0 to e_max
		//
		p = random.Flat(1.0) * mPhysicsDb->e_max;
		
		// number of electrons in this cluster
		//
		n = 1;
		while ( n <= mPhysicsDb->e_distribut.size() && p >= mPhysicsDb->e_distribut[n-1] )
		    n++;
		
	
		if (StRichViewer::histograms )
		    StRichViewer::getView()->mClusterElectrons->Fill(n);
	n = 1;
		
		for (unsigned int j=0; j<n; j++ ) {
		    StRichGHit aGHit(x,y,z,hit.quad,hit.id);
		    induceSignal(aGHit);
		}
	    }
	
#endif
					      hit->id(),
					      hit->mass(),
					      eCharged));
	}
    }
#ifndef ST_NO_NAMESPACES
//}
#endif

    return (I/Io);
}
