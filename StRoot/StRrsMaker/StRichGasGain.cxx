/******************************************************
 * $Id: StRichGasGain.cxx,v 1.2 2000/01/25 22:02:20 lasiuk Exp $
 *
 * Description:
 *  StRichGasGain computes an amplification factor of an
 *  avalanche of electrons on a wire under the effect
 *  of an electrostatic field. This factor depends
 *  on a parameter, which describes the number
 *  of generations in an avalanche (cf. "Particle
 *  Detection with Drift Chambers" by W.Blum, L.Rolandi),
 *  as well as on the characteristics of the gas in the 
 *  chamber.
 *  Each avalanche may also produce feedback photons.
 *
 *  In this implementation:
 *    - Polia is generated using RnGamma from CERNLIB
 *    - the hit x coordinate is changed so that it 
 *      reflects its position on the wire
 *
 *
 *  
 *  Each avalanche may also produce feedback photons,
 *  which frequency depends on the gas' characteristics. 
 *  The three other parameters are the efficiency with
 *  which feedback photons are created in an avalanche
 *  and the efficiency with which they kick out an electron
 *  from CsI. Finally, geomtrical considerations have to be
 *  taken in account. 
 *
 *  In this implementation:
 *    - Distribution is Poisson, based on avalanche, feedback 
 *      photon probability, photosensitive efficiency 
 *      and geometrical considerations
 *    - For each feedback photon, randomly compute a position
 *      on CsI layer, and induce signal. 
 *      
 *
 **********************************************************
 * $Log: StRichGasGain.cxx,v $
 * Revision 1.2  2000/01/25 22:02:20  lasiuk
 * Second Revision
 *
 *
 * Revision 1.4  2000/02/08 23:51:13  lasiuk
 * removal of rrs macro---CC4.2 cannot handle it!
 *
 * Revision 1.3  2000/02/08 16:24:08  lasiuk
 * use of dbs
 **********************************************************/
 * Revision 1.2  2000/01/25 22:02:20  lasiuk
 * Second Revision
 *
#include "StRichGeometryDb.h"
#include "StRichPhysicsDb.h"


#include "StRichInduceSignal.h"
#include "StRichOtherAlgorithms.h"
#include "StRichInduceSignal.h"
#include "StRichGHit.h"
#include "StRichGasGain.h"
#include "StRichGHit.h"
#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif

    double StRichGasGain::operator()(StRichGHit& hit, double wirePos )
    {
	static StRichGeometryDb* geoDB = StRichGeometryDb::getDb();         // DBs
	static StRichPhysicsDb* physDB = StRichPhysicsDb::getDb();          // idem
	static Randoms random;

	double q;
	hit.x = wirePos;
	hit.y = 0;
	double p = random.Polia(physDB->polia());
    hit.position().setZ(mAnodePadPlaneSeparation);
	if ( StRichViewer::histograms )
	    StRichViewer::getView()->mPolia->Fill(p);
    hit->position().setZ(mAnodePadPlaneSeparation);
	q = geoDB->ampl_factor * p;    
                                                          
	feedbackPhoton( hit, q );                               // produce feedback photon

	return q ; 
    }

  
    void StRichGasGain::feedbackPhoton( const StRichGHit& hit, double q ) const
    {
	StRichInduceSignal induceSignal;
	static StRichPhysicsDb* physDB = StRichPhysicsDb::getDb();          // declarations
	static StRichGeometryDb* geoDB = StRichGeometryDb::getDb();         // of temporaries
	static Randoms random;
	static const double pi = 3.1415926535897928;
	static double height    = geoDB->height;

	double elec2feed = physDB->avl2phot*q;                
	double phot2elec = elec2feed*physDB->phot2elec/2;
	int P = random.Poisson(phot2elec);
	
    //StRichInduceSignal induceSignal;
	if ( StRichViewer::histograms )
	    StRichViewer::getView()->mFeedback->Fill(P);
    double phot2elec = elec2feed*(mPhotoConversion)/2;
	double dist, x, y, z, cost, phi;
    if(RRS_DEBUG)
	for (int i=1; i<=P; i++) {
	    cost = random.Flat();
	    phi  = 2*pi *  random.Flat();
	    
	    dist = height * sqrt( 1 - cost*cost ) / cost;
	    x    = hit.x + dist * sin(phi);
	    z    = hit.z + dist * cos(phi);
	    y    = height;

	    StRichGHit aGHit(x,y,z, hit.quad, hit.id);
	    induceSignal(aGHit);
	}
					    hit->id(),
					    eFeedback));
	induceSignal(aGHit);
	cout << "StRichGasGain::feedbackPhoton()-->induceSignal! " << endl; 


#ifndef ST_NO_NAMESPACES
//}
#endif
    //induceSignal(aGHit);
    }
}
