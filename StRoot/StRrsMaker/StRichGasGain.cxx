/****************************************************************
 * $Id: StRichGasGain.cxx,v 1.7 2000/03/17 14:54:34 lasiuk Exp $
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
 ****************************************************************
 * $Log: StRichGasGain.cxx,v $
 * Revision 1.7  2000/03/17 14:54:34  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.7  2000/03/17 14:54:34  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.6  2000/03/12 23:56:33  lasiuk
 * new coordinate system
 * exchange MyRound with inline templated funtion
 *
 * Revision 1.5  2000/02/14 01:13:25  lasiuk
 * add track_p to the GHit c'tor
 *
 * Revision 1.4  2000/02/08 23:51:13  lasiuk
 * removal of rrs macro---CC4.2 cannot handle it!
 *
 * Revision 1.3  2000/02/08 16:24:08  lasiuk
 * use of dbs
 *
 * Revision 1.2  2000/01/25 22:02:20  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:01  lasiuk
 * Initial Revision
 *
 ****************************************************************/

#include <math.h>

#include "StRichGasGain.h"
#include "StRichGeometryDb.h"
#include "StRichPhysicsDb.h"
#include "StRichOtherAlgorithms.h"
#ifdef RICH_WITH_VIEWER
#include "StRichViewer.h" 
#endif

StRichGasGain::StRichGasGain()
{
    StRichPhysicsDb* tmpPhysicsDb   = StRichPhysicsDb::getDb();
    StRichGeometryDb* tmpGeometryDb = StRichGeometryDb::getDb();

    mAnodePadPlaneSeparation = tmpGeometryDb->anodeToPadSpacing();
    mPhotonFeedback          = tmpPhysicsDb->feedBackPhotonProbability();
    mPhotoConversion         = tmpPhysicsDb->photoConversionEfficiency();
    mGasGainAmplification    = tmpPhysicsDb->gasGainAmplification();
    mPolia                   = tmpPhysicsDb->polia();
}

StRichGasGain::~StRichGasGain()
{/* nopt */}

double StRichGasGain::avalanche(StRichMiniHit* hit, double wirePos, list<StRichMiniHit*>& aList)
{
    double q;

    // X better stay where it is
    hit->position().setY(wirePos);
    hit->position().setZ(mAnodePadPlaneSeparation);

    double p = mRandom.Polia(mPolia);

#ifdef RICH_WITH_VIEWER
    if ( StRichViewer::histograms )
	StRichViewer::getView()->mPolia->Fill(p);
#endif
    //q = mPhysDB->ampl_factor * p;
    q = mGasGainAmplification * p;    

    // produce feedback photon?
    feedbackPhoton(hit, q, aList);
    if(RRS_DEBUG)
	cout << "StRichGasGain::operator() q = " << q << endl;
    return q ; 
}

void StRichGasGain::feedbackPhoton(StRichMiniHit* hit, double q, list<StRichMiniHit*>& theList)
{
    //StRichInduceSignal induceSignal;

    double elec2feed = mPhotonFeedback*q;                
    double phot2elec = elec2feed*(mPhotoConversion)/2;
    int P = mRandom.Poisson(phot2elec);
    if(RRS_DEBUG)
	cout << "StRichGasGain::feedbackPhoton() P = " << P << endl;
#ifdef RICH_WITH_VIEWER
    if ( StRichViewer::histograms )
	StRichViewer::getView()->mFeedback->Fill(P);
#endif
    double dist, x, y, z, cost, phi;
	
    for (int i=1; i<=P; i++) {
	cost = mRandom.Flat();
	phi  = 2*M_PI *  mRandom.Flat();
	
	dist = mAnodePadPlaneSeparation * sqrt( 1 - cost*cost ) / cost;
	x    = hit->position().x() + dist * cos(phi);
 	y    = hit->position().y() + dist * sin(phi);
 	z    = mAnodePadPlaneSeparation;
// 	x    = hit.position().x() + dist * sin(phi);
// 	z    = hit.position().z() + dist * cos(phi);
// 	y    = mAnodePadPlaneSeparation;
	
	theList.push_back(new StRichMiniHit(StThreeVector<double>(x,y,z),
					    hit->momentum(),
					    hit->trackp(),
					    hit->id(),
					    hit->mass(),
					    eFeedback));
    if(RRS_DEBUG)
	cout << "StRichGasGain::feedbackPhoton()-->induceSignal! " << endl; 

    //induceSignal(aGHit);
    }
}
