/****************************************************************
 * $Id: StRichGasGain.cxx,v 1.3 2000/02/08 16:24:08 lasiuk Exp $
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
 * Revision 1.3  2000/02/08 16:24:08  lasiuk
 * use of dbs
 *
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
#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif


#include "StRichInduceSignal.h"
#include "StRichGHit.h"
#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif

#include "StRichGeometryDb.h"
#include "StRichPhysicsDb.h"
#include "StRichOtherAlgorithms.h"
#ifdef RICH_WITH_VIEWER
#include "StRichViewer.h" 
#endif
    mPhysicsDb  = StRichPhysicsDb::getDb();
    mGeometryDb = StRichGeometryDb::getDb();
{
    mAnodePadPlaneSeparation = mGeometryDb->anodeToPadSpacing();
    mPhotonFeedback          = mPhysicsDb->feedBackPhotonProbability();
    mPhotoConversion         = mPhysicsDb->photoConversionEfficiency();
    mPhotoConversion         = tmpPhysicsDb->photoConversionEfficiency();
    mGasGainAmplification    = tmpPhysicsDb->gasGainAmplification();
    mPolia                   = tmpPhysicsDb->polia();
}
    hit.position().setX(wirePos);
    hit.position().setY(0);
{
    hit.position().setY(wirePos);
    hit.position().setZ(mAnodePadPlaneSeparation);
    // X better stay where it is
    double p = mRandom.Polia(mPhysicsDb->polia());
    hit->position().setZ(mAnodePadPlaneSeparation);

    double p = mRandom.Polia(mPolia);

#ifdef RICH_WITH_VIEWER
    if ( StRichViewer::histograms )
    rrs << "StRichGasGain::operator() q = " << q << endl;
    //q = mPhysDB->ampl_factor * p;
    feedbackPhoton( hit, q );

    // produce feedback photon?
    feedbackPhoton(hit, q, aList);
    if(RRS_DEBUG)
	cout << "StRichGasGain::operator() q = " << q << endl;
void StRichGasGain::feedbackPhoton( const StRichGHit& hit, double q ) const
}
    StRichInduceSignal induceSignal;
    rrs << "StRichGasGain::feedbackPhoton() P = " << P << endl;
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
	x    = hit.position().x() + dist * sin(phi);
	z    = hit.position().z() + dist * cos(phi);
	y    = mAnodePadPlaneSeparation;
	dist = mAnodePadPlaneSeparation * sqrt( 1 - cost*cost ) / cost;
	StRichGHit aGHit(x,y,z, hit.id());
	rrs << "StRichGasGain::feedbackPhoton()-->induceSignal! " << endl; 
// 	x    = hit.position().x() + dist * sin(phi);
	StRichGHit aGHit(x,y,z, hit.trackp(), hit.id());
					    hit->id(),
					    hit->mass(),
					    eFeedback));
	induceSignal(aGHit);
	cout << "StRichGasGain::feedbackPhoton()-->induceSignal! " << endl; 


#ifndef ST_NO_NAMESPACES
//}
#endif
    //induceSignal(aGHit);
    }
}
