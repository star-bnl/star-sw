/**********************************NoiseSimulator.cxx**\
 * $Id: StRichNoiseSimulator.cxx,v 1.2 2000/01/25 22:02:21 lasiuk Exp $
 *
 * Description:
 *
 *  StRichNoiseSimulator generates an electric noise
 *  depending on an experimental factor from a 
 *  database.
 *  In this implementation, a Gaussian distribution
 *  is applied to the factor. 
 *
 *
 ******************************************************
 * $Log: StRichNoiseSimulator.cxx,v $
 * Revision 1.2  2000/01/25 22:02:21  lasiuk
 * Second Revision
 *
 * Revision 1.2  2000/01/25 22:02:21  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:03  lasiuk
 * Initial Revision
 *
 ******************************************************/

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif

#include "StRichOtherAlgorithms.h"
#include "StRichPhysicsDb.h"
#include "StRichNoiseSimulator.h"
#ifdef RICH_WITH_VIEWER
#include "StRichViewer.h"
#endif

  
    double StRichNoiseSimulator::operator()(void) const
    {
	static StRichPhysicsDb* physDB     = StRichPhysicsDb::getDb();
	static double electric_noise = physDB->electric_noise;
	static Randoms random;                 // declarations
 
	double noise = electric_noise * random.Gauss();
#ifdef RICH_WITH_VIEWER
	if (StRichViewer::histograms )
	    StRichViewer::getView()->mNoise->Fill(noise);
#endif
	return noise;
    }
    
#ifndef ST_NO_NAMESPACES
//}
#endif
