/**********************************************************************
 * $Id: StRichNoiseSimulator.cxx,v 1.3 2000/02/08 16:28:30 lasiuk Exp $
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
 **********************************************************************
 * $Log: StRichNoiseSimulator.cxx,v $
 * Revision 1.3  2000/02/08 16:28:30  lasiuk
 * change to class.  Use dbs and random number generators
 * from data members
 *
 * Revision 1.2  2000/01/25 22:02:21  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:03  lasiuk
 * Initial Revision
 *
 **********************************************************************/
#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif

#include "StRichNoiseSimulator.h"

#ifdef RICH_WITH_VIEWER
#include "StRichViewer.h"
#endif

StRichNoiseSimulator::StRichNoiseSimulator()
{
    mPhysicsDb = StRichPhysicsDb::getDb();
    mElectricNoise = mPhysicsDb->electronicNoiseLevel();
}

StRichNoiseSimulator::~StRichNoiseSimulator()
{ /* nopt */ }

double StRichNoiseSimulator::operator()(void) const
{
    double noise = mElectricNoise * mRandom.Gauss();

#ifdef RICH_WITH_VIEWER
    if (StRichViewer::histograms )
	StRichViewer::getView()->mNoise->Fill(noise);
#endif
	return noise;
}
    
#ifndef ST_NO_NAMESPACES
//}
#endif
