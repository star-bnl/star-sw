/*****************************************************************
 *
 * $Id: StTpcROOTGeometry.cc,v 1.4 2000/01/10 23:14:29 lasiuk Exp $
 *
 * Author: brian May March 22, 1998
 *
 *****************************************************************
 * Description: Database for Geometrical parameters for
 *              the STAR Main TPC              
 *
 *****************************************************************
 *
 * $Log: StTpcROOTGeometry.cc,v $
 * Revision 1.4  2000/01/10 23:14:29  lasiuk
 * Include MACROS for compatiblity with SUN CC5
 *
 * Revision 1.3  1999/12/08 02:10:42  calderon
 * Modified to eliminate warnings on Linux.
 *
 * Revision 1.2  1999/04/07 00:48:00  lasiuk
 * add z offset for driftLength
 *
 * Revision 1.1  1999/03/23 03:39:22  lasiuk
 * Initial Revsion
 *
 ******************************************************************/
#ifdef __ROOT__
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif
#ifndef ST_NO_EXCEPTIONS
#   include <stdexcept>
#   if !defined(ST_NO_NAMESPACES)
        using std::invalid_argument;
        using std::range_error;
#   endif
#endif
#include "StTpcROOTGeometry.hh"

StTpcGeometry* StTpcROOTGeometry::mInstance = 0; // static data member

StTpcROOTGeometry::StTpcROOTGeometry() { /* nopt */ }

StTpcROOTGeometry::StTpcROOTGeometry(geometryDataSet* dS)
{
    mPadRows        = dS->padRows;
    mInnerPadRows   = dS->innerPadRows;
    mInnerPadRows48 = dS->innerPadRows48;
    mInnerPadRows52 = dS->innerPadRows52;
    mOuterPadRows   = dS->outerPadRows;
    mTimeBuckets    = dS->timeBuckets;
    mSectors        = dS->sectors;
    mIfcRadius      = dS->ifcRadius;
    mOfcRadius      = dS->ofcRadius;
    mEndCapZ        = dS->endCapZ;
    mInnerSectorPadWidth    = dS->innerSectorPadWidth;
    mInnerSectorPadLength   = dS->innerSectorPadLength;
    mInnerSectorPadPitch    = dS->innerSectorPadPitch;
    mInnerSectorRowPitch1   = dS->innerSectorRowPitch1;
    mInnerSectorRowPitch2   = dS->innerSectorRowPitch2;
    mFirstPadRow            = dS->firstPadRow;
    mFirstOuterSectorPadRow = dS->firstOuterSectorPadRow;
    mLastOuterSectorPadRow  = dS->lastOuterSectorPadRow;
    mFirstRowWidth          = dS->firstRowWidth;
    mLastRowWidth           = dS->lastRowWidth;
    mOuterSectorPadWidth    = dS->outerSectorPadWidth;
    mOuterSectorPadLength   = dS->outerSectorPadLength;
    mOuterSectorPadPitch    = dS->outerSectorPadPitch;
    mOuterSectorRowPitch    = dS->outerSectorRowPitch;
    mOuterSectorLength      = dS->outerSectorLength;
    mIoSectorSeparation     = dS->ioSectorSeparation;
    
    mFrischGrid           = dS->frischGrid;
    mDriftDistance        = dS->maximumDriftDistance;
    mInnerSectorzOffSet   = dS->innerSectorzOffSet;
    mOuterSectorzOffSet   = dS->outerSectorzOffSet;
    
    mAnodeWirePitch       = dS->anodeWirePitch;
    mFrischGridWirePitch  = dS->frischGridWirePitch;
    mGateWirePitch        = dS->gateWirePitch;
    mAnodeWireRadius      = dS->anodeWireRadius;
    mFrischGridWireRadius = dS->frischGridWireRadius;
    mGateWireRadius       = dS->gateWireRadius;
	
    mInnerSectorAnodeWirePadPlaneSeparation  = dS->iSAnodeWirePadPlaneSeparation;
    mInnerSectorFrischGridPadPlaneSeparation = dS->iSFrischGridPadPlaneSeparation;
    mInnerSectorGatingGridPadPlaneSeparation = dS->iSGatingGridPadPlaneSeparation;
    mOuterSectorAnodeWirePadPlaneSeparation  = dS->oSAnodeWirePadPlaneSeparation;
    mOuterSectorFrischGridPadPlaneSeparation = dS->oSFrischGridPadPlaneSeparation;
    mOuterSectorGatingGridPadPlaneSeparation = dS->oSGatingGridPadPlaneSeparation;
	
    mFirstInnerSectorAnodeWire     = dS->firstInnerSectorAnodeWire;
    mLastInnerSectorAnodeWire      = dS->lastInnerSectorAnodeWire;
    mNumberOfInnerSectorAnodeWires = static_cast<int>(dS->numberOfInnerSectorAnodeWires);

    mFirstOuterSectorAnodeWire     = dS->firstOuterSectorAnodeWire;
    mLastOuterSectorAnodeWire      = dS->lastOuterSectorAnodeWire;
    mNumberOfOuterSectorAnodeWires = static_cast<int>(dS->numberOfOuterSectorAnodeWires);
    mInnerSectorEdge               = dS->innerSectorEdge;
    mOuterSectorEdge               = dS->outerSectorEdge;

    // should be assign --- not supported by egcs 1.0.2
    //mPadsInRow.assign(mPadRows);
    mPadsInRow.resize(mPadRows);
    for(unsigned int ii=0; ii<mPadsInRow.size(); ii++)
	mPadsInRow[ii]= dS->padsInRow[ii];
    
    mRadialDistanceAtRow.resize(mPadRows);
								
     // Initialize pad map

     // !!!! In principle use data-base values for the loop variables!!!!!
     // first 8 pad rows
    for(unsigned int i=1; i<9; i++)
	mRadialDistanceAtRow[i-1] =
	    mFirstPadRow+(i-1)*mInnerSectorRowPitch1;
	
    // padrows 9-13
    double base =
	mFirstPadRow + 7*mInnerSectorRowPitch1;
    for(unsigned int jj=9; jj<14; jj++)
	mRadialDistanceAtRow[jj-1] =
	    base+(jj-8)*mInnerSectorRowPitch2;

    // pad row 14
    mRadialDistanceAtRow[13] =
	mFirstPadRow + 7*mInnerSectorRowPitch1 + 5*mInnerSectorRowPitch2 + mIoSectorSeparation;

    // pad row 15-45
    for(unsigned int j=15; j<46; j++)
	mRadialDistanceAtRow[j-1] =
	    mFirstOuterSectorPadRow + (j-14)*mOuterSectorRowPitch;

    // Make sure units are as expected:
    
    mIfcRadius           *= millimeter;
    mOfcRadius           *= millimeter;
    mEndCapZ             *= millimeter;
    mInnerSectorPadWidth *= millimeter;
    mInnerSectorPadLength *= millimeter;
    mInnerSectorPadPitch  *= millimeter;
    mInnerSectorRowPitch1 *= millimeter;
    mInnerSectorRowPitch2 *= millimeter;
    mFirstPadRow          *= millimeter;
    mFirstOuterSectorPadRow *= millimeter;
    mLastOuterSectorPadRow  *= millimeter;
    mFirstRowWidth          *= millimeter;
    mLastRowWidth           *= millimeter;
    mInnerSectorEdge        *= millimeter;
    
    mOuterSectorPadWidth   *= millimeter;
    mOuterSectorPadLength  *= millimeter;
    mOuterSectorPadPitch   *= millimeter;
    mOuterSectorRowPitch   *= millimeter;
    mOuterSectorLength     *= millimeter;

    mIoSectorSeparation    *= millimeter;
    mOuterSectorEdge       *= millimeter;
    //mIoSectorSpacing       *= millimeter;
    
    mFrischGrid            *= millimeter;
    mDriftDistance         *= millimeter;
    mInnerSectorzOffSet    *= millimeter;
    mOuterSectorzOffSet    *= millimeter;

    
    for(unsigned int kk=0; kk<mRadialDistanceAtRow.size(); kk++)
	mRadialDistanceAtRow[kk] *= millimeter;

    // Wires
    mAnodeWirePitch       *= millimeter;
    mFrischGridWirePitch  *= millimeter;
    mGateWirePitch        *= millimeter;
    mAnodeWireRadius      *= micrometer;
    mFrischGridWireRadius *= micrometer;
    mGateWireRadius       *= micrometer;
    
    mInnerSectorAnodeWirePadPlaneSeparation  *= millimeter;
    mInnerSectorFrischGridPadPlaneSeparation *= millimeter;
    mInnerSectorGatingGridPadPlaneSeparation *= millimeter;

    mOuterSectorAnodeWirePadPlaneSeparation  *= millimeter;
    mOuterSectorFrischGridPadPlaneSeparation *= millimeter;
    mOuterSectorGatingGridPadPlaneSeparation *= millimeter;
    
    mFirstInnerSectorAnodeWire *= millimeter;
    mLastInnerSectorAnodeWire  *= millimeter;

    mFirstOuterSectorAnodeWire *= millimeter;
    mLastOuterSectorAnodeWire  *= millimeter;
}

StTpcROOTGeometry::~StTpcROOTGeometry()
{
    mPadsInRow.clear();
    mRadialDistanceAtRow.clear();
}

StTpcGeometry*
StTpcROOTGeometry::instance()
{
    if (!mInstance) {
#ifndef ST_NO_EXCEPTIONS
	throw invalid_argument("StTpcROOTGeometry::getInstance(): Argument Missing!");
#else
	cerr << "StTpcROOTGeometry::getInstance(): Argument Missing!" << endl;
	cerr << "No arguments for instantiantion" << endl;
	cerr << "Exiting..." << endl;
	exit(1);
#endif
    }
    return mInstance;
}

StTpcGeometry*
StTpcROOTGeometry::instance(geometryDataSet* dS)
{
    if (!mInstance) {
	mInstance = new StTpcROOTGeometry(dS);
    }
    else {
	cerr << "StTpcROOTGeometry::getInstance()\a\a\a"  << endl;
	cerr << "\tWARNING:" << endl;
	cerr << "\tSingleton class is already instantiated" << endl;
	cerr << "\tDataSet Argument ignored!!" << endl;
	cerr << "\tContinuing..." << endl;
    }
    return mInstance;
}

int StTpcROOTGeometry::numberOfPadsAtRow(int r) const
{
    if(r<1 || r>mPadRows) {
#ifndef ST_NO_EXCEPTIONS
	throw invalid_argument("Row limits are from 1--45");
#else
	cerr << "Error in StTpcROOTGeometry::numberOfPadsAtRow() "
	     << "Row limits [1--" << mPadRows << "]"
	     << " (r=" << r << ")" << endl;
	exit(1);
#endif
    }
    else
	return (mPadsInRow[r-1]);  // careful indexing...
}

double StTpcROOTGeometry::radialDistanceAtRow(int r) const
{
    if(r<0 || r>mPadRows) {
#ifndef ST_NO_EXCEPTIONS
	throw invalid_argument("Row limits are from 1--45");
#else
	cerr << "Error in StTpcROOTGeometry::numberOfPadsAtRow() "
	     << "Row limits [1--" << mPadRows << "]"
	     << " (r=" << r << ")" << endl;
	exit(1);
#endif
    }
    else {
	return (mRadialDistanceAtRow[r-1]); // careful indexing...
    }
}

double StTpcROOTGeometry::outerSectorAnodeWire(int n) const
{
    if(n>0 && n<numberOfOuterSectorAnodeWires()) {
#ifndef ST_NO_EXCEPTIONS
	throw range_error("Invalid wire number");
#else
	cerr << "Wire Range must be 0 < n < " << numberOfOuterSectorAnodeWires() << endl;
	cerr << "Exitting..." << endl;
	exit(0);
#endif	
    }
    return (firstOuterSectorAnodeWire() + (n-1)*anodeWirePitch());
}

double StTpcROOTGeometry::innerSectorAnodeWire(int n) const
{
    if(n>0 && n<numberOfInnerSectorAnodeWires()) {
#ifndef ST_NO_EXCEPTIONS
	throw range_error("Invalid wire number");
#else
	cerr << "Wire Range must be 0 < n < " << numberOfInnerSectorAnodeWires() << endl;
	cerr << "Exitting..." << endl;
	exit(0);
#endif
    }
    return (firstInnerSectorAnodeWire() + (n-1)*anodeWirePitch());
}

// change return type for sun ??
bool StTpcROOTGeometry::acceptance(StThreeVector<StDouble>& tmp) const
{
    double radial = sqrt((tmp.x()*tmp.x())+(tmp.y()*tmp.y()));

    if((radial > mIfcRadius) &&
       (radial < mOfcRadius) &&
       (fabs(tmp.z()) < mEndCapZ))
	return true;
    else
	return false;  
}

void StTpcROOTGeometry::print(ostream& os) const
{
    os.width(23);
    os << "STAR-TPC DataBase" << endl;
    os << "=========================================="                     << endl;
    os << "padRows                = " << mPadRows                          << endl;
    os << "timeBuckets            = " << mTimeBuckets                      << endl;
    os << "sectors                = " << mSectors                          << endl;
    os << "\nField Cage:" << endl;
    os << "ifcRadius              = " << mIfcRadius/millimeter              << " mm" << endl;
    os << "ofcRadius              = " << mOfcRadius/millimeter              << " mm" << endl;
    os << "endCapZ                = " << mEndCapZ/millimeter                << " mm" << endl;
    os << "DriftDistance          = " << mDriftDistance/millimeter          << " mm" << endl;
    os << "innerSectorzOffSet = " << mInnerSectorzOffSet/millimeter    << " mm"  << endl;
    os << "outerSectorzOffSet = " << mOuterSectorzOffSet/millimeter    << " mm"  << endl;
    os << "\nPads & Rows:" << endl;
    os << "innerSectorPadWidth    = " << mInnerSectorPadWidth/millimeter    << " mm" << endl;
    os << "innerSectorPadLength   = " << mInnerSectorPadLength/millimeter   << " mm" << endl;
    os << "innerSectorPadPitch    = " << mInnerSectorPadPitch/millimeter    << " mm" << endl;
    os << "innerSectorRowPitch1   = " << mInnerSectorRowPitch1/millimeter   << " mm" << endl;
    os << "innerSectorRowPitch2   = " << mInnerSectorRowPitch2/millimeter   << " mm" << endl;
    os << "firstPadRow            = " << mFirstPadRow/millimeter            << " mm" << endl;
    os << "firstOuterSectorPadRow = " << mFirstOuterSectorPadRow/millimeter << " mm" << endl;
    os << "lastOuterSectorPadRow  = " << mLastOuterSectorPadRow/millimeter  << " mm" << endl;
    os << "firstRowWidth          = " << mFirstRowWidth/millimeter          << " mm" << endl;
    os << "lastRowWidth           = " << mLastRowWidth/millimeter           << " mm" << endl;
    os << "outerSectorPadWidth    = " << mOuterSectorPadWidth/millimeter    << " mm" << endl;
    os << "outerSectorPadLength   = " << mOuterSectorPadLength/millimeter   << " mm" << endl;
    os << "outerSectorPadPitch    = " << mOuterSectorPadPitch /millimeter   << " mm" << endl;
    os << "outerSectorRowPitch    = " << mOuterSectorRowPitch/millimeter    << " mm" << endl;
    os << "outerSectorLength      = " << mOuterSectorLength /millimeter     << " mm" << endl;
    os << "ioSectorSeparation     = " << mIoSectorSeparation/millimeter     << " mm" << endl;
    os << "\nWires:" << endl;
    os << "anodeWirePitch         = " << mAnodeWirePitch/millimeter         << " mm" << endl;
    os << "frischGridWirePitch    = " << mFrischGridWirePitch/millimeter    << " mm" << endl;
    os << "gatingGridWirePitch    = " << mGateWirePitch/millimeter          << " mm" << endl;
    os << "anodeWireRadius        = " << mAnodeWireRadius/micrometer        << " um" << endl;
    os << "frischGridWireRadius   = " << mFrischGridWireRadius/micrometer   << " um" << endl;
    os << "gatingGridWireRadius   = " << mGateWireRadius/micrometer         << " um" << endl;
   
    os << "Inner Sector:" << endl;
    os << "AnodeWirePadPlane  Sep = " << mInnerSectorAnodeWirePadPlaneSeparation/millimeter  << " mm" << endl;
    os << "FrischGridPadPlane Sep = " << mInnerSectorFrischGridPadPlaneSeparation/millimeter << " mm" << endl;
    os << "GatingGridPadPlane Sep = " << mInnerSectorGatingGridPadPlaneSeparation/millimeter << " mm" << endl;

    os << "Outer Sector:" << endl;
    os << "AnodeWirePadPlane  Sep = " << mOuterSectorAnodeWirePadPlaneSeparation/millimeter  << " mm" << endl;
    os << "FrischGridPadPlane Sep = " << mOuterSectorFrischGridPadPlaneSeparation/millimeter << " mm" << endl;
    os << "GatingGridPadPlane Sep = " << mOuterSectorGatingGridPadPlaneSeparation/millimeter << " mm" << endl;

    os << "numberOfInnerSectorAnodeWires = " << mNumberOfInnerSectorAnodeWires <<  endl;
    os << "firstInnerSectorAnodeWire     = " << mFirstInnerSectorAnodeWire/millimeter     << " mm" << endl;
    os << "lastInnerSectorAnodeWire      = " << mLastInnerSectorAnodeWire/millimeter      << " mm" << endl;
    os << "numberOfOuterSectorAnodeWires = " << mNumberOfOuterSectorAnodeWires          << endl;
    os << "firstOuterSectorAnodeWire     = " << mFirstOuterSectorAnodeWire/millimeter     << " mm" << endl;
    os << "lastOuterSectorAnodeWire      = " << mLastOuterSectorAnodeWire/millimeter      << " mm" << endl;
    os << "innerSectorEdge               = " << mInnerSectorEdge/millimeter << " mm" << endl;
    os << "outerSectorEdge               = " << mOuterSectorEdge/millimeter << " mm" << endl;
    //os << "ioSectorSpacing               = " << mIoSectorSpacing/millimeter << " mm" << endl;
    os << endl;
    os << " Row      Pads      Radial Distance to Row" << endl;
    os << "==========================================" << endl;
    for(int i=0; i<mPadRows; i++) {
	os.width(3);
	os.setf(ios::right,ios::adjustfield);
	os << i;
	os.width(9);
	os << mPadsInRow[i];
	os.width(15);
	os << mRadialDistanceAtRow[i]/millimeter << " mm" << endl;
    }
    os << endl;
}
#endif
