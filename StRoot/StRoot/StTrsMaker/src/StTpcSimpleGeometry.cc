/*****************************************************************
 *
 * $Id: StTpcSimpleGeometry.cc,v 1.12 2012/06/11 15:04:56 fisyak Exp $
 *
 * Author: brian May 20, 1998
 *
 *****************************************************************
 * Description: Database for Geometrical parameters for
 *              the STAR Main TPC              
 *
 *****************************************************************
 *
 * $Log: StTpcSimpleGeometry.cc,v $
 * Revision 1.12  2012/06/11 15:04:56  fisyak
 * std namespace
 *
 * Revision 1.11  2003/09/02 17:59:18  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.10  2001/03/30 21:23:40  jeromel
 * Fixes for Insure smooth compilation
 *
 * Revision 1.9  2000/06/07 02:03:11  lasiuk
 * exit/abort ultimatum
 *
 * Revision 1.8  2000/03/15 17:39:49  calderon
 * Remove beeps
 *
 * Revision 1.7  2000/01/10 23:14:30  lasiuk
 * Include MACROS for compatiblity with SUN CC5
 *
 * Revision 1.6  1999/12/08 02:10:42  calderon
 * Modified to eliminate warnings on Linux.
 *
 * Revision 1.5  1999/10/11 23:55:21  calderon
 * Version with Database Access and persistent file.
 * Not fully tested due to problems with cons, it
 * doesn't find the local files at compile time.
 * Yuri suggests forcing commit to work directly with
 * files in repository.
 *
 * Revision 1.4  1999/04/07 00:48:00  lasiuk
 * add z offset for driftLength
 *
 * Revision 1.3  1999/03/15 13:46:54  lasiuk
 * do not use mIoSectorSpacing; use separation instead
 *
 * Revision 1.2  1998/12/15 11:21:20  lasiuk
 * add i/o sector spacing = 3 mm
 *
 * Revision 1.1  1998/11/10 17:12:22  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.7  1998/11/08 17:01:49  lasiuk
 * allocators for vectors, resize() for LINUX compatibility
 *
 * Revision 1.6  1998/11/05 19:03:52  lasiuk
 * fix unit integrity to make sure DB call returns in base unit
 *
 * Revision 1.5  1998/11/05 18:18:50  lasiuk
 * additional wire info; drift distance
 *
 * Revision 1.4  1998/11/04 20:19:58  lasiuk
 * ensure unit integrity
 *
 * Revision 1.3  1998/06/30 22:47:39  lasiuk
 * add anode wire members
 *
 * Revision 1.2  1998/05/25 17:05:27  lasiuk
 * use databases instead of filenames
 *
 * Revision 1.1  1998/05/21 21:27:58  lasiuk
 * Initial revision
 *
 *
 ******************************************************************/
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

#include "StTpcSimpleGeometry.hh"
#include "StGetConfigValue.hh"
using namespace std;
StTpcGeometry* StTpcSimpleGeometry::mInstance = 0; // static data member

StTpcSimpleGeometry::StTpcSimpleGeometry() { /* nopt */ }

StTpcSimpleGeometry::StTpcSimpleGeometry(const char* file)
{
    StGetConfigValue(file,"padRows",mPadRows);
    StGetConfigValue(file,"innerPadRows",mInnerPadRows);
    StGetConfigValue(file,"innerPadRows48",mInnerPadRows48);
    StGetConfigValue(file,"innerPadRows52",mInnerPadRows52);
    StGetConfigValue(file,"outerPadRows",mOuterPadRows);
    StGetConfigValue(file,"timeBuckets",mTimeBuckets);
    StGetConfigValue(file,"sectors",mSectors);
    StGetConfigValue(file,"ifcRadius",mIfcRadius);
    StGetConfigValue(file,"ofcRadius",mOfcRadius);
    StGetConfigValue(file,"endCapZ",mEndCapZ);
    StGetConfigValue(file,"innerSectorPadWidth",mInnerSectorPadWidth);
    StGetConfigValue(file,"innerSectorPadLength",mInnerSectorPadLength);
    StGetConfigValue(file,"innerSectorPadPitch",mInnerSectorPadPitch);
    StGetConfigValue(file,"innerSectorRowPitch1",mInnerSectorRowPitch1);
    StGetConfigValue(file,"innerSectorRowPitch2",mInnerSectorRowPitch2);
    StGetConfigValue(file,"firstPadRow",mFirstPadRow);
    StGetConfigValue(file,"firstOuterSectorPadRow",mFirstOuterSectorPadRow);
    StGetConfigValue(file,"lastOuterSectorPadRow",mLastOuterSectorPadRow);
    StGetConfigValue(file,"firstRowWidth",mFirstRowWidth);
    StGetConfigValue(file,"lastRowWidth",mLastRowWidth);
    StGetConfigValue(file,"outerSectorPadWidth",mOuterSectorPadWidth);
    StGetConfigValue(file,"outerSectorPadLength",mOuterSectorPadLength);
    StGetConfigValue(file,"outerSectorPadPitch",mOuterSectorPadPitch);
    StGetConfigValue(file,"outerSectorRowPitch",mOuterSectorRowPitch);
    StGetConfigValue(file,"outerSectorLength",mOuterSectorLength);
    StGetConfigValue(file,"ioSectorSeparation",mIoSectorSeparation);
    StGetConfigValue(file,"frischGrid",mFrischGrid);
    StGetConfigValue(file,"innerSectorzOffSet",mInnerSectorzOffSet);
    StGetConfigValue(file,"outerSectorzOffSet",mOuterSectorzOffSet);
    StGetConfigValue(file,"maximumDriftDistance",mDriftDistance);

    StGetConfigValue(file,"anodeWirePitch",mAnodeWirePitch);
    StGetConfigValue(file,"frischGridWirePitch",mFrischGridWirePitch);
    StGetConfigValue(file,"gateWirePitch",mGateWirePitch);
    StGetConfigValue(file,"anodeWireRadius",mAnodeWireRadius);
    StGetConfigValue(file,"frischGridWireRadius",mFrischGridWireRadius);
    StGetConfigValue(file,"gateWireRadius",mGateWireRadius);

    StGetConfigValue(file,"iSAnodeWirePadPlaneSeparation",mInnerSectorAnodeWirePadPlaneSeparation);
    StGetConfigValue(file,"iSFrischGridPadPlaneSeparation",mInnerSectorFrischGridPadPlaneSeparation);
    StGetConfigValue(file,"iSGatingGridPadPlaneSeparation",mInnerSectorGatingGridPadPlaneSeparation);
    StGetConfigValue(file,"oSAnodeWirePadPlaneSeparation",mOuterSectorAnodeWirePadPlaneSeparation);
    StGetConfigValue(file,"oSFrischGridPadPlaneSeparation",mOuterSectorFrischGridPadPlaneSeparation);
    StGetConfigValue(file,"oSGatingGridPadPlaneSeparation",mOuterSectorGatingGridPadPlaneSeparation);
    
    StGetConfigValue(file,"firstInnerSectorAnodeWire",mFirstInnerSectorAnodeWire);
    StGetConfigValue(file,"lastInnerSectorAnodeWire",mLastInnerSectorAnodeWire);
    StGetConfigValue(file,"numberOfInnerSectorAnodeWires",mNumberOfInnerSectorAnodeWires);
    StGetConfigValue(file,"firstOuterSectorAnodeWire",mFirstOuterSectorAnodeWire);
    StGetConfigValue(file,"lastOuterSectorAnodeWire",mLastOuterSectorAnodeWire);
    StGetConfigValue(file,"numberOfOuterSectorAnodeWires",mNumberOfOuterSectorAnodeWires);
    StGetConfigValue(file,"innerSectorEdge",mInnerSectorEdge);
    StGetConfigValue(file,"outerSectorEdge",mOuterSectorEdge);
    //StGetConfigValue(file,"ioSectorSpacing",mIoSectorSpacing);

    // should be assign --- not supported by egcs 1.0.2
    //mPadsInRow.assign(mPadRows);
    mPadsInRow.resize(mPadRows);
    StGetConfigValue(file,"padsInRow",mPadsInRow,mPadRows);
    //mRadialDistanceAtRow.assign(mPadRows);
    mRadialDistanceAtRow.resize(mPadRows);

    // Initialize pad map

    // !!!! In principle use data-base values for the loop variables!!!!!
    // first 8 pad rows
    for(int ii=1; ii<9; ii++)
	mRadialDistanceAtRow[ii-1] =
	    mFirstPadRow+(ii-1)*mInnerSectorRowPitch1;
	
    // padrows 9-13
    double base =
	mFirstPadRow + 7*mInnerSectorRowPitch1;
    for(int i=9; i<14; i++)
	mRadialDistanceAtRow[i-1] =
	    base+(i-8)*mInnerSectorRowPitch2;

    // pad row 14
    mRadialDistanceAtRow[13] =
	mFirstPadRow + 7*mInnerSectorRowPitch1 + 5*mInnerSectorRowPitch2 + mIoSectorSeparation;

    // pad row 15-45
    for(int jj=15; jj<46; jj++)
	mRadialDistanceAtRow[jj-1] =
	    mFirstOuterSectorPadRow + (jj-14)*mOuterSectorRowPitch;

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
    
    for(unsigned int j=0; j<mRadialDistanceAtRow.size(); j++)
	mRadialDistanceAtRow[j] *= millimeter;

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

StTpcSimpleGeometry::~StTpcSimpleGeometry()
{
    mPadsInRow.clear();
    mRadialDistanceAtRow.clear();
}

StTpcGeometry*
StTpcSimpleGeometry::instance()
{
    if (!mInstance) {
#ifndef ST_NO_EXCEPTIONS
	throw invalid_argument("StTpcSimpleGeometry::getInstance(): Argument Missing!");
#else
	cerr << "StTpcSimpleGeometry::getInstance(): Argument Missing!" << endl;
	cerr << "No arguments for instantiantion" << endl;
	cerr << "Aborting..." << endl;
	abort();
#endif
    }
    return mInstance;
}

StTpcGeometry*
StTpcSimpleGeometry::instance(const char* file)
{
    if (!mInstance) {
	mInstance = new StTpcSimpleGeometry(file);
    }
    else {
	cerr << "StTpcSimpleGeometry::instance()"  << endl;
	cerr << "\tWARNING:" << endl;
	cerr << "\tSingleton class is already instantiated" << endl;
	cerr << "\tArgument \"" << file << "\" ignored!!" << endl;
	cerr << "\tContinuing..." << endl;
    }
    return mInstance;
}

int StTpcSimpleGeometry::numberOfPadsAtRow(int r) const
{
    if(r<1 || r>mPadRows) {
#ifndef ST_NO_EXCEPTIONS
	throw invalid_argument("Row limits are from 1--45");
#else
	cerr << "Error in StTpcSimpleGeometry::numberOfPadsAtRow() "
	     << "Row limits [1--" << mPadRows << "]"
	     << " (r=" << r << ")" << endl;
	abort();
#endif
    }
    return (mPadsInRow[r-1]);  // careful indexing...
}

double StTpcSimpleGeometry::radialDistanceAtRow(int r) const
{
    if(r<0 || r>mPadRows) {
#ifndef ST_NO_EXCEPTIONS
	throw invalid_argument("Row limits are from 1--45");
#else
	cerr << "Error in StTpcSimpleGeometry::numberOfPadsAtRow() "
	     << "Row limits [1--" << mPadRows << "]"
	     << " (r=" << r << ")" << endl;
	abort();
#endif
    }
    return (mRadialDistanceAtRow[r-1]); // careful indexing...
}

double StTpcSimpleGeometry::outerSectorAnodeWire(int n) const
{
    if(n>0 && n<numberOfOuterSectorAnodeWires()) {
#ifndef ST_NO_EXCEPTIONS
	throw range_error("Invalid wire number");
#else
	cerr << "Wire Range must be 0 < n < " << numberOfOuterSectorAnodeWires() << endl;
	cerr << "Aborting..." << endl;
	abort();
#endif	
    }
    return (firstOuterSectorAnodeWire() + (n-1)*anodeWirePitch());
}

double StTpcSimpleGeometry::innerSectorAnodeWire(int n) const
{
    if(n>0 && n<numberOfInnerSectorAnodeWires()) {
#ifndef ST_NO_EXCEPTIONS
	throw range_error("Invalid wire number");
#else
	cerr << "Wire Range must be 0 < n < " << numberOfInnerSectorAnodeWires() << endl;
	cerr << "Aborting..." << endl;
	abort();
#endif
    }
    return (firstInnerSectorAnodeWire() + (n-1)*anodeWirePitch());
}

// change return type for sun ??
bool StTpcSimpleGeometry::acceptance(StThreeVector<StDouble>& tmp) const
{
    double radial = ::sqrt((tmp.x()*tmp.x())+(tmp.y()*tmp.y()));

    if((radial > mIfcRadius) &&
       (radial < mOfcRadius) &&
       (fabs(tmp.z()) < mEndCapZ))
	return true;
    else
	return false;  
}

void StTpcSimpleGeometry::print(ostream& os) const
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
    os << "frishGrid              = " << mFrischGrid/millimeter             << " mm" << endl;
    os << "\nWires:" << endl;
    os << "anodeWirePitch         = " << mAnodeWirePitch/millimeter         << " mm" << endl;
    os << "frischGridWirePitch    = " << mFrischGridWirePitch/millimeter    << " mm" << endl;
    os << "gatingGridWirePitch    = " << mGateWirePitch/millimeter          << " mm" << endl;
    os << "anodeWireRadius        = " << mAnodeWireRadius/millimeter        << " um" << endl;
    os << "frischGridWireRadius   = " << mFrischGridWireRadius/millimeter   << " um" << endl;
    os << "gatingGridWireRadius   = " << mGateWireRadius/millimeter         << " um" << endl;
   
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
