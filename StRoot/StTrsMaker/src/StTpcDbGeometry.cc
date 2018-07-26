/*****************************************************************
 *
 * $Id: StTpcDbGeometry.cc,v 1.12 2018/06/21 22:23:08 perev Exp $
 *
 * Authors: Brain Lasiuk & Manuel Calderon de la Barca Sanchez September 8, 1999
 *
 *****************************************************************
 * Description: Database for Geometrical parameters for TRS using
 *              the STAR Main TPC DB made by Dave Hardtke             
 *
 *****************************************************************
 *
 * $Log: StTpcDbGeometry.cc,v $
 * Revision 1.12  2018/06/21 22:23:08  perev
 * TpcGroup fixes
 *
 * Revision 1.11  2018/02/20 22:45:53  smirnovd
 * Revert "Changes from Irakli's directory to make the code compile"
 *
 * Revision 1.9  2003/09/02 17:59:18  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.8  2001/03/30 21:22:52  jeromel
 * Fixes for Insure smooth compilation
 *
 * Revision 1.7  2000/06/07 02:03:11  lasiuk
 * exit/abort ultimatum
 *
 * Revision 1.6  2000/03/15 17:39:48  calderon
 * Remove beeps
 *
 * Revision 1.5  2000/02/25 17:25:36  long
 * move hardwired parameters into database
 *
 * Revision 1.4  2000/02/10 01:21:49  calderon
 * Switch to use StTpcDb.
 * Coordinates checked for consistency.
 * Fixed problems with StTrsIstream & StTrsOstream.
 *
 * Revision 1.3  2000/01/10 23:14:29  lasiuk
 * Include MACROS for compatiblity with SUN CC5
 *
 * Revision 1.2  1999/12/08 02:10:41  calderon
 * Modified to eliminate warnings on Linux.
 *
 * Revision 1.1  1999/10/11 23:55:21  calderon
 * Version with Database Access and persistent file.
 * Not fully tested due to problems with cons, it
 * doesn't find the local files at compile time.
 * Yuri suggests forcing commit to work directly with
 * files in repository.
 *
 *
 *******************************************************************/

#ifndef ST_NO_EXCEPTIONS
#include <stdexcept>
#endif

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

#include "StTpcDbGeometry.hh"
//#include "StUtilities/StMessMgr.h"
#include "StTpcDb/StTpcDb.h"

//  public Memeber functions of StTpcDb class
//    StTpcDb(StMaker* mk);
//    ~StTpcDb();
//    StTpcPadPlaneI* PadPlaneGeometry();
//    StTpcWirePlaneI* WirePlaneGeometry();
//    StTpcDimensionsI* Dimensions();
//    StTpcSlowControlSimI* SlowControlSim();
//    StTpcGainI* Gain(int sector);
//    StTpcT0I* T0(int sector);

StTpcGeometry* StTpcDbGeometry::mInstance = 0; // static data member

// Constructor for the Geometry DB based on the global pointer
// to the STAR TPC DB.  
StTpcDbGeometry::StTpcDbGeometry(StTpcDb* globalDbPointer)
{
    gTpcDbPtr               = globalDbPointer;
    mPadRows                = St_tpcPadConfigC::instance()->numberOfRows(20);
    mInnerPadRows           = St_tpcPadConfigC::instance()->numberOfInnerRows(20);
    mInnerPadRows48         = St_tpcPadConfigC::instance()->numberOfInnerRows48(20);
    mInnerPadRows52         = St_tpcPadConfigC::instance()->numberOfInnerRows52(20);
    mOuterPadRows           = St_tpcPadConfigC::instance()->numberOfOuterRows(20);
    mInnerSectorPadWidth    = St_tpcPadConfigC::instance()->innerSectorPadWidth(20);
    mInnerSectorPadLength   = St_tpcPadConfigC::instance()->innerSectorPadLength(20);
    mInnerSectorPadPitch    = St_tpcPadConfigC::instance()->innerSectorPadPitch(20);
    mInnerSectorRowPitch1   = St_tpcPadConfigC::instance()->innerSectorRowPitch1(20);
    mInnerSectorRowPitch2   = St_tpcPadConfigC::instance()->innerSectorRowPitch2(20);
    mFirstPadRow            = St_tpcPadConfigC::instance()->firstPadRow(20);
    mFirstOuterSectorPadRow = St_tpcPadConfigC::instance()->firstOuterSectorPadRow(20);
    mLastOuterSectorPadRow  = St_tpcPadConfigC::instance()->lastOuterSectorPadRow(20);
    mFirstRowWidth          = St_tpcPadConfigC::instance()->firstRowWidth(20);
    mLastRowWidth           = St_tpcPadConfigC::instance()->lastRowWidth(20);
    mOuterSectorPadWidth    = St_tpcPadConfigC::instance()->outerSectorPadWidth(20);
    mOuterSectorPadLength   = St_tpcPadConfigC::instance()->outerSectorPadLength(20);
    mOuterSectorPadPitch    = St_tpcPadConfigC::instance()->outerSectorPadPitch(20);
    mOuterSectorRowPitch    = St_tpcPadConfigC::instance()->outerSectorRowPitch(20);
    mOuterSectorLength      = St_tpcPadConfigC::instance()->outerSectorLength(20);
    mIoSectorSeparation     = St_tpcPadConfigC::instance()->ioSectorSeparation(20);
    mInnerSectorEdge        = St_tpcPadConfigC::instance()->innerSectorEdge(20);
    mOuterSectorEdge        = St_tpcPadConfigC::instance()->outerSectorEdge(20);

    mSectors   = gTpcDbPtr->Dimensions()->numberOfSectors();
    mIfcRadius = gTpcDbPtr->Dimensions()->ifcRadius();
    mOfcRadius = gTpcDbPtr->Dimensions()->ofcRadius();

    mAnodeWireRadius      = gTpcDbPtr->WirePlaneGeometry()->anodeWireRadius();
    mFrischGridWireRadius = gTpcDbPtr->WirePlaneGeometry()->frischGridWireRadius();
    mGateWireRadius       = gTpcDbPtr->WirePlaneGeometry()->gateWireRadius();
    mAnodeWirePitch       = gTpcDbPtr->WirePlaneGeometry()->anodeWirePitch();
    mFrischGridWirePitch  = gTpcDbPtr->WirePlaneGeometry()->frischGridPitch();
    mGateWirePitch        = gTpcDbPtr->WirePlaneGeometry()->gatePitch();

    mInnerSectorAnodeWirePadPlaneSeparation  = gTpcDbPtr->WirePlaneGeometry()->innerSectorAnodeWirePadPlaneSeparation();
    mInnerSectorFrischGridPadPlaneSeparation = gTpcDbPtr->WirePlaneGeometry()->innerSectorFrischGridPadPlaneSeparation();
    mInnerSectorGatingGridPadPlaneSeparation = gTpcDbPtr->WirePlaneGeometry()->innerSectorGatingGridPadPlaneSeparation();
    mOuterSectorAnodeWirePadPlaneSeparation  = gTpcDbPtr->WirePlaneGeometry()->outerSectorAnodeWirePadPlaneSeparation();
    mOuterSectorFrischGridPadPlaneSeparation = gTpcDbPtr->WirePlaneGeometry()->outerSectorFrischGridPadPlaneSeparation();
    mOuterSectorGatingGridPadPlaneSeparation = gTpcDbPtr->WirePlaneGeometry()->outerSectorGatingGridPadPlaneSeparation();

    mNumberOfInnerSectorAnodeWires = gTpcDbPtr->WirePlaneGeometry()->numberOfInnerSectorAnodeWires();
    mFirstInnerSectorAnodeWire     = gTpcDbPtr->WirePlaneGeometry()->firstInnerSectorAnodeWire();
    mLastInnerSectorAnodeWire      = gTpcDbPtr->WirePlaneGeometry()->lastInnerSectorAnodeWire();

    mNumberOfOuterSectorAnodeWires = gTpcDbPtr->WirePlaneGeometry()->numberOfOuterSectorAnodeWires();
    mFirstOuterSectorAnodeWire     = gTpcDbPtr->WirePlaneGeometry()->firstOuterSectorAnodeWire();
    mLastOuterSectorAnodeWire      = gTpcDbPtr->WirePlaneGeometry()->lastOuterSectorAnodeWire();

//     mTimeBuckets = gTpcDbPtr->()->();
//     mEndCapZ = gTpcDbPtr->()->();
//     mFrischGrid = gTpcDbPtr->()->();
//     mInnerSectorzOffSet = gTpcDbPtr->()->();
//     mOuterSectorzOffSet = gTpcDbPtr->()->();
//     mDriftDistance = gTpcDbPtr->()->();

    mTimeBuckets = gTpcDbPtr->Electronics()->numberOfTimeBins();
    mEndCapZ     = 200.; 
    mFrischGrid  = St_tpcPadConfigC::instance()->outerSectorPadPlaneZ(20) - 
     gTpcDbPtr->WirePlaneGeometry()->outerSectorFrischGridPadPlaneSeparation();
    mInnerSectorzOffSet = gTpcDbPtr->Dimensions()->zInnerOffset();
    mOuterSectorzOffSet = gTpcDbPtr->Dimensions()->zOuterOffset();
    mDriftDistance = gTpcDbPtr->Dimensions()->gatingGridZ();

    
    //mIoSectorSpacing = gTpcDbPtr->()->();

    // should be assign --- not supported by egcs 1.0.2
    //mPadsInRow.assign(mPadRows);
    mPadsInRow.resize(mPadRows);
    for(unsigned int ii=0; ii<mPadsInRow.size(); ii++)
      mPadsInRow[ii] = St_tpcPadConfigC::instance()->numberOfPadsAtRow(20,ii+1); //careful indexing..
    //mRadialDistanceAtRow.assign(mPadRows);
    mRadialDistanceAtRow.resize(mPadRows);

    // Initialize pad map

    // The variables used to create the pad map have been already created
    // from the DB, so can leave this part as is.  However, since the DB is
    // there already, we can just loop over it.  Does this help?

    for(unsigned int jj=1; jj<46; jj++)
      mRadialDistanceAtRow[jj-1] = St_tpcPadConfigC::instance()->radialDistanceAtRow(20,jj);
    
    // Make sure units are as expected, the DB units are centimeters and so are SCL units:
    
    mIfcRadius           *= centimeter;
    mOfcRadius           *= centimeter;
    mEndCapZ             *= centimeter;
    mInnerSectorPadWidth *= centimeter;
    mInnerSectorPadLength *= centimeter;
    mInnerSectorPadPitch  *= centimeter;
    mInnerSectorRowPitch1 *= centimeter;
    mInnerSectorRowPitch2 *= centimeter;
    mFirstPadRow          *= centimeter;
    mFirstOuterSectorPadRow *= centimeter;
    mLastOuterSectorPadRow  *= centimeter;
    mFirstRowWidth          *= centimeter;
    mLastRowWidth           *= centimeter;
    mInnerSectorEdge        *= centimeter;
    
    mOuterSectorPadWidth   *= centimeter;
    mOuterSectorPadLength  *= centimeter;
    mOuterSectorPadPitch   *= centimeter;
    mOuterSectorRowPitch   *= centimeter;
    mOuterSectorLength     *= centimeter;

    mIoSectorSeparation    *= centimeter;
    mOuterSectorEdge       *= centimeter;
    //mIoSectorSpacing       *= centimeter;
    
    mFrischGrid            *= centimeter;
    mDriftDistance         *= centimeter;
    mInnerSectorzOffSet    *= centimeter;
    mOuterSectorzOffSet    *= centimeter;
    
    for(unsigned int k=0; k<mRadialDistanceAtRow.size(); k++)
	mRadialDistanceAtRow[k] *= centimeter;

    // Wires
    mAnodeWirePitch       *= centimeter;
    mFrischGridWirePitch  *= centimeter;
    mGateWirePitch        *= centimeter;
    mAnodeWireRadius      *= centimeter;
    mFrischGridWireRadius *= centimeter;
    mGateWireRadius       *= centimeter;
    
    mInnerSectorAnodeWirePadPlaneSeparation  *= centimeter;
    mInnerSectorFrischGridPadPlaneSeparation *= centimeter;
    mInnerSectorGatingGridPadPlaneSeparation *= centimeter;

    mOuterSectorAnodeWirePadPlaneSeparation  *= centimeter;
    mOuterSectorFrischGridPadPlaneSeparation *= centimeter;
    mOuterSectorGatingGridPadPlaneSeparation *= centimeter;
    
    mFirstInnerSectorAnodeWire *= centimeter;
    mLastInnerSectorAnodeWire  *= centimeter;

    mFirstOuterSectorAnodeWire *= centimeter;
    mLastOuterSectorAnodeWire  *= centimeter;
}
StTpcDbGeometry::~StTpcDbGeometry()
{
    mPadsInRow.clear();
    mRadialDistanceAtRow.clear();
}
StTpcGeometry*
StTpcDbGeometry::instance()
{
    if (!mInstance) {
#ifndef ST_NO_EXCEPTIONS
	throw invalid_argument("StTpcDbGeometry::getInstance(): Argument Missing!");
#else
	cerr << "StTpcDbGeometry::getInstance(): Argument Missing!" << endl;
	cerr << "No arguments for instantiantion" << endl;
	cerr << "Aborting..." << endl;
	abort();
#endif
    }
    return mInstance;
}

StTpcGeometry*
StTpcDbGeometry::instance(StTpcDb* gDbPtr)
{
    if (!mInstance) {
	mInstance = new StTpcDbGeometry(gDbPtr);
    }
    else {
	cerr << "StTpcDbGeometry::instance()"  << endl;
	cerr << "\tWARNING:" << endl;
	cerr << "\tSingleton class is already instantiated" << endl;
	cerr << "\tDataSet Argument ignored!!" << endl;
	cerr << "\tContinuing..." << endl;
    }
    return mInstance;
}

int StTpcDbGeometry::numberOfPadsAtRow(int r) const
{
    if(r<1 || r>mPadRows) {
#ifndef ST_NO_EXCEPTIONS
	throw invalid_argument("Row limits are from 1--45");
#else
	cerr << "Error in StTpcDbGeometry::numberOfPadsAtRow() "
	     << "Row limits [1--" << mPadRows << "]"
	     << " (r=" << r << ")" << endl;
	abort();
#endif
    }
    return (mPadsInRow[r-1]);  // careful indexing...
}

double StTpcDbGeometry::radialDistanceAtRow(int r) const
{
    if(r<0 || r>mPadRows) {
#ifndef ST_NO_EXCEPTIONS
	throw invalid_argument("Row limits are from 1--45");
#else
	cerr << "Error in StTpcDbGeometry::numberOfPadsAtRow() "
	     << "Row limits [1--" << mPadRows << "]"
	     << " (r=" << r << ")" << endl;
	abort();
#endif
    }
    return (mRadialDistanceAtRow[r-1]); // careful indexing...
}

double StTpcDbGeometry::outerSectorAnodeWire(int n) const
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

double StTpcDbGeometry::innerSectorAnodeWire(int n) const
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
bool StTpcDbGeometry::acceptance(StThreeVector<StDouble>& tmp) const
{
    double radial = ::sqrt((tmp.x()*tmp.x())+(tmp.y()*tmp.y()));

    if((radial > mIfcRadius) &&
       (radial < mOfcRadius) &&
       (fabs(tmp.z()) < mEndCapZ))
	return true;
    else
	return false;  
}

void StTpcDbGeometry::print(ostream& os) const
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
    os << "frischGrid             = " << mFrischGrid/millimeter             << " mm" << endl;
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
	os << i+1;
	os.width(9);
	os << mPadsInRow[i];
	os.width(15);
	os << mRadialDistanceAtRow[i]/millimeter << " mm" << endl;
    }
    os << endl;
}
