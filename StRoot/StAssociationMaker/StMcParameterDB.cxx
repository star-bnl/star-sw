/*****************************************
 *
 * $Id: StMcParameterDB.cxx,v 1.3 1999/10/01 14:08:58 calderon Exp $
 *
 * Changed extension to .cxx so that it
 * would be accessible from Root macro
 *
 * $Log: StMcParameterDB.cxx,v $
 * Revision 1.3  1999/10/01 14:08:58  calderon
 * Added Local Hit resolution Histogram. It is made by default
 * without any requirement of association, to serve
 * as a diagnostic.
 * Before building track multimap, check the size of the
 * tpc hit map.  If it is too small, print out a warning
 * and exit.
 *
 * Revision 1.2  1999/09/23 21:25:21  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *****************************************/
#include "StMcParameterDB.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"

ClassImp(StMcParameterDB)
    
StMcParameterDB* StMcParameterDB::mParamDB = 0;

StMcParameterDB* StMcParameterDB::instance()
{
    if (!mParamDB) mParamDB = new StMcParameterDB;
    return mParamDB;
}

StMcParameterDB::StMcParameterDB()
{
    mXCut = 3. * millimeter;
    mZCut = 3. * millimeter;
    mReqCommonHits = 3;
}

//copy constructor
StMcParameterDB::StMcParameterDB(const StMcParameterDB& p)
{
    
}
// assignment operator (which should never be used, since there is only one instance)
StMcParameterDB& StMcParameterDB::operator= (const StMcParameterDB& p)
{
    if (this != &p) { // protect against self assignment
	// delete old elements (none here)

	// initialize (nothing here)

	// copy in new elements
	mXCut = p.mXCut;
	mZCut = p.mZCut;
	mReqCommonHits = p.mReqCommonHits;
    }

    return *this;
}
void StMcParameterDB::setXCut(float val) { mXCut = val ;}
void StMcParameterDB::setZCut(float val) { mZCut = val ;}
void StMcParameterDB::setReqCommonHits(unsigned int val) { mReqCommonHits = val;}

// float StMcParameterDB::xCut() const { return mXCut; }

// float StMcParameterDB::zCut() const { return mZCut; }

// unsigned int StMcParameterDB::reqCommonHits() const { return mReqCommonHits; }

ostream& operator<<(ostream &os, const StMcParameterDB& mcDb)
{
    os << " X Cut : " << mcDb.xCut()/millimeter << " mm" << endl;
    os << " Z Cut : " << mcDb.zCut()/millimeter << " mm" << endl;
    os << " Required Hits for Associating Tracks: " << mcDb.reqCommonHits() << endl;
    return os;
}
