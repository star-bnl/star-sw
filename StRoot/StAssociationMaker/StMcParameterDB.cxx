/*****************************************
 *
 * StMcParameterDB.cxx
 *
 * Changed extension to .cxx so that it
 * would be accessible from Root macro
 *
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
    mXCut = 1. * millimeter;
    mZCut = 1. * millimeter;
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
void StMcParameterDB::setXCut(double val) { mXCut = val ;}
void StMcParameterDB::setZCut(double val) { mZCut = val ;}
void StMcParameterDB::setReqCommonHits(unsigned int val) { mReqCommonHits = val;}

double StMcParameterDB::xCut() const { return mXCut; }

double StMcParameterDB::zCut() const { return mZCut; }

unsigned int StMcParameterDB::reqCommonHits() const { return mReqCommonHits; }
