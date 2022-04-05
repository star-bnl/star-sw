/********************************************************************
 *
 * $Id: StPhmdDetector.cxx,v 2.4 2007/10/25 19:38:47 ullrich Exp $
 *
 * Author: Subhasis Chattopadhyay, Dec 2002
 ********************************************************************
 *
 * Description: Base class for PMD detector
 *
 ********************************************************************
 *
 * $Log: StPhmdDetector.cxx,v $
 * Revision 2.4  2007/10/25 19:38:47  ullrich
 * Added missing const version of method cluster().
 *
 * Revision 2.3  2007/10/25 19:24:14  ullrich
 * Added missing const version of method module().
 *
 * Revision 2.2  2003/11/07 18:33:03  perev
 * Zeroing in constructor added
 *
 * Revision 2.1  2002/12/20 22:33:00  ullrich
 * Initial Revision.
 *
 ********************************************************************/
#include "StPhmdDetector.h"
#include "StPhmdHit.h"
#include "StPhmdModule.h"
#include "StPhmdClusterCollection.h"


ClassImp(StPhmdDetector)

StPhmdDetector::StPhmdDetector() 
{
   mDetectorId = kUnknownId;
   memset(mModulesNHit,0,sizeof(mModulesNHit));
   memset(mModules    ,0,sizeof(mModules    ));
   mClusters = 0;	
}

StPhmdDetector::StPhmdDetector(StDetectorId id)
{
    mDetectorId = id;
    memset(mModulesNHit,0,sizeof(mModulesNHit));
    memset(mModules    ,0,sizeof(mModules    ));
    mClusters = 0;	
    for (int i=0; i<mMaxModules; i++)
	this->setModule(new StPhmdModule(), i);
}

StPhmdDetector::~StPhmdDetector()
{
    for (int i=0; i<mMaxModules; i++) delete mModules[i];
    memset(mModules    ,0,sizeof(mModules    ));
    delete mClusters; mClusters = 0;
}

bool
StPhmdDetector::addHit(StPhmdHit* hit)
{
    if (hit) {
	unsigned int m = static_cast<unsigned int>(hit->module());
	if (m <= mMaxModules) {
	    mModules[m]->hits().push_back(hit);
	    mModulesNHit[m]++;
	    return true;
	}
    }
    return false;
}

int
StPhmdDetector::moduleHits(unsigned int i) 
{ 
    if (i <= mMaxModules)
	return mModulesNHit[i];
    else
	return 0;
}

unsigned int
StPhmdDetector::numberOfModules() const { return mMaxModules; }

unsigned int
StPhmdDetector::numberOfHits() const
{
    unsigned int sum = 0;
    return sum;
}

StPhmdModule*
StPhmdDetector::module(unsigned int i)
{
    if (i < mMaxModules)
	return (mModules[i]);
    else
	return 0;
}

const StPhmdModule*
StPhmdDetector::module(unsigned int i) const
{
    if (i < mMaxModules)
	return (mModules[i]);
    else
	return 0;
}

void
StPhmdDetector::setModule(StPhmdModule* val, unsigned int IdMod)
{
    if (val) {
	if (IdMod < mMaxModules) {
	    if (mModules[IdMod]) delete mModules[IdMod];
	    mModules[IdMod] = val;
	}
    }
}

StPhmdClusterCollection*
StPhmdDetector::cluster() {return mClusters;}

const StPhmdClusterCollection*
StPhmdDetector::cluster() const {return mClusters;}

void
StPhmdDetector::setCluster(StPhmdClusterCollection* val)
{
    if (mClusters) delete mClusters;
    mClusters = val;
}

ostream&  operator<<(ostream& os, const StPhmdHit& h)
{
    os << "super="         << h.superModule();
    os << "\tsubDetector=" << h.subDetector();
    os << "\trow="         << h.row();
    os << "\tcolumn="      << h.column();
    os << "\tedep="        << h.energy();
    return os;
}
