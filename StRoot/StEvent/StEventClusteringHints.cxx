/***************************************************************************
 *
 * $Id: StEventClusteringHints.cxx,v 2.1 2001/04/06 17:47:20 ullrich Exp $
 *
 * Author: Thomas Ullrich, Apr 2001
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEventClusteringHints.cxx,v $
 * Revision 2.1  2001/04/06 17:47:20  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEventClusteringHints.h"

static const char rcsid[] = "$Id: StEventClusteringHints.cxx,v 2.1 2001/04/06 17:47:20 ullrich Exp $";

ClassImp(StEventClusteringHints)

StEventClusteringHints* StEventClusteringHints::mSelf = 0;

StEventClusteringHints*
StEventClusteringHints::instance()
{
    if (!mSelf)
	mSelf = new StEventClusteringHints;
    return mSelf;
}

StEventClusteringHints::~StEventClusteringHints() {/*noop*/}

StEventClusteringHints::StEventClusteringHints()
{
    setMiniDstMode();
    setBranch("StEventInfo",                 "header");
    setBranch("StEventSummary",              "header");
    setBranch("StSoftwareMonitor",           "header");
    setBranch("StEmcCollection",             "emc");
    setBranch("StRichCollection",            "aux");
    setBranch("StTofCollection",             "aux");
    setBranch("StSsdHitCollection",          "hits");
    setBranch("StSvtHitCollection",          "hits");
    setBranch("StTpcHitCollection",          "hits");
    setBranch("StFtpcHitCollection",         "hits");
    setBranch("StL0Trigger",                 "trigger");
    setBranch("StL3Trigger",                 "trigger");
    setBranch("StTriggerDetectorCollection", "trigger");
    setBranch("StSPtrVecKinkVertex",         "vertices");
    setBranch("StSPtrVecV0Vertex",           "vertices");
    setBranch("StSPtrVecXiVertex",           "vertices");
    setBranch("StSPtrVecTrackDetectorInfo",  "tracks");
    setBranch("StSPtrVecPrimaryVertex",      "tracks");
    setBranch("StSPtrVecTrackNode",          "tracks");

    setDstMode();
    setBranch("StEventInfo",                 "event");
    setBranch("StEventSummary",              "event");
    setBranch("StSoftwareMonitor",           "event");
    setBranch("StEmcCollection",             "event");
    setBranch("StRichCollection",            "event");
    setBranch("StTofCollection",             "event");
    setBranch("StSsdHitCollection",          "event");
    setBranch("StSvtHitCollection",          "event");
    setBranch("StTpcHitCollection",          "event");
    setBranch("StFtpcHitCollection",         "event");
    setBranch("StL0Trigger",                 "event");
    setBranch("StL3Trigger",                 "event");
    setBranch("StTriggerDetectorCollection", "event");
    setBranch("StSPtrVecKinkVertex",         "event");
    setBranch("StSPtrVecV0Vertex",           "event");
    setBranch("StSPtrVecXiVertex",           "event");
    setBranch("StSPtrVecTrackDetectorInfo",  "event");
    setBranch("StSPtrVecPrimaryVertex",      "event");
    setBranch("StSPtrVecTrackNode",          "event");
} 

void
StEventClusteringHints::setDstMode() {mNameMap = &mDstMap;}

void
StEventClusteringHints::setMiniDstMode() {mNameMap = &mMiniDstMap;}

const char*
StEventClusteringHints::branchName(const char* classname) const
{
    map<string,string>::const_iterator i = mNameMap->find(string(classname));
    return i != mNameMap->end() ? i->second.c_str() : 0;
}

void
StEventClusteringHints::setBranch(const char* classname, const char* branchname)
{
    (*mNameMap)[string(classname)] = string(branchname);
}

void
StEventClusteringHints::print(ostream& os)
{
    map<string,string>::iterator i;
    for (i = mNameMap->begin(); i != mNameMap->end(); i++)
	os << i->first << " -> " << i->second << endl;
}
