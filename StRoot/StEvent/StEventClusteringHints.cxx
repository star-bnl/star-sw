/***************************************************************************
 *
 * $Id: StEventClusteringHints.cxx,v 2.2 2001/04/20 00:50:48 ullrich Exp $
 *
 * Author: Thomas Ullrich, Apr 2001
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEventClusteringHints.cxx,v $
 * Revision 2.2  2001/04/20 00:50:48  ullrich
 * Added new query methods.
 *
 * Revision 2.1  2001/04/06 17:47:20  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEventClusteringHints.h"
#include <algorithm>

static const char rcsid[] = "$Id: StEventClusteringHints.cxx,v 2.2 2001/04/20 00:50:48 ullrich Exp $";

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

vector<string>
StEventClusteringHints::listOfBranches() const
{
    vector<string> result, tmp;
    map<string,string>::iterator i;
    for (i = mNameMap->begin(); i != mNameMap->end(); i++)
	tmp.push_back(i->second);
    sort(tmp.begin(), tmp.end());
    insert_iterator<vector<string> > ins(result, result.begin());
    unique_copy(tmp.begin(), tmp.end(), ins);
    return result;
}

vector<string>
StEventClusteringHints::listOfClasses() const
{
    vector<string> result;
    map<string,string>::iterator i;
    for (i = mNameMap->begin(); i != mNameMap->end(); i++)
	result.push_back(i->first);
    sort(result.begin(), result.end());    
    return result;
}

vector<string>
StEventClusteringHints::listOfClasses(const char* branchname) const
{
    vector<string> result;
    map<string,string>::iterator i;
    for (i = mNameMap->begin(); i != mNameMap->end(); i++)
	if (string(branchname) == i->second) result.push_back(i->first);
    sort(result.begin(), result.end());    
    return result;
}

void
StEventClusteringHints::print(ostream& os)
{
    vector<string> branches = listOfBranches();
    for (unsigned int j=0; j<branches.size(); j++) {
	vector<string> classes = listOfClasses(branches[j].c_str());
	cout << branches[j] << endl;
	for (unsigned int k=0; k<classes.size(); k++)
	    cout << '\t' << classes[k] << endl;
    }
}
