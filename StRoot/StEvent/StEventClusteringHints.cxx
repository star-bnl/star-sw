/***************************************************************************
 *
 * $Id: StEventClusteringHints.cxx,v 2.4 2001/05/01 03:48:28 ullrich Exp $
 *
 * Author: Thomas Ullrich, Apr 2001
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEventClusteringHints.cxx,v $
 * Revision 2.4  2001/05/01 03:48:28  ullrich
 * Added branch IDs.
 *
 * Revision 2.3  2001/04/23 19:28:53  ullrich
 * Inherit from StObject. Not a singleton anymore.
 *
 * Revision 2.2  2001/04/20 00:50:48  ullrich
 * Added new query methods.
 *
 * Revision 2.1  2001/04/06 17:47:20  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEventClusteringHints.h"
#include <algorithm>

static const char rcsid[] = "$Id: StEventClusteringHints.cxx,v 2.4 2001/05/01 03:48:28 ullrich Exp $";

ClassImp(StEventClusteringHints)

StEventClusteringHints::~StEventClusteringHints() {/*noop*/}

StEventClusteringHints::StEventClusteringHints()
{
    setMiniDstMode();
    setBranch("StEventInfo",                 "header",   2);
    setBranch("StEventSummary",              "header",   2);
    setBranch("StSoftwareMonitor",           "header",   2);
    setBranch("StL0Trigger",                 "trigger",  3);
    setBranch("StL3Trigger",                 "trigger",  3);
    setBranch("StTriggerDetectorCollection", "trigger",  3);
    setBranch("StSPtrVecTrackDetectorInfo",  "tracks",   4);
    setBranch("StSPtrVecPrimaryVertex",      "tracks",   4);
    setBranch("StSPtrVecTrackNode",          "tracks",   4);
    setBranch("StSPtrVecKinkVertex",         "vertices", 5);
    setBranch("StSPtrVecV0Vertex",           "vertices", 5);
    setBranch("StSPtrVecXiVertex",           "vertices", 5);
    setBranch("StEmcCollection",             "emc",      6);
    setBranch("StRichCollection",            "aux",      7);
    setBranch("StTofCollection",             "aux",      7);
    setBranch("StSsdHitCollection",          "hits",     8);
    setBranch("StSvtHitCollection",          "hits",     8);
    setBranch("StTpcHitCollection",          "hits",     8);
    setBranch("StFtpcHitCollection",         "hits",     8);

    setDstMode();
    setBranch("StEventInfo",                 "event", 1);
    setBranch("StEventSummary",              "event", 1);
    setBranch("StSoftwareMonitor",           "event", 1);
    setBranch("StEmcCollection",             "event", 1);
    setBranch("StRichCollection",            "event", 1);
    setBranch("StTofCollection",             "event", 1);
    setBranch("StSsdHitCollection",          "event", 1);
    setBranch("StSvtHitCollection",          "event", 1);
    setBranch("StTpcHitCollection",          "event", 1);
    setBranch("StFtpcHitCollection",         "event", 1);
    setBranch("StL0Trigger",                 "event", 1);
    setBranch("StL3Trigger",                 "event", 1);
    setBranch("StTriggerDetectorCollection", "event", 1);
    setBranch("StSPtrVecKinkVertex",         "event", 1);
    setBranch("StSPtrVecV0Vertex",           "event", 1);
    setBranch("StSPtrVecXiVertex",           "event", 1);
    setBranch("StSPtrVecTrackDetectorInfo",  "event", 1);
    setBranch("StSPtrVecPrimaryVertex",      "event", 1);
    setBranch("StSPtrVecTrackNode",          "event", 1);
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
StEventClusteringHints::setBranch(const char* classname, const char* branchname, int id)
{
    (*mNameMap)[string(classname)] = string(branchname);
    mBranchIds[string(branchname)] = id;
}

int
StEventClusteringHints::branchId(const char* branchname) const
{
    map<string,int>::const_iterator i = mBranchIds.find(string(branchname));
    return i != mBranchIds.end() ? i->second : 0;  
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
	cout << branches[j] << "[id=" << branchId(branches[j].c_str()) << "]" << endl;
	for (unsigned int k=0; k<classes.size(); k++)
	    cout << '\t' << classes[k] << endl;
    }
}
