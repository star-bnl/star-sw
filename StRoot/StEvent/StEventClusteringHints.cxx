/***************************************************************************
 *
 * $Id: StEventClusteringHints.cxx,v 2.20 2007/11/19 19:31:42 ullrich Exp $
 *
 * Author: Thomas Ullrich, Apr 2001
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEventClusteringHints.cxx,v $
 * Revision 2.20  2007/11/19 19:31:42  ullrich
 * Added class StTriggerData2008.
 *
 * Revision 2.19  2007/02/24 03:03:02  ullrich
 * Added StTriggerData2007.
 *
 * Revision 2.18  2006/01/19 21:51:51  ullrich
 * Added RnD collection.
 *
 * Revision 2.17  2004/11/02 21:19:25  ullrich
 * Added StTriggerData2005 class.
 *
 * Revision 2.16  2003/12/23 21:58:28  ullrich
 * Modifications to handle StTruggerData2004.
 *
 * Revision 2.15  2003/10/02 16:39:25  jeromel
 * Unitialized data member mNameMap
 *
 * Revision 2.14  2003/07/25 19:47:37  ullrich
 * Added StSPtrVecCalibrationVertex to I/O list.
 *
 * Revision 2.13  2003/04/16 17:48:32  ullrich
 * Added StTriggerData and inherited classe(s).
 *
 * Revision 2.12  2003/01/30 18:37:55  ullrich
 * Added Phmd and TriggerId stuff.
 *
 * Revision 2.11  2002/12/20 22:41:30  ullrich
 * Added PMD.
 *
 * Revision 2.10  2002/02/19 16:42:05  ullrich
 * Fixed bug: StDetectorState now StSPtrVecDetectorState.
 *
 * Revision 2.9  2002/01/03 20:59:33  ullrich
 * Added BBC and FPD.
 *
 * Revision 2.8  2001/12/01 15:41:55  ullrich
 * Added StDetectorState container hints.
 *
 * Revision 2.7  2001/11/07 21:19:43  ullrich
 * Added L1 trigger.
 *
 * Revision 2.6  2001/10/15 23:15:50  ullrich
 * Added StRunInfo.
 *
 * Revision 2.5  2001/05/30 17:45:54  perev
 * StEvent branching
 *
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

static const char rcsid[] = "$Id: StEventClusteringHints.cxx,v 2.20 2007/11/19 19:31:42 ullrich Exp $";

ClassImp(StEventClusteringHints)

StEventClusteringHints::~StEventClusteringHints() {/*noop*/}

StEventClusteringHints::StEventClusteringHints()
{
    fParent = 0;
    mNameMap= 0;

    setMiniDstMode();
    setBranch("StRunInfo",                   "evt_header",   2);
    setBranch("StEventInfo",                 "evt_header",   2);
    setBranch("StEventSummary",              "evt_header",   2);
    setBranch("StSPtrVecDetectorState",      "evt_header",   2);
    setBranch("StEventClusteringHints",      "evt_header",   2);
    setBranch("StSoftwareMonitor",           "evt_header",   2);
    setBranch("StL0Trigger",                 "evt_trigger",  3);
    setBranch("StL1Trigger",                 "evt_trigger",  3);
    setBranch("StL3Trigger",                 "evt_trigger",  3);
    setBranch("StTriggerDetectorCollection", "evt_trigger",  3);
    setBranch("StTriggerIdCollection",       "evt_trigger",  3);
    setBranch("StTriggerData",               "evt_trigger",  3);
    setBranch("StTriggerData2003",           "evt_trigger",  3);
    setBranch("StTriggerData2004",           "evt_trigger",  3);
    setBranch("StTriggerData2005",           "evt_trigger",  3);
    setBranch("StTriggerData2007",           "evt_trigger",  3);
    setBranch("StTriggerData2008",           "evt_trigger",  3);
    setBranch("StSPtrVecTrackDetectorInfo",  "evt_tracks",   4);
    setBranch("StSPtrVecPrimaryVertex",      "evt_tracks",   4);
    setBranch("StSPtrVecTrackNode",          "evt_tracks",   4);
    setBranch("StSPtrVecKinkVertex",         "evt_vertices", 5);
    setBranch("StSPtrVecV0Vertex",           "evt_vertices", 5);
    setBranch("StSPtrVecXiVertex",           "evt_vertices", 5);
    setBranch("StSPtrVecCalibrationVertex",  "evt_vertices", 5);
    setBranch("StEmcCollection",             "evt_emc",      6);
    setBranch("StRichCollection",            "evt_aux",      7);
    setBranch("StTofCollection",             "evt_aux",      7);
    setBranch("StFpdCollection",             "evt_aux",      7);
    setBranch("StPhmdCollection",            "evt_aux",      7);
    setBranch("StSsdHitCollection",          "evt_hits",     8);
    setBranch("StSvtHitCollection",          "evt_hits",     8);
    setBranch("StTpcHitCollection",          "evt_hits",     8);
    setBranch("StFtpcHitCollection",         "evt_hits",     8);
    setBranch("StRnDHitCollection",          "evt_hits",     8);

    setDstMode();
    setBranch("StRunInfo",                   "event", 1);
    setBranch("StEventInfo",                 "event", 1);
    setBranch("StEventSummary",              "event", 1);
    setBranch("StSPtrVecDetectorState",      "event", 1);
    setBranch("StEventClusteringHints",      "event", 1);
    setBranch("StSoftwareMonitor",           "event", 1);
    setBranch("StEmcCollection",             "event", 1);
    setBranch("StRichCollection",            "event", 1);
    setBranch("StTofCollection",             "event", 1);
    setBranch("StFpdCollection",             "event", 1);
    setBranch("StSsdHitCollection",          "event", 1);
    setBranch("StSvtHitCollection",          "event", 1);
    setBranch("StTpcHitCollection",          "event", 1);
    setBranch("StFtpcHitCollection",         "event", 1);
    setBranch("StL0Trigger",                 "event", 1);
    setBranch("StL1Trigger",                 "event", 1);
    setBranch("StL3Trigger",                 "event", 1);
    setBranch("StTriggerDetectorCollection", "event", 1);
    setBranch("StTriggerIdCollection",       "event", 1);
    setBranch("StTriggerData",               "event", 1);
    setBranch("StTriggerData2003",           "event", 1);
    setBranch("StTriggerData2004",           "event", 1);
    setBranch("StTriggerData2005",           "event", 1);
    setBranch("StTriggerData2007",           "event", 1);
    setBranch("StTriggerData2008",           "event", 1);
    setBranch("StSPtrVecKinkVertex",         "event", 1);
    setBranch("StSPtrVecV0Vertex",           "event", 1);
    setBranch("StSPtrVecXiVertex",           "event", 1);
    setBranch("StSPtrVecTrackDetectorInfo",  "event", 1);
    setBranch("StSPtrVecCalibrationVertex",  "event", 1);
    setBranch("StSPtrVecPrimaryVertex",      "event", 1);
    setBranch("StSPtrVecTrackNode",          "event", 1);
    setBranch("StPhmdCollection",            "event", 1);
    setBranch("StRnDHitCollection",          "event", 1);
} 

void
StEventClusteringHints::setDstMode()
{ 
  if (mNameMap == &mDstMap) return;
  mNameMap = &mDstMap;
  if (fParent) fParent->Notify();
}

void
StEventClusteringHints::setMiniDstMode()
{
  if (mNameMap == &mMiniDstMap) return;
  mNameMap = &mMiniDstMap;
  if (fParent) fParent->Notify();
}
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
    if (fParent) fParent->Notify();
    
}

int
StEventClusteringHints::branchId(const char* branchname) const
{
    map<string,int>::const_iterator i = mBranchIds.find(string(branchname));
    return i != mBranchIds.end() ? i->second : -1;  
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

static TBuffer&  operator<<(TBuffer& buf, const map<string,string> &s)
{
  TString ts1,ts2;
  map<string,string>::const_iterator i;
  buf << s.size();
  for (i = s.begin(); i != s.end(); i++){
    ts1 = i->first. c_str(); ts1.Streamer(buf);
    ts2 = i->second.c_str(); ts2.Streamer(buf);}
  return buf;
}

static TBuffer&  operator>>(TBuffer& buf,      map<string,string> & s)
{
  TString ts1,ts2; int i,size;
  buf >> size;
  for (i = 0;i<size;i++){
    ts1.Streamer(buf); ts2.Streamer(buf);
    s[string(ts1.Data())] = string(ts2.Data());}  
  return buf;
}
   
static TBuffer&  operator<<(TBuffer& buf, const map<string,int> &s)
{
  TString ts1;
  map<string,int>::const_iterator i;
  int size = s.size();buf << size;
  for (i = s.begin(); i != s.end(); i++){
    ts1 = i->first.c_str(); ts1.Streamer(buf);
    buf << i->second;}
  return buf;
}

static TBuffer&  operator>>(TBuffer& buf,      map<string,int> & s)
{
  TString ts1;
  int i,size,ii;
  buf >> size;
  for (i = 0;i<size;i++){
    ts1.Streamer(buf); buf>>ii;
    s[string(ts1.Data())] = ii;}
  return buf;
}



void StEventClusteringHints::Streamer(TBuffer &R__b)
{
// Stream an object of class StEventClusteringHints

   UChar_t mode;
   UInt_t R__s, R__c;
   if (R__b.IsReading()) {
      Version_t R__v = R__b.ReadVersion(&R__s, &R__c); if (R__v) { }
      R__b >> mode; mNameMap=(mode)? &mMiniDstMap:&mDstMap;
      R__b >>  mDstMap;     
      R__b >>  mMiniDstMap;     
      R__b >>  mBranchIds;
      R__b.CheckByteCount(R__s, R__c, Class());  
      
   } else { /*writing*/   
      R__c = R__b.WriteVersion(Class(), kTRUE);
      mode = (mNameMap == &mMiniDstMap);
      R__b <<  mode;     
      R__b <<  mDstMap;     
      R__b <<  mMiniDstMap;     
      R__b <<  mBranchIds;
      R__b.SetByteCount(R__c, kTRUE);
   }
}
