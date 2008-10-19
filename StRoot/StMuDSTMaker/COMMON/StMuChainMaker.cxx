/***************************************************************************
 *
 * $Id: StMuChainMaker.cxx,v 1.31 2008/10/19 21:35:55 tone421 Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 **************************************************************************/
#include <fstream>

#include "StMuException.hh"
#include "StMuDebug.h"
#include "StMuChainMaker.h"
#include "StMuDbReader.h"
#include "StMuTimer.h"

#include "StMaker.h"
#include "StChain.h"

#include "TFile.h"
#include "TTree.h"
#include "TChain.h"
#include "TSystem.h"
#include "StMessMgr.h"

extern TSystem* gSystem;

string StMuChainMaker::mSQLConnection ="";

ClassImp(StMuChainMaker)
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/**
   Constructor: The argument 'name' is the name of the TTrees be chained   
*/
    StMuChainMaker::StMuChainMaker(const char* name) : mTreeName(name) {
    DEBUGMESSAGE2("");
    mChain = new TChain(mTreeName.c_str());
    mChain->SetDirectory(0);
    mDbReader = StMuDbReader::instance();
    mFileCounter=0;
    //  mSubFilters = new string[100];
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/** 
    Destructor: The TChain will not be deleted since it is passed to the 
    outside.
 */
StMuChainMaker::~StMuChainMaker() {
  DEBUGMESSAGE2("");
  //  delete []mSubFilters;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/** 
    Returns a full filename, simply concats the three arguments 'dir', 
    'fileName' and extention
*/
string StMuChainMaker::buildFileName(string dir, string fileName, string extention){
  DEBUGMESSAGE2("");
  fileName = dir + fileName + extention;
  return fileName;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/**
   Return the input string's basename by stripping of all characters from 
   the first '.' to the end and all characters after the last '/'.
 */
string StMuChainMaker::basename(string s){
  string name(s);
  size_t pos;
  pos = name.find_last_of("/");
  if (pos!=string::npos ) name.erase(0, pos+1 );
  pos = name.find_first_of(".");
  if (pos!=string::npos ) name.erase(pos,name.length() );
  DEBUGVALUE2(name);
  return name;
} 
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/**
   Return a inputs string's directory name by erasing the basename() and
   all charcters after the last '/'. If the only remaining character is 
   '/' and empty string "" will be returned.
 */
string StMuChainMaker::dirname(string s){
  string name(s);
  string base(basename(s));
  name.erase(name.find(base),base.length());
  size_t pos;
  pos = name.find_last_of("/");
  if (pos!=string::npos ) name.erase(pos, name.length());
  if (name=="/") name = "";
  DEBUGVALUE2(name);
  return name;
} 
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/**
   Parses the input strings. Multiple sub-filters will be built our of 
   'filter'. Here, ":" separates the individual filter strings (e.g.
   "MuDst:st_physics_2:raw_0001" will accept only files which have all of 
   the sub-strings "MuDst", "st_physics_2" and "raw_0001" in them.

   - If 'file' is empty, the directory 'dir' will be scanned for files.
   - If 'file' has a substring "MuDst.root" a single file will be opened.
   - If 'file' has a substring ".lis" the file will be expected to be a list.
   - In the case 'file' is not empty, 'dir' will be ignored, hence the filenames
     provided have to be full filenames (including path)

   A TChain will be built for files matching the sub filters (in all cases).
   The chain will be returned.

   There are a few caveats 
   - Directories 'dir' MUST be slash terminated !!!
   - You should NOT pass wildcard in any of the syntax but rather use 'dir'
     name, empty 'file' and a valid 'filter' as explained above.
   
 */
TChain* StMuChainMaker::make(string dir, string file, string filter, int maxFiles) {
  DEBUGMESSAGE1("");
  mMaxFiles = maxFiles;
  subFilter(filter);

  string dirFile = dir+file;
  DEBUGVALUE1(dir.c_str());
  DEBUGVALUE1(file.c_str());
  DEBUGVALUE1(dirFile.c_str());

  if (dirFile.find(".lis")!=string::npos) 	        fromList(dirFile);
  else if (dirFile.find(".files")!=string::npos)        fromList(dirFile);
  else if (dirFile.find(".MuDst.root")!=string::npos)   fromFile(dirFile);
  else if (dirFile.rfind("/") == dirFile.length()-1 )   fromDir(dirFile);
  else {
    FORCEDDEBUGMESSAGE("ATTENTION: don't know how to read input (you may have used a bogus constructor syntax)");
    return NULL;
  }
 
  if ( mFileList.size() == 0 ) {
    DEBUGMESSAGE("No files found");
    return 0;
  }

  add( mFileList );
  
  DEBUGMESSAGE2("return");
  return mChain;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuChainMaker::subFilter(string filter) {
  DEBUGMESSAGE2("");
  string tmp(filter);
  int n=0;
  size_t pos=0;
  while ( tmp.length() ) {
    DEBUGMESSAGE3("");
    DEBUGVALUE3(tmp);
    DEBUGVALUE3(string::npos);
    pos = tmp.find_first_of(":");
    if ( pos==string::npos ) pos = tmp.length();
    mSubFilters[n] = string( tmp.substr(0,pos) );
    DEBUGVALUE3(mSubFilters[n]);
    tmp.erase(0,pos+1);
    DEBUGVALUE3(tmp);
    n++;
  }				
  mSubFilters[n] = string("endOfFilters");
  DEBUGMESSAGE2("return");
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuChainMaker::add( StMuStringIntPairVector fileList ) {
    DEBUGMESSAGE3("");
    // if no entries in db, just add file

    StMuStringIntPairVectorIterator iter;
    for ( iter=fileList.begin(); iter!=fileList.end(); iter++) {
	if (mFileCounter>=mMaxFiles) break;
	add( *iter );
    }
}	

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/// This method contains a hidden assumption for STAR Scheduler purposes
void StMuChainMaker::add( StMuStringIntPair filenameEvents) { 
    string file = filenameEvents.first;
    int entries = filenameEvents.second;

    string rootdTag = "root://";
    // if this is a rootd file, check whether you are on the corresponding host
    //cout << file.c_str() << endl;
    if ( file.find(rootdTag)==0 ) {
	// get local machine name 
        string machine(gSystem->HostName());

	//if (machine.find("rcas")!=string::npos) machine += ".rcf.bnl.gov";
	//if (machine.find("rcrs")!=string::npos) machine += ".rcf.bnl.gov";
	//if (machine.find("pdsf")!=string::npos) machine += ".nersc.gov";
	//cout << machine.c_str() << endl;
	// get name of machine holding the file
	int pos = file.find("//",rootdTag.length());
	string node = file.substr(rootdTag.length(),pos-rootdTag.length());
	//cout << node.c_str() << endl;
	if ( node == machine ) {
	    LOG_INFO << " filename changed from " << file.c_str();  
	    file.erase(0,pos+1);
	    LOG_INFO << " to : " << file.c_str() << endm;
	}
    }
    
    if (entries==0 || entries==TChain::kBigNumber) { // try to read the number of event from the db reader 
	int tmp_entries = mDbReader->entries(file.c_str());
        if (tmp_entries != 0)
           entries = tmp_entries;
        else 
           entries = TChain::kBigNumber;  // If still not known, set to kBigNumber to avoid opening of file 
    }
    // If entries==0, TChain will open the file and get the number of entries
    // If entries==TChain::kBigNumber, TChain will start reading 
    //    and figure out the numbers of events while going along
    mChain->Add( file.c_str(), entries );
    mFileCounter++;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuChainMaker::fromDir(string dir) {
  DEBUGMESSAGE2("");
  DEBUGVALUE2(gSystem);

  void *pDir = gSystem->OpenDirectory(dir.c_str());
  // now find the files that end in the specified extention
  const char* fileName(0);
  while((fileName = gSystem->GetDirEntry(pDir))){
    bool good = true;
    string name(fileName);
    if( strcmp(fileName,".")==0 || strcmp(fileName,"..")==0) good=false;
    if( strcmp(fileName,".root")==0 || strcmp(fileName,"..")==0) good=false;
    if ( name.find(".MuDst.root")==string::npos ) good=false;
    if ( good && pass(name,mSubFilters) ) {
      char* fullFile = gSystem->ConcatFileName(dir.c_str(),fileName);
      // add it to the list of files
      mFileList.push_back( StMuStringIntPair( fullFile, TChain::kBigNumber ) );
      delete []fullFile;
    }
  }   
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
#include "TSQLServer.h"
#include "TSQLResult.h"
#include "TSQLRow.h"
void StMuChainMaker::fromFileCatalog(string list) { 
  DEBUGMESSAGE2("");
  TSQLServer* server = TSQLServer::Connect(mSQLConnection.c_str(),"","");
  if ( !server ) DEBUGMESSAGE("could not connect to server");

  /// get machine name 
  string machine(gSystem->Getenv("HOSTNAME"));
  //if (machine.find(".rcf.bnl.gov")==string::npos) machine += ".rcf.bnl.gov";

  /// read list of files
  string files("(filename='dummy')");
  ifstream in(list.c_str());
  if (!in) {
    DEBUGMESSAGE("can not open file");
    DEBUGVALUE(list);
    return;
  }
  char line[512];
  while ( in.getline(line,511) ) {
    string full(line);
    int split = full.rfind("/");
    string name = full.substr(split+1);
    string path = full.substr(0,split);
    DEBUGVALUE(path);
    DEBUGVALUE(name);
    if (path.find("/star/data")==0) { // if it's NFS  disk
	files += " || (filePath='" + path + "'&&fileName='" + name +"')";
    }
    else { // if it's a local disk, then add the maschine to the querey
	files += " || (filePath='" + path + "'&&fileName='" + name + "'&&nodeName='" + machine + "')";
    }
  }
  in.close();
	  
  DEBUGVALUE(files);

  // now query the database
  StMuTimer timer;
  timer.stop();
  timer.reset();
  string query = "SELECT filePath,fileName,numEntries FROM FileData LEFT JOIN FileLocations USING (fileDataId) WHERE " + files;
  DEBUGVALUE2(query.c_str());
  TSQLResult* result = server->Query(query.c_str());
  timer.stop();
  
  DEBUGVALUE(timer.elapsedTime());
  DEBUGVALUE(result->GetRowCount());
  
  // now fill chain with query results
  TSQLRow* row;
  string file;
  int entries;
  while ( (row=result->Next()) ) {
    file = row->GetField(0); 
    file += +"/";
    file += row->GetField(1);
    entries = atoi(row->GetField(2));
    mFileList.push_back( StMuStringIntPair(file, entries ));
  }

  server->Close();
  delete server;
  DEBUGVALUE2(mFileCounter);
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuChainMaker::fromList(string list) {
  DEBUGMESSAGE2("");
  ifstream inputStream(list.c_str());
  if (!(inputStream.good())) {
     LOG_ERROR << "ERROR: Cannot open list file " << list << endm;
  }
  char line[512];
  char name[500];
  string ltest;
  DEBUGVALUE(inputStream.good());
  while (inputStream.good()) {
      inputStream.getline(line,512);
	  string ltest(line);
	  if  (inputStream.good()) {
	  int numberOfEvents = TChain::kBigNumber;
	  int iret = sscanf(line,"%s%i",name, &numberOfEvents);
		  if(iret) {/*warnOff*/}
	  if ( pass(name,mSubFilters) && ltest!="") {
	      mFileList.push_back( StMuStringIntPair( name, numberOfEvents) );
	  }
      }
  }   
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuChainMaker::fromFile(string file) {
  DEBUGMESSAGE2("");
  DEBUGMESSAGE2(mTreeName.c_str());
  mFileList.push_back( StMuStringIntPair( file, TChain::kBigNumber ) );
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
 bool StMuChainMaker::pass(string file, string* filters) {
     bool good = true;
     int n=0;
     while (filters[n].find("endOfFilters")==string::npos  && good) {
	 if ( StMuDebug::level()==3 ) printf("%s %s %d ",file.c_str(),filters[n].c_str(), file.find(filters[n])==string::npos);
	 if ( (file.find(filters[n])==string::npos) ) good=false;
	 if ( StMuDebug::level()==3 ) printf("good= %d \n",good);
	 n++;
     }
     DEBUGVALUE3(good);
     return good;
 }
 /***************************************************************************
  *
  * $Log: StMuChainMaker.cxx,v $
  * Revision 1.31  2008/10/19 21:35:55  tone421
  * Code added in StMuChainMaker::fromList(string list) to ensure blank lines are ignored when they occur in the filelist paased to the StMuDstMaker constructor.
  *
  * Revision 1.30  2008/07/15 18:22:34  jeromel
  * Replace GetEnv by HostName()
  *
  * Revision 1.29  2007/08/02 18:57:49  mvl
  * One more change to avoid reading all input files on initialisation: If the number of events is '0' for a given file, set it to TChain::kBigNumber.
  *
  * Revision 1.28  2007/05/16 18:50:49  mvl
  * Cleanup of output. Replaced cout with LOG_INFO etc.
  *
  * Revision 1.27  2006/12/20 21:53:15  mvl
  * Added warning when file list not found (read mode)
  *
  * Revision 1.26  2006/06/22 23:30:55  mvl
  * Minor change to prevent reading all files during initialisation.
  * Files ar enow added with TChain::Add(filename,kBigNumber) if no event count is available.
  *
  * Revision 1.25  2005/10/06 01:30:30  mvl
  * Changed some of the logic in StMuChainMaker: Now files are no longer opened
  * and checked at the start of the job, but simply added to the TChain. TChain
  * automatically skips corrupted files (this is a new feature).
  *
  * Revision 1.24  2004/04/09 21:09:27  jeromel
  * Did not think of wildcards ...
  *
  * Revision 1.23  2004/04/08 23:58:39  jeromel
  * Used to crash on opening file if zero size. Corrected
  *
  * Revision 1.22  2004/02/17 04:56:36  jeromel
  * Extended help, added crs support, restored __GNUC__ for PRETTY_FUNCTION(checked once
  * more and yes, it is ONLY defined in GCC and so is __FUCTION__),  use of a consistent
  * internal __PRETTYF__, return NULL if no case selected (+message) and protected against
  * NULL mChain.
  *
  * Revision 1.21  2003/08/04 18:43:19  perev
  * warnOff
  *
  * Revision 1.20  2003/04/28 14:02:49  laue
  * Reversed faulty check in from Friday. Reading directories should work again
  *
  * Revision 1.19  2003/04/24 22:22:11  laue
  * Bug fixed when reading directories. Forgot the string terminator when
  * searching for the last character.
  *
  * Revision 1.18  2003/04/21 18:18:52  laue
  * Modifications for the new scheduler implementation:
  * - the filenames and the number of events per files are now supplied
  * - files on local disk are given in the rootd format
  *
  * Revision 1.17  2003/04/15 16:15:29  laue
  * Minor changes to be able to filter MuDst.root files and an example
  * how to do this. The StMuDstFilterMaker is just an example, it has to be
  * customized (spoilers, chrome weels, etc.) by the user.
  *
  * Revision 1.16  2003/03/19 18:58:04  laue
  * StMuChainMaker: updates for moved file catalog
  * StTriggerIdCollection added to the createStEvent function in StMuDst.cxx
  *
  * Revision 1.15  2003/03/06 01:34:18  laue
  * StAddRunInfoMaker is a make helper maker to add the StRunInfo for the
  * only year1 Au+Au 130GeV data
  *
  * Revision 1.14  2003/01/22 13:49:12  laue
  * debug message removed
  *
  * Revision 1.13  2003/01/10 16:37:37  laue
  * Bug fix for FileCatalog look-up. Don't require machine for NFS files on
  * /star/data... . I know a hard-wired string this is not the nice way to do
  * it, I'll improve this once I find the time.
  *
  * Revision 1.12  2002/12/31 19:52:11  laue
  * bug fix in built of filters
  *
  * Revision 1.11  2002/12/19 19:44:25  laue
  * update to read number of events from database, for files ending with .list
  *
  * Revision 1.10  2002/11/27 20:37:02  laue
  * output removed
  *
  * Revision 1.9  2002/11/18 14:29:31  laue
  * update for Yuri's new StProbPidTraits
  *
  * Revision 1.8  2002/10/01 23:46:13  laue
  * setting all unused subFilters explicitly to NULL
  *
  * Revision 1.7  2002/08/27 21:20:07  laue
  * Fei Du's request
  * fileCouter>maxFiles changed to fileCounter>=maxFiles
  * Now maxFiles and not maxFiles+1 are added to the list
  *
  * Revision 1.6  2002/08/20 19:55:48  laue
  * Doxygen comments added
  *
  * Revision 1.5  2002/05/04 23:56:29  laue
  * some documentation added
  *
  * Revision 1.4  2002/04/17 21:04:15  laue
  * minor updates
  *
  * Revision 1.3  2002/04/15 22:18:15  laue
  * bug fix in reading of single file
  *
  * Revision 1.2  2002/04/11 14:19:30  laue
  * - update for RH 7.2
  * - decrease default arrays sizes
  * - add data base readerfor number of events in a file
  *
  * Revision 1.1  2002/04/01 22:42:29  laue
  * improved chain filter options
  *
  *
  **************************************************************************/
 
 
 















