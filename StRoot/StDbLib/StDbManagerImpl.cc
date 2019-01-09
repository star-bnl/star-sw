/***************************************************************************
 *   
 * $Id: StDbManagerImpl.cc,v 1.51 2019/01/08 19:12:54 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Manages access to Servers and passes Query-by-Table to db
 *
 ***************************************************************************
 *
 * $Log: StDbManagerImpl.cc,v $
 * Revision 1.51  2019/01/08 19:12:54  dmitry
 * new subsystem: ETOF
 *
 * Revision 1.50  2018/11/30 20:04:01  dmitry
 * new detector added - RHICf
 *
 * Revision 1.49  2017/02/24 18:52:25  dmitry
 * new detector added - EPD
 *
 * Revision 1.48  2016/05/24 20:26:48  dmitry
 * coverity - unreachable delete loop suppression
 *
 * Revision 1.47  2015/04/13 19:43:43  dmitry
 * added new db domain: SST
 *
 * Revision 1.46  2014/10/31 16:11:59  dmitry
 * adding FPS to the database domain list
 *
 * Revision 1.45  2013/09/11 17:40:06  dmitry
 * new database support: IST
 *
 * Revision 1.44  2013/06/10 17:45:15  dmitry
 * unknown db fix - to be backported into old libraries
 *
 * Revision 1.43  2013/05/28 18:07:43  dmitry
 * new db domain: MTD
 *
 * Revision 1.42  2013/04/01 14:42:51  dmitry
 * added new domain - PXL
 *
 * Revision 1.41  2012/06/11 14:33:47  fisyak
 * std namespace
 *
 * Revision 1.40  2012/04/09 14:32:26  dmitry
 * AFS-related patch commented out
 *
 * Revision 1.39  2012/04/08 20:48:09  dmitry
 * added alternate hardcoded location for dbLoadBalancerLocalConfig_BNL.xml
 *
 * Revision 1.38  2011/06/16 14:44:00  dmitry
 * added new domain - FGT
 *
 * Revision 1.37  2011/04/04 15:44:24  dmitry
 * fix to blacklist Calibrations_bla only
 *
 * Revision 1.36  2011/02/10 17:30:42  dmitry
 * added an option to blacklist domains
 *
 * Revision 1.35  2009/12/04 16:06:52  dmitry
 * StDbLib in standalone mode cannot use SafeDelete - proper wrapper added
 *
 * Revision 1.34  2009/11/10 20:24:45  fisyak
 * Use SafeDelete
 *
 * Revision 1.33  2009/10/12 15:06:11  dmitry
 * added new domain: pp2pp
 *
 * Revision 1.32  2009/09/11 13:11:49  dmitry
 * added FMS to domain list
 *
 * Revision 1.31  2009/09/10 18:06:07  dmitry
 * struct alignment fix, does not rely on fixed 4 byte cap anymore - runtime align calculation is now in use
 *
 * Revision 1.30  2008/04/08 02:08:34  fine
 * restore the previouse version
 *
 * Revision 1.28  2007/08/24 21:02:11  deph
 * *** empty log message ***
 *
 * Revision 1.27  2007/08/20 18:21:29  deph
 * New Version of Load Balancer
 *
 * Revision 1.26  2007/05/16 22:48:10  deph
 * Replaced cerr with LOG_ERROR <<endm; for logger
 *
 * Revision 1.25  2007/03/08 22:01:44  deph
 * minor change (removed a break) to allow load balancer to be backward compatible with online migration code
 *
 * Revision 1.24  2007/01/09 16:36:57  deph
 * differnt config file name for seamless autobuild
 *
 * Revision 1.23  2007/01/09 16:27:40  deph
 * Updates for load balancing "added 1)write privilege 2)xml comments 3)camelCase notation
 *
 * Revision 1.22  2006/11/16 21:50:40  deph
 * additional files needed for db load balancing
 *
 * Revision 1.21  2006/08/17 02:58:57  deph
 * updated load balancer - removing hard-coded nodes from API to xml
 *
 * Revision 1.20  2005/01/25 22:05:48  deph
 * Reset precedence for connecting to db. From Low to High: star,home,envVar
 *
 * Revision 1.19  2004/08/11 02:09:35  deph
 * added zdc domain
 *
 * Revision 1.18  2004/02/02 00:41:24  deph
 * Added bbc and tracker domains
 *
 * Revision 1.17  2004/01/15 00:02:25  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.16  2003/09/16 22:44:17  porter
 * got rid of all ostrstream objects; replaced with StString+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.15  2003/09/02 17:57:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.14  2002/11/24 01:39:16  porter
 * added Pmd domain
 *
 * Revision 1.13  2002/11/14 17:43:00  porter
 * added dbEemc to enumerated domain list
 *
 * Revision 1.12  2002/10/29 21:40:39  porter
 * Added dbScalers as a dbType; dbScaler is a dbDomain from before but now
 * use as a dbType is more in line with how it will be handled
 *
 * Revision 1.11  2002/01/30 15:40:47  porter
 * changed limits on flavor tag & made defaults retrieving more readable
 *
 * Revision 1.10  2001/12/21 04:54:45  porter
 * sped up table definition for emc and changed some ostrstream usage for
 * insure tests
 *
 * Revision 1.9  2001/10/24 04:05:20  porter
 * added long long type to I/O and got rid of obsolete dataIndex table
 *
 * Revision 1.8  2001/04/25 17:18:40  perev
 * HPcorrs
 *
 * Revision 1.7  2001/04/18 19:33:37  porter
 * added dbRts enumeration for run-time systems domain
 *
 * Revision 1.6  2001/03/31 15:03:46  porter
 * fix bug in StDbManagerImpl::getDbName accidently introduced yesterday
 * & added new diagnostic message in MysqlDb
 *
 * Revision 1.5  2001/03/30 18:48:26  porter
 * modified code to keep Insure from wigging-out on ostrstream functions.
 * moved some messaging into a StDbSql method.
 *
 * Revision 1.4  2001/02/22 23:01:55  porter
 * Re-introduced many-to-one name-to-table capability
 * & robustness for query errors
 *
 * Revision 1.3  2001/02/09 23:06:24  porter
 * replaced ostrstream into a buffer with ostrstream creating the
 * buffer. The former somehow clashed on Solaris with CC5 iostream (current .dev)
 *
 * Revision 1.2  2001/02/08 23:23:56  porter
 * fixed initialization of schemaID in table & fixed some warnings when
 * compiled with NODEBUG
 *
 * Revision 1.1  2001/01/22 18:37:55  porter
 * Update of code needed in next year running. This update has little
 * effect on the interface (only 1 method has been changed in the interface).
 * Code also preserves backwards compatibility so that old versions of
 * StDbLib can read new table structures.
 *  -Important features:
 *    a. more efficient low-level table structure (see StDbSql.cc)
 *    b. more flexible indexing for new systems (see StDbElememtIndex.cc)
 *    c. environment variable override KEYS for each database
 *    d. StMessage support & clock-time logging diagnostics
 *  -Cosmetic features
 *    e. hid stl behind interfaces (see new *Impl.* files) to again allow rootcint access
 *    f. removed codes that have been obsolete for awhile (e.g. db factories)
 *       & renamed some classes for clarity (e.g. tableQuery became StDataBaseI
 *       and mysqlAccessor became StDbSql)
 *
 * Revision 1.26  2000/08/15 22:51:51  porter
 * Added Root2DB class from Masashi Kaneta
 * + made code more robust against requesting data from non-existent databases
 *
 * Revision 1.25  2000/06/30 01:57:02  porter
 * fixed a delete bug & small memory leak found by Akio via Insure++ ,
 * updated SetTable() method for containing idList, corrected enumeration
 * map to rhic domain for Conditions_rhic database
 *
 * Revision 1.24  2000/06/02 13:37:36  porter
 * built up list of minor changes:
 *  - made buffer more robust for certain null inputs
 *  - fixed small leak in StDbTables & restructure call to createMemory
 *  - added dbRhic as a database domain in StDbDefs
 *  - added setUser() in StDbManager
 *  - added more diagnostic printouts in mysqlAccessor.cc
 *
 * Revision 1.23  2000/05/04 15:13:11  porter
 * added dbOnl, dbRich, dbMwc domains as standards
 *
 * Revision 1.22  2000/04/25 18:26:03  porter
 * added flavor & production time as settable query fields in
 * table &/or node. Associated SQL updated in mysqlAccessor.
 * Flavor key supports "+" as an OR symbol.
 *
 * Revision 1.21  2000/03/28 17:03:18  porter
 * Several upgrades:
 * 1. configuration by timestamp for Conditions
 * 2. query by whereClause made more systematic
 * 3. conflict between db-stored comments & number lists resolved
 * 4. ensure endtime is correct for certain query falures
 * 5. dbstl.h->handles ObjectSpace & RogueWave difference (Online vs Offline)
 *
 * Revision 1.20  2000/03/01 20:56:16  porter
 * 3 items:
 *    1. activated reConnect for server timeouts
 *    2. activated connection sharing; better resource utilization but poorer
 *       logging
 *    3. made rollback method in mysqlAccessor more robust (affects writes only)
 *
 * Revision 1.19  2000/02/24 20:30:45  porter
 * fixed padding for uchar; beginTime in mysqlAccessor;
 * added rollback safety checkes in StDbManger
 *
 * Revision 1.18  2000/02/18 16:58:09  porter
 * optimization of table-query, + whereClause gets timeStamp if indexed
 *  + fix to write multiple rows algorithm
 *
 * Revision 1.17  2000/02/15 20:27:44  porter
 * Some updates to writing to the database(s) via an ensemble (should
 * not affect read methods & haven't in my tests.
 *  - closeAllConnections(node) & closeConnection(table) method to mgr.
 *  - 'NullEntry' version to write, with setStoreMode in table;
 *  -  updated both StDbTable's & StDbTableDescriptor's copy-constructor
 *
 * Revision 1.16  2000/01/27 20:27:17  porter
 * fixed error logic for table, config, or table-list not-found
 *
 * Revision 1.15  2000/01/27 05:54:34  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
 * Revision 1.14  2000/01/19 20:20:05  porter
 * - finished transaction model needed by online
 * - fixed CC5 compile problem in StDbNodeInfo.cc
 * - replace TableIter class by StDbTableIter to prevent name problems
 *
 * Revision 1.13  2000/01/14 14:50:52  porter
 * expanded use of verbose mode & fixed inconsistency in
 * StDbNodeInfo::getElementID
 *
 * Revision 1.12  2000/01/10 20:37:54  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.11  1999/12/28 21:31:42  porter
 * added 'using std::vector' and 'using std::list' for Solaris CC5 compilation.
 * Also fixed some warnings arising from the CC5 compiles
 *
 * Revision 1.10  1999/12/03 22:24:01  porter
 * expanded functionality used by online, fixed bug in
 * mysqlAccessor::getElementID(char*), & update StDbDataSet to
 * conform to changes in Xml reader & writer
 *
 * Revision 1.9  1999/10/19 14:30:39  porter
 * modifications relevant to use with StDbBroker and future merging with
 * "params" database structure + some docs + suppressing diagnostics messages
 *
 * Revision 1.8  1999/09/30 02:06:07  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include "StDbManagerImpl.hh"
#include "StDbDefaults.hh"
#include "StDbTableFactory.hh"
#include "StDbConfigNodeImpl.hh"
#include "StDbServerImpl.hh"
#include "StDbTable.h"
#include "StDbTime.h"
#include "StDbTableIter.hh"
#include "dbCollection.h"
#include "StDbMessenger.hh"

#include <cstdlib>

#ifndef __STDB_STANDALONE__
#include "StMessMgr.h"
#else
#define LOG_DEBUG cout
#define LOG_INFO cout
#define LOG_WARN cout
#define LOG_ERROR cerr
#define LOG_FATAL cerr
#define LOG_QA cout
#define endm "\n"
#endif

#include "stdb_streams.h"
//#include "StDbDefs.hh"


#ifndef NoXmlTreeReader
//#include <libxml/nanohttp.h>
//#include <sys/stat.h>
//#include <sys/types.h>
//#include <fstream>
#endif

#include <string.h>
#include <algorithm>

#ifdef HPUX
#define freeze(i) str()
#endif

//#include <stdio.h>
using namespace std;
extern char** environ;

#define __CLASS__ "StDbManagerImpl"

StDbManagerImpl::StDbManagerImpl(): StDbManager(), dbTypeFree(dbTUser1), dbDomainFree(dbDUser1), mhasServerList(false), mhasDefaultServer(false) { 

  initTypes(); 
  initDomains(); 
  mfactory = new StDbTableFactory();
#ifndef NoXmlTreeReader
  myServiceBroker = 0;
#endif
};

////////////////////////////////////////////////////////////////

StDbManagerImpl::~StDbManagerImpl(){
 
  deleteServers();
  deleteDomains();
  deleteTypes();
#ifndef __STDB_STANDALONE__
  SafeDelete(mfactory);
  SafeDelete(Messenger);
#else
  if (mfactory) { 
	delete mfactory; 
    mfactory = 0; 
  };
  if (Messenger) { 
	delete Messenger; 
	Messenger = 0; 
  };

#endif
  mInstance=0;
#ifndef NoXmlTreeReader

#ifndef __STDB_STANDALONE__
  SafeDelete(myServiceBroker);
#else
  if (myServiceBroker) {
	delete myServiceBroker;
    myServiceBroker = 0;
  }
#endif

#endif
}

////////////////////////////////////////////////////////////////
//
//
// protected methods
//
//
////////////////////////////////////////////////////////////////

void
StDbManagerImpl::initTypes(){

addDbType(dbStDb,"StarDb"); 
addDbType(dbRunLog,"RunLog"); 
addDbType(dbServer," "); 
addDbType(dbConditions,"Conditions"); 
addDbType(dbCalibrations,"Calibrations"); 
addDbType(dbGeometry,"Geometry"); 
addDbType(dbRunCatalog,"RunCatalog"); 
addDbType(dbConfigurations,"Configurations"); 
addDbType(dbRunParams,"RunParams"); 
addDbType(dbScalers,"Scalers"); 
addDbType(dbTestScheme,"TestScheme"); 

}

////////////////////////////////////////////////////////////////

void
StDbManagerImpl::initDomains(){

addDbDomain(dbStar,"Star"); 
addDbDomain(dbTpc,"tpc"); 
addDbDomain(dbFtpc,"ftpc"); 
addDbDomain(dbEmc,"emc"); 
addDbDomain(dbSvt,"svt"); 
addDbDomain(dbCtb,"ctb"); 
addDbDomain(dbTrg,"trg"); 
addDbDomain(dbDaq,"daq"); 
addDbDomain(dbScaler,"scaler"); 
addDbDomain(dbGlobal,"global"); 
addDbDomain(dbL3,"l3"); 
addDbDomain(dbOnl,"onl"); 
addDbDomain(dbRich,"rich"); 
addDbDomain(dbMwc,"mwc"); 
addDbDomain(dbRhic,"rhic"); 
addDbDomain(dbSsd,"ssd"); 
addDbDomain(dbRts,"rts"); 
addDbDomain(dbTof,"tof"); 
addDbDomain(dbFpd,"fpd"); 
addDbDomain(dbEemc,"eemc"); 
addDbDomain(dbPmd,"pmd"); 
addDbDomain(dbBbc,"bbc"); 
addDbDomain(dbTracker,"tracker"); 
addDbDomain(dbZdc,"zdc"); 
addDbDomain(dbFms,"fms"); 
addDbDomain(dbpp2pp,"pp2pp"); 
addDbDomain(dbFgt,"fgt"); 
addDbDomain(dbPxl,"pxl"); 
addDbDomain(dbMtd,"mtd"); 
addDbDomain(dbIst,"ist"); 
addDbDomain(dbFps,"fps"); 
addDbDomain(dbSst,"sst");
addDbDomain(dbEpd,"epd");
addDbDomain(dbRhicf,"rhicf");
addDbDomain(dbEtof,"etof");
}

////////////////////////////////////////////////////////////////

void
StDbManagerImpl::addDbType(StDbType type, const char* typeName){
  mTypes.push_back(new dbType(type,typeName));
}

////////////////////////////////////////////////////////////////

void
StDbManagerImpl::addDbDomain(StDbDomain domain, const char* domainName){
  mDomains.push_back(new dbDomain(domain,domainName));
}

////////////////////////////////////////////////////////////////

StDbType
StDbManagerImpl::newDbType(const char* typeName){

  StDbType retVal=dbTypeFree;
  switch(dbTypeFree) {
    case dbTUser1:
      {
       addDbType(dbTUser1,typeName);
       dbTypeFree = dbTUser2;
       break;
      }
    case dbTUser2:
      {
       addDbType(dbTUser2,typeName);
       dbTypeFree = dbTUser3;
       break;
      }
    case dbTUser3:
      {
       addDbType(dbTUser3,typeName);
       dbTypeFree = dbTEnd;
       break;
      }
    default:
      {
       break;
      }
  }

  return retVal;
}

////////////////////////////////////////////////////////////////

StDbDomain
StDbManagerImpl::newDbDomain(const char* domainName){

  StDbDomain retVal=dbDomainFree;
  switch(dbDomainFree) {
    case dbDUser1:
      {
       addDbDomain(dbDUser1,domainName);
       dbDomainFree = dbDUser2;
       break;
      }
    case dbDUser2:
      {
       addDbDomain(dbDUser2,domainName);
       dbDomainFree = dbDUser3;
       break;
      }
    case dbDUser3:
      {
       addDbDomain(dbDUser3,domainName);
       dbDomainFree = dbDEnd;
       break;
      }
    default:
      {
       break;
      }
  }

  return retVal;
}

////////////////////////////////////////////////////////////////

void
StDbManagerImpl::deleteServers() {  

  if ( !mhasServerList ) return;

  for( auto &it : mservers ) delete it;
  mservers.clear();

}

////////////////////////////////////////////////////////////////

void
StDbManagerImpl::deleteDomains(){

  for( auto &it : mDomains ) delete it;
  mDomains.clear();

}

////////////////////////////////////////////////////////////////

void
StDbManagerImpl::deleteTypes(){

  for( auto &it : mTypes ) delete it;
  mTypes.clear();

}


////////////////////////////////////////////////////////////////
void StDbManagerImpl::lookUpServers(){
#define __METHOD__ "lookUpServer()"

  /* MLK: I considered changing the mode numbering and concluded that its semantics is 
     different. It numbers locations of a particular config file only.
     We add a different file.
  */
  
#ifndef NoXmlTreeReader

  string dbLoadBalancerLocalConfig = "dbLoadBalancerLocalConfig.xml";
  string dbLoadBalancerGlobalConfig = "dbLoadBalancerGlobalConfig.xml";

  vector<string> configFileNames;
      const char* fLocalConfig = getenv("DB_SERVER_LOCAL_CONFIG");
     

	if (!fLocalConfig)
	  {
	    LOG_ERROR << "StDbManagerImpl::lookUpServers(): DB_SERVER_LOCAL_CONFIG is undefined! "<<endm;
	  }
	else
	  {
	    configFileNames.push_back(fLocalConfig);
		//configFileNames.push_back("/star/data07/dbbackup/dbLoadBalancerLocalConfig_BNL.xml"); // alternate local config, used during AFS outage
	  }



/******Removing option to allow LB in $HOME***********************
	const char* HOME = getenv("HOME");

	if (!HOME)
	  {
	    LOG_ERROR << "StDbManagerImpl::lookUpServers(): HOME is undefined! "<<endm;
	  }
	else
	  {
	    configFileNames.push_back((string)HOME+"/"+dbLoadBalancerLocalConfig);
	  }

******************************/

	const char* STAR = getenv("STAR");

	if (!STAR)
	  {
	     LOG_ERROR << "StDbManagerImpl::lookUpServers(): STAR is undefined! "<<endm;
	  }
//	else
//	  {
//	    configFileNames.push_back((string)STAR+"/StDb/servers/"+dbLoadBalancerLocalConfig);
//	  }

	const char* fGlobalConfig = getenv("DB_SERVER_GLOBAL_CONFIG");

	if (!fGlobalConfig)
	  {
	    //LOG_ERROR << "StDbManagerImpl::lookUpServers(): DB_SERVER_GLOBAL_CONFIG is undefined! "<<endm;
	  }
	else
	  {
	    configFileNames.push_back(fGlobalConfig);
	  }

	if (STAR)
	  {
	     configFileNames.push_back((string)STAR+"/StDb/servers/"+dbLoadBalancerGlobalConfig);
	  }


	// try opening the files until the first one that opens is found

      string dbLoadBalancerConfig = "";

      vector<string>::iterator I= configFileNames.begin(); 
      while (I!=configFileNames.end())
	{
	  fstream dbLbIn;
	  dbLbIn.open((*I).c_str(),ios::in);
	  if(dbLbIn.is_open())
	    {
	      dbLoadBalancerConfig = (*I);
	      dbLbIn.close();
	      break;
	    }
	  else
	    {
	      LOG_ERROR << "StDbManagerImpl::lookUpServers(): could not open "<<(*I)<<endm;
	    }
	  ++I;
	}
      
      LOG_INFO << "StDbManagerImpl::lookUpServers(): config file name is "<<dbLoadBalancerConfig<<endm;

    /// we have determined the name of the configuration file dbLoadBalancerConfig

  myServiceBroker = new StDbServiceBroker(dbLoadBalancerConfig);
  short SBStatus = myServiceBroker->GetStatus();
  if (SBStatus == st_db_service_broker::NO_ERROR)
    {
      mhasServerList = true;
    }
  else
    {
      delete myServiceBroker;
      myServiceBroker = 0;
      LOG_ERROR << "StDbManagerImpl::lookUpServers() StDbServiceBroker error "<<SBStatus<<" disable XML loadbalancing "<<endm;
    }
  
  ///////////////////////////////////////////////////////////////
  
#endif

 char* xmlFile[3]={NULL,NULL,NULL};
 // dbFindServerMode mode[3]={userHome,serverEnvVar,starDefault};
 dbFindServerMode mode[3]={serverEnvVar,userHome,starDefault};

 StString costr;
 costr<<stendl<<"******** Order of Files searched for dbServers ********* "<<stendl;
 
 for(int i=0;i<3; i++){
   xmlFile[i]=StDbDefaults::Instance()->getServerFileName(mode[i]);
   if(xmlFile[i]){
     ifstream is(xmlFile[i]);
     if(is){

       cout << " looking at "<<i << " " << xmlFile[i]<< endl;
        costr<<"  "<<i+1<<". "<< xmlFile[i] <<stendl;
         findServersXml(is);
	 is.close();

       xmlInputSource = mode[i];
       // break;  // THIS REMOVED TO SUPPORT MANY SERVERS USED IN ONLINE MIGRATION MACROS
     } else {
       cout << "There is no "<< i << "  " << xmlFile[i] << endl;
     }
     delete [] xmlFile[i];
     xmlFile[i]=NULL;
   }
 }
 costr <<"********************************************************" << stendl;
 printInfo((costr.str()).c_str(),dbMConnect,__LINE__,__CLASS__,__METHOD__);

 // cout <<"****************HERE B ****************"<<endl;
mhasServerList = true;

#undef __METHOD__
}

////////////////////////////////////////////////////////////////
void StDbManagerImpl::findServersXml(ifstream& is){

  char* stardatabase=NULL;

  /*
MLK: changes to undo load-balancing private confuguration. 
Only basic (as before May 2006) private XML configuration is
supported. Central load-balancing configuration file is being introduced with this version.
  */

  while(!is.eof()){

  stardatabase = findServerString(is); 

  if(!stardatabase) continue;

  // local DTD ...
  char bserver[32]="<server>"; char eserver[32]="</server>";
  char bhost[32]="<host>"; char ehost[32]="</host>";
  char bsock[32]="<socket>"; char esock[32]="</socket>";
  char bport[32]="<port>"; char eport[32]="</port>";
  char bdb[32]="<databases>"; char edb[32]="</databases>";
  //  int portNum = 3306;
  int portNum = 3316;

  char* servName = mparser.getString(stardatabase,(char*)bserver,(char*)eserver);
  char* hostName = mparser.getString(stardatabase,(char*)bhost,(char*)ehost);
  char* uSocket = mparser.getString(stardatabase,(char*)bsock,(char*)esock);
  char* portNumber = mparser.getString(stardatabase,(char*)bport,(char*)eport);

  if(portNumber)portNum = atoi(portNumber);
#ifndef NoXmlTreeReader
  StDbServerImpl* myserver = new StDbServerImpl(servName,hostName,uSocket,portNum);
  myserver->PointMysqlDb(this);
  StDbServer* server = myserver; 
#else
  StDbServer* server = new StDbServerImpl(servName,hostName,uSocket,portNum);
#endif

  if(muserName) server->setUser(muserName,mpWord);
  delete [] servName; 
  delete [] hostName; 
  delete [] uSocket; 
  if(portNumber)delete [] portNumber;
  mservers.push_back(server);
  server->setTimeLogging(misTimeLogged);

  char* dbNames = mparser.getString(stardatabase,(char*)bdb,(char*)edb);
 

  if( !dbNames && !mhasDefaultServer ){


    server->setIsDefaultServer();
    mhasDefaultServer = true;

  } else {

    //    char* p1 = &dbNames[0];
    char* p1 = dbNames;
    char* aname;
    StDbType type; StDbDomain domain;
    while(p1 && (aname=getNextName(p1))){
      if(getDataBaseInfo(aname,type,domain))server->addDataBase(type,domain);
      delete [] aname;
    }
   if(dbNames)delete [] dbNames;
  }
    if(stardatabase){
      delete [] stardatabase;
      stardatabase=0;
    }
  }
}

////////////////////////////////////////////////////////////////
char* StDbManagerImpl::findServerString(ifstream& is){

char tmpline[256];
bool done = false;
bool started = false;
char* id;

 char* line=NULL;
 StString os;

while(!done){

   if(is.eof()){
     done = true; 
     return line;
   } else {
     is.getline(tmpline,255);

     if((id=strstr(tmpline,"//")))continue;

     if(!started){
        id= strstr(tmpline,"<StDbServer>");
        if(id){
          os<<tmpline;
          started = true;
        }
     } else {
        os<<tmpline;
        id=strstr(tmpline,"</StDbServer>");
        if(id)done=true;
        
     } // started check          
   } // eof check 
 } // while loop

 string osstr=os.str();
 line= new char[osstr.length()+1];
 strcpy(line,osstr.c_str());

return line;
}

////////////////////////////////////////////////////////////////
char* StDbManagerImpl::getNextName(char*& names){

char* nextName = 0;
if(!names)return nextName;

char* id = strstr(names,",");

if(!id) {
 nextName = mparser.removeBlankEnds(names);
 names = 0;
} else {
  int iloc = id-names;
  char* saveName = new char[iloc+1];
  strncpy(saveName,names,iloc);
  saveName[iloc]='\0';
  nextName=mparser.removeBlankEnds(saveName);
  delete [] saveName;
  names = id; names++;
}

return nextName;
}
////////////////////////////////////////////////////////////////
//
//  public methods
//
///////////////////////////////////////////////////////////////
void  StDbManagerImpl::turnOffTimeLogging(){
  StDbManager::turnOffTimeLogging();
  if(!mhasServerList)lookUpServers();
  for(ServerList::iterator itr = mservers.begin();
      itr != mservers.end(); ++itr)
     (*itr)->setTimeLogging(misTimeLogged);
}    
///////////////////////////////////////////////////////////////

StDataBaseI* StDbManagerImpl::findDb(StDbType type, StDbDomain domain){
  return findServer(type,domain)->useDb();
}

StDataBaseI* StDbManagerImpl::findDb(const char* dbType, const char* dbDomain){
  return findServer(dbType,dbDomain)->useDb();
}

StDataBaseI* StDbManagerImpl::findDb(const char* databaseName){
  return findServer(databaseName)->useDb();
}

StDbServer* StDbManagerImpl::findServer(StDbType type, StDbDomain domain){
#define __METHOD__

 if(!mhasServerList)lookUpServers();
 StDbServer* server = 0;

 // first check if it exists in list
 for(ServerList::iterator itr = mservers.begin();
     itr != mservers.end(); ++itr){
   if((*itr)->useDb(type,domain)){
     server = *itr;
     break;
   }
 }

 // if not build from default server
 if(!server){ 
   server=findDefaultServer();
   server->addDataBase(type,domain);
 }
return server;
#undef __METHOD__
}

////////////////////////////////////////////////////////////////

StDbServer* StDbManagerImpl::findServer(StDbNode* node){
return findServer(node->getDbType(),node->getDbDomain());
}

////////////////////////////////////////////////////////////////

StDbServer* StDbManagerImpl::findServer(const char* typeName, const char* domainName){
return findServer(getDbType(typeName),getDbDomain(domainName));
}

////////////////////////////////////////////////////////////////

StDbServer* StDbManagerImpl::findServer(const char* databaseName){

  // if databaseName contains "_" then = 'dbTypeName_dbDomainName'
  // else = 'dbTypeName' and dbDomainName="Star"

char* typeName = new char[strlen(databaseName)+1];
strcpy(typeName,databaseName);
char* domainName;

char* id=strstr(typeName,"_");
if(id){
   *id='\0';
    id++;
    domainName=new char[strlen(id)+1];
    strcpy(domainName,id);
} else {
    domainName=new char[strlen("Star")+1];
    strcpy(domainName,"Star");
}

StDbServer* server=findServer(typeName,(const char*) domainName);
delete [] domainName;
delete [] typeName;

return server;
}


////////////////////////////////////////////////////////////////

StDbServer* StDbManagerImpl::findDefaultServer(){

 
 if(!mhasServerList)
   {
     lookUpServers();
    }

 StDbServer* server = 0;

 

for(ServerList::iterator itr = mservers.begin(); itr != mservers.end(); ++itr){
   if((*itr)->isDefault()){
     server = *itr;
     break;
   }
 }

return server;
}

////////////////////////////////////////////////////////////////

char* StDbManagerImpl::getDbTypeName(StDbType type){
#define __METHOD__ "getDbTypeName(StDbType)"

char* name=0;
  for(dbTypes::iterator itr=mTypes.begin();
      itr != mTypes.end(); ++itr){
    if((*itr)->type == type){
      name = (*itr)->name;
      break;
    }
  }

  if(name) return mstringDup(name); 
  printInfo(" No dbType name found ",dbMErr,__LINE__,__CLASS__,__METHOD__);
  return name;
#undef __METHOD__
}
     

////////////////////////////////////////////////////////////////

char* StDbManagerImpl::getDbDomainName(StDbDomain domain){
#define __METHOD__ "getDbDomainName(StDbDomain)"

if(domain==dbDomainUnknown)return mstringDup("Star");

char* name=0;
  for(dbDomains::iterator itr=mDomains.begin();
      itr != mDomains.end(); ++itr){
    if((*itr)->domain == domain){
      name = (*itr)->name;
      break;
    }
  }

  if(name) return mstringDup(name);
  printInfo(" Domain Name not found",dbMErr,__LINE__,__CLASS__,__METHOD__);
  return name;
#undef __METHOD__
}


////////////////////////////////////////////////////////////////

StDbType StDbManagerImpl::getDbType(const char* typeName){
#define __METHOD__ "getDbType(typeName)"
  StDbType retType=dbStDb;
  bool found=false;
  for(dbTypes::iterator itr=mTypes.begin();
      itr != mTypes.end(); ++itr){
    if(strcmp((*itr)->name,typeName)==0){
      retType = (*itr)->type;
      found = true;
      break;
    }
  }

  if(found)return retType;
  printInfo("Adding New User dbType=",typeName,dbMDebug,__LINE__,__CLASS__,__METHOD__);

 return newDbType(typeName); //This'll overwrite User db definition
#undef __METHOD__
}     


////////////////////////////////////////////////////////////////

StDbDomain StDbManagerImpl::getDbDomain(const char* domainName){
#define __METHOD__ "getDbDomain(domainName)"
  StDbDomain retType=dbDomainUnknown;

  bool found=false;
  for(dbDomains::iterator itr=mDomains.begin();
      itr != mDomains.end(); ++itr){
    if(strcmp((*itr)->name,domainName) ==0){
      retType = (*itr)->domain;
      found=true;
      break;
    }
  }

  if(found)return retType;
  printInfo("Adding New dbDomain=",domainName,dbMWarn,__LINE__,__CLASS__,__METHOD__);
return newDbDomain(domainName);
#undef __METHOD__
}     

void StDbManagerImpl::blacklistDbDomain(const char* domainName) {
	std::string domain(domainName);
	std::transform(domain.begin(), domain.end(), domain.begin(), ::tolower);
	if (mBlacklist.find(domain) == mBlacklist.end()) {
		mBlacklist.insert(domain);
	}
}

////////////////////////////////////////////////////////////////
char* StDbManagerImpl::printDbName(StDbType type, StDbDomain domain){
  StDataBaseI* db=findServer(type,domain)->useDb();
  if(db) return db->printDbName();
  return (char*)"Unknown DataBase";
}

////////////////////////////////////////////////////////////////
StDbConfigNode* StDbManagerImpl::initConfig(const char* dbName){
 StDbType type; StDbDomain domain;
 getDataBaseInfo(dbName,type,domain);
 return initConfig(type,domain);
}

////////////////////////////////////////////////////////////////
StDbConfigNode* StDbManagerImpl::initConfig(const char* dbName, const char* configName, int opt){
#define __METHOD__ "initConfig(dbName,configName,opt)"
 StDbType type; StDbDomain domain;
 getDataBaseInfo(dbName,type,domain);
return initConfig(type,domain,configName,opt);
#undef __METHOD__
}

////////////////////////////////////////////////////////////////
StDbConfigNode* StDbManagerImpl::initConfig(StDbType type, StDbDomain domain){
  // create Config node with appropriate name for db type & domain 
  char* name = 0;
  name = getConfigNodeName(type,domain);
  StDbConfigNode* configNode = new StDbConfigNodeImpl(type,domain,name);
  configNode->setIsDbNode(true);
  delete [] name;
  return configNode;
}

////////////////////////////////////////////////////////////////
StDbConfigNode* StDbManagerImpl::initConfig(StDbType type, StDbDomain domain, const char* configName, int opt){

  if(misTimeLogged)mnodeLog.start();
  StDbConfigNode* configNode=initConfig(type,domain);
  configNode->setVersion(configName);
  configNode->buildTree(opt);
  if(misTimeLogged)mnodeLog.end();

 return configNode;
}

////////////////////////////////////////////////////////////////
StDbConfigNode* StDbManagerImpl::initConfig(StDbType type, StDbDomain domain, unsigned int requestTime, int opt){
#define __METHOD__ "initConfig(type,domain,time)"

 StDbConfigNode* configNode=0;
 StDbTable* table = new StDbTable("dbCollection");
 StDataBaseI* db = findServer(type,domain)->useDb();
 if(!db){
   printInfo("DataBase Not Found",dbMErr,__LINE__,__CLASS__,__METHOD__);
   return configNode;
 }

 if(!db->QueryDb(table,requestTime)){
   printInfo("Timestamped collection Not found",dbMErr,__LINE__,__CLASS__,__METHOD__);
   return configNode;
 } 
 dbCollection* collection = (dbCollection*)table->GetTable();
 configNode=initConfig(type,domain,collection->name,opt);
 delete table;
 return configNode;
#undef __METHOD__
}

////////////////////////////////////////////////////////////////
StDbConfigNode* StDbManagerImpl::initConfig(const char* dbName, unsigned int requestTime, int opt){
 StDbType type;  StDbDomain domain;
 getDataBaseInfo(dbName,type,domain);  // fill type & domain strings via dbName
return initConfig(type,domain,requestTime,opt);
}

////////////////////////////////////////////////////////////////
char*  StDbManagerImpl::getConfigNodeName(StDbType type, StDbDomain domain){
  // config node name is based on type IF domain is "dbStar", 
  // else it is based on the domain
 if(domain == dbStar) return getDbTypeName(type);
 return getDbDomainName(domain);
}

////////////////////////////////////////////////////////////////
char* StDbManagerImpl::getExternalVersion(StDbType type, StDbDomain domain){
  if (!type || !domain) return 0;
  char* dbname = printDbName(type,domain);
  if (!dbname) return 0;
  return getenv(dbname);
}
  
////////////////////////////////////////////////////////////////
dbEnvList*  StDbManagerImpl::getEnvList(const char* name){

  dbEnvList* retVal=new dbEnvList;

  for(int i=0;;i++){
    if(!environ[i])break;
    if(strstr(environ[i],name)){
      char* tmpName=mstringDup(environ[i]);
      char* id=strstr(tmpName,"=");
      if(id){
        *id='\0';id++;
	if(strstr(tmpName,name)){
           retVal->envVar[retVal->num]=mstringDup(tmpName);
           retVal->envDef[retVal->num]=mstringDup(id);
           retVal->num++;
	} // check of name in env-var side
        id--;*id='='; 
      } // check of "=" in env-var
      delete [] tmpName;
    } // check of name in env-var
  } // loop over all env-vars

  if(retVal->num==0){
    delete retVal;
    retVal=0;
  }

  return retVal;
}

////////////////////////////////////////////////////////////////
StDbTable* StDbManagerImpl::newDbTable(const char* dbName, const char* tabName){
  return mfactory->newDbTable(dbName,tabName);
}

////////////////////////////////////////////////////////////////
StDbTable* StDbManagerImpl::newDbTable(StDbNode* node){
  StDbTable* table=mfactory->newDbTable(node->printDbName(),node->printName());
  if(table)table->setNodeInfo(node);
  return table;
}

////////////////////////////////////////////////////////////////
void StDbManagerImpl::setRequestTime(unsigned int time){
 mcheckTime.munixTime = time;
 updateDateTime(mcheckTime);
}

////////////////////////////////////////////////////////////////
void StDbManagerImpl::setRequestTime(const char* time){
 mcheckTime.setDateTime(time);
 updateUnixTime(mcheckTime);
}

////////////////////////////////////////////////////////////////
void StDbManagerImpl::setStoreTime(unsigned int time){
 mstoreTime.munixTime = time;
 updateDateTime(mstoreTime);
}

////////////////////////////////////////////////////////////////
void StDbManagerImpl::setStoreTime(const char* time){
 mstoreTime.setDateTime(time);
 updateUnixTime(mstoreTime);
}

////////////////////////////////////////////////////////////////
void StDbManagerImpl::updateDateTime(StDbTime& t){
#define __METHOD__ "updateDateTime(StDbTime)"
  StDbServer* server=findDefaultServer();
  StDataBaseI* db=server->useDb();
  char* dt;
  if(db && (dt=db->getDateTime(t.munixTime))){ 
     t.setDateTime(dt);
     delete [] dt;
  }
#undef __METHOD__
}

////////////////////////////////////////////////////////////////
void StDbManagerImpl::updateUnixTime(StDbTime& t){
  StDbServer* server = findDefaultServer();
  StDataBaseI* db=server->useDb();
  if(db)t.setUnixTime(db->getUnixTime(t.mdateTime));
}

////////////////////////////////////////////////////////////////
unsigned int StDbManagerImpl::getUnixCheckTime(){ return mcheckTime.munixTime;}
unsigned int StDbManagerImpl::getUnixRequestTime(){return mcheckTime.munixTime;}
char* StDbManagerImpl::getDateCheckTime()  { return mcheckTime.mdateTime; }
char* StDbManagerImpl::getDateRequestTime(){ return mcheckTime.mdateTime; }

////////////////////////////////////////////////////////////////
unsigned int StDbManagerImpl::getUnixStoreTime(){ return mstoreTime.munixTime; }
char* StDbManagerImpl::getDateStoreTime(){ return mstoreTime.mdateTime; }

////////////////////////////////////////////////////////////////
bool StDbManagerImpl::IsValid(StDbTable* table){
 bool retVal = false;
 if(table) {
  unsigned int time = mcheckTime.munixTime;
  if(time >= table->getBeginTime() && time < table->getEndTime())retVal=true;
 }
 return retVal;
}

////////////////////////////////////////////////////////////////
bool StDbManagerImpl::fetchDbTable(StDbTable* table){

 bool retVal = false;
 if(!table) return false;
 StDataBaseI* db=findDb(table->getDbType(),table->getDbDomain());
 if(!db) return false; 

 if(misTimeLogged)mdataLog.start();
 if(db->QueryDb(table,mcheckTime.munixTime))retVal=true;
 if(misTimeLogged)mdataLog.end();

 return retVal;
}

bool StDbManagerImpl::fetchDbTable(StDbTable* table, char* whereClause){

 if(!table) return false;
 StDataBaseI* db=findDb(table->getDbType(),table->getDbDomain());
 if(!db) return false;
 if(misTimeLogged)mdataLog.start();
 bool retVal=(bool) db->QueryDb(table,whereClause);
 if(misTimeLogged)mdataLog.end(); 
 return retVal;
}

////////////////////////////////////////////////////////////////
bool StDbManagerImpl::fetchAllTables(StDbConfigNode* node){

bool tables=true;
bool children = true;
bool siblings = true;

  if(node->hasData()){
    StDbTableIter* itr = node->getStDbTableIter();
    while(!itr->done()) {
      StDbTable* tab=itr->next();
      tables = (tables && fetchDbTable(tab));
    }
    delete itr;
  }

  if(node->hasChildren())children = fetchAllTables(node->getFirstChildNode());
  StDbConfigNode* nextNode;
  if((nextNode=node->getNextNode()))siblings = fetchAllTables(nextNode);

return (tables && children && siblings);
}   

////////////////////////////////////////////////////////////////
bool StDbManagerImpl::storeDbTable(StDbTable* table, bool commitWhenDone){
#define __METHOD__ "storeDbTable(StDbTable*)"

if(!table) return (bool) printInfo("Cannot store Null Table",dbMErr,__LINE__,__CLASS__,__METHOD__);
if(mstoreTime.munixTime==0) return (bool) printInfo("Cannot store withou timestamp",dbMErr,__LINE__,__CLASS__,__METHOD__);

  StDataBaseI* db = findDb(table->getDbType(),table->getDbDomain());
  if(!db) return false;
  bool retVal = (bool)db->WriteDb(table,mstoreTime.munixTime);
  if(commitWhenDone)table->commitData();

return retVal;
#undef __METHOD__
}

////////////////////////////////////////////////////////////////
bool StDbManagerImpl::storeAllTables(StDbConfigNode* node, bool commitWhenDone){
#define __METHOD__ "storeAllTable(StDbConfigNode*)"

if(!node)  return (bool) printInfo("Cannot store Null Node",dbMErr,__LINE__,__CLASS__,__METHOD__);
if(mstoreTime.munixTime==0) return (bool) printInfo("Cannot store withno timestamp",dbMErr,__LINE__,__CLASS__,__METHOD__);

  bool tables=true;
  if(node->hasData()){
    StDbTableIter* itr = node->getStDbTableIter();
    while(!itr->done()){
      if(tables){
        tables = (tables && storeDbTable(itr->next(), false));
      } else {
        (itr->next())->commitData(); // prevent rollback of non-stored tables
      }
    }
  delete itr;
  }

  // end it here if store fails
if(!tables) {
  rollBackAllTables(node);
  return tables;
}

// do children & siblings
bool children = true;
bool siblings = true;

  if(node->hasChildren()) children = storeAllTables(node->getFirstChildNode(),false);

  if(!children){
    rollBackAllTables(node);
    return false;
  }
 
  StDbConfigNode* nextNode;
  if((nextNode=node->getNextNode())) siblings = storeAllTables(nextNode,false);

  if(!siblings){
    rollBackAllTables(node);
    return false;
  }
  if(commitWhenDone)commitAllTables(node);
return true;
#undef __METHOD__
}

///////////////////////////////////////////////////////////////
int
StDbManagerImpl::storeConfig(StDbConfigNode* node, int currentID, int& configID, bool commitWhenDone){
#define __METHOD__ "storeConfig(node,ID,configID,commitFlag)"

 StDataBaseI* db = findDb(node->getDbType(),node->getDbDomain());
 if(!db) return 0;
 int nodeID, childID, sibID;

if(!(nodeID=db->WriteDb(node,currentID,configID))) return printInfo(" Write Failed ",dbMErr,__LINE__,__CLASS__,__METHOD__);

 node->addWrittenNode(nodeID);

 if(node->hasChildren())
   if(!(childID=storeConfig(node->getFirstChildNode(),nodeID,configID,false))){
        rollBack(node);
        return printInfo(" Write Failed in Child Node",dbMErr,__LINE__,__CLASS__,__METHOD__);
   }

 if(node->getNextNode())
   if(!(sibID=storeConfig(node->getNextNode(),currentID,configID,false))) {
        rollBackAllNodes(node->getFirstChildNode());
        rollBack(node);
        return printInfo(" Write Failed in Sibling Node",dbMErr,__LINE__,__CLASS__,__METHOD__);
   }

if(commitWhenDone) commitAllNodes(node);
return nodeID;
#undef __METHOD__
}

////////////////////////////////////////////////////////////////
bool
StDbManagerImpl::rollBackAllTables(StDbConfigNode* node){

  bool tables, children, siblings;
  tables=children=siblings=true;

  if(node->hasData()){
    StDbTableIter* itr = node->getStDbTableIter();
    while(!itr->done()){
     StDbTable* table=itr->next();
     if(table && table->canRollBack())tables = (tables && rollBack(table));
    }
    delete itr;
  }

  if(node->hasChildren())
      children = rollBackAllTables(node->getFirstChildNode());
  StDbConfigNode* nextNode;
  if((nextNode=node->getNextNode()))
      siblings = rollBackAllTables(nextNode);

return (tables && children && siblings);
}

////////////////////////////////////////////////////////////////
bool
StDbManagerImpl::rollBackAllNodes(StDbConfigNode* node){

  bool tables, children, siblings, thisNode;
  tables=children=siblings=true;

  if(node->hasData()){
    StDbTableIter* itr = node->getStDbTableIter();
    while(!itr->done()){
      StDbTable* tab = itr->next();
      tables=(tables && rollBack((StDbNode*)tab));
    }
    delete itr;
  }

if(node->hasChildren()) 
   children=rollBackAllNodes(node->getFirstChildNode());
  StDbConfigNode* nextNode;
if((nextNode=node->getNextNode())) 
   siblings=rollBackAllNodes(nextNode);

 thisNode=rollBack(node);

return (tables && children && siblings && thisNode);
}

////////////////////////////////////////////////////////////////
bool
StDbManagerImpl::rollBack(StDbNode* node){
#define __METHOD__ "rollBack(node)"

 if(!node->canRollBack())
    return (bool)printInfo(" Cannot rollback Store of Node ",dbMErr,__LINE__,__CLASS__,__METHOD__);

 StDataBaseI* db=findDb(node->getDbType(),node->getDbDomain());
 if(!db) return false;
 return db->rollBack(node);
#undef __METHOD__
}

////////////////////////////////////////////////////////////////////////
bool
StDbManagerImpl::rollBack(StDbTable* table){

  int numRows;
  int* dataRows=table->getWrittenRows(numRows);
  if(!dataRows)return true;
  StDataBaseI* db=findDb(table->getDbType(),table->getDbDomain());
  if(!db) return false;
  return db->rollBack(table);
}

////////////////////////////////////////////////////////////////////////
bool
StDbManagerImpl::commitAllTables(StDbConfigNode* node){

bool retVal=true;

  if(node->hasData()){
    StDbTableIter* itr = node->getStDbTableIter();
    while(!itr->done()){
      StDbTable* tab = itr->next();
      tab->commitData();
    }
    delete itr;
  }

bool children = true;
bool siblings = true;

  if(node->hasChildren())children = commitAllTables(node->getFirstChildNode());
  StDbConfigNode* nextNode;
  if((nextNode=node->getNextNode()))siblings = commitAllTables(nextNode);

return (retVal && children && siblings);
}

////////////////////////////////////////////////////////////////////////
bool
StDbManagerImpl::commitAllNodes(StDbConfigNode* node){

  if(node->hasData()){
    StDbTableIter* itr = node->getStDbTableIter();
    while(!itr->done()){
      StDbNode* ntab= (StDbNode*)itr->next();
      ntab->commit();
    }
    delete itr;
  }
if(node->hasChildren())commitAllNodes(node->getFirstChildNode());
  StDbConfigNode* nextNode;
if((nextNode=node->getNextNode()))commitAllNodes(nextNode);
node->commit();

return true;
}

////////////////////////////////////////////////////////////////
void
StDbManagerImpl::closeAllConnections(){

      for(ServerList::iterator itr = mservers.begin(); 
          itr != mservers.end(); ++itr)
          (*itr)->closeConnection();
}

////////////////////////////////////////////////////////////////
void
StDbManagerImpl::closeAllConnections(StDbConfigNode* node){

if(!node) return;

 closeConnection(node);
 if(node->hasChildren())closeAllConnections(node->getFirstChildNode());
 StDbConfigNode* next = node->getNextNode();
 if(next)closeAllConnections(next);
  
} 

////////////////////////////////////////////////////////////////
void
StDbManagerImpl::closeConnection(StDbNode* node){

if(!node) return;
StDbServer* server=findServer(node->getDbType(),node->getDbDomain());
if(server && (server->isConnected())) server->closeConnection();

}

////////////////////////////////////////////////////////////////
void
StDbManagerImpl::printTimeStats(){
#define __METHOD__ "printTimeStats()"

if(!misTimeLogged){ cout<< "Timing Not Logged"<<stendl; return; }
double queryTimes=0;
double socketTimes=0;
 double connectTimes[10];
 char*  serverID[10];
 int j=0;
for(ServerList::iterator itr = mservers.begin();
    itr != mservers.end(); ++itr){
      queryTimes+=(*itr)->getQueryTimes();
      socketTimes+=(*itr)->getSocketTimes();
      connectTimes[j]=(*itr)->getConnectTimes();
      serverID[j]=(*itr)->printServerName();
      j++;
}
 double dbTotalConnect=0.;
 int i;
 for(i=0;i<j;i++)dbTotalConnect+=connectTimes[i];

 double dbTotalTimes=mnodeLog.getTotalTimes() + mdataLog.getTotalTimes();
 double dbNodeTotal=mnodeLog.getTotalTimes();// - dbTotalConnect;
 double dbNF=100.0*(dbNodeTotal/dbTotalTimes);
 double dbDF=100.0*(mdataLog.getTotalTimes()/dbTotalTimes);
 double dbQF=100.0*(queryTimes/dbTotalTimes);
 double dbSF=100.0*(socketTimes/dbTotalTimes);
 // double dbTF=100.0*((socketTimes+queryTimes)/dbTotalTimes);
 double dbCF=100.0*(dbTotalConnect/dbTotalTimes);

 StString cos;
 cos<<stendl<<"*************************** DataBase ClockTime Stats *************************** "<<stendl;

 cos<<"Total Time in DB-API       = "<<dbTotalTimes<<"\t sec"<<stendl;
 cos<<" --------------------- In aggregate ------------------"<<stendl;
 cos<<" - Prepare Nodes & Servers = "<<dbNodeTotal<<"\t sec --> "<<dbNF<<"% of total"<<stendl;
 cos<<" - Select & Retrieve data  = "<<mdataLog.getTotalTimes()<<"\t sec --> "<<dbDF<<"% of total"<<stendl;
 cos<<" --------------- In MySQL C-API (approximate) ---------"<<stendl;
 cos<<" - Connecting to Servers   = "<<dbTotalConnect<<"\t sec --> "<<dbCF<<"% of total"<<stendl;
 if(j>1){
  cos<<" [ Connections per Server; ";
  for(i=0;i<j;i++)cos<<serverID[i]<<"="<<connectTimes[i]<<" sec ";
  cos<<" ] "<<stendl;
  }
 cos<<" - SQL Query Times         = "<<queryTimes<<"\t sec --> "<<dbQF<<"% of total"<<stendl;
 cos<<" - socket transfer Times   = "<<socketTimes<<"\t sec --> "<<dbSF<<"% of total"<<stendl;

 cos<<"********************************************************************************"<<stendl;

 printInfo((cos.str()).c_str(),dbMConnect,__LINE__,__CLASS__,__METHOD__);

#undef __METHOD__
}

////////////////////////////////////////////////////////////////
bool
StDbManagerImpl::getDataBaseInfo(const char* dbName, char*& type, char*& domain){
#define __METHOD__ "getDataBaseInfo(dbName,type,domain)"
char* tmpName=mstringDup(dbName);
if(!tmpName) return false;
char* id; 
 if( !(id=strstr(tmpName,"_"))){
    type=tmpName; 
    domain = mstringDup("Star");
 } else {
    *id='\0'; id++;
    type=mstringDup(tmpName);
    domain=mstringDup(id);
    id--; *id='_';
    delete [] tmpName;
 }
 StString ni;
 ni<<" Found dbType="<<type<<" & dbDomain="<<domain;
 ni<<" from DataBase Name="<<dbName;

 printInfo((ni.str()).c_str(),dbMDebug,__LINE__,__CLASS__,__METHOD__);

return true;
#undef __METHOD__
}

bool
StDbManagerImpl::getDataBaseInfo(const char* dbName, StDbType& type, StDbDomain& domain){
  char* typeName=0; char* domainName=0;
  if(!dbName || !(getDataBaseInfo(dbName,typeName,domainName)))return false;
  type=getDbType(typeName);
  domain=getDbDomain(domainName);
  if(typeName)delete [] typeName;
  if(domainName) delete [] domainName;
return true;
} 

//////////////////////////////////////////////////////////////////
char* 
StDbManagerImpl::getDbName(const char* typeName, const char* domainName){
  if (!typeName || !domainName) return 0;
  std::string tpName(typeName);
  std::string dmName(domainName);
  std::string mergedName = tpName + "_" + dmName;
  std::string completeName;
  std::string blacklisted_domain;

  for (std::set<std::string>::iterator it = mBlacklist.begin(); it != mBlacklist.end(); ++it) {
    blacklisted_domain = *it;
    completeName = "Calibrations_" + blacklisted_domain;
    if (mergedName == completeName) {
        mergedName = "blacklist_" + mergedName;
        return mstringDup(mergedName.c_str());
    }
  }
 
  StString dbname;
  dbname<<typeName;
  if(strcmp(domainName,"Star")!=0)dbname<<"_"<<domainName;
  char* retName = mstringDup((dbname.str()).c_str());
  
  return retName;
}

#undef __CLASS__





