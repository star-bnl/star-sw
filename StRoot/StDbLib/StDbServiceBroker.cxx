#ifndef NoXmlTreeReader
#include "StDbServiceBroker.h"
#include "ChapiStringUtilities.h"
#include "mysql.h"
#include "math.h"
#include <string.h>
#include <libxml/nanohttp.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
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
#include <algorithm>
using namespace std;
using namespace chapi_string_utilities;

typedef vector<string>::const_iterator VCI;

using st_db_service_broker::MyScatalogVersion; 
using st_db_service_broker::NO_ERROR;
using st_db_service_broker::NO_USER;
using st_db_service_broker::NO_DOMAIN;
using st_db_service_broker::NO_HOSTS;
using st_db_service_broker::NightBegins;  
using st_db_service_broker::NightEnds;  

using stl_xml_tree::sep;

string nn[] = 
  {
    "Scatalog",
    "Site",
    "Server",
    "Host",
    "Access"
  };

enum 
  {
    SCATALOG,
    SITE,
    SERVER,
    HOST,
    ACCESS
  };

string NAME = "name";
string PORT = "port";
string USER = "user";
string SCOPE = "scope";
string WHEN_ACTIVE = "whenActive";
string ACCESS_MODE = "accessMode";
string WRITER = "writer";
string POWER = "machinePower";
string CAP = "cap"; // max # connections to a server
string INTERACTIVE = "interactive";


static MYSQL *conn;

char* Socket = 0;


//////////////////////////////////////////////////////
StDbServiceBroker::StDbServiceBroker(const string xmlbase) : 
  MyHostList(vector<ChapiDbHost>()),
  MyStatus(st_db_service_broker::NO_ERROR)
{
  last_succeeded_connect_time = time(NULL);
  seconds_to_reach_for_connect = 1800;
  char* secs = 0;
  secs = getenv("STAR_DEBUG_DB_RETRIES_SECONDS");
  if (secs) {
	seconds_to_reach_for_connect = atoi(secs);
  }

  char* whoami = getenv("USER");
  if (!whoami) whoami = getenv("LOGNAME");
  if (!whoami) 
    {
      MyStatus = NO_USER;
      return;  
    }

  const char* access_mode = getenv("DB_ACCESS_MODE");
  if (!access_mode)  
    {
      access_mode = "read";
    }

  StlXmlTree* f = new StlXmlTree();
  string ScatalogKey = sep+nn[SCATALOG];
  f->InsertKeyValuePair(ScatalogKey, MyScatalogVersion);

  string QualifiedScatalog = sep + StlXmlTree::QualifyParent(nn[SCATALOG],MyScatalogVersion);
  string ServerKey = QualifiedScatalog + sep + nn[SERVER];

  struct tm *tp;  
  time_t timeNow;
  timeNow = time(NULL);
  tp = localtime(&timeNow);
  short hour = tp->tm_hour;
  string DayOrNight;
  if (hour<NightBegins && hour >NightEnds) 
    {
      DayOrNight = "day";
    }
  else
    {
      DayOrNight = "night";
    }

  // test for the interactive user session
  std::string isInteractive = "no";
  char* testInteractive = 0;
  testInteractive = getenv ("INTERACTIVE");

  if (testInteractive) { // result would be either "yes" or "no", all lowercase
	isInteractive = testInteractive; 
    if (isInteractive == "1" || isInteractive == "YES" || isInteractive == "yes" 
		|| isInteractive == "Yes" || isInteractive == "true" || isInteractive == "TRUE") {
	  isInteractive = "yes";
    } else {
	  isInteractive = "no";
	};
  } 

  string ServerAttr =  USER+"="+(string)whoami+";"+WHEN_ACTIVE+"="+DayOrNight+";"+ACCESS_MODE+"="+access_mode+";"+INTERACTIVE+"="+isInteractive;
  if (strcmp(whoami,"starreco")==0)
  {
    ServerAttr = SCOPE+"=Production;"+ServerAttr;
  }
  else
    {
      ServerAttr = SCOPE+"=Analysis;"+ServerAttr;
    }
  f->InsertKeyValuePair(ServerKey, ServerAttr);

#ifdef DEBUG
  LOG_INFO << " Filter XML "<<endm;
  f->ShowTree();
#endif

  ParsedXml = StlXmlTree(xmlbase,f);
  if (ParsedXml.GetStatus()==stl_xml_tree::NO_XML_BASE)
    {
      MyStatus = st_db_service_broker::NO_XML_BASE;
      LOG_WARN <<"StDbServiceBroker::StDbServiceBroker: no XML description of services found "<<endm;
    }
  if (ParsedXml.GetStatus()==stl_xml_tree::BAD_XML)
    {
      MyStatus = st_db_service_broker::BAD_XML;
      LOG_WARN <<"StDbServiceBroker::StDbServiceBroker: malformed/incorrect XML description of services found "<<endm;
    }

#ifdef DEBUG
  LOG_INFO << " Parsed XML "<<endm;
  ParsedXml.ShowTree();
#endif

  xmlCleanupParser();
  delete f;
  FormHostList();
}
//////////////////////////////////////////////////////
StDbServiceBroker::StDbServiceBroker
(const string xmlbase, const string xmlfilter) : 
  MyHostList(vector<ChapiDbHost>()),
  MyStatus(st_db_service_broker::NO_ERROR)
{
  last_succeeded_connect_time = time(NULL);
  seconds_to_reach_for_connect = 1800;
  char* secs = 0;
  secs = getenv("STAR_DEBUG_DB_RETRIES_SECONDS");
  if (secs) {
	seconds_to_reach_for_connect = atoi(secs);
  }


  StlXmlTree* f = new StlXmlTree(xmlfilter);
  f->ShowTree();
  ParsedXml = StlXmlTree(xmlbase,f);

  if (ParsedXml.GetStatus()==stl_xml_tree::NO_XML_BASE)
    {
      MyStatus = st_db_service_broker::NO_XML_BASE;
      LOG_WARN <<"StDbServiceBroker::StDbServiceBroker: no XML description of services found "<<endm;
    }
  if (ParsedXml.GetStatus()==stl_xml_tree::BAD_XML)
    {
      MyStatus = st_db_service_broker::BAD_XML;
      LOG_WARN <<"StDbServiceBroker::StDbServiceBroker: malformed/incorrect XML description of services found "<<endm;
    }

  ParsedXml.ShowTree();
  xmlCleanupParser();
  delete f;
  FormHostList();
}
//////////////////////////////////////////////////////
void StDbServiceBroker::DoLoadBalancing()
{
  if (MyStatus!=st_db_service_broker::NO_ERROR)
    {
      LOG_ERROR << " StDbServiceBroker::DoLoadBalancing() errors" <<MyStatus<<endm;
    }
#ifdef DEBUG
  PrintHostList();
#endif
  bool host_found = false;
  const int MAX_COUNT = 500;
  last_succeeded_connect_time = time(NULL);
  for (int i = 0; i < MAX_COUNT; i++) {
	if (!RecommendHost()) {
		host_found = true;
		break;
	}
    LOG_WARN << "Load Balancing attempt No " << i << ". Scanned all hosts in a Load Balancer list, no usable hosts found. Will try again in 60 seconds." << endm;                                    
	sleep(60);
  }
  if (!host_found) {
    LOG_WARN << "Tried to find a host for " << MAX_COUNT << " times, will abort now." << endm;                                    
	exit(0);
  }

#ifdef DEBUG
  cout << " go to "<<GiveHostName()<<" port "<<GiveHostPort()<<"\n";
#endif
}
//////////////////////////////////////////////////////
void StDbServiceBroker::FormHostList()
{
  /*
We expect:
-- a single SCATALOG version
-- possibly access to multiple sites
-- obviously multiple hosts
  */
  
  MyHostList.clear();
  string key = StlXmlTree::MakeKey("",nn[SCATALOG]);
  vector<string> versions = ParsedXml.LookUpValueByKey(key);

  if (versions.size()!=1) return;

  string my_version = versions[0];

      vector<string> services = ParsedXml.LookUpValueByKey(key,my_version,nn[SERVER]);
      
      for (VCI ii = services.begin(); ii!=services.end(); ++ii)
	{
	  vector<string> hosts = ParsedXml.LookUpValueByKey(key,*ii,nn[HOST]);
	  
	  for (VCI iii = hosts.begin(); iii!=hosts.end(); ++iii)
	    {
	      map<string,string> host_data = StlXmlTree::ParseAttributeString(*iii);

	      int port;
	      if (!from_string<int>(port,host_data[PORT],std::dec))
	      {
		// error: non-numeric port string
		port = DefaultPort;
	      }

	      double power;
	      short cap;

	      if (!from_string<double>(power,host_data[POWER],std::dec))
	      {
		// error: non-numeric port string
#ifdef DEBUG
		LOG_ERROR << "StDbServiceBroker::FormHostList():  non-numeric port, using default power for host "<<*iii<<endm;
#endif
		power = DefaultPower;
	      }


	      if (!from_string<short>(cap,host_data[CAP],std::dec))
	      {
		// error: non-numeric cap string
#ifdef DEBUG
		LOG_ERROR << "StDbServiceBroker::FormHostList(): non-numeric cap, using default cap for host "<<*iii<<endm;
#endif
		cap = DefaultCap;
	      }

	      ChapiDbHost host_entry = ChapiDbHost(host_data[NAME],port,power,cap);
	      MyHostList.push_back(host_entry);
	    }
	  cut_string_after_sub(key,">");
	  cut_string_after_sub(key,"(");
	}

#ifdef DEBUG
  PrintHostList();
#endif

  if (MyHostList.size()==0)
    {
      MyStatus = NO_HOSTS;
      LOG_DEBUG<<" StDbServiceBroker::RecommendHost() will have no hosts to choose from !"<<endm;
      return;
    }

}
//////////////////////////////////////////////////////
void StDbServiceBroker::PrintHostList()
{
  LOG_DEBUG << " MyHostList contains:"<<endm;
  for (vector<ChapiDbHost>::const_iterator i = MyHostList.begin(); i!=MyHostList.end(); ++i)
    {
      LOG_DEBUG << (*i).HostName << endm;
    }
}

namespace {

const std::string currentDateTime() {
    time_t     now = time(0);
    struct tm  tstruct;
    char       buf[80];
    tstruct = *localtime(&now);
    strftime(buf, sizeof(buf), "%Y-%m-%d.%X", &tstruct);
    return buf;
}

}

void StDbServiceBroker::SendEmail(time_t timediff) {

  std::string admin_emails;
  char* admins = getenv("STAR_DEBUG_DB_RETRIES_ADMINS");
  if (!admins) { return; }
  char* hostname = getenv("HOSTNAME");
  pid_t mypid = getpid();
  std::string host = "unknown host";
  if (hostname) { host = hostname; }
  admin_emails = admins;

  // check modification ts of the marker file ( email auto-suppression )
  struct stat attrib;
  int res = stat( "/tmp/db_network_error.txt", &attrib );

  // create or re-create marker file file
  std::ofstream marker_file( "/tmp/db_network_error.txt" );
  if ( marker_file.is_open() ) {
    marker_file << time(0);
    marker_file.close();
	chmod( "/tmp/db_network_error.txt", 0666 );
  }

  if ( res == 0 && ( ( time(0) - attrib.st_mtime ) < seconds_to_reach_for_connect ) ) {
    // if original file time was modified recently, do nothing
    return;
  }

  std::string curtime = currentDateTime();
  std::stringstream exec_command;
  exec_command << "echo \"We waited for " << timediff << " seconds (threshold: "<< seconds_to_reach_for_connect <<"), and did not get a db connection at " << host 
	<< " at " << curtime << ", process id = " << mypid << "\" | /bin/mail -s \"DB RETRIES - Problem detected on "<< host << " at " << curtime << "\" " << admin_emails;
  system(exec_command.str().c_str());

}

//////////////////////////////////////////////////////
int StDbServiceBroker::RecommendHost()
{
  double dproc_min = HUGE_VAL;

  // default initialization for vector iterator
  MyBestHost = MyHostList.end();

  if (MyHostList.size()==1)
    {
      MyBestHost = MyHostList.begin();
      return 0;
    }

  srand ( unsigned ( time (NULL) ) );                                                                                                                        
  random_shuffle( MyHostList.begin(), MyHostList.end() ); 

  int scanned_hosts = 0;
  bool host_found = false;
  for (vector<ChapiDbHost>::const_iterator I=MyHostList.begin(); I!=MyHostList.end(); ++I)
    {
      conn = mysql_init(0);

      if (conn==0)
        {
          LOG_WARN << "StDbServiceBroker::RecommendHost() mysql_init(0) failed "<<endm;
          continue;
        }

      conn->options.connect_timeout = 30;

      if (mysql_real_connect
	  (conn,((*I).HostName).c_str(), "loadbalancer","lbdb","test",(*I).Port,Socket,0)==NULL)
        {	
          LOG_WARN << "StDbServiceBroker::RecommendHost() mysql_real_connect "<< 
	    conn << " "<<((*I).HostName).c_str()<<" "<<(*I).Port <<" did not succeed"<<endm;
          mysql_close(conn);
		  time_t timediff = time(NULL) - last_succeeded_connect_time;
		  if ( timediff > seconds_to_reach_for_connect) { // default: 1800 
			last_succeeded_connect_time = time(NULL);
			SendEmail(timediff);
		  }
          continue;
        } else {
			last_succeeded_connect_time = time(NULL);
		}

      if (mysql_query(conn, "show status like \"%Threads_running\"") != 0 )
        {
          LOG_WARN <<"StDbServiceBroker::RecommendHost() show processlist failed"<<endm;
          continue;
        }

      unsigned int nproc = 0;


      MYSQL_RES* res_set = mysql_store_result(conn);
     if (res_set==0)
        {
          LOG_WARN << "StDbServiceBroker::RecommendHost(): mysql_store_result failed"<<endm; 
        }
     else
       {
	 MYSQL_ROW row = mysql_fetch_row(res_set);
	 if (row==0)
	   {
	     LOG_WARN << "StDbServiceBroker::RecommendHost(): mysql_fetch_row failed"<<endm;
	   }
	 else
	   {
	     if(!from_string<unsigned int>(nproc,row[1],std::dec))
	     {
	       LOG_WARN << "StDbServiceBroker::RecommendHost():  mysql_fetch_row returns non-numeric"<<endm;
	     }
	   }
	 mysql_free_result(res_set);
       }


      //      unsigned int nproc = mysql_num_rows(res_set);



      double dproc = nproc/(*I).Power;
#ifdef DEBUG
      LOG_DEBUG <<" Server "<<((*I).HostName).c_str()<< " actual "<< nproc << " effective "<< dproc <<" processes "<<endm;
#endif
      mysql_close(conn);


      if ( dproc<dproc_min && nproc<(*I).Cap ) {
          	dproc_min = dproc;
	  		MyBestHost = I;
			host_found = true;
        }
	  	scanned_hosts += 1;

		if (scanned_hosts > 2 && host_found == true ) {
			LOG_INFO << "StDbLib: db server found in " << scanned_hosts << " iterations" << endm;
			return 0;
		}
    }

    if ( MyBestHost != MyHostList.end() ) {                                                                                                                  
		// found good host
        return 0;                                                                                                                                            
    } 
	// no hosts found..
	return 1;
}
////////////////////////////////////////////////////////////////
string StDbServiceBroker::GiveHostName()
{
  return (*MyBestHost).HostName;
}
////////////////////////////////////////////////////////////////
short StDbServiceBroker::GiveHostPort()
{
  return (*MyBestHost).Port;
}
////////////////////////////////////////////////////////////////

int StDbServiceBroker::updateLocalLbPolicy()
{
// MLK: this method will be most likely run as a static method through a root 
// call by a specially created user once a day.

  string writableDir;
  string dbLoadBalancerConfig;

  const char* loConfig = getenv("DB_SERVER_LOCAL_CONFIG");
  dbLoadBalancerConfig = loConfig ? loConfig : "";
  
  if (!loConfig)
    {
      LOG_ERROR << "StDbManagerImpl::updateLocalLbPolicy(): DB_SERVER_LOCAL_CONFIG is undefined! "<<endm;
      return lb_error::NO_LPD_ENV_VAR;
    }
  else
    {
      string::size_type last_slash = dbLoadBalancerConfig.find_last_of("/");
      writableDir = dbLoadBalancerConfig.substr(0,last_slash+1);


      struct stat dir_status;
      if (stat(writableDir.c_str(),&dir_status)==0)
	{
	  if (dir_status.st_mode & S_IWUSR)
	    {
	      // OK
	    }
	  else
	    {
	      LOG_ERROR << "StDbManagerImpl::lookUpServers() "<<writableDir<<" is not writable"<<endm;
	      return lb_error::NO_WRITE_PERMISSION;
	    }
	}
      else
	{
	  LOG_ERROR << "StDbManagerImpl::lookUpServers() invalid dir "<<writableDir <<endm;
	  return lb_error::NO_LPD_DIR;
	}

    }

  //////////////////////////////////////////////////////////	  
// do we need to fetch XML from the Web ?

  struct stat file_status;
  bool fetchWorldConfig = true;
  
  if (stat(dbLoadBalancerConfig.c_str(), &file_status) == 0) 
    {
	  /* A STAR site has an option to state that they do not want to use global XML. They do that by
	     1) creating in a specific, group-writeable directory's WorldConfig file an element <Site>
	     2) making sure a <Site> element with name "World" does NOT exist in that file
	     3) the file has to be error-free XML
	     These conditions can be fullfilled by e.g. having <Site name="MyLittleWorld" /> as the only 
	     Site element in that file. In that case, global XML from the Web will not be downloaded to that
	     group-writeable directory.
	  */
      
      StlXmlTree myLittleWorldCheck = StlXmlTree(dbLoadBalancerConfig.c_str());
#ifdef DEBUG
      LOG_INFO << "myLittleWorldCheck is:"<<endm;
      myLittleWorldCheck.ShowTree();
#endif
      if (myLittleWorldCheck.GetStatus()==stl_xml_tree::NO_ERROR)
	{
	  string key = StlXmlTree::MakeKey("","Scatalog");
	  
	  vector<string> versions = myLittleWorldCheck.LookUpValueByKey(key);
	  
	  string my_version = versions[0];
	  
	  vector<string> sites = myLittleWorldCheck.LookUpValueByKey(key,my_version,"Site");
	  
	  vector<string>::const_iterator I = sites.begin();
	  bool WorldNotFound = true;
	  if (sites.size()==0)
	    {
	      // protection is invalid, no Site element
	      WorldNotFound = false;
	    }
	  while( I!=sites.end() && WorldNotFound)
	    {
	      if (StlXmlTree::AttributesContain(*I,"name","World"))
		    {
		      WorldNotFound = false;
		    }
	      ++I;
	    }
	  
	  if (WorldNotFound)
	    {
#ifdef DEBUG
	      LOG_INFO <<"StDbManagerImpl::updateLocalLbPolicy()  protection against WWW XML is activated"<<endm;
#endif
	      fetchWorldConfig = false; 
	    }
	  else
	    {
#ifdef DEBUG
	      LOG_INFO<<"StDbManagerImpl::updateLocalLbPolicy()  we found World, the user wants to read from the Web"<<endm;
#endif
	    }
	  
	  if (fetchWorldConfig)
	    {
	      time_t glbModTime = file_status.st_mtime;
	      time_t nowTime = time(0);
	      if (nowTime-glbModTime<60) 
		{
		  fetchWorldConfig = false;
		}
	    }
	}
      else
	{
	  LOG_ERROR << "StDbManagerImpl::updateLocalLbPolicy() invalid XML file "<<dbLoadBalancerConfig <<endm;
	}
    }
  else
    {
      LOG_ERROR << "StDbManagerImpl::lookUpServers(): config file " << dbLoadBalancerConfig << " is invalid "<< endm;
    }
  
  if (fetchWorldConfig)
    {
      // try AFS first
      const char* glConfig = getenv("DB_SERVER_GLOBAL_CONFIG");
      const string dbLoadBalancerWorldAFS = glConfig ? glConfig : "";
      
      if (!glConfig)
	{
	  LOG_ERROR << "StDbManagerImpl::updateLocalLbPolicy(): DB_SERVER_GLOBAL_CONFIG is undefined! "<<endm;
	}
      else
	{
	  
	  struct stat file_status;
	  if (stat(dbLoadBalancerWorldAFS.c_str(), &file_status) == 0) 
	    {
	      system(("/bin/cp "+dbLoadBalancerConfig+" "+dbLoadBalancerConfig+".old").c_str());
	      system(("/bin/cp "+dbLoadBalancerWorldAFS+" "+dbLoadBalancerConfig).c_str());
	      system(("/bin/chmod u+w "+dbLoadBalancerConfig).c_str());
	      return lb_error::NO_ERROR;
	    }
	  else
	    {
	      LOG_ERROR << "StDbManagerImpl::updateLocalLbPolicy(): DB_SERVER_GLOBAL_CONFIG points to invalid file! "<<endm;
	    }

	}

      // try WWW
      const char* wwwConfig = getenv("DB_SERVER_GLOBAL_CONFIG_URL");
      const string dbLoadBalancerWorldURL = wwwConfig ? wwwConfig : "";

      if (!wwwConfig)
	{
	  LOG_ERROR << "StDbManagerImpl::updateLocalLbPolicy(): DB_SERVER_GLOBAL_CONFIG_URL is undefined! "<<endm;
	  return lb_error::NO_GPD_ENV_VAR;
	}

#ifdef DEBUG
      LOG_INFO <<"StDbManagerImpl::updateLocalLbPolicy() fetching world config "<<endm;
#endif
      system(("cp "+dbLoadBalancerConfig+" "+dbLoadBalancerConfig+".old").c_str());
      const char* proxy = getenv("http_proxy");
      if (proxy)
	{
	  xmlNanoHTTPScanProxy(proxy);
	}
      
      int ret = xmlNanoHTTPFetch(dbLoadBalancerWorldURL.c_str(), dbLoadBalancerConfig.c_str(), 0);

      if (ret!=0)
	{
	  LOG_ERROR << "StDbManagerImpl::updateLocalLbPolicy() xmlNanoHTTPFetch error "<<ret<<endm;
	  return lb_error::WWW_ERROR;
	}
      else
	{
	  system(("chmod u+w "+dbLoadBalancerConfig).c_str());
	  return lb_error::NO_ERROR;
	}
    }
return lb_error::NO_ERROR;

}  


#endif
