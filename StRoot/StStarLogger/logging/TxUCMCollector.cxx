/*****************************************************************
 * @file TxUCMCollector.cpp
 * @author Roopa Pundaleeka
 *
 * @(#)cpp/api:$Id: TxUCMCollector.cxx,v 1.26 2010/09/17 20:15:15 fine Exp $
 *
 * Please see TxUCMCollector.h for more documentation.
 * "Translated" from the original TxUCMCOllector.java version 
 */
#include "StStarLogger/StLoggerConfig.h"
#include "TxUCMCollector.h"
#include "StDbFieldI.h"
#include "FieldList.h"
#include "StDbFieldIIterator.h"
#include "StUcmTasks.h"
#include "StUcmJobs.h"
#include "StUcmEvents.h"

#include <stdlib.h>
#include <iostream>
#include <sstream>
#include <log4cxx/logger.h>
#include <log4cxx/consoleappender.h>
#include <log4cxx/patternlayout.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/helpers/stringhelper.h>
#include <stdio.h>
using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::spi;
using namespace std;

using namespace TxLogging;
using namespace StDbField;

#define TRY
#define CATCH(a)

    const char *TxUCMCollector::fgTs         = "ts";
    const char *TxUCMCollector::fgEvent      = "event";
    const char *TxUCMCollector::fgBJobID     = "broker.job.id";
    const char *TxUCMCollector::fgBTaskID    = "broker.task.id";
    const char *TxUCMCollector::fgRequester  = "requester.name";
    const char *TxUCMCollector::fgContext    = "context";
    const char *TxUCMCollector::fgLevel      = "level";
    const char *TxUCMCollector::fgStage      = "stage";
    const char *TxUCMCollector::fgKey        = "key";
    const char *TxUCMCollector::fgValue      = "value";

    const char *TxUCMCollector::fgNewTask      = "com.txcorp.ucm.newtask";
    const char *TxUCMCollector::fgUpdateTask   = "com.txcorp.ucm.updatetask";
    const char *TxUCMCollector::fgAddJob       = "com.txcorp.ucm.addjob";
    const char *TxUCMCollector::fgUpdateJob    = "com.txcorp.ucm.updatejob";
    const char *TxUCMCollector::fgSiteLocation = "com.txcorp.ucm.job.siteLocation";
    const char *TxUCMCollector::fgStateID      = "com.txcorp.ucm.job.stateID";
    const char *TxUCMCollector::fgGridJobID    = "com.txcorp.ucm.job.gridJobID";
    const char *TxUCMCollector::fgAppStart     = "com.txcorp.ucm.app.start";
    const char *TxUCMCollector::fgAppEnd       = "com.txcorp.ucm.app.end";

    const char *TxUCMCollector::fgStatusFile        = "txucmcollectorstatus.properties";
    const char *TxUCMCollector::fgStatusFileName    = "current.logfile.name";
    const char *TxUCMCollector::fgStatusFileModTime = "current.logfile.modtime";
    const char *TxUCMCollector::fgStatusFilePos     = "current.logfile.pos";

    
//_____________________________________________________________________________
namespace {
   string itoa(int i)
   {
      char buffer[100];
      sprintf(buffer,"%d",i);
      return string(buffer);
   }
}

//_________________________________________________________________________
MYSQL *TxUCMCollector::getConnection()
{
  const char *host   = "heston.star.bnl.gov";
  const char *user   = "StarLogger";
  const char *passwd = "logger";
  return getConnection(host,user,passwd);
}

//_________________________________________________________________________
MYSQL *TxUCMCollector::getConnection (const string&dbUrl,const string&dbUsername
      , const string&dbPassword)
{
   return getConnection (dbUrl.c_str(),dbUsername.c_str(),dbPassword.c_str());
}
 
//_________________________________________________________________________
MYSQL *TxUCMCollector::getConnection (const char *cdbUrl,const char *cdbUsername
      , const char *cdbPassword)
{
   if (!fIsConnectionOpen) {
     if ( !(connection= mysql_init(connection)) ) {
         log->error("MYSQL:  ---- > No init connection");
     } else {    
         const char *host   = cdbUrl;
         const char *user   = cdbUsername;
         const char *passwd = cdbPassword;
         const char *db     = dbName.c_str();
         unsigned int port  = 3306;
         // fprintf(stderr,"TxUCMCollector::getConnection:  ---- >  Establishing MySQL connection open %d \n", fIsConnectionOpen);
         if (!(mysql_real_connect(connection
                     , host
                     , user
                     , passwd
                     , db
                     , port
                     , 0,0
                     )))
         {
            string error = __FUNCTION__ 
                          + string("host: ") + host
                          + string("; user: ") + user
                          + string(" passwd: ") + passwd
                          + string(" db: ") + db
                          + string(" port:") + itoa(port)
                          + " error: " + mysql_error(connection);
             log->debug(error.c_str());
             connection = 0;
             fIsConnectionOpen = false;
         } else {
            string error = "Ok connection to Db : "  
                          + string("host: <") + host
                          + string("> user: <") + user
                          + string("> passwd: <") + passwd
                          + string("> db: <") + db
                          + string("> port: ") + itoa(port);
            log->debug(error.c_str());
            fIsConnectionOpen = true;
         }
      }
   }
	return connection;
}

//_________________________________________________________________________
unsigned int TxUCMCollector::execute(const string &sql)
{
   return execute(sql.c_str());
}

//_________________________________________________________________________
unsigned int  TxUCMCollector::execute(const char *sql)
{
   unsigned int ret=1;
   if (getConnection()) {
      String query = sql;
      log->debug(string("TxUCMCollector::execute ") + sql);
      if (fResult) {
         mysql_free_result(fResult); 
         fResult = 0;
      }
      if (( ret = mysql_query(connection,query.c_str()) )) {
         log->error(std::string("MYSQL QUERY:") + mysql_error(connection));
      } else {
         fResult = mysql_store_result(connection); 
      }
    }
    return ret;
}
//_________________________________________________________________________
/* The default behavior holds a single connection open until the appender is closed (typically when garbage collected). */
void TxUCMCollector::closeConnection()
{
  if (fIsConnectionOpen) {
     if (fResult) {
         mysql_free_result(fResult); 
         fResult = 0;
     }
     mysql_close(connection); 
     if (mysql_errno(connection))   fprintf(stderr,"MYSQL close ERROR %s  \n",mysql_error(connection));
     connection = 0;
     fIsConnectionOpen = false;
  }
}

//______________________________________________________________________
TxUCMCollector::TxUCMCollector ()
: connection()
  ,fResult(),fField(),fRow()   
  ,fIsConnectionOpen(false), sleepTime(10),currLogFilePos(0)
  ,fBrokerJobID(-1), fDbJobID(-1)

{
   // init the logger
   log =  Logger::getLogger(_T("TxUCMCollector")); 
   // check for appender 
   AppenderList apps = log->getAllAppenders();
   if (!apps.size()) {
      // make one 
      ConsoleAppenderPtr appender = new ConsoleAppender(
            new PatternLayout("TxUCMCollector: %-3c{2}:%-5p - %m%n"));
       appender->setName(_T("TxUCMCollectorAppender"));
       log->addAppender(appender);
   }
   // log->setLevel(Level::DEBUG);
}

TxUCMCollector::~TxUCMCollector ()
{
    closeConnection();
}

 /**
  * Tests if this string ends with the specified suffix.
  */
boolean endsWith (std::string str, const char *suffix)
{
    return StringHelper::endsWith(str, _T(suffix));
}

 /**
  * Returns a copy of the string, with leading and trailing whitespace omitted.
  */
static string  trim (std::string str)
{
   return StringHelper::trim(str);
}

 /**
  * Splits this string around matches of the given regular expression. 
  * @str std::string input strig to be splited
  * @sep std::string the string separator to split the input.
  *                   the separator is excluded from any ourpur string  
  * @return vector<std::string" with all found components
  */
static vector<std::string> split(const std::string &str, const std::string &sep)
{
   vector<std::string> splits;
   size_t posOld = 0; 
   size_t posNew = 0; 
   while ((posNew = str.find(sep,posOld)) != string::npos)
   {
      splits.push_back(str.substr(posOld,posNew-posOld));
      posOld=posNew+sep.size(); 
   }
   // add the tail
   if (posOld < str.size()) splits.push_back(str.substr(posOld,string::npos));

   return splits;
}
//________________________________________________
boolean TxUCMCollector::initDb ()  {
   boolean success = false;
   TRY
   {
           log->debug (" TxUCMCollector::initDb  . . . "); 
           success = this->init ();
           success = success & this->loadDatabase ();
    }
//     CATCH (Exception e) {
//        log->error ("Failed to initialize and/or load database");
//        log->error (e.getMessage());
//     }

    // if any failures, exit 
    if (!success) {
           log->error("Failed to initialize, review log file");
           TRY {
              exit (1);
           }
//            CATCH (SecurityException se) {
//               log->error ("Problem, exiting");
//            }
    }
    return success;
}

    /**
     * Read from properties file and get the necessary info
     *
     * @return true if successful, false, if failed
     */
//________________________________________________
boolean TxUCMCollector::init ()
//            throws IOException, Exception 
{
   boolean success = false;
#if 0
   Properties properties = new Properties();
   TRY {
           properties.load (new FileInputStream ("txucmcollector.properties"));
           
           // Read db info from properties file
           dbName = properties.getProperty ("db.name");
           dbUrl = "jdbc:mysql://" + properties.getProperty ("db.host") + ":"
              + properties.getProperty ("db.port") + "/" + dbName;
           dbUsername = properties.getProperty ("db.username");
           dbPassword = properties.getProperty ("db.password");

           // Read log file directory and sleep time as set by user
           // from properties file
           logsDir = properties.getProperty ("logs.dir");
           sleepTime = (new Long (properties.getProperty ("sleep.time.sec"))).longValue ();

           success = true;
        } 
//       CATCH (IOException ioe) 
//        {
//             log->error (ioe.getMessage ());
//             throw ioe;
//        } 
//       CATCH (Exception e) 
//        {
//             log->error (e.getMessage ());
//             throw e;
//        }
#else
           dbName = "logger";
           dbUrl  = "heston.star.bnl.gov";
           dbUsername = "StarLogger";
           dbPassword = "logger";
#endif
       return success=true;
    }
    

    /** 
     * Load the MySQL driver and create the class scope DB connection
     *
     * @return true if successful, false, if failed
     */
//________________________________________________
boolean TxUCMCollector::loadDatabase() 
//       throws ClassNotFoundException, IllegalAccessException,
//              SQLException, InstantiationException 
{
    boolean success = false;

    TRY {
        log->debug ("dbName: " + dbName);
        log->debug ("dbUrl: " + dbUrl);

        connection = getConnection (dbUrl.c_str(), dbUsername.c_str(), dbPassword.c_str());
        log->debug ("Successfully loaded database: dbName = " + dbName); 

        success = true;
     }
//        CATCH (ClassNotFoundException cnfe)
//        {
//            log->error("Mysql driver class not found");
//            throw cnfe;
//        }        CATCH (InstantiationException ie) 
//        {
//           
//            log->error("Failed to instantiate MySQL driver");
//            throw ie;
//        } CATCH (IllegalAccessException iae) {
//            log->error("Failed to get access to MySQL driver");
//            throw iae;
//         } CATCH (SQLException se) {
//            log->error("Failed to load driver, or get DB connection");
//            throw se;
//         }
       
       return success;
    }

    /**
     * startProcess: reads the todo list and starts processing the log
     * file
     *
     */
//________________________________________________
void TxUCMCollector::startProcess () {
   TRY {
#ifdef FUTURE
      while (true) {
            // set new file to be processed
         setCurrLogFile ();

              // Start processing the oldest log file first
         processLogFile ();
              // processing this file is done, now check to see if it is
              // older than 24 hours. If so, rename to *.log->archive, so it
              // wont show up in the processing again. 
         File file = new File (currLogFile);
         if ((Calendar.getInstance ().getTimeInMillis () - file.lastModified ())
                  >= (24 * 60 * 60 * 1000)) {
                  file.renameTo (new File (currLogFile + ".archive"));
              }
              // the file that was just processed is not over a day
              // old. So, store its name, the last modified time and
              // the current position in a properties file.
         else {
             Properties properties = new Properties();
             TRY {
                     properties.setProperty (statusFileName, currLogFile);
                     properties.setProperty (statusFileModTime, 
                                          new Long (file.lastModified ()).toString ());
                     properties.setProperty (statusFilePos, 
                                          new Long (currLogFilePos).toString ());
                     properties.store (new FileOutputStream (statusFile), null);

                     // reset position
                     currLogFilePos = 0;
                  } CATCH (IOException ioe) {
                     log->error ("Failed to write current file info to <" + 
                               statusFile + ">");
                     log->error (ioe.getMessage ());
                  } 
              }
           }
#endif
   }
//       CATCH (Exception e) {
//            log->error ("Failed while processing log file <" + currLogFile + ">");
//            log->error (e.getMessage ());
//            System.exit (2);
//        }
}
    

    /**
     * setCurrLogFile: 
     *
     *                 Check if the status properties file exists, and
     *                 see if the last modified time as set in the
     *                 status file has changed. If so, process this
     *                 file from the point where it was left off.
     *
     *                 If there is no status file, then list all the
     *                 files that end with .log in the logs dir and
     *                 set the oldest file as the current file to
     *                 process.
     *                 
     * @return true if successful, false, if failed
     */
//________________________________________________
void TxUCMCollector::setCurrLogFile () {
   TRY {
#ifdef FUTURE

           //
           // Read the status properties file, if it exists, to get
           // the last file that was not processed completely or was
           // not archived
           //
           if (new File (statusFile).exists ()) {
              Properties properties = new Properties();
              properties.load (new FileInputStream (statusFile));
              
              currLogFile      = properties.getProperty (statusFileName);
              if (currLogFile != null) {
                  const char * strTime = properties.getProperty (statusFileModTime);
                  long longTime = (new Long (strTime)).longValue ();
                  currLogFilePos   = 
                     (new Long (properties.getProperty (statusFilePos))).longValue ();
                  
                  // Sleep if the file being processed has not changed
                  if (new File (currLogFile).exists ()) {
                     while (true) {
                         if (new File (currLogFile).lastModified () == longTime) {
                            System.out.println ("Waiting for " + currLogFile + " to be updated...");
                            log->info ("Waiting for " + currLogFile + " to be updated...");
                            Thread.sleep (sleepTime * 1000);
                         }
                         else {
                            break;
                         }
                     }
                  }
                  // Seems like the status file is outdated. Delete it
                  else {
                     new File (statusFile).delete ();
                  }
              }
           }

           // 
           // Look at the logs dir for new files
           //
           currLogFile = "";
           currLogFilePos = 0;
           File logFileDir = new File (logsDir);
           
           while (true) {
              FilenameFilter filter = new FilenameFilter() {
                      boolean accept (File file, const char * name) {
                         return name.endsWith (".log");
                     }
                  };
              
              File[] logFiles = logFileDir.listFiles (filter);

              System.out.println ("Found " + logFiles.length + " log files in " + logsDir);
              log->info ("Found " + logFiles.length + " log files in " + logsDir);
              
              // If there are log files available to be processed,
              // find the oldest one and continue processing it.
              if (logFiles.length > 0) {
                  File file = logFiles [0];
                  for (int i = 1; i < logFiles.length; i++) {
                     if (file.lastModified () > logFiles [i].lastModified ()) {
                         file = logFiles [i];
                     }
                  }
                  
                  // oldest .log file in the logs dir is set to be processed
                  currLogFile = file.toString ();
                  break;
              }
              // If there are no log files available in the
              // directory, sleep for a while and check again
              else {
                  System.out.println ("Waiting for new log files in " + logsDir);
                  log->info ("Waiting for new log files in " + logsDir);
                  Thread.sleep (sleepTime * 1000);
              }
           }
#endif
     }
//        CATCH (IOException ioe) {
//            log->error (ioe.getMessage ());
//        } 
//        CATCH (Exception e) {
//             log->error (e.getMessage ());
//        }
}

    /**
     * startProcess: reads the designated log file and creates data
     * base entries
     *
     */
//________________________________________________
void TxUCMCollector::processLogFile () {
   TRY {
#ifdef FUTURE
           System.out.println ("processing " + currLogFile);

           BufferedReader in = new BufferedReader (new FileReader (currLogFile));
           const char * str;
           
           // Skip to the next message that needs to be processed
           for (int i = 0; i < currLogFilePos; i++) {
              in.readLine ();
           }

           while ((str = in.readLine()) != null) {
              processMessage (str);
              currLogFilePos ++;
           }
           in.close();
#endif
   }
//        CATCH (IOException e) {
//            log->error ("Failed while processing log file <" + currLogFile + ">");
//            log->error (e.getMessage ());
//            System.exit (2);
//        }
}


    /**
     * Process the message and create a hash map with key value pairs
     * Then process the hash map to create/update/insert tables
     *
     * @param const char * message to process
     * @return boolean true if successful, false otherwise
     */
//________________________________________________
void TxUCMCollector::processMessage (const string &msg) 
{  processMessage(msg.c_str());                     }

//________________________________________________
void TxUCMCollector::processMessage (const char * msg) {
   //
   // Assumption: Header and message are separated by ":"
   //
   // If message has a colon, make sure it is at the end of the
   // header and not a part of the message itself.
   //
   msgHashMap.clear(); 
   vector<std::string> keysNVals;
   std::string message = msg;
   size_t hdrDelimIndex = message.find(':');
   if (hdrDelimIndex != string::npos && hdrDelimIndex < message.find("=\"")) {
      keysNVals = split(message.substr(hdrDelimIndex + 1),"\" ");
   } else {
   //
   // Taking care of  where there is no header at all
   //
      keysNVals = split(message,"\" ");
   }
   //const char *[] keysNVals = (message.split (":") [1]).split ("\" ");
   log->debug(_T("TxUCMCollector::processMessage: ")+ message);
   for (size_t i = 0; i < keysNVals.size(); i++) {

       // get the key value pairs separated by ="
       vector<std::string> keyNVal = split(keysNVals [i],"=\"");

       // take care of the case where the value is null
       std::string value = (keyNVal.size() == 2) 
              ? trim(keyNVal [1])
              : " ";

       // remove trailing double quotes if any
       value = endsWith(value,"\"") 
                   ? value.substr (0, value.size() - 1)
                   : value;
       // add it to the message hash map
       msgHashMap.insert(pair<std::string,std::string>(trim(keyNVal [0]), value));
       log->debug(string("next pair: ") + trim(keyNVal [0]) + "<" + value + ">");
    }
    // 
    // Check for special messages:
    //
    std::string keyVal = msgHashMap[string(fgKey)];
    if (keyVal.empty()) {
       char buffer[20];
       sprintf(buffer,"%d",(int)keysNVals.size());
       log->error (string("Wrong message format: \"")
                   + message
                   + "\" par:"
                   + buffer
                   + "does not contains any <" 
                   + fgKey 
                   + ">");

       return;
    }

    //
    // newTask      = "com.txcorp.ucm.newtask";
    //
    if (keyVal == fgNewTask )        this->createNewTask ();
       //
       // updateTask   = "com.txcorp.ucm.updatetask";
       //
    else if (keyVal == fgUpdateTask)  this->updateTask ();
       //
       // addJob       = "com.txcorp.ucm.addjob";
       //
    else if (keyVal == fgAddJob)      this->addJob ();
       //
       // updateJob    = "com.txcorp.ucm.updatejob";
       //
    else if (keyVal == fgUpdateJob)   this->updateJob ();
       //
       // siteLocation = "com.txcorp.ucm.job.siteLocation";
       //
    else if (keyVal == fgSiteLocation) this->setJobsField ("siteLocation");
       //
       // stateID      = "com.txcorp.ucm.job.stateID";
       //
    else if (keyVal == fgStateID)      this->setJobsField ("stateID");
       //
       // gridJobID    = "com.txcorp.ucm.job.gridJobID";
       //
    else if (keyVal == fgGridJobID)    this->setJobsField ("gridJobID");
       //
       // appStart     = "com.txcorp.ucm.app.start";
       // appEnd       = "com.txcorp.ucm.app.end";
       // and all others just go into the associated events table
       //
    else                               this->addEvent ();
}


    /**
     * Create a new record in Tasks table: the values are in the value
     * field of the log message
     *
     */
//________________________________________________
     void TxUCMCollector::createNewTask () {
      string newTaskKeys = "brokerTaskID, requesterID";
      string newTaskVals = "'" + msgHashMap[fgBTaskID] + "'" +
           ", '" + msgHashMap[fgRequester] + "'";

       // Insert new record only if it does not exist
       if (!this->recordExists (string("brokerTaskID = \"") +  msgHashMap[fgBTaskID] + "\"",
                            "Tasks")) {
           vector <string> newTask = split(msgHashMap[fgValue],"', ");
           for (size_t i = 0; i < newTask.size(); i++) {
              vector <string> taskKeyNVal = split(newTask [i],"='");
              if (taskKeyNVal.size() == 2) {
                  newTaskKeys += ", " + trim(taskKeyNVal [0]);

                  newTaskVals += ", '";
                  newTaskVals += endsWith (trim(taskKeyNVal [1]),"'") 
                     ? taskKeyNVal [1].substr (0, taskKeyNVal[1].size()-1)
                     : trim(taskKeyNVal [1]);
                  newTaskVals += "'";
              }
           }
           insertRecord (string("(") + newTaskKeys + ") VALUES (" + newTaskVals + ")", 
                       "Tasks");
           
           this->createJobsTable ();
           this->createEventsTable ();
       }
       else {
           log->debug (string("Record with brokerTaskID = ") + msgHashMap[fgBTaskID] +
                    " already exists");
           
       }
    }
    /**
     * Update a record in Tasks table: the values are in the value
     * field of the log message
     *
     */
//________________________________________________
     void TxUCMCollector::updateTask () {
       // Update only if a record exists. If it does not exist, just
       // create a new task entry
       if (this->recordExists (string("brokerTaskID = \"") + msgHashMap[fgBTaskID] + "\"",
                            "Tasks")) {
           updateRecord (msgHashMap[fgValue],
                       "Tasks",
                       string("brokerTaskID = '") + msgHashMap[fgBTaskID] + "'");
       }
       else {
           log->error (string("Record with brokerTaskID = ") + msgHashMap[fgBTaskID] +
                     " does not exist, so creating a new record instead of updating");
           this->createNewTask ();
           // Try one more time
           if (this->recordExists (string("brokerTaskID = \"") + msgHashMap[fgBTaskID] + "\"",
                            "Tasks")) {
              updateRecord (msgHashMap[fgValue],
                       "Tasks",
                       string("brokerTaskID = '") + msgHashMap[fgBTaskID] + "'");
          }
       }
    }

    /**
     * Add a new record to the Jobs table: the values are in the value
     * field of the log message Need to make sure that the Jobs table
     * exists, and also there is an associated task table entry
     *
     */
//________________________________________________
void TxUCMCollector::addJob () {
   // Check if a task table entry exists corresponding to this
   // job.  If not, create one for this new job. Also create new
   // jobs and events tables
   if (!this->recordExists (string("brokerTaskID = \"") + msgHashMap[fgBTaskID] + "\"",
                            "Tasks")) {
      log->info (msgHashMap[fgBTaskID] 
                    + " does not exist in Tasks table");
      insertRecord (string("(brokerTaskID, requesterID) VALUES ") +
                       "('" + msgHashMap[fgBTaskID] + "', " +
                       "'" + msgHashMap[fgRequester] + "')",
                       "Tasks");
   }
   // Create new tables
   this->createJobsTable ();
   this->createEventsTable ();

   // Insert new record only if it does not exist
   if (!this->recordExists (string("brokerJobID = \"") + msgHashMap[fgBJobID] + "\"",
                             jobTableName())) {

       std::string newJobKeys = "taskID, brokerJobID";
       std::string newJobVals = string("(SELECT taskID FROM Tasks WHERE brokerTaskID=") +
              "'" + msgHashMap[fgBTaskID] + "')" +
              ", '" + msgHashMap[fgBJobID] + "'";
           
       vector<std::string> newJob = split(msgHashMap[fgValue],"', ");

       for (size_t i = 0; i < newJob.size(); i++) {
           vector<std::string>  jobKeyNVal = split(newJob [i],"='");
           if (jobKeyNVal.size() == 2) {
               newJobKeys += ", " + trim(jobKeyNVal [0]);

               newJobVals += ", '";
               newJobVals += endsWith(trim(jobKeyNVal [1]),"'") 
                     ? jobKeyNVal [1].substr (0, jobKeyNVal.size() - 1)
                     : trim(jobKeyNVal [1]);
               newJobVals += "'";
           }
        }

        insertRecord (string("(") + newJobKeys + ") VALUES (" + newJobVals + ")", 
                       jobTableName());
    } else {
       log->debug ("Record with brokerJobID = " + msgHashMap[fgBJobID] +
                    " already exists");
    }
} 

//________________________________________________
string TxUCMCollector::tableNamePrefix(const char *prefix) const
{
   string fullTableName = 
            string(prefix)
          + string("_") + msgHashMap.find(fgRequester)->second
          + string("_") + msgHashMap.find(fgBTaskID)->second; 
    ((TxUCMCollector*) this)->log->debug(string(__FUNCTION__)+ "<" + fullTableName + ">");
    return fullTableName;
}

//________________________________________________
string TxUCMCollector::jobTableName() const 
{
    return  tableNamePrefix("Jobs");
}
//________________________________________________
std::string TxUCMCollector::eventTableName() const
{
   return  tableNamePrefix("Events");
}

    /**
     * Update a record in a Jobs table: the values are in the value
     * field of the log message
     *
     */
//________________________________________________
void TxUCMCollector::updateJob () {
  // Update only if a record exists. If it does not exist, just
  // create a new entry
  if (this->recordExists (string("brokerJobID = \"") + msgHashMap[fgBJobID] + "\"",
                            jobTableName() ) )  {
      updateRecord ( msgHashMap[fgValue], jobTableName(),
                       "brokerJobID = '" + msgHashMap[fgBJobID] + "'");
  } else {
      log->debug ("Record with brokerJobID = " +  msgHashMap[fgBJobID] +
                     " does not exist, so creating a new record instead of updating");
      this->addJob ();
  }
}

    /**
     * Set the site location of the Job if a record exists
     *
     */
void TxUCMCollector::setJobsField (const string &fieldName) {
   setJobsField (fieldName.c_str());
}

void TxUCMCollector::setJobsField (const char * fieldName) {
   if (this->recordExists (string("brokerJobID = \"") + msgHashMap[fgBJobID] + "\"",
                            jobTableName() )) {
      updateRecord (string(fieldName) + " = '" +  msgHashMap[fgValue] + "'", 
                       jobTableName() , 
                       "brokerJobID = '" + msgHashMap[fgBJobID] + "'");
   } else {
      log->error (string("Record with brokerJobID = ") + msgHashMap[fgBJobID] +
                     " does not exist, so creating a new record instead of updating");
  }
}

    /**
     * Add a new record to the Events table. Need to make sure that
     * the Events table exists, and also there are associated jobs and
     * task table entries
     *
     */
//________________________________________________
void TxUCMCollector::addEvent () {
   // Check if a task table entry exists corresponding to this
   // event/job.  If not, create one for this new job. Also create new
   // jobs and events tables
   if (!this->recordExists (string("brokerTaskID = \"") + msgHashMap[fgBTaskID] + "\"",
                            "Tasks")) {
      log->info (msgHashMap[fgBTaskID]
                    + " does not exist in Tasks table");
      insertRecord (string("(brokerTaskID, requesterID) VALUES ") +
                       "('" + msgHashMap[fgBTaskID] + "', " +
                       "'" + msgHashMap[fgRequester] + "')",
                       "Tasks");
   }

   // Create new tables
   this->createJobsTable ();
   this->createEventsTable ();

        // Check if a jobs table entry exists corresponding to this
        // event. If not, create one for this new job. Also create new
        // jobs and events tables if they dont exist already
   if (!this->recordExists (string("brokerJobID = \"") + msgHashMap[fgBJobID] + "\"",
                            jobTableName())) {

      log->info (msgHashMap[fgBTaskID] 
                    + " does not exist in Jobs table");

      std::string newJobKeys = "taskID, brokerJobID";
      std::string newJobVals = string("(SELECT taskID FROM Tasks WHERE brokerTaskID=")
             + "'"   + msgHashMap[fgBTaskID] + "')" 
             + ", '" + msgHashMap[fgBJobID] + "'";

      insertRecord (string("(taskID, brokerJobID) VALUES ") +
                       "((SELECT taskID FROM Tasks WHERE brokerTaskID=" +
                       "'" + msgHashMap[fgBTaskID] + "')" +
                       ", '" + msgHashMap[fgBJobID] + "')",
                       jobTableName());
   }

   // Insert the new events table record

   std::string newEventKeys = "jobID, levelID, context, time, stageID, messageKey, messageValue";
   std::string newEventVals = string("(SELECT jobID FROM `") + jobTableName() + "` WHERE brokerJobID=" +
           "'" + msgHashMap[fgBJobID] + "')" +
           ", '" + msgHashMap[fgLevel] + "'" +
           ", '" + msgHashMap[fgContext] + "'" +
           ", '" + msgHashMap[fgTs] + "'" +
           ", '" + msgHashMap[fgStage] + "'" +
           ", '" + msgHashMap[fgKey] + "'" +
           ", '" + msgHashMap[fgValue] + "'";
                  
   insertRecord (string("(") + newEventKeys + ") VALUES (" + newEventVals + ")", 
                    eventTableName());
   static string  FAILED_JOB_ATTRIBUTE; 
   if  ( FAILED_JOB_ATTRIBUTE.empty() ) {
      stringstream oss(FAILED_JOB_ATTRIBUTE);
      oss <<  TxEventLog::FAILED;
   }
   static string  DONE_JOB_ATTRIBUTE;
   if  ( DONE_JOB_ATTRIBUTE.empty() ) {
      stringstream oss(DONE_JOB_ATTRIBUTE);
      oss <<  TxEventLog::DONE;
   }
   if (msgHashMap[fgStage] == FAILED_JOB_ATTRIBUTE  || msgHashMap[fgStage] == DONE_JOB_ATTRIBUTE ) {
       updateRecord ("taskRemainSize=taskRemainSize-1"
                     , "Task"
                     , string("brokerTaskID=") + "'" + msgHashMap[fgBTaskID] + "'" );
   }
}

 /**
   * Create Jobs table
   */
//________________________________________________
void TxUCMCollector::createJobsTable () {
       // create new jobs table if it does not exist
    std::string tableName = "`" + jobTableName() + "` ";
    this->createTable (tableName , std::string("jobspattern"));
//    this->createTable (tableName + fgJobsTableCols);
}

    /**
     * Create Events table
     */
//________________________________________________
void TxUCMCollector::createEventsTable () {
       // create new events table if it does not exist
   std::string tableName = "`" + eventTableName() + "` ";
   this->createTable (tableName, std::string("eventspattern"));
//   this->createTable (tableName + fgEventsTableCols);
}

    /**
     * insertRecord: Execute SQL query to INSERT a record into a
     *               given table
     *
     * @param const char * insertStr
     * @param const char * tableName
     */
void TxUCMCollector::insertRecord (const string &insertStr, const string &tableName) 
{
     insertRecord(insertStr.c_str(),tableName.c_str());
}

void TxUCMCollector::insertRecord (const char * insertStr, const char * tableName) {
       TRY{
//           Statement stmt = connection->createStatement();
       if (!execute(string("INSERT INTO `") + tableName + "` " + insertStr)) 
          log->debug (string("Created new record for ") + tableName + ": " + insertStr);
       else 
          log->error (string(mysql_error(connection)) + " the new record for " + tableName + ": " + insertStr);
       closeConnection();
       }
//        CATCH(SQLException se) {
//           log->error (se.getMessage ());
//        }
}

    /**
     * updateRecord: Execute SQL query to UPDATE a table record of a
     *               given table
     *
     * @param const char * updateString
     * @param const char * tableName
     * @param const char * condition to select unique record
     */
//________________________________________________
void TxUCMCollector::updateRecord (const string&updateStr, const string&tableName, const string&condition)
{
   updateRecord (updateStr.c_str(), tableName.c_str(), condition.c_str());
}
//________________________________________________
void TxUCMCollector::updateRecord (const char * updateStr, const char * tableName, const char * condition)
{
   TRY{
          // Statement stmt = connection->createStatement();
      if (!execute(string("UPDATE `")   + tableName 
                           +   "` SET " + updateStr
                           + " WHERE " + condition))
         log->debug(string("Updated new record for ") + tableName + " with values: " + updateStr);
         closeConnection();
      }
//        CATCH(SQLException se) {
//            log->error (se.getMessage ());
//        }
    }

    /**
     * recordExists: Check if a record exists in a given table
     *
     * @param const char * selectStr condition
     * @param const char * tableName
     */
//________________________________________________
boolean TxUCMCollector::recordExists (const string&selectStr, const string&tableName)
{
   return 
         recordExists (selectStr.c_str(),tableName.c_str());
}

//________________________________________________
boolean TxUCMCollector::recordExists (const char * selectStr, const char * tableName) {
   unsigned long nRows = 0;
   TRY
   {
      execute(string("SELECT * FROM `") + tableName 
                                    + "` WHERE " + selectStr);
      if (fResult) {
         nRows = mysql_num_rows(fResult);
      }
   }
   closeConnection();
//        CATCH(SQLException se) {
//            log->error (se.getMessage ());
//            exists = false;
//        }

   return nRows ? true: false;
}

//___________________________________________________________________________________
 /**
     * createTable: Execute the SQL query to CREATE a table if it does
     *              not already exist
     *
     * @param Table info
     */
void TxUCMCollector::createTable (const string&table,const string&like)
{
   const char *likestr = like.empty() ? 0 : like.c_str();
   createTable (table.c_str(), likestr);
}

//___________________________________________________________________________________
void TxUCMCollector::createTable (const char * table, const char *like) {
   TRY{
       std::string query = string("CREATE TABLE IF NOT EXISTS ") + table;
       if (like && like[0]) query += std::string(" LIKE `") + like + "`"; 
       if (!execute(query)) 
          log->debug (string("Created new table: ") + table);
   }
   closeConnection();
//        CATCH(SQLException se) {
//            log->info (se.getMessage ());
//        }
}

    /**
     * print usage
     * @param options the encapsulated list of options
     *    
     */  
//___________________________________________________________________________________
void TxUCMCollector::usage (Options options)
{
#ifdef FUTURE
    // Use the built-in formatter class
     HelpFormatter formatter = new HelpFormatter ();
      formatter.printHelp ("Tx UCM Collector", options);
#endif
}

//___________________________________________________________________________________
StUcmTasks *TxUCMCollector::getTaskList(int limit, int offset)
{
   static  StUcmTasks tasks;
   fillTaskList(tasks,limit,offset);
   return &tasks;
}

//___________________________________________________________________________________
StUcmJobs  *TxUCMCollector::getJobList(StRecord *task, int limit, int offset)
{
   static StUcmJobs jobs;
   fillJobList(jobs,limit,offset,task);
   return &jobs;
}
//___________________________________________________________________________________
int  TxUCMCollector::getJobId(const char *reqName, const char *taskBrokerID, int brokerJobID)
{
   int id = -1;
   setRequesterName(reqName);
   setBrokerTaskID (taskBrokerID);
   setBrokerJobID (brokerJobID);
   try { 
       string where = string(" brokerJobID='") + itoa(brokerJobID) + "' ";
       queryTable(jobTableName().c_str(),0,0,where.c_str());
       if (fResult) {
          int nRows = mysql_num_rows(fResult) ;
          if (nRows<=0 || nRows >1 ) {
              log->error(string("Can not fetch the job id for the ") + taskBrokerID + " broker id=" + itoa(brokerJobID) + " nrow=" + itoa(nRows)); 
          } else {
               StRecord *job = new StRecord;     
               fillFields(job->getFields());
               id = job->getField("jobID")->toInt();
          }
       }
   } catch (const StDataException &e) {
       log->error(e.getDescription() );
   }

   setDbJobID(id);
   return id;
}

//___________________________________________________________________________________
StUcmEvents  *TxUCMCollector::getEventList(StRecord *job,int limit, int offset)
{
   static StUcmEvents events;
   fillEventList(events,limit,offset);
   return &events;
}
  
//___________________________________________________________________________________
int  TxUCMCollector::fillTaskList(StUcmTasks &tasks, int limit, int offset) 
{
   RecordList &l = tasks.getTasks();
   return  fillUcmList("Tasks",l,limit,offset);
}

//___________________________________________________________________________________
int TxUCMCollector::fillUcmList(const char *type, RecordList &records, int limit, int offset) 
{
    my_ulonglong nRows = 0;
    records.Clear();
    try {
        if (string(type) == "Tasks")  {
	        queryTaskTable(limit,offset);
        } else if (string(type) == "Jobs" ) {
	        queryJobTable(limit,offset);
        } else if (string(type) == "Events" ) {
           queryEventTable(limit,offset);
	    } else {
           queryTable(type,limit,offset);
       }
	    if (fResult) {
            nRows = mysql_num_rows(fResult) ;
            log->debug(string(itoa(nRows)) + " rows from " + itoa(offset) + " row" );
            for (my_ulonglong i=0; i<nRows;++i) 
            {
                StRecord *task = new StRecord;     
                fillFields(task->getFields());
                records.push_back(task);
            }
	    }
    } catch (const StDataException &e) {
       log->error(e.getDescription() );
    }
    return nRows;
}

//___________________________________________________________________________________
int TxUCMCollector::fillJobList(StUcmJobs &jobs, int limit, int offset)
{
   RecordList &l = jobs.getJobs();
   return fillUcmList("Jobs",l,limit,offset);
}

//___________________________________________________________________________________
void  TxUCMCollector::setBrokerTaskID(const StRecord *task)
{
   const char *requestId = 0;
   if (task) {
      requestId = task->getField("brokerTaskID")->getValueAsString();
      setBrokerTaskID(requestId);
   }
}

//___________________________________________________________________________________
void  TxUCMCollector::setBrokerJobID(const StRecord *job)
{
   if (job) { 
      int brokerJobID = job->getField("brokerJobID")->toInt();
      setBrokerJobID(brokerJobID);
      int jobID       = job->getField("jobID")->toInt(); 
      setDbJobID(jobID);
   }
}
//___________________________________________________________________________________
int TxUCMCollector::fillJobList(StUcmJobs &jobs, int limit, int offset,const StRecord *task)
{
   int row = 0;
   RecordList &l = jobs.getJobs();
   setBrokerTaskID(task);
   row = fillUcmList("Jobs",l,limit,offset);
//   if (!row) { //old style to be done yet if needed
//      requestId = task->getField("taskID")->getValueAsString();
//   }
   return row;
}


//___________________________________________________________________________________
int  TxUCMCollector::fillEventList(StUcmEvents &events, int limit, int offset,const StRecord *job)
{
   RecordList &l = events.getEvents();
   if (job) setBrokerJobID (job);
   return fillUcmList("Events",l,limit,offset);
}

//___________________________________________________________________________________
void TxUCMCollector::fillFields(FieldList &fields)
{
   fRow = mysql_fetch_row(fResult);
   if (fRow) {
      fField = mysql_fetch_fields(fResult);
      unsigned int n_fields = mysql_num_fields(fResult);
      log->debug(string("Fetching ") + itoa(n_fields) + " fields ");
      for (unsigned int i=0;i<n_fields; ++i)
      {         
         fields.push_back(createField(i));
      }
   }  else {
      log->error(mysql_error(connection));
   }
}
//___________________________________________________________________________________
StDbFieldI::EDataType TxUCMCollector::MapSqlField(enum_field_types type) 
{
 StDbFieldI::EDataType ucmType = StDbFieldI::kINVALID;
  switch (type) {
   case MYSQL_TYPE_TINY: case MYSQL_TYPE_SHORT:case MYSQL_TYPE_LONG:
        ucmType =StDbFieldI::kINT; 
        break;
   case MYSQL_TYPE_TIMESTAMP: case MYSQL_TYPE_DATE: case  MYSQL_TYPE_TIME:
        case MYSQL_TYPE_DATETIME:  case MYSQL_TYPE_YEAR:  
        ucmType = StDbFieldI::kUNIXTIME; 
        break;   
#if 0
   case ucmType =StDbFieldI::kLONG; break;
   case ucmType =StDbFieldI::kULONG; break;
   case ucmType =StDbFieldI::kDOUBLE; break;
#endif
   case MYSQL_TYPE_STRING: case MYSQL_TYPE_VAR_STRING:
        ucmType =StDbFieldI::kCHAR; break;
   // case ucmType =StDbFieldI::kSTRING; break;
   default:  break;
  }
  return ucmType;
}

//___________________________________________________________________________________
StDbFieldI *TxUCMCollector::createField(unsigned int fieldIndx)
{
   StDbFieldI *field = new StDbFieldI(fField[fieldIndx].org_name, fRow[fieldIndx],MapSqlField(fField[fieldIndx].type),1);
   return field;
}

     
    /**
     * print current version
     *
     */
    void TxUCMCollector::printVersion(){
       log->info("Tx UCM Collector, version 0.4");
    }


    /** 
     * main to get things going
     */ 
 void TxUCMCollector::main(const char *args[]) {
#ifdef FUTURE
       boolean debug = false;
       Option h = new Option ("h", "help", false, "print this message");
       Option v = new Option ("v", "version", false, "print the version information");
       Option d = new Option ("d", "debug", false, "set log level to debug");
       Option t = new Option ("t", "test", false, "run the setUpTest()");
       Option m = new Option ("m", "message", true, "parse one message and exit");

       // create options list       
       Options options = new Options ();
        options.addOption (h);
        options.addOption (v);
        options.addOption (d);
        options.addOption (t);
        options.addOption (m);

        CommandLineParser parser = new PosixParser();
        CommandLine cmd;
       const char * jobMessage=null;
        TRY{
           cmd = parser.parse (options, args);
        } 
       CATCH (ParseException pe) {
            System.out.println ("** Error ** : Check your input parametres:");
           usage (options); 
           return; 
       }

        if (cmd.hasOption ("m")) {
           //useString (options);
             jobMessage = cmd.getOptionValue("m");
           if (jobMessage.isEmpty () ) {
               System.out.println ("Error: no message has been provided");
                usage (options); 
                return;
           }
           const char * jobMessagesArgs[] = cmd.getArgs() ;
           int n = 0;
           for (n=0; n < jobMessagesArgs.length; n++)
               jobMessage += " " + jobMessagesArgs[n] ;
       }
        if (cmd.hasOption ("h")) {
           usage (options);
           return;
       }

        if (cmd.hasOption ("v")) {
           printVersion ();
           return;
       }

        if (cmd.hasOption ("d") ){
           // set log level to debug
           debug = true;
            if (debug) {
              System.out.println("option:debug");
           }
        }

       // create the collector object
       TxUCMCollector collector = new TxUCMCollector ();
       if (collector.initDb() ) {
          if (jobMessage == null )  {
              // start processing log files and creating database entries
             collector.startProcess ();
          } else  {
              collector.processMessage(jobMessage);
           }
        }
       if (debug) {
           System.out.println ("created collector object");
       }
#endif
}// end main()
    // Task table column names
     const char * TxUCMCollector::fgTaskCols = "('taskID', 'brokerTaskID', 'brokerID', "
       "'requesterID', 'taskName', 'taskDescription', 'taskSize', "
       "'taskRemainSize', 'submitTime', 'updateTime', 'archiveFlag')";

    // Jobs table columns
     const char *TxUCMCollector::fgJobsTableCols = 
       "(" 
       "jobID             int(11) NOT NULL AUTO_INCREMENT KEY COMMENT 'ID of job when entry is created, unique within table', " 
       "updateTime        timestamp    NOT NULL default CURRENT_TIMESTAMP COMMENT 'Time that job execution state was last updated', "
       "brokerJobID       int(11) NOT NULL COMMENT 'ID of job as assigned by Broker', " 
       "taskID            int(11) NOT NULL COMMENT 'Foreign key reference to Tasks table', " 
       "gridJobID         varchar(64) default NULL  COMMENT 'ID for job as assigned by Grid Resource Allocation Manager (GRAM)', " 
       "localJobID        int(11)     default NULL  COMMENT 'ID for job as assigned by local resource manager or scheduler', "
       "gridSubmitTime    datetime default NULL  COMMENT 'Time that job was submitted to the GRAM', "
       "localSubmitTime   datetime default NULL  COMMENT 'Time that job was submitted to the local resource manager or scheduler', "
       "siteLocation      varchar(64)  default NULL  COMMENT 'Physical local of job, could be grid site or local cluster description', "
       "queue             varchar(64)  default NULL  COMMENT 'Name and short description of queue that shedules job', "
       "queuePosition     int(11)      default NULL  COMMENT 'Integer slot position of job in local resource manager or scheduler', "
       "nodeLocation      varchar(64)  default NULL  COMMENT 'Name of worker node that job lands on', "
       "startTime         datetime     default NULL  COMMENT 'Time that job started execution', "
       "executionUserName varchar(32)  default NULL  COMMENT 'A login ID on the local resource site & worker node that actually executes', "
       "stateID           int(11)      NOT NULL default '1' COMMENT 'Foreign key reference to StateDictionary table', "
       "CONSTRAINT UNIQUE INDEX jobID (brokerJobID, taskID)"
       ")";

    // Jobs table column names
     const char * TxUCMCollector::fgJobCols = "('jobID', 'brokerJobID', 'taskID', " 
       "'gridJobID', 'localJobID', 'gridSubmitTime', " 
       "'localSubmitTime', 'siteLocation', 'queue', 'queuePosition', "
       "'nodeLocation', 'startTime', 'executionUserName', 'stateID')";


    // Events table columns
     const char * TxUCMCollector::fgEventsTableCols = 
       "(" 
       "eventID      int(11) NOT NULL AUTO_INCREMENT KEY COMMENT 'ID of event when entry is created, unique within table', "
       "time         timestamp   NOT NULL default CURRENT_TIMESTAMP COMMENT 'Time that event was recorded by the Tracking Library', "
       "jobID        int(11) NOT NULL COMMENT 'Job that this message is associated with', "
       "levelID      int(11) NOT NULL COMMENT 'The ID of the log level of the event (WARNING, DEBUG, ERROR, etc.)', "
       "context      VARCHAR(40) NOT NULL COMMENT 'The bulk category of the log event or the facilty or code where the event happens', "
       "stageID      int(11)     NOT NULL COMMENT 'The ID of the logging stage of the event (i.e., START, STATUS, or END)', "
       "messageKey   VARCHAR(40)  COMMENT 'A user defined property key or SYSTEM for system event', "
       "messageValue VARCHAR(120) COMMENT 'A user defined property value or textual content of a log message for a system event', "
       "cpuLoad      double       default NULL COMMENT 'Optional benchmarking value, program CPU load in %', "
       "totalMem     int(11)      default NULL COMMENT 'Optional benchmarking value, total system memory in KiB', "
       "usedMem      int(11)      default NULL COMMENT 'Optional benchmarking value, total used memory in KiB', "
       "appMem       int(11)      default NULL COMMENT 'Optional benchmarking value, total app memory in KiB', "
       "INDEX (jobID)"
       ")";

    // Events table column names
     const char * TxUCMCollector::fgEventCols = "('eventID', 'jobID', 'levelID', "
       "'context', 'time', 'stageID', 'messageKey', 'messageValue', "
       "'cpuLoad', 'totalMem', 'usedMem', 'appMem')";
//_____________________________________________________________________________
int  TxUCMCollector::queryTableSize(const char *tableName,const StRecord *where)
{
   int size = 0;
   if(string(tableName) == "Jobs") {
      setBrokerTaskID(where);
   } else if (string(tableName) == "Events") {
      setBrokerJobID(where);
   }
   size = queryTableSize(tableName);
   return size;
}    
//_____________________________________________________________________________
int  TxUCMCollector::queryTableSize(const char *tableName, const char *where)
{
   int size = 0;
   string whichTask;
   bool countSelectedEvents = false;
   if ( !where ) { 
      if ((string(tableName) == "Tasks" ) && (msgHashMap.find(fgRequester) != msgHashMap.end())) {
         whichTask=string("requesterID='")+ msgHashMap[fgRequester]+"' ";
         where = whichTask.c_str();
      } else if (string(tableName) == "Jobs" ) {
         tableName = jobTableName().c_str();
      } else if (string(tableName) == "Events" ) {
         tableName = eventTableName().c_str();
         countSelectedEvents =  (fDbJobID >= 0 );
      }
   }
   string query = string("select count(*) from `")+ tableName + "`";
   if (where &&where[0]) {
       query += string(" WHERE ") + where;
   } else if (countSelectedEvents) {
       query += string(" WHERE ") + string("jobID='")+ itoa(fDbJobID)+"' ";
   }
   execute (query);
   if (fResult) {
      if (mysql_num_rows(fResult)==1) { 
         fRow = mysql_fetch_row(fResult);
         if (!fRow) {
            log->error(mysql_error(connection));
         }  else {
            size = atoi(*fRow);
         } //
      } else {
            log->error(string("wrong  result for <") + query + ">");
      }
   }
   return size;
}
//_____________________________________________________________________________
void  TxUCMCollector::queryTable(const char *tableName, int limit, int offset, const char *where)
{
   // limit = 0 - no limit
    string query =  string("select * from `") + tableName + "` ";
    if (where  && where[0] ) {
       query += string("WHERE ") + where;
    }
    if (limit > 0)  query += " LIMIT " + itoa(limit);
    if (offset > 0) {
       if(limit <= 0 ) query += "LIMIT 9999999 ";
       query += " OFFSET " + itoa(offset);
    }
    execute (query);
}

//_____________________________________________________________________________
void  TxUCMCollector::queryTaskTable(int limit, int offset) 
{
   string where=string(" requesterID='")+ msgHashMap[fgRequester]+"' ";
   where += string(" ORDER BY ") + "taskID" + " DESC ";
//   where += " ORDER BY " + fgTaskCols[0] + " DESC ";
   queryTable("Tasks",limit,offset,where.c_str());
}
//_____________________________________________________________________________
void  TxUCMCollector::queryJobTable(int limit, int offset) 
{
   queryTable(jobTableName().c_str(),limit,offset);
   if (!fResult) {
       string where = " taskID=47948 ";
       queryTable("Jobs",limit,offset, where.c_str()); // old style
   }
}

//_____________________________________________________________________________
void  TxUCMCollector::queryJobTable(const char *taskID,int limit, int offset) 
{
   queryTable(jobTableName().c_str(),limit,offset);
   if (!fResult) {
       string where = string(" taskID='") + taskID + "' ";
       queryTable("Jobs",limit,offset, where.c_str()); // old style
   }
}
//_____________________________________________________________________________
void  TxUCMCollector::queryEventTable(int limit, int offset)
{
   string where=string("jobID='")+ itoa(fDbJobID)+"' ";
   queryTable(eventTableName().c_str(),limit,offset,where.c_str());
   if (!fResult) queryTable("Messages",limit,offset,where.c_str()); // old style
}

//_____________________________________________________________________________
void  TxUCMCollector::queryEventTable(const char *jobDbID, int limit, int offset)
{
   string where=string(" jobID='")+ jobDbID+"' ";
   queryTable(eventTableName().c_str(),limit,offset,where.c_str());
   if (!fResult) queryTable("Messages",limit,offset,where.c_str()); // old style
}

// TxLogEvent interface implemantation


void TxUCMCollector::writeDown(const std::string& message)
{

}
//___________________________________________________________________________________________________
void TxUCMCollector::setEnvBrokerTaskID (const std::string& envBrokerTaskID)
{
}
   
//___________________________________________________________________________________________________
void TxUCMCollector::setEnvBrokerJobID (const std::string& envBrokerJobID)
{
}

//___________________________________________________________________________________________________
void TxUCMCollector::setBrokerTaskID (const std::string& brokerTaskID)
{
    msgHashMap[fgBTaskID] = brokerTaskID;
}

//___________________________________________________________________________________________________
void TxUCMCollector::setBrokerJobID (int brokerJobID)
{ 
    fBrokerJobID = brokerJobID;
}

//___________________________________________________________________________________________________
void TxUCMCollector::setDbJobID (int dbJobID)
{ 
    fDbJobID = dbJobID;
}

//___________________________________________________________________________________________________
void TxUCMCollector::setRequesterName (const std::string& requester)
{
   msgHashMap[fgRequester] = requester;
}

//___________________________________________________________________________________________________
void TxUCMCollector::setContext (const std::string& context)
{
}

//___________________________________________________________________________________________________
void TxUCMCollector::logStart (const std::string& key, const std::string& value)
{
}

//___________________________________________________________________________________________________
void TxUCMCollector::logJobAttribute (const std::string& key , const std::string&value)
{
 
}

//___________________________________________________________________________________________________
void TxUCMCollector::logJobSubmitLocation (const std::string&url)
{
 
}

//___________________________________________________________________________________________________
void TxUCMCollector::setJobSubmitLocation (const std::string& url)
{
 
}

//___________________________________________________________________________________________________
void TxUCMCollector::logTask (unsigned int size)
{
}

//___________________________________________________________________________________________________
void TxUCMCollector::logTask (const std::string& taskAttributes)
{
}

//___________________________________________________________________________________________________
void TxUCMCollector::logJobSubmitState (State state)
{
}

//___________________________________________________________________________________________________
void TxUCMCollector::setJobSubmitState (State state)
{
}

void TxUCMCollector::logJobSubmitID (const std::string& ID)
{
}

void TxUCMCollector::setJobSubmitID (const std::string& ID)
{
}

void TxUCMCollector::logEvent (const std::string& logMsg, 
         Level level,Stage stage, const std::string& msgContext)
{
}         

void TxUCMCollector::logEvent (const std::string& userKey, 
         const std::string& userValue, Level level, Stage stage, 
         const std::string& msgContext)
{
}

void TxUCMCollector::logEnd (const std::string& key, const std::string& value)
{
}

TXEVENT_DEFAULT_IMPLEMENTAION(TxUCMCollector)

