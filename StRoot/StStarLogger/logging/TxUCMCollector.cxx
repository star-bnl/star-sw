/*****************************************************************
 * @file TxUCMCollector.cpp
 * @author Roopa Pundaleeka
 *
 * @(#)cpp/api:$Id: TxUCMCollector.cxx,v 1.9 2009/06/26 20:48:39 fine Exp $
 *
 * Please see TxUCMCollector.h for more documentation.
 * "Translated" from the original TxUCMCOllector.java version 
 */
#include "TxUCMCollector.h"
#include <stdlib.h>
#include <log4cxx/logger.h>
#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/helpers/stringhelper.h>
#include <log4cxx/patternlayout.h>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::spi;
using namespace std;

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
             fprintf(stderr, "TxUCMCollector::getConnection:  ---- > No connection: %s %s %s %s %d  %s  \n",host, user, passwd, db, port,mysql_error(connection));
             connection = 0;
             fIsConnectionOpen = false;
         } else {
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
      if (( ret = mysql_query(connection,query.c_str()) )) {
         log->error(std::string("MYSQL QUERY:") + mysql_error(connection));
      }
    }
    return ret;
}
//_________________________________________________________________________
/* The default behavior holds a single connection open until the appender is closed (typically when garbage collected). */
void TxUCMCollector::closeConnection()
{
  if (fIsConnectionOpen) {
     mysql_close(connection); 
     if (mysql_errno(connection))   fprintf(stderr,"MYSQL close ERROR %s  \n",mysql_error(connection));
     connection = 0;
     fIsConnectionOpen = false;
  }
}

//______________________________________________________________________
TxUCMCollector::TxUCMCollector ()
: connection(0),fIsConnectionOpen(false), sleepTime(10),currLogFilePos(0)
{ 
   log =  Logger::getLogger(_T("TxUCMCollector")); 
   // log->setLevel(Level::DEBUG);
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
       sprintf(buffer,"%d",keysNVals.size());
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
                             "Jobs_" + msgHashMap[fgRequester] + 
                            "_" + msgHashMap[fgBTaskID])) {

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
                       "Jobs_" + msgHashMap[fgRequester] + 
                       "_"     + msgHashMap[fgBTaskID]);
    } else {
       log->debug ("Record with brokerJobID = " + msgHashMap[fgBJobID] +
                    " already exists");
    }
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
                            "Jobs_" + msgHashMap[fgRequester] + 
                            "_"     + msgHashMap[fgBTaskID])) {
      updateRecord (                  msgHashMap[fgValue], 
                          "Jobs_"        + msgHashMap[fgRequester] + 
                          "_"            + msgHashMap[fgBTaskID], 
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
                            "Jobs_" + msgHashMap[fgRequester] + 
                            "_" + msgHashMap[fgBTaskID])) {
      updateRecord (string(fieldName) + " = '" +  msgHashMap[fgValue] + "'", 
                       "Jobs_" + msgHashMap[fgRequester] + 
                       "_" + msgHashMap[fgBTaskID], 
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
                            "Jobs_" + msgHashMap[fgRequester] + 
                            "_" + msgHashMap[fgBTaskID])) {

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
                       "Jobs_" + msgHashMap[fgRequester] + 
                       "_" + msgHashMap[fgBTaskID]);
   }

   // Insert the new events table record

   std::string newEventKeys = "jobID, levelID, context, time, stageID, messageKey, messageValue";
   std::string newEventVals = string("(SELECT jobID FROM `Jobs_") + msgHashMap[fgRequester] + 
           "_" +  msgHashMap[fgBTaskID] + "` WHERE brokerJobID=" +
           "'" + msgHashMap[fgBJobID] + "')" +
           ", '" + msgHashMap[fgLevel] + "'" +
           ", '" + msgHashMap[fgContext] + "'" +
           ", '" + msgHashMap[fgTs] + "'" +
           ", '" + msgHashMap[fgStage] + "'" +
           ", '" + msgHashMap[fgKey] + "'" +
           ", '" + msgHashMap[fgValue] + "'";
                  
   insertRecord (string("(") + newEventKeys + ") VALUES (" + newEventVals + ")", 
                    "Events_" + msgHashMap[fgRequester] + 
                    "_" + msgHashMap[fgBTaskID]);
   }

 /**
   * Create Jobs table
  */
void TxUCMCollector::createJobsTable () {
       // create new jobs table if it does not exist
    std::string tableName = "`" + string("Jobs_")
           + msgHashMap[fgRequester] + "_"
           + msgHashMap[fgBTaskID]+ "` ";
    this->createTable (tableName + fgJobsTableCols);
}

    /**
     * Create Events table
     */
//________________________________________________
void TxUCMCollector::createEventsTable () {
       // create new events table if it does not exist
   std::string tableName = "`" + string("Events_")
           + msgHashMap[fgRequester] + "_"
           + msgHashMap[fgBTaskID] + "` ";
   this->createTable (tableName + fgEventsTableCols);
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
   MYSQL_RES *exists = 0;
   unsigned long nRows = 0;
   TRY
   {
      execute(string("SELECT * FROM `") + tableName 
                                    + "` WHERE " + selectStr);
      exists = mysql_store_result(connection); 
      if (exists) {
         nRows = mysql_num_rows(exists);
         mysql_free_result(exists);
      }
   }
   closeConnection();
//        CATCH(SQLException se) {
//            log->error (se.getMessage ());
//            exists = false;
//        }

   return nRows ? true: false;
}

    /**
     * createTable: Execute the SQL query to CREATE a table if it does
     *              not already exist
     *
     * @param Table info
     */
void TxUCMCollector::createTable (const string&table) 
{
   createTable (table.c_str());
}

//________________________________________________
void TxUCMCollector::createTable (const char * table) {
   TRY{
       if (!execute(string("CREATE TABLE IF NOT EXISTS ") + table))
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
//________________________________________________
void TxUCMCollector::usage (Options options)
{
#ifdef FUTURE
    // Use the built-in formatter class
     HelpFormatter formatter = new HelpFormatter ();
      formatter.printHelp ("Tx UCM Collector", options);
#endif
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
