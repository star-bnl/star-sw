#ifndef STAR_TxUCMCollector
#define STAR_TxUCMCollector
/*
 * UCM Collector
 *
 */
/*****************************************************************
 * @file TxUCMCollector.h
 * @author Roopa Pundaleeka
 *
 * @(#)cpp/api:$Id: TxUCMCollector.h,v 1.8 2010/09/17 19:34:54 fine Exp $
 *
 * Please see TxUCMCollector.h for more documentation.
 * "Translated" from the original TxUCMCOllector.java version 
 */

#include <map>
#include <string>
#include "mysql.h"
#include <log4cxx/logger.h>

#include "StDbFieldI.h"
#include "TxEventLog.h"

typedef bool boolean;
typedef std::string Options;

namespace TxLogging {

class StRecord;
class StUcmTask;
class StUcmTasks;
class StUcmJobs;
class StUcmEvents;
class FieldList;
class RecordList;

class TxUCMCollector : public TxEventLog{ 

    /**
     * Constructor
     */
    public:
       TxUCMCollector ();
       virtual ~TxUCMCollector ();
    
   /**
     * Init Db properties and connection
     *
     * @return true if successful, false, if failed
     */
   boolean initDb ();
    /**
     * Read from properties file and get the necessary info
     *
     * @return true if successful, false, if failed
     */
    private:
           boolean init ();
    /** 
     * Load the MySQL driver and create the class scope DB connection
     *
     * @return true if successful, false, if failed
     */
    boolean loadDatabase();
    MYSQL *getConnection();
    MYSQL *getConnection (const char *dbUrl,const char *dbUsername
      , const char *dbPassword);
    MYSQL *getConnection (const std::string &dbUrl,const std::string &dbUsername
      , const std::string &dbPassword); 
    unsigned int execute(const std::string &sql);
    unsigned int execute(const char   *sql);
/**
			* Override this to return the connection to a pool, or to clean up the
			* resource.
			*
			* The default behavior holds a single connection open until the appender
			* is closed (typically when garbage collected).
			*/
			virtual void closeConnection();    
   /**
     * startProcess: reads the todo list and starts processing the log
     * file
     *
     */
    public:
        void startProcess ();
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
    private:
      void setCurrLogFile ();
    /**
     * startProcess: reads the designated log file and creates data
     * base entries
     *
     */
    void processLogFile ();
    /**
     * Process the message and create a hash map with key value pairs
     * Then process the hash map to create/update/insert tables
     *
     * @param const char * message to process
     * @return boolean true if successful, false otherwise
     */
    public:
       void processMessage (const std::string &message);
       void processMessage (const char *message);
    /**
     * Create a new record in Tasks table: the values are in the value
     * field of the log message
     *
     */
      virtual  StUcmTasks  *getTaskList ();
      virtual  StUcmTasks  *getTaskList (int limit);
      virtual  StUcmTasks  *getTaskList (int limit, int offset); 
	  
      virtual  StUcmJobs   *getJobList();
      virtual  StUcmJobs   *getJobList(StRecord *task);
      virtual  StUcmJobs   *getJobList(StRecord *task, int limit);
      virtual  StUcmJobs   *getJobList(int limit);
      virtual  StUcmJobs   *getJobList(int limit, int offset);
      virtual  StUcmJobs   *getJobList(StRecord *task, int limit, int offset);
      virtual  int          getJobId(const char * reqName, const char *taskBrokerId, int jobBrokerId);

      virtual  StUcmEvents *getEventList();
      virtual  StUcmEvents *getEventList(StRecord *job);
      virtual  StUcmEvents *getEventList(StRecord *job,int limit);
      virtual  StUcmEvents *getEventList(int limit);
      virtual  StUcmEvents *getEventList(int limit, int offset);
      virtual  StUcmEvents *getEventList(StRecord *job,int limit, int offset);

      int fillTaskList(StUcmTasks &tasks,int limit=0, int offset=0);
      int fillJobList(StUcmJobs &jobs,int limit=0, int offset=0);
      int fillJobList(StUcmJobs &jobs,int limit, int offset,const StRecord *task);
      int fillEventList(StUcmEvents &event,int limit=0, int offset=0,const StRecord *job=0);
      void fillFields(FieldList &fields);
      int  fillUcmList(const char *type,RecordList &records, int limit=0, int offset=0);
      int  queryTableSize(const char *tableName);
      int  queryTableSize(const char *tableName, const char *where);
      int  queryTableSize(const char *tableName, const StRecord *where);
      void queryTable(const char *tableName,int limit=0, int offset=0, const char *where=0);
      void queryTaskTable(int limit=0, int offset=0);
      void queryJobTable(int limit=0, int offset=0);
      void queryJobTable(const char *taskID,int limit=0, int offset=0);
      void queryEventTable(int limit=0, int offset=0);
      void queryEventTable(const char *jobID,int limit=0, int offset=0);

    private:
      StDbFieldI *createField(unsigned int fieldIndx);
      static StDbFieldI::EDataType MapSqlField(enum_field_types type);
      void createNewTask ();
      std::string tableNamePrefix(const char *)  const;
      std::string jobTableName() const;
      std::string eventTableName() const;
      void  setBrokerJobID(const StRecord *job);
      void  setBrokerTaskID(const StRecord *job);
    /**
     * Update a record in Tasks table: the values are in the value
     * field of the log message
     *
     */
     void updateTask ();
    /**
     * Add a new record to the Jobs table: the values are in the value
     * field of the log message Need to make sure that the Jobs table
     * exists, and also there is an associated task table entry
     *
     */
     void addJob ();
    /**
     * Update a record in a Jobs table: the values are in the value
     * field of the log message
     *
     */
     void updateJob ();
    /**
     * Set the site location of the Job if a record exists
     *
     */
     void setJobsField (const std::string &fieldName);
     void setJobsField (const char * fieldName);
    /**
     * Add a new record to the Events table. Need to make sure that
     * the Events table exists, and also there are associated jobs and
     * task table entries
     *
     */
     void addEvent ();
    /**
     * Create Jobs table
     */
    void createJobsTable ();
    /**
     * Create Events table
     */
     void createEventsTable ();
     
    /**
     * insertRecord: Execute SQL query to INSERT a record into a
     *               given table
     *
     * @param const char * insertStr
     * @param const char * tableName
     */
     void insertRecord (const char * insertStr, const char * tableName);
     void insertRecord (const std::string&insertStr, const std::string&tableName);
    /**
     * updateRecord: Execute SQL query to UPDATE a table record of a
     *               given table
     *
     * @param const char * updateString
     * @param const char * tableName
     * @param const char * condition to select unique record
     */
     void updateRecord (const char * updateStr, const char * tableName, const char * condition);
     void updateRecord (const std::string&updateStr, const std::string&tableName, const std::string&condition);
    /**
     * recordExists: Check if a record exists in a given table
     *
     * @param const char * selectStr condition
     * @param const char * tableName
     */
     boolean recordExists (const char * selectStr, const char * tableName);
     boolean recordExists (const std::string&selectStr, const std::string&tableName);
    /**
     * createTable: Execute the SQL query to CREATE a table if it does
     *              not already exist
     *
     * @param Table info
     */
     void createTable (const char * table, const char *like=0);
     void createTable (const std::string &table, const std::string &like=std::string());
    /**
     * print usage
     * @param options the encapsulated list of options
     *    
     */  
     static void usage (Options options);
    /**
     * print current version
     *
     */
     void printVersion();

    /** 
     * main to get things going
     */ 
    public:
     static void main(const char *args[]);
    /**
     * Constants 
     */
    private:
    static const char*fgTs;
    static const char*fgEvent;
    static const char*fgBJobID;
    static const char*fgBTaskID;
    static const char*fgRequester;
    static const char*fgContext;
    static const char*fgLevel;
    static const char*fgStage;
    static const char*fgKey;
    static const char*fgValue;

    static const char*fgNewTask;
    static const char*fgUpdateTask;
    static const char*fgAddJob;
    static const char*fgUpdateJob;
    static const char*fgSiteLocation;
    static const char*fgStateID;
    static const char*fgGridJobID;
    static const char*fgAppStart;
    static const char*fgAppEnd;

    static const char*fgStatusFile;
    static const char*fgStatusFileName;
    static const char*fgStatusFileModTime;
    static const char*fgStatusFilePos;

    
    // Class logger
    private:
      log4cxx::LoggerPtr  log;

    // global connection for updating UCMDB
       MYSQL *connection;
       MYSQL_RES   *fResult;
       MYSQL_FIELD *fField;
       MYSQL_ROW   fRow;
       
       bool fIsConnectionOpen;
    // db info
       std::string dbName;
       std::string dbUrl;
       std::string dbUsername;
       std::string dbPassword;

    // logs dir as set by user to read log files from
       std::string logsDir;

    // sleep time as set by user (default = 10 seconds)
       long sleepTime;

    // current log file thats being processed
       const char * currLogFile;
    
    // current position in the log file thats being processed
       long currLogFilePos;
    
    // hash map to hold key value pairs from a message
       std::map<std::string,std::string> msgHashMap;
	// Current broker Job ID to query
        int fBrokerJobID;
	// Current Job Db ID to query
        int fDbJobID;

    // Task table column names
       static const char * fgTaskCols;

    // Jobs table columns
       static const char *fgJobsTableCols;

    // Jobs table column names
   static const char * fgJobCols;


    // Events table columns
   static const char * fgEventsTableCols;

    // Events table column names
   static const char * fgEventCols;
   
// TxEventLog interface
protected:
    virtual void writeDown(const std::string& message);
    virtual void writeDown(const char *message);
public:
//___________________________________________________________________________________________________
 /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the task ID by passing the
     * name of the environment variable
     *
     * @param string envBrokerTaskID, Name of the environment variable
     *
     */
    virtual void setEnvBrokerTaskID (const std::string& envBrokerTaskID);
    
 /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the job ID by passing the
     * name of the environment variable
     *
     * @param string envBrokerJobID, Name of the environment variable
     *
     */
   virtual void setEnvBrokerJobID (const std::string& envBrokerJobID);
    
    /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the value of Task ID here.
     *       
     * @param string brokerTaskID, value of Task ID
     *
     */
    virtual void setBrokerTaskID (const std::string& brokerTaskID);
    
    /**
     * The concept of a job ID and task ID assigned by broker is
     * assumed to be available here. Set the value of Job ID here.
     *       
     * @param int brokerJobID, value of Job ID
     *
     */
   virtual void setBrokerJobID (int brokerJobID);
    /**
     * The current Db if for the last selected job
     *       
     * @param int dbJobID, value of Job ID
     *
     */
   virtual void setDbJobID (int dbJobID);
    
    /**
     * Set the requester name
     *       
     * @param string requesterName
     *
     */
    virtual void setRequesterName (const std::string& requester);
    
    /**
     * Set context for this task/job
     *
     * @param context, the message context
     */
    virtual void setContext (const std::string& context);

/**
     * Called by the *application* to log a START event
     *
     */
    virtual void logStart (const std::string& key, const std::string& value);

    /**
     * Log the job attrbutes. This method will be called by the
     * Broker.
     *
     * @param string attrbutes key,  For example: "que"
     * @param string attrbutes value, For example queu name 
     *
     */ 
     virtual void logJobAttribute (const std::string& key, const std::string& value);
/**
     * Log the job submit location. This method will be called by the
     * Broker.
     *
     * @param string url, Job submit location. 
     *
     */    
    virtual void logJobSubmitLocation (const std::string& url);
    virtual void setJobSubmitLocation (const std::string& url);

   /**
     * Log the task size. This method will be called by the
     * Broker to log the new task and its size.
     *
     * @param int size, The new task size (the total number of the jobs)
     *
     */    
    virtual void logTask (unsigned int size=1);
    
   /**
     * Log the task with the attributes. 
     * This method will be called by the
     * Broker to log the new task and its attributes.
     * The attribuites MUST match the "Task" MySQL table
     * column names. The attribute types must match 
     * the mySql Task table column types
     *
     * @param string attributes. Task attributes
     *
     */    
    virtual void logTask (const std::string& taskAttributes);
    /**
     * Log the job state. This method will be called by the Broker.
     *
     * @param enum Stage, Job state. 
     *
     */    
    virtual void logJobSubmitState (State state);
    virtual void setJobSubmitState (State state);

    /**
     * Log the job submit ID. This method will be called by the
     * Broker.
     *
     * @param string ID, Job submit ID. 
     *
     */    
    virtual void logJobSubmitID (const std::string& ID);
    virtual void setJobSubmitID (const std::string& ID);

    /**
     * Log a simple message event. This event is always associated
     * with the currnet brokerTaskID and brokerJobID.
     *
     * @param logMsg The message to log.  
     * @param level The level of severity of the event.
     * @param stage The stage of the application assocated with the event.
     * @param msgContext The context in which the event occurs.
     *
     */
    virtual void logEvent (const std::string& logMsg, 
         Level level = LEVEL_INFO,
         Stage stage = STATUS, 
         const std::string& msgContext = TxUCMConstants::defaultContext);

    /**
     * Log a key-value pair type event. This event is always
     * associated with the current brokerTaskID and brokerJobID.
     *
     * @param userKey, The key of the key-value to log. 
     * @param userValue,  The value of the key-value to log. 
     * @param level The level of severity of the event.
     * @param stage The stage of the application assocated with the event.
     * @param msgContext The context in which the event occurs.
     *
     */    
    virtual void logEvent (const std::string& userKey, 
         const std::string& userValue, 
         Level level = LEVEL_INFO, 
         Stage stage = STATUS, 
         const std::string& msgContext = TxUCMConstants::defaultContext);

    /**
     * Called by the *application* to log an END event
     *
     */
    virtual void logEnd (const std::string& key, const std::string& value);
  
};
}

#endif
