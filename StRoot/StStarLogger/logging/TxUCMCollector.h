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
 * @(#)cpp/api:$Id: TxUCMCollector.h,v 1.3 2009/07/17 13:34:59 fine Exp $
 *
 * Please see TxUCMCollector.h for more documentation.
 * "Translated" from the original TxUCMCOllector.java version 
 */

#include <map>
#include <string>
#include <mysql/mysql.h>
#include <log4cxx/logger.h>

typedef bool boolean;
typedef std::string Options;

class TxUCMCollector {

    /**
     * Constructor
     */
    public:
       TxUCMCollector ();
    
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
    private:
           void createNewTask ();
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
};


#endif
