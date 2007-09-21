////////////////////////////////
//        Logic Tier          // 
////////////////////////////////
class TXUcmLog {

enum EFacility { ETask, EJob, EEvent, EBroker, ETransport };
enum EStage { EBegin, EEnd, EPRogress };

// Presentation layer
// The class generates the record to file with UCM system
// Each record may contain  the list of the optional attribites 
// followed by the optional user message.

// It is supposed to support the list of the "known attributes",
// where the list of attributes is defined by the current Db schema 
// or other external "configuration" facility.

// All "unknown" attributes are treated as the ordinar "user's message"

protected:
    TXUcmLog();
public:
    virtual ~TXUcmLog(){ closelog();}

    // Logging options other than the defaults 
    static TXUcmLog *openlog(const char* ident,int logopt, EFacility facility);
    static TXUcmLog *openlog(const char* ident,int logopt);
    static TXUcmLog *openlog(const char* ident="ucmlog");

    // Generate and Send the ucm record, 
    // the optional attributes, and the optional user's message
    void ucmlog(unsigned int pripority, const char*message);

    // Generate and Send the record that may contains the attributes only
    void ucmlog(const char*message=0);

    // Generates and Send the ucm record that contains 
    // Begin/End tag, the optional attributes and optional user's message
    void ucmlogevent(EStage stage=EBegin, const char*message=0);

    // send the message that contains the attributes and tags only
    void ucmlogmessage(const char*message=0);

    // send "Job finished" record and close the ucm systen.
    int closelog();

    // register the task with the ID provided and make it current
    void openTask(unsigned int brokerTaskID);

    // register the job of the current task the ID provided
    void openJob(unsigned int brokerJobID);

    // register the current task
    void openTask();

    // register the current job
    void openJob();

    void setlogmask(unsigned int logmask);
    // set the pair of the attribute : value
    void setAttribute(const char *key, const char * value);

    // set the current task ID
    void setTaskId(unsigned int brokerTaskID);

    // set the current job ID
    void setJobId(unsigned int brokerJobID);

    // set the current event ID
    void setEventId(unsigned int taskId);

    // return the attribute value by the attribute key
    const char *getAttribute(const char *key) const;

    // return the attribute value by the attribute index
    const char *getAttribute(int attributeIndex) const;

    // return the number of the different attributes;
    int   getNumberofAttributes() const;

    // Reset the "key" attribute
    void resetAttribute(const char *key);

    // Reset all attributes (clean up the "known" attribute list)
    void resetAttributes();

    unsigned long getTaskId()  const;
    unsigned long getJobId()   const;
    unsigned long getEventId() const;

    bool isTaskOpen()  const;
    bool isJobOpen()   const;
};

/*
------------------------------------------------------------------------------------------------------------------
Message

          PRI                       HEADER                                MESSAGE 
 <Facility*8 + Severity>   TIMESTAMP          HOSTNAME  
                         Mmm dd hh:mm:ss rcas6002.rcf.bnl.gov         TAG       : CONTENT 
                                                                  Task[taskid]  : start
                                                                  Job[jobid]    : Task[Taskid] start
                                                                  Job[jobid]    : Task[Taskid] finish
                                                                  Event[eventid]: Job[jobid], Task[taskid] [attribute=value,] message start
                                                                  Event[eventid]: Job[jobid], Task[taskid] finish
                                                                  Message       : Job[jobid], Task[taskid] attribute=value message

------------------------------------------------------------------------------------------------------------------

*/
