/*	This is the basic msg controls and data-structure include-file:  msgdata.h  */
/*	Created:  9oct1996   R. Hackenburg                                          */

static const char sccsid_msgDataH[] = "@(#)"__FILE__"\t\t1.55\tCreated 3/8/98 02:07:35, \tcompiled "__DATE__" "__TIME__;



/*	Note:  +1 is often added to these items, because:
	1)  prefix[0] and class[0] are unused, and "uncounted";
	2)  to allow space for the terminating NULL in character strings.  */

#define MAXPREFIXES      1000  /* Maximum number of unique message-prefixes  */
#define MAXCLASSES       1000  /* Maximum number of unique message-classes   */

#define PREFIX_MAXLEN      48  /* Maximum prefix length                      */
#define CLASS_MAXLEN        1  /* Maximum class length                       */
#define SAMPLE_MAXLEN      64  /* Maximum "last-message" sample length       */
#define NODE_NAME_MAXLEN   48  /* Maximum Node-name length                   */

#define SUMMARY_WIDTH     132  /* Width of summary output (columns).         */


/*	msg's (maybe shared) control data structure:  */
typedef struct {

  int TimeStampCPU;      /* If true, time-stamps occur on changed CPU times. */
  int Sorted;            /* Whether ID list is sorted                        */
  int shmid;             /* Shared memory ID, if shared, -1 if not.          */
  pid_t ProcessID;       /* Process ID of owner of shared memory (zero means self.) */

  /*	msg Summary output features:                                                     */
  int SummaryPageLength;   /* Number of lines output on summary page between form feeds. */
  int SummaryModeActive;   /* Whether Active messages are listed (default = T).          */
  int SummaryModeCounting; /* Whether Counting (but not displaying) messages are listed (default = T).  */
  int SummaryModeInactive; /* Whether Inactive messages are listed (default = F).        */
  int SummaryModeAborted;  /* Whether the Aborted message is listed (default = T).       */

  /*	msg new-definition "classless" defaults:                        */
  int Active;      /*  If true, newly defined, classless messages are active.      */
  int Counting;    /*  If true, newly defined, classless messages will count.      */
  int Alarming;    /*  If true, newly defined, classless messages will alarm.      */
  int CountLimit;  /*  Newly defined, classless messages get this count-limit.     */
  int AbortLimit;  /*  Newly defined, classless messages get this abort-limit.     */
  int AlarmLevel;  /*  Newly defined, classless messages get this alarm-level.     */

  int Nprefixes;   /* Number of prefixes now defined.                   */
  int Nclasses;    /* Number of classes now defined.                    */
  int Nlookups;    /* msg count of total slow prefix-lookups (ie, character-searches) */

  char NodeName[NODE_NAME_MAXLEN+1]; /* Application-specified node-name.  */
} control_t;

/*	The Prefixes' data structure:  (maybe shared) */
typedef struct {
  int SID;         /* Sorted ID list.                                                              */
  int Iclass;      /* Class ID of each defined prefix.                                             */
  int Counts;      /* Occurance-counts for each defined prefix.                                    */
  int Lookups;     /* Slow-lookup-counts for each defined prefix.                                  */
  int Marked;      /* Whether CPU usage is marked -- see MsgMark in msgPublic.c .                  */
  int CPUmark;     /* CPU usage at Mark -- see MsgMark in msgPublic.c .                            */
  int CPUdelta;    /* "Delta" CPU usage -- see MsgMark in msgPublic.c .                            */
  int CPUtotal;    /* "Total" CPU usage -- see MsgMark in msgPublic.c .                            */
  int CountLimit;  /* Occurance-count at which each defined prefix is disabled.                    */
  int AbortLimit;  /* Occurance-count at which each defined prefix causes an abort.                */
  int AlarmLevel;  /* Alarm level for each defined prefix.                                         */
  int Active;      /* Whether each prefix is active (enabled -- produces a message).               */
  int Counting;    /* Whether each is counting.                                                    */
  int Alarming;    /* Whether each prefix is alarming (for application-registered alarm handler).  */
  char Sample[SAMPLE_MAXLEN+1]; /* Sample from last-occurance of a message for each prefix.        */
  char Prefix[PREFIX_MAXLEN+1]; /* Each defined prefix.                                            */
} prefix_t;

/*	The Classes' data structure:  (maybe shared) */
typedef struct {
  int Active;     /* Default active-state for messages of each class.   */
  int Counting;   /* Default counting-state for messages of each class. */
  int Alarming;   /* Default alarming-state for messages of each class. */
  int CountLimit; /* Default count limit for messages of each class.    */
  int AbortLimit; /* Default abort limit for messages of each class.    */
  int AlarmLevel; /* Default alarm level.                               */
  char Class[CLASS_MAXLEN+1]; /* Each defined class.                      */
} class_t;


/*	All the "maybe shared" structures in one:  */
typedef struct {
  control_t control;               /*  msg's control structure.  */
  prefix_t  prefix[MAXPREFIXES+1];   /*  msg's prefix  structure.  */
  class_t   class[ MAXCLASSES+1];    /*  msg's class   structure.  */
} msgData_t;
