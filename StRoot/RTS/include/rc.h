#ifndef _RC_H_
#define _RC_H_


/*******************************************************/
/* States                                              */
/*******************************************************/
#define RC_NONE 0
#define RC_PRESENT 1
#define RC_READY 2     
#define RC_RUNNING 3
#define RC_PAUSED 4
#define RC_ERROR 5
#define RC_CONFIGURING 6	/* new state as of Feb 2019 */

#define RC_WAITING 128  /* Used as a flag on top of other status's */
#define RC_READY_WAIT (RC_WAITING + RC_READY)    
#define RC_RUNNING_WAIT (RC_WAITING + RC_RUNNING) 
#define RC_PAUSED_WAIT (RC_WAITING + RC_PAUSED) 
#define RC_PRESENT_WAIT (RC_WAITING + RC_PRESENT)

// Global Handler states (new model)
#define GS_ZERO		0	//unused; but for completeness
#define GS_NONE		1
#define GS_PRESENT	1
#define GS_READY	2
#define GS_CONFIGURING	3
#define GS_CONFIGURED	4
#define GS_STARTING	5
#define GS_RUNNING	6
#define GS_PAUSING	7
#define GS_PAUSED	8
#define GS_STOPPING	9
#define GS_ERROR	10
#define GS_WAITING	11	// mix of states for multinode detectors...
#define GS_ANY		100	// I want to move this to a sane value like 12

// Reason for last run stop...
#define GSR_NO_RUNS 0
#define GSR_STOPRUN 1
#define GSR_STOPRUN_OPER_BAD 2      // operator marked bad
#define GSR_FLUSHTOKENS 3
#define GSR_FORCESTOP 4        // Also fill in node
#define GSR_DISCONNECTED 5     // Also fill in node
#define GSR_REBOOTED 6
#define GSR_STOPRUN_FAILED 7   // the stoprun command failed: file in node

/*******************************************************/
/* Default Handler locations                           */
/*******************************************************/
#define ETH_OUT_SERVER		"daqman"
#define ETH_SERVER_PORT		4000
#define ETH_UNIX_PORT ETH_SERVER_PORT
#define ETH_CLIENT_PORT	        4001
#define ETH_VXWORKS_PORT ETH_CLIENT_PORT
#define ETH_RC_PORT             4010


/*******************************************************/
/* Configuration File Field Definitions                */
/*******************************************************/
#define EVB_DEST_SCSI 0
#define EVB_DEST_TAPE 1
#define EVB_DEST_FILE 2
#define EVB_DEST_NONE 3
#define EVB_DEST_ETHNET 4
#define EVB_DEST_MYRINET 5
#define EVB_DEST_MYRINET2 6

#define BB_DEST_NONE 0
#define BB_DEST_RCF 1
#define BB_DEST_DISK 2
#define BB_DEST_DBONLY 3
#define BB_DEST_COPY 4   /* both rcf and disk */
#define BB_DEST_WRITER_NONE 5
#define BB_DEST_LOCAL_DISK 6
#define BB_DEST_NONE_EVB 7
#endif 
