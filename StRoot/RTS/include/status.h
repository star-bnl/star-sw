/******
*
*     Modifications:
*    
*     07Dec99 JMN: Defined error codes from tape manager
*
************************************************************/

#ifndef _STATUS_H_
#define _STATUS_H_
/* general error status codes */
#define STAT_OK        		0
#define STAT_ERROR		1		/* generic error */
#define STAT_TOKEN      	2		/* nonexisting token */
#define STAT_PROTO      	3		/* protocol violation */
#define STAT_CMD        	4		/* unknown command */
#define STAT_SEQ        	5		/* sequence violation */
#define STAT_RELEASE    	6		/* error releasing the event */
#define STAT_TIMED_OUT  	7		/* timout */
#define STAT_FORCED_STOP	8		/* ??? Tonko */



// John's stuff ????

                                               /* Status to Trigger of returned tokens: STAT_OK event accepted */
#define  STAT_DAQ_REJ   1                      /* Event rejected by DAQ TDI */
#define  STAT_TM_REJ    2                      /* Event rejected by error in TM processing */
#define  STAT_GL3_REJ   3                      /* Event rejected by GL3 */
#define  STAT_SB_REJ    4                      /* Event rejected by error in SB processing */
#define  STAT_EVB_REJ   5                      /* Event rejected by error in EVB processing */
#define  STAT_TDI_TIME  6                      /* DAQ received a token from DET but not TDI */
#define  STAT_DET_TIME  7                      /* DAQ received a token from TDI but not DET */
#define  STAT_TDI_REJ  16                      /* Token NOT sent to DAQ by TDI because of error condition */

#define  CHECK_CONDITION         2             /* Error codes from tape_man:  SCSI Check Condition */
#define  PING_FAILURE            3             /* Cannot connect to remote host */           
#define  TAPER_DISABLED          4             /* Taper disabled by Run Control for reboot */
#define  WRITE_NOT_RDY          11             /* Test Unit Ready failed during write sequence */
#define  UNKNOWN_ERR            12             /* Unprogrammed Sense Key */
#define  UNIT_ATN_ERR           13             /* Unrecovered Unit Atn error */
#define  UNIT_NOT_RDY_ERR       14             /* Could not get Unit Ready */
#define  EVB_ILL_REQ            15             /* Illegal Request from EVB */
#define  DISC_ERR               16             /* Error wwhile writing to disc file */
#define  OPEN_ERR               17             /* Could not open disc file */



#endif 
