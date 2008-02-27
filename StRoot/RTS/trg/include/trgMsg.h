/*
 * Package Name: All     
 * Created: zm 9-30-99   
 * Description: Global messaging header file. 
 *              Contains definitions of global variables, queue names etc... 
 * History:                                                 
 *     28Sep99 zm    created by taking messaging info from trigger.h 
 *     07Oct99 zm    adding new message Q's for L1DC token Manager, etc. 
 *     15Nov99 zm    added read DSM command to distinguish from Build Event
 *     11Dec99 jmn   added structure and defines for message monitoring    
 *     19Feb00 egj   added RESYNCH command and changed CONFIG_W from 0x01 to 0x02
 *     24May00 zm    increased message q length to 8120 or 2X previous
 *     08Feb00 zm    deleted obsolete SCI message defines for new L2 configuration
 *     28Jun01 JMN   added TCUS task number for TCU Scaler Daemon. 
 *                   added new command UPDATE to send to RCC  
 *     09Jun02 JMN   updated for myrinet  
 *     07Aug02 JMN   Commented out task names.  Use /RTS/include/tasks_jmn_draft.h 
 *     03Dec02 zm    made compatible for linux
 *     13Jan03 zm    added write counters command
 *     23Oct03 JMN   Added RTS_ICCP_MSG_LEN since no longer defined in RC_Config.h
 *     13Oct04 JMN   Replaced SET_T0 command with ABORT_L2
 *     04Aor04 JMN   Replaced CLR_BIT command with FLUSH
 */

#ifndef _trgMsg_h
#define _trgMsg_h

#define  MAXMSGQ       30              /* Number of message queues that will be set up */
#define  MSGQLEN     8190              /* Maximum number of messages a queue can hold */
#define  RTS_ICCP_MSG_LEN  120         /* No longer defined in RC_Config.h */

#define  MAX_MON      300              /* Depth of message monitoring ring buffer */
#define  MON_MSG_LEN    3              /* Length (in words) of copy to monitoring buffer */

#define  STOP_RUN      0x00
#define  START_RUN     0x01
#define  ADD_TOKEN     0x02
#define  CONFIG        0x03
#define  CONFIG_CTB    0x04
#define  DONE_CONFIG   0x05
#define  RESYNCH       0x06            /* used to re-synchronize DSM Address Pointers */
#define  DONE_RESYNCH  0x07
#define  LOAD_TKN      0x10
#define  LOAD_ABORT    0x11
#define  LOAD_MTKN     0x12
#define  FLUSH         0x13            /* Changed from CLR_BIT, previously unused */
#define  FIND_TOKEN    0x16            /* token managing command */
#define  TOKEN_OK      0x17            /* token managing command */
#define  TOKEN_ERR     0x18            /* token managing command */
#define  BUILD_EVT     0x20            /* Confusing:  same as RTS_RUN_START Use TRG_BUILD_EVT (rtsCmds.h) */
#define  L1_SEND_EVT   0x21            /* send L1 Event to L2 */
#define  L1_ACC        0x22            /* L1 to L1EventBuilder, or L1:TCU */
#define  L1_ABORT      0x23            /* L1 to TM or HI */
#define  L1_ANA        0x24            /* Analyze L1 Event */
#define  READ_DSM      0x25            /* Read DSM's */
#define  ABORT_L2      0x26            /* L1 to L2 to dump all DSMs */
#define  L2_SEND_EVT   0x30            /* send L1 Event to Analyze */
#define  L2_ACC        0x31            /* L2 to L1 */
#define  L2_ABORT      0x32            /* L2 to L1 */
#define  L2_ANA        0x33            /* Analyze L2 Event */
#define  L3_ABORT      0x34            /* L2 to L1 */
#define  STOP_DSM      0x35            /* RCC requested to put DSMs into load mode */
#define  SC_UPDATE     0x40            /* Update scalers:  TCU_Scaler to RCC */
#define  SEND_DAQ      0x74            /* Taken from DAQ protocols */
#define  RELEASE       0x85
#define  DATA_ANNOUNCE 0x50            /* temporary command to L2 */
#define  WRITE_COUNTERS 0x60
#define  WRITE_ARRAY   0x61
#define  TW_UPDATE     0x62
#define  LIVE_UPDATE   0x63

#define  STOP_RUN_W    0x01
#define  START_RUN_W   0x02
#define  ADD_TOKEN_W   0x02
#define  CONFIG_W      0x02
#define  CONFIG_CTB_W  0x02
#define  DONE_CONFIG_W 0x02
#define  RESYNCH_W     0x01
#define  DONE_RESYNCH_W 0x01
#define  LOAD_TKN_W    0x08
#define  LOAD_MTKN_W   0x08
#define  FLUSH_W       0x01
#define  LOAD_ABORT_W  0x02
#define  FIND_TOKEN_W  0x02
#define  TOKEN_OK_W    0x02
#define  TOKEN_ERR_W   0x02
#define  BUILD_EVT_W   0x04
#define  L1_SEND_EVT_W 0x01
#define  L1_ACC_W      0x02
#define  L1_ABORT_W    0x02
#define  L1_ANA_W      0x02
#define  READ_DSM_W    0x02
#define  ABORT_L2_W    0x01
#define  L2_SEND_EVT_W 0x01
#define  L2_ACC_W      0x02
#define  L2_ABORT_W    0x02
#define  L2_ANA_W      0x02
#define  L3_ABORT_W    0x02
#define  STOP_DSM_W    0x01
#define  SC_UPDATE_W   0x02
#define  SEND_DAQ_W    0x04
#define  RELEASE_W     0x01
#define  DATA_ANNOUNCE_W 0x01
#define  WRITE_COUNTERS_W 0x01
#define  WRITE_ARRAY_W 0x3
#define  TW_UPDATE_W   0x2
#define  LIVE_UPDATE_W 0x2

#define  STAT_OK        0                 /* Status to L2 of returned tokens: event accepted */
#define  STAT_DAQ_REJ   1                 /* Event rejected by DAQ TDI */
#define  STAT_TM_REJ    2                 /* Event rejected by error in TM processing */
#define  STAT_GL3_REJ   3                 /* Event rejected by GL3 */
#define  STAT_SB_REJ    4                 /* Event rejected by error in SB processing */
#define  STAT_EVB_REJ   5                 /* Event rejected by error in EVB processing */
#define  STAT_TDI_REJ  16                 /* Token NOT sent to DAQ by TDI because of error condition */

#define MSG_TIMEOUT 500                   /* Message time out 5 secs */
#define TIMEOUT -1                        /* Timeout gives error */


#ifdef LITTLE_ENDIAN
typedef struct msg_pkt {    
  unsigned token :12;
  unsigned status :4;
  char dst_task, cmd;
  unsigned short sourceId;
  unsigned domain :4;
  unsigned valid :12;    
  char src_task, reserved;
  short transaction;
  int payload[13];
} msg_pkt;
#else
typedef struct msg_pkt {                  /* ICCP9 layout */ 
  char cmd, dst_task;
  unsigned status :4;           
  unsigned token :12;
  unsigned valid :12;    
  unsigned domain :4;
  unsigned short sourceId;
  unsigned short targetId;
  char reserved, src_task;
  int payload[27];
} msg_pkt;
#endif

typedef struct msg_monitor {              /* Used for message monitoring */
  char cmd, dst_task;
  unsigned status :4;           
  unsigned token :12;
  unsigned valid :12;    
  unsigned domain :4;
  unsigned short sourceId;
  unsigned short targetId;
  char reserved, src_task;
} msg_monitor;

#endif
