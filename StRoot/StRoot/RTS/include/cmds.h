/******
*
*     29Oct99 JMN: Modified to incorporate changes for ICCP 8
*                  Include modifications from Tonko.
*     11Dec00 JMN: New commands added for ICCP 9. Removed xxx_EVENT_STATUS
*****************************************************************/

#ifndef DAQ_ALL_CMDS_HEADER
#define DAQ_ALL_CMDS_HEADER

#include "rtsCmds.h"

#define PING                           0x01
//#define RESPONSE                       0x02
#define CANT_SEND			0x2	/* Tonko, Sep04 - response to failed TCP/IP send */
#define ACK                            0x03
#define EVENT_DONE                     0x04     /* JMN 11Dec00 */
#define ANNOUNCE_NODEID                0x05     /* JMN 11Dec00 */
#define CONFIRM_RELEASE_TOKEN          0x06
#define RELEASE_TOKEN                  0x07
#define ANNOUNCE_TOKEN                 0x08
#define CONFIRM_SEND                   0x09
#define SEND_SUMMARY                   0x0A
#define REBOOT		               0x0B
#define FAILED_SEND                    0x0C	// from MYRINET
#define RELEASE_EVENT		       0x0D	// from EVB to GB
#define DAQ_TIMEOUT			0x0E	// general timeout occured
#define RTS_WRITE_COUNTERS             0x0f
#define RUN_DONE                       0x10
#define EVENT_DONE_BLOCKED             0x11

// Run control reserves 0x20 -- 0x4f
// Handler sends these to DAQ systems
// These commands are for DAQ users

// These commands are private to Run Control
#define DAQ_LOG_MODIFY                 0x29
#define DAQ_LOG_DATA                   0x2a

// Internal DAQ commands
#define DAQ_CONNECT                    0x2b
#define DAQ_ERROR                      0x2c

// Run control commands (also in rtsCmds.h because of history)
#ifndef _RTSCMDS_H_
#define _RTSCMDS_H_

#define DAQ_RUN_START                  0x20 
#define RTS_RUN_START                  DAQ_RUN_START

#define DAQ_RUN_STOP                   0x21 
#define RTS_RUN_STOP                   DAQ_RUN_STOP

#define DAQ_RUN_PAUSE                  0x22 
#define RTS_RUN_PAUSE                  DAQ_RUN_PAUSE

#define DAQ_RUN_RESUME                 0x23 
#define RTS_RUN_RESUME                 DAQ_RUN_RESUME

#define DAQ_SEND_CONFIG                0x24 
#define RTS_SEND_CONFIG                DAQ_SEND_CONFIG

#define DAQ_FLUSH_TOKENS               0x27
#define RTS_FLUSH_TOKENS               DAQ_FLUSH_TOKENS

#define DAQ_QUERY_TOKENS               0x28
#define RTS_QUERY_TOKENS               DAQ_QUERY_TOKENS

#define RTS_FORCE_STOP		       0x46 
#define RTS_RECONNECT                  0x48
#endif

// Commands from DRC
#define DRC_STOPHANDLER                0x2d
#define DRC_QUERYSYSTEM                0x2e
#define DRC_RUN_START                  0x2f
#define DRC_RUN_STOP                   0x30
#define DRC_RUN_PAUSE                  0x31
#define DRC_RUN_RESUME                 0x32
#define DRC_SET_CONFIG                 0x33
#define DRC_SEND_CONFIG                0x34
#define DRC_GET_CONFIG                 0x35
#define DRC_FLUSH_TOKENS               0x37
#define DRC_PING                       0x38
#define DRC_QUERY_TOKENS               0x39
#define DRC_CLEAR_HANDLER              0x3a
#define DRC_COMMENT                    0x3b
#define DRC_GET_SYSTEMS                0x3c
#define DRC_CONNECT                    0x3d
#define DAQ_CONFIRM                    0x3e
#define DRC_NOP                        0x3f
#define DRC_TIMEOUT                    0x40
#define DRC_RESPONSE                   0x41
#define DRC_DISCONNECT                 0x42
#define DRC_CLIENT_CONTROL             0x43
#define DRC_SEND_ERROR                 0x44
#define DRC_MONITOR_SEND               0x45
#define DRC_RECONNECT                  0x49
#define DRC_GET_THREADS                0x4a
#define DRC_GET_ERRORS                 0x4b
#define DRC_CLEAR_ERRORS               0x4c
#define DRC_UPDATE_QUERY               0x4d
#define DRC_LOGMESSAGE                 0x4e
#define DRC_SET_SYSTEMS                0x4f

// Tonko, 9/13/00


#define DRC_SEND_LINE                  0x47


// EVB and Taper
#define ETH_ANNOUNCE                   0x50
#define EVB_FORMAT_DATA                0x51
#define EVB_SEND_DATA                  0x52
#define EVB_TAPE_REQUEST               0x53
#define TAPER_DONE                     0x54
#define EVB_TAPE_REQUEST_DONE          0x55
#define EVB_TIMEOUT                    0x56
#define EVB_ANNOUNCE_MEMCPY2           0x5a
#define SPOOL_WRITE_LIST               0x57   // unused
#define SPOOL_LIST_WRITTEN             0x58   // unused
#define RCF_GET_FILE                   0x59   // unused
#define RCF_WRITE_FILE                 0x60   // unused
#define RCF_RELEASE_FILE               0x61   // unused
#define SPOOL_GET_DISK                 0x62   // unused
#define SPOOL_USE_DISK                 0x63   // unused
#define SPOOL_FREE_DISK                0x64   // unused
#define ETH_ANNOUNCE_NOTKN             0x65

#define SPOOL_WRITE_EVENT              0x67   // Spool to SpoolWriter
#define SPOOL_EVENT_WRITTEN            0x68   // SpoolWriter to Spool

// Token Manager
#define TM_RESULTS             0x70
#define TM_ANNOUNCE            0x71

// GL3
#define GL3_SEND_RESULTS               0x70
#define GL3_ANNOUNCE_DATA              0x72
#define GL3_BUILD_DECISION             0x73
#define GL3_ANNOUNCE_TOKEN             0x74 
#define GL3_PRESCALE                   0x75 

// GB stuff...
#define GB_BUILD_EVENT                 0x80
#define GB_ANNOUNCE_DATA	       0x81
#define GB_SL3_NODES			0x82

//#define GB_CONFIRM_SL3	               0x83	// Tonko,9/26/00


// DET
#define DET_ANNOUNCE_RESULTS           0x90	// to SL3
#define DET_ANNOUNCE_TOKEN	       0x91	// to GB
#define DET_START_SL3		       0x92	// to SL3
#define DET_CONFIG_DONE		       0x94
#define DET_ANNOUNCE_DATA   	       0x93	// to EVB


#define DET_CMD_AUTOID                 0x96	// to myself
#define DET_CMD_ROSEBUD                0x97	// to myself

//#define DET_CPY_SL3_HEADER	       0x98
//#define DET_CPY_EVB_HEADER	       0x99
//#define DET_FORMAT_DATA_DELAYED	       0x9A
//#define DET_RELEASE_SL3		       0x9C

#define DET_EVB_SEND_DONE		0x9D	// to myself
#define DET_SL3_SEND_DONE		0x9E	// to myself

//#define L4_STARTEVENT                  0xa0
//#define L4_EVENTDECISION               0xa1

#define L4_INIT                        0xa0
#define L4_INIT_DONE                   0xa1
#define L4_SIMU                        0xa2
#define L4_SIMU_DONE                   0xa3
#define L4_FINISH                      0xa4
#define L4_FINISH_DONE                 0xa5
#define L4_DECIDE                      0xa6
#define L4_DECIDE_DONE                 0xa7

#define L4_CFGDONE                     0xa8
#define L4_RUNSTARTDONE                0xa9
#define L4_RUNSTOPDONE                 0xaa
#define L4_SHIPEVENT                   0xab
#define L4_SHIPEVENTDONE               0xac
#define L4_EVP_DONE                    0xad
#define L4_LOCALWRITE                  0xae
#define L4_LOCALWRITEDONE              0xaf

// SL3
#define SL3_SEND_RESULTS	       0xa0	// SL3->DET, DET->RB
#define SL3_ANNOUNCE_RESULTS   	       0xa1	// ?
#define SL3_CONFIRM_SEND	       0xa2	// ?
#define SL3_SEND_SUMMARY	       0xa3	// ?

// L3 EVP
#define EVPL3_SEND_DATA                0xaa

#define MZ_PROCESS_SL3			0xb0	/* Tonko */
#define MZ_ANNCE_SL3			0xb1
#define MZ_DMA_SL3			0xb2
#define MZ_ANNCE_DATA			0xb3
#define MZ_DMA_DATA			0xb4
#define MZ_EMUL_FIBER			0xb5	/* Tonko */
#define MZ_RUN_START_OK			0xb6
#define MZ_RUN_STOP_OK			0xb7
#define MZ_EMUL_DMA			0xb8
#define MZ_KILL_TOKEN			0xb9

#define RB_ANNOUNCE_RESULTS            0xc0	// RB->DET
#define RB_ANNOUNCE_DATA               0xc1	// RB->DET
#define RB_CONFIRM_SL3			0xc2	// RB->DET
#define RB_CONFIRM_DATA			0xc3	// RB->DET

#define RB_EMC_EVENT			0xc5	/* Tonko */
#define RB_EMC_ERROR			0xc6	/* Tonko */

// SPECIAL, RESERVED for TRIGGER stuff
#define TRG_BUILD_EVT			0xca	/* L1CTL->DET; Tonko */
#define TRG_DATA_ANNOUNCE		0xcb	/* L2->DET; Tonko */


#define MYRI_QUASI_DIRECT_SEND         0xd0
#define BB_ANNOUNCE_EVENT              0xd1
#define BB_ANNOUNCE_DATA               0xd2
#define BB_SEND_DATA                   0xd3
#define BB_ANNOUNCE_EVENT_SCSI         0xd4
#define BB_CLOSE_SCSI                  0xd5

#define RCF_WRITE_EVENT                0xd6
#define RCF_WRITER_FLUSH               0xd7
#define RCF_LIST_WRITTEN               0xd8
#define RCF_CONFIRM_EVENT              0xd9

#define BBM_ANNOUNCE_DATA              0xda
#define BBM_SEND_DATA                  0xdb



#define QDSEND_ANNOUNCE_CHUNK          0xdc
#define QDSEND_SEND_CHUNK              0xde

#define VX_REQUEST_FILE                0xdf
#define VX_RESPONSE                    0xe6

#define QMYRI_MEMCPY_START             0xf0
#define QMYRI_MEMCPY_DONE              0xf1
#define QMYRI_CTL                      0xf2

#define EVP_REQ_EVENT                  0xe0
#define EVP_ANNOUNCE_EVENT             0xe1
#define EVP_SEND_EVENT                 0xe2
#define EVP_CONFIRM_SEND               0xe3
#define EVP_EVENT_DONE                 0xe4
#define RTS_ETHDOOR_ANNOUNCE		0xe5

/* subcommands */
#define MCTL_REG_MEM 1
#define MCTL_LOG_LEVEL 2
#define MCTL_NODE 3

#endif
