#ifndef ICCP_TASKS_HEADER
#define ICCP_TASKS_HEADER

/* 
	June 6, 2003; Tonko; Cleaned up old stuff. Added GB stuff. 
*/

#define DET_TASK              1
#define DET_FAST_TASK	      2	/* for fast messaging GB->DET */
#define EMU_TASK	      3 /* any kind of emulator task */

#define EVB_TASK             20
#define EVB_STAT_TASK        25
#define EVB_DATA_TASK(x)     (60+x)
#define EVB_SUPERMON_TASK    65

#define TAPER_TASK           30    /* this is the mainstream taper */
#define TAPER_2_TASK         31    /* this is the aux-taper       */
#define EVP_TAPER_TASK       31

// EVBX2 task definitions
// #define EVB_TASK 20
// #define ETHLIB_OUTGOING_TASK  171
#define BUILDEVENT_TASK      42
#define L2_THREAD_TASK       43
#define PRE_TOKEN_MANAGER_INTERNAL 44
#define PRE_TOKEN_MANAGER_TASK 45



// end EVBX2 tasks

#define TAPER_3_TASK         32
#define TAPER_4_TASK         33
#define TAPER_5_TASK         34
#define TAPER_6_TASK         35
#define TAPER_7_TASK         36

#define EVP_TASK		50
#define EVP_TASK_READER		51
#define EVP_EMUL_TASK		52
#define EVP_TASK_FIRST		52
#define EVP_TASK_FIRST3		53
#define EVP_TASK_FIRST4		54
#define EVP_TASK_FIRST5		55
#define EVP_TASK_FIRST6		56
#define EVP_TASK_FIRST7		57
#define EVP_TASK_FIRST8		58
#define EVP_TASK_LAST		59


#define GB_TASK              60  
#define GL3_TASK             61  
#define EVPL3_TASK           62
#define EVENT_DONE_SERVER_TASK 63

#define TDI_SIM_TASK         70   /* JMN 11Dec00 */
#define TM_TASK              TDI_SIM_TASK
#define ZERO_TOKEN_TASK      71

#define SL3_TASK             80
#define L3EVP_TASK           EVPL3_TASK
#define L3DISP_TASK          82


/* Tonko, June 6, 2003 - added a bunch of newGB-specific tasks */
#define GB_DONE_TASK    100		// from EVB
#define GB_DET_TASK     101		// from DETs
#define GB_PING_TASK    102		// from EVB, SL3, GL3

/* below are local tasks */
#define GB_DET_MSG_TASK 103		
#define GB_EVB_MSG_TASK 104
#define GB_MON_TASK     105
#define GB_CHECKER_TASK 106

/* There are also used by DETs as well as the myriWrapper! */
#define GB_MYRICPY_TASK  110
#define GB_MYRICPY_TASK1 111
#define GB_MYRICPY_TASK2 112
#define GB_MYRICPY_TASK3 113
#define GB_MYRICPY_TASK4 114
#define GB_MYRICPY_TASK5 115
#define GB_MYRICPY_TASK6 116
#define GB_MYRICPY_TASK7 117
#define GB_MYRICPY_TASK8 118
#define GB_MYRICPY_TASK9 119

/* used for multi-node DETs to wrap Myrinet */
#define DET_MYRIWRAP_TASK	120

/* DET task which handles DDL */
#define RB_TASK              130
/* DET Linux receiver Threads */
#define DET_RCV_TASK		131
#define DET_RCV_TASK1		132
#define DET_RCV_TASK2		133
#define DET_RCV_TASK3		134


#define DAQ_TASK            150
#define DAQ_RC_CMD_RCV      151    /* The command receiver thread */
#define DAQ_RC_CMD_SND      152    /* The command sender thread */
#define DAQ_RC_MSG_SND	    153    /* The message sender thread */
#define DAQ_RC_HANDLER      154    /* The handler itself */
#define DAQ_RC_CONNECTOR    155    /* The connector thread */
#define DAQ_RC_SND          156    /* The sender threads */
#define DAQ_RC_RCV          157    /* The reciever threads */
#define DAQ_RC_CLIENT_SND   161
#define DAQ_RC_CLIENT_RCV   162
#define DAQ_RC_STUB         161
#define DAQ_RC              158
#define DAQ_ONLINE          159
#define DAQ_MONITOR         160
#define DUMMY_TASK          164 

#define DISK_MANAGER_TASK   165
#define RCF_WRITER_TASK0    166
#define RCF_WRITER_TASK1    167
#define RCF_WRITER_TASK2    168
#define RCF_WRITER_TASK3    169


#define MON_TASK		170
#define ETH_TASK		171	/* the ethLib outgoing task/que */

#define BB_MANAGER          180
#define BB_READER           181
#define BB_TASK             BB_READER
#define BB_MEM              182
#define BB_OUT              183
#define BB_ETH_OUT          184
#define ETHLIB              184
#define BB_READ_MAN_PRIVATE 185
#define SPOOL_TASK          TAPER_TASK
#define SPOOL_WRITER_TASK   186
#define SPOOL_WRITER_TASK0  186
#define SPOOL_WRITER_TASK1  187
#define SPOOL_WRITER_TASK2  188

#define DB_TASK_CONDITIONS  190
#define DB_TASK_EVENT_TAG   191
#define DB_TASK_FILE_TAG    192
#define DB_TASK_RUN_TAG     193
#define DB_TASK_FILE_TAG_UPDATE 194
#define DB_TASK_CONDENDRUN  195
#define DB_TASK_HPSS_FILE_TAG_UPDATE 196
#define DB_TASK_L1_COUNTER  197
#define DB_TASK_L2_COUNTER  198
#define DB_TASK_L3_1_COUNTER 171
#define DB_TASK_L3_2_COUNTER 172
#define DB_TASK_L3_3_COUNTER 173
#define DB_TASK_SCALER      174
#define DB_TASK_CNDTW       175

#define TRG_TASK            210
#define TRG_DSM_TASK        201 
#define TRG_L1_TASK         202                /* Level 1 Control Task */
#define TRG_TM_TASK         203                /* Token Manager Task number */
#define TRG_HI_TASK         204                /* Hardware Interface Task number */
#define TRG_CON_TASK        205                /* Level 1 Software Configuration Task number */
#define TRG_ANA_TASK        206                /* Level 1 Analysis */
#define TRG_ANASC_TASK      207                /* L1 Scaler task */
#define TRG_L2_TASK         208                /* Level 2 Event Builder */
#define TRG_TCTR_TASK	    209			/* Tonko, requested by John, Sep 3, 2003 */
#define TRG_HISUB_TASK	    211			/* Tonko, requested by John, Mar 16, 2006 */
#define TRG_QT_TASK         212
#define TRG_SCA_MON_TASK    213
#define TRG_SHARE_TASK      214

#define MYRI_MSG_SND        220    // board 3
#define MYRI_MSG_SND_2      221    // board 4
#define MYRI_CPY_INIT       222
#define MYRI_CPY_INIT_2     223
#define MYRI_CPY_SEND       224
#define MYRI_CPY_SEND_2     225
#define MYRI_CPY_RCV        226
#define MYRI_CPY_RCV_2      227

#define VX_REQUEST_FILE_TASK 228
#define VX_REQUEST_FILE_Q   VX_REQUEST_FILE_TASK
#define VX_REQUEST_FILE_TASK_2 229
#define VX_REQUEST_FILE_Q_2    VX_REQUEST_FILE_TASK_2

#define MYRI_CLIENT_STARTQ   230    // Uses 230...239
#define MYRI_CLIENT_ENDQ     249

#define SC_TASK             230

#define EVP_ETH_RCV_TASK 51


#define L4_TASK 80        // use L3 task
#define L4_EVB_TASK 82

#define TOKEN_MANAGER_TASK 19
#define TOKEN_MANAGER_TKN_TASK 20
#define TOKEN_MANAGER_EXT_TRG_TASK 21
#define TOKEN_MANAGER_TKN_RETURN_TASK 63
#define TOKEN_MANAGER_EVT_RETURN_TASK 64

#endif 
