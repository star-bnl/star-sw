/******
*
*     A temporary header file until the change has been made.  All references
*     to cmds.hh etc have been changed to cmds.8.0.hh etc
*
*     J.M. Nelson        October 1999
*
*  mws added the gb_build_event
*****************************************************************************/


#ifndef _ICCP_H_
#define _ICCP_H_

/* changes :
// 11/18/97  changed name of SB_ANNOUNCE_DATA -> SB_ANNOUNCE_FORMATTED_DATA
//           The SB_CONFIRM_FORMATTED_DATA  is renamed to 
//           CMD_CONFIRM_FORMATTED_DATA since it will be used by SB and GL3
//           The associated structs are renamed too.
//           New task names added.
//           Added new commands for taper evb communication	     
//           Added new DAQ commands 
// this is iccp.6.0 
// the whole message structure has changed from 5.x
//
// 8/25/98   MWS changed SB_ANNOUNCE_SL3_RESULTS, etc.
//
// 9/08/98   MWS removed all commands, tasks and cmd_size defines
//           inserted includes that represent the cmd numbering sceme
//           described in the document
//           added the cmds2names and task2names files
// 3/23/99   MWS major change. The ###2names.hh contain now just externals.
//           if the name arrays are wanted link with /DAQ/lib/VX/UTIL/iccpNameLib.o
//           moved the message header into another file 
*/
 
// need daq1000 defintions as well, for ethernet 
// but only in linux...
#include "iccp2k.h"     

typedef unsigned int   uint ;
typedef unsigned short ushort ;
typedef unsigned char  uchar ;
typedef char           byte ;

typedef unsigned char UINT8;
typedef unsigned short UINT16;
typedef unsigned int UINT32;
typedef unsigned long long int UINT64;

#if USE_64BITS == 1
typedef unsigned long long int UINTPTR;
#else
typedef unsigned int UINTPTR;
#endif

#ifndef vxworks
#include <stdlib.h>
#endif

#define IC_MSG_LEN 120

#include "cmds.h" 
#include "tasks.h"
#include "status.h"

// Formats send to L3

#define L3_DEFINES    0
#define L3_GL3_ONLY   1
#define L3_GL3_SL3    2
#define L3_GL3_COMPRESSED     3
#define L3_GL3_SL3_COMPRESSED 4

// formats send to the meszzanine 

#define MEZZANINE_DEFINES           0
#define RAW                         1
#define RAW_COMPRESSED              2
#define ZERO_SUPPRESSED             3
#define ZERO_SUPPRESSED_COMPRESSED   4

// some defined event status codes 
// for the SB 
#define SB_TOKEN_UNKNOWN          0
#define SB_CLUSTER_FINDER_STARTED 1
#define SB_CLUSTER_FINDER_DONE    2
#define SB_L3_SUMMARY_REPORTED    3
#define SB_FORMATTING_STARTED     4
#define SB_FORMATTING_DONE        5
#define SB_EVENT_CANCELLED_BY_TRG 6

// for the EVB 

#define EVB_TOKEN_UNKNOWN          0
#define EVB_FORMATTING             1
#define EVB_BUILDING               2
#define EVB_TAPING                 3
#define EVB_NET_FEED               4
#define EVB_EVENT_CANCELLED_BY_TRG 5  

// for the GL3 

#define GL3_TOKEN_UNKNOWN          0
#define GL3_WAITING_SB_ANNOUNCE    1
#define GL3_BUILDING_SL3           2
#define GL3_PROCESSING             3
#define GL3_REJECT                 4
#define GL3_PASSED_TO_EVB          5
#define GL3_EVENT_CANCELLED_BY_TRG 6  


// and now new with 6.0 domain bits
// Tonko: rearranged the numbers and added ZERO_DOMAIN
#define ZERO_DOMAIN         0
#define ETHERNET_DOMAIN     1
#define VME_DOMAIN	    2
#define MYRI_DOMAIN         3

#define LOCAL_DOMAIN	    4
#define EVB_LOCAL_DOMAIN    4 

#define SB_LOCAL_DOMAIN     5 
#define MZ_LOCAL_DOMAIN     6
#define SECTOR_DOMAIN       7 
#define SCI_SL3_SB_DOMAIN   8 
#define SCI_ICCP_DOMAIN     9

#define ETHDOOR_DOMAIN	    10

// the src id can contain the following transport marker in the top 4 bits
#define  SCI_TRANSPORT 0xF
#define  LOCAL_TRANSPORT 0 


// I guess this is a good place for the structs that describe the
// messages 
// this is what ICCP 6.0 messages contain
// the message is called  ic_msg 
// and the union that contains all the different message 
// bodies is ld (for load) I am happy to get input on 
// name changes!!!!! (announce is a good candidate to be replaced)

#pragma pack(1)

// the CMD struct
struct ic_cmd_ping {  uint dummy;};
struct ic_cmd_response {  uint transaction ;};
struct ic_cmd_ack      {  uint transaction ;};  
struct ic_cmd_cancel   {  uint transaction ;}; 
struct ic_cmd_req_stat       {  uint dummy   ;};
struct ic_cmd_release_token  {  uint dummy ; };
struct ic_cmd_confirm_release{  uint dummy   ;};
struct ic_cmd_confirm_formatted_data{ uint dummy   ;}; 
struct ic_confirm_send { uint dummy ; };
// EVB 
struct ic_evb_stat           {  uint dummy   ;};
struct ic_evb_format_data{ uchar cs ; uchar res ; ushort format ;} ;
struct ic_evb_send_data
{ 
  uint haddr;
  uint laddr;
  uint det_evt_id;
  uint evb_evt_id;
} ;
struct ic_det_announce_data {
  uint words;
  uint evt_id;
  uint task;
};
struct ic_eth_announce {
  uint words;
  uint ptr;   // actually a pointer! eth->evb same address space
}; 
// struct ic_evb_tape_request{ uint* addr;} ;
struct ic_rts_write_counters {
  int readoutPeriod;
};
struct ic_evb_tape_request {
  int run;
  int size;
  int offset;
  int event;
  int type;
  int token;
  int evp_type;               // used by evp
  int n_evp_type;             // used by evp
  int buffer;
  int flags;    // bit 0 set, tpc raw data inside
};
struct ic_evbx_tape_request {
  int size;
  int token;
  int buffer;
  int flags;    // bit 0 set, tpc raw data inside
};
struct ic_spool_write_event {
  int offset;     // used for location of data...
  int size;
  int disk;
  char name[20];  // used for filename
  int run;
  int streamIdx;
  int fileIdx;
  int buffer;    // not used by taper, but passed back
  int stream;    // not used by taper, but passed back
};

struct ic_spool_event_written {
  int offset;
  int size;
  int buffer;
  int stream;
  int disk;
};

struct ic_evb_announce_memcpy2 {
  uint buff;
  uint words;
  uint node;
};



struct ic_evbx_spool_write_event {
  int size;
  int disk;
  char name[20];  // used for filename
  int run;
  int streamIdx;
  int fileIdx;
  int eventIdx;
  int stream;    // not used by taper, but passed back
};

struct ic_evbx_spool_event_written {
  int size;
  int eventIdx;
  int stream;
  int disk;
};

// taper
//struct ic_taper_done{ uint dummy; }  ;
struct ic_taper_done {
  int run;
  int size;
  int offset;
  int buffer;
};

struct ic_spool_write_list { int listIdx; };

struct ic_spool_list_written { int listIdx; };

//  daq
struct ic_daq_run_start{ uint dummy ;} ;
struct ic_daq_run_stop
{
  int run_number;
  int num_files;
  int num_events;
  int junk;
} ;
struct ic_daq_run_pause{ uint run_number ;} ;
struct ic_daq_run_resume
{ 
  uint run_number;
  uint num_triggers;
} ;
struct ic_daq_send_config { uint port; uint handler_id; };
struct ic_daq_flush_tokens { uint run_number; };
struct ic_reboot { uint system; };
struct ic_drc_run_start 
{ 
  uint run_number; 
  uint num_triggers; 
};
struct ic_drc_run_stop 
{ 
  uint run_number;
  uint junk;
};
struct ic_drc_run_pause 
{
  uint run_number;
};
struct ic_drc_run_resume               
{
  uint run_number;
  uint num_triggers;
};
struct ic_drc_get_config               
{
  uint dummy;
};
struct ic_drc_set_config              
{
  uint dummy;
};
struct ic_drc_send_config             
{
  uint dummy;
};
struct ic_drc_flush_tokens            
{
  uint run_number;
  uint type;
};
struct ic_mz_emul_fiber               
{
  uint ntriggers;
};
struct ic_drc_query_tokens
{
  uint num_tokens;
  uint state;
  uint rb[12];
};


// GL3
struct ic_gl3_build  
{ 
  uint bxm ; uint bxl ;
  ushort trg_cmd ; ushort trg_word ;
  ushort res ; unsigned trg_type :4 ; unsigned  trg_token :12 ;
} ; 
// GB
struct ic_gb_build  
{ 
  ushort gl3Node ;
  uchar accept;
  uchar build ;
  uint   eventDesc[10] ;   // take from data!
  uint   L3summ[4] ;
  uint   L2summary[2];
  uint   L1summary[2];
  uint   rtsDetMask ;
  uint eventNumber;
  uint sec;
  uint usec;
  uint flags;       // bit 0 set, tpc raw data inside
} ; 

//struct ic_gl3_send_sl3 { uint* dest_header ; uint*  dest_clusters ; uint* dest_tracks ; uint* dest_debug; };
struct ic_gl3_stat     { uint dummy   ;};       
struct ic_gl3_announce_data
{
  uint size_L3_header ;  // this can be GL3 or GL3+SL3s
  uint size_TRG_data ;
  uint size_L3_data[11] ;
};

struct ic_l4_startevent {
    uint sz;   // event size
    uint buff_id;   // event buffer id
    int local_id;
    int tmtoken;
    int evbidx;
};

struct ic_l4_internal {
    int cmd;
    int buff_id;
    int idx;
};

/*
struct ic_l4_event {
    uint l2trg_lo;
    uint l2trg_hi;
    uint l4trg_lo;   // lo 32 bits of the trigger mask
    uint l4trg_hi;   // hi 32 bits of the trigger mask
    uint buff_id;  // event buffer id for the hlt data
    uint evt_sz;   // sz of the original event
    uint l4_sz;    // sz of the l4 output buffer
    int local_id;
    int tmtoken;
    int evbidx;
    int writeTracks;
};
*/

// Payloads for the l4 taping commands
struct ic_l4_evt_descriptor {
    uint buff_id;
    uint sz;
    uint disk;
    uint fd;
};


//struct ic_announce_trg_sum {uint* addr ;};
// SB
struct ic_sb_announce_sl3{ uint size_of_header ; uint size_of_clusters ; uint size_of_tracks ; uint size_of_debug ; } ;
struct ic_sb_announce_formatted_data
{
  uint size_header ;
  uint size_data[12] ; // size is in long words 
} ;
struct ic_rcv_stat{ uchar res ; uchar mz_A ;uchar mz_B ;uchar mz_C ;};
struct ic_sb_stat { ic_rcv_stat rcv[12] ; } ; 

struct ic_sb_confirm_sl3 { uint dummy   ;};

//struct ic_bb_announce_event {
//  uint size;
//  uint token;
//};
//struct ic_bb_announce_data { 
//  uint buffer;
//  uint offset;               // Offset of this buffer in the event
//  uint size;                 // Must be <= BB_STAGE_BUFF_SZ
//};
struct ic_bb_send_data {
  uint buffer;
  uint addr_hi;
  uint addr_lo;
};
//struct ic_bb_confirm_send {  // use with CONFIRM_SEND command
//  uint buffer;
//  uint last;
//};
//struct ic_bbm_announce_data {
//  uint size;
//};
//struct ic_bbm_send_data {
//  uint offset;
//  uint shm;
//  int size;
//};
//struct ic_qdsend_announce_chunk {
//  uint chunkwords;      // For next chunk, 0 if no more
//  uint last_chunkwords; // was there a previous send/?
//  uint source_hi;      // just gets passed back to sender
//  uint source_lo;
//  uint final_dest_hi;  // rcv saves this 
//  uint final_dest_lo;
//};
//struct ic_qdsend_send_chunk {
//  uint addr_hi;                  // Source address's (actual)
//  uint addr_lo;
//  uint dest_hi;                  // Destination address's (actual)
//  uint dest_lo;                  //
//  uint chunkwords;
//};
//struct ic_qmyri_memcpy_start {
//  uint pTargetBuff_hi;
//  uint pTargetBuff_lo;
//  uint localBuffSegment;
//  uint localBuffOffset;
//  uint len;
//  uint targetId;
//};
//struct ic_qmyri_memcpy_done {
//  uint dummy;
//};
//struct ic_qmyri_ctl {
//  int ctl_cmd;
//  int seg;        // if MCTL_REG_MEM
//  int size;       // if MCTL_REG_MEM
//  int log_level;  // if MCTL_LOG_LEVEL
//  int daqNode;    // if MCTL_NODE
//  int myriNode;   // if MCTL_NODE
//  char name[32];  // if MCTL_NODE
//};

struct ic_evp_req_event {
  int type;               // 1=token 0, 2=physics, 4=special, 7=any...
};
struct ic_evp_announce_event {
  int size;               // words
  int type;
  int run;
  int evts_of_type;
  int evts;
  int addr;               // new... evpx protocol!
};
struct ic_evp_send_event {
  int addr_hi;
  int addr_lo;
};
struct ic_evp_confirm_event {
  int dummy;
};
struct ic_evp_event_done {
  int dummy;
};

// struct ic_rcf_write_event {
//   int run;
//   int size;
//   int shm;
//   int offset;
//   int event;
//   int type;
//   int token;
// };
struct ic_rcf_writer_flush {
  uint dummy;
};
//struct ic_rcf_list_written {
// struct rcfEventList *el;
//};
struct ic_vx_request_file {
  uint direction;
  uint addr_hi;
  uint addr_lo;
  uint size;
  char filename[92];    // 120 - (4 * 4) - (4 * 3)
};
// struct ic_rcf_confirm_event {
//   int run;
//   int size;
//   int shm;
//   int offset;
//   int event;
// };

struct ic_rcf_get_file
{
  int dummy;
};
struct ic_rcf_write_file
{
  int disk;
  char filename[100];
};
struct ic_rcf_release_file
{
  int disk;
  char filename[100];
};
struct ic_spool_get_disk
{
  int run;
  int seq;
};
struct ic_spool_use_disk
{
  int disk;
  int throttleRate;
};
struct ic_spool_free_disk
{
  int disk;
  int run;
  int seq;
};


struct TokenManagerIccpPayload {
    int sz;
    int detLocalId;  // ID passed to and from dets...
    int dummy;
    UINT64 rtsMask;
};

struct TokenManagerResults {
    int TknMgrEventNumber;
    int evbNodeIdx;
    int detLocalId;  // ID passed to and from dets...
};

//-----======------||||||------~~~~~~-----======------||||||------~~~~~~
union ic_load
{
  uint  dword[27] ;
  ushort word[54] ;
  uchar byte[108]  ;                                      // this is the max you have
#if !(defined(NOT_DAQ) || defined(alpha))
  ic_cmd_ping                      cmd_ping ;
  ic_cmd_response                  cmd_rsp ;
  ic_cmd_ack                       cmd_ack ;
  ic_cmd_cancel                    cmd_cancel ;
  ic_cmd_req_stat                  cmd_req_stat ;
  ic_cmd_release_token             cmd_release_token;   
  ic_cmd_confirm_release           cmd_confirm_release ;
  ic_cmd_confirm_formatted_data    cmd_confirm_data    ;
  ic_confirm_send                  confirm_send;

  ic_evb_stat                      evb_stat;                       // I'll send my status (Iam EVB)
  ic_evb_format_data               evb_format ;    
  ic_evb_send_data                 evb_send_data;               
  ic_det_announce_data             det_announce_data;

  ic_evb_announce_memcpy2          evb_announce_memcpy2;
  ic_rts_write_counters            rts_write_counters ;

  ic_evb_tape_request              evb_tape_request ;
  ic_spool_event_written           spool_event_written ;
  ic_spool_write_event             spool_write_event;
 
  ic_taper_done                    taper_done ;

  ic_evbx_tape_request              evbx_tape_request ;
  ic_evbx_spool_event_written           evbx_spool_event_written ;
  ic_evbx_spool_write_event             evbx_spool_write_event;

  
  ic_daq_run_start                 daq_run_start ;
  ic_daq_run_stop                  daq_run_stop ;
  ic_daq_run_pause                 daq_run_pause ;
  ic_daq_run_resume                daq_run_resume ;
  ic_daq_send_config               daq_send_config ;
  ic_daq_flush_tokens              daq_flush_tokens;

  ic_reboot                        reboot ;
  ic_drc_run_start                 drc_run_start ;
  ic_drc_run_stop                  drc_run_stop ;
  ic_drc_run_pause                 drc_run_pause ;
  ic_drc_run_resume                drc_run_resume ;
  ic_drc_get_config                drc_get_config ;
  ic_drc_set_config                drc_set_config ;
  ic_drc_send_config               drc_send_config ;
  ic_drc_flush_tokens              drc_flush_tokens ;
  ic_drc_query_tokens              drc_query_tokens ;
  ic_mz_emul_fiber                 mz_emul_fiber ;
  
  ic_gb_build                      gb_build ;
  
  ic_gl3_build                     gl3_build  ;   
  
  //ic_gl3_send_sl3                  gl3_snd_sl3 ;
  
  ic_gl3_stat                      gl3_stat ;
  ic_gl3_announce_data             gl3_announce_data ;
  //ic_announce_trg_sum              announce_trg_sum;               // this is not a nice name !!! 

  ic_sb_announce_sl3               sb_announce_sl3 ;
  ic_sb_announce_formatted_data    sb_announce_data ;              // this is for the evb
  ic_sb_stat                       sb_stat ;
  ic_sb_confirm_sl3                sb_confirm_sl3     ;            // I have send the SL3 results
  //ic_bb_announce_event             bb_announce_event;
  //ic_bb_announce_data              bb_announce_data;    
  ic_bb_send_data                  bb_send_data;               // use by vxFileRequest Server
  //ic_bb_confirm_send               bb_confirm_send;
  //  ic_rcf_write_event               rcf_write_event;
  ic_rcf_writer_flush              rcf_writer_flush;
  //ic_rcf_list_written              rcf_list_written;
  //  ic_rcf_confirm_event             rcf_confirm_event;
  //  ic_bbm_send_data                 bbm_send_data;
  //ic_bbm_announce_data             bbm_announce_data;
  

  //ic_qmyri_memcpy_start            qmyri_memcpy_start;
  //ic_qmyri_memcpy_done             qmyri_memcpy_done; 
  //ic_qmyri_ctl                     qmyri_ctl;
  
  ic_evp_req_event                 evp_req_event;
  ic_evp_announce_event            evp_announce_event;
  ic_evp_send_event                evp_send_event;
  ic_evp_confirm_event             evp_confirm_event;
  ic_evp_event_done                evp_event_done;
  
  ic_vx_request_file               vx_request_file;

  ic_spool_list_written            spool_list_written;
  ic_spool_write_list              spool_write_list;

  ic_rcf_get_file                  rcf_get_file;
  ic_rcf_write_file                rcf_write_file;
  ic_rcf_release_file              rcf_release_file;
  ic_spool_get_disk                spool_get_disk;
  ic_spool_use_disk                spool_use_disk;
  ic_spool_free_disk               spool_free_disk;

  ic_eth_announce                  eth_announce;
 
    ic_l4_internal                 l4_internal;
    ic_l4_startevent                 l4_startevent;
    //ic_l4_event l4_event;
    ic_l4_evt_descriptor l4_evt_descriptor;

    //TokenManagerResults tokenManagerResults;
    //TokenManagerIccpPayload tokenManagerIccpPayload;
  
#endif /* NOT_DAQ */
  //  ic_qdsend_announce_chunk         qdsend_announce_chunk;
  // ic_qdsend_send_chunk             qdsend_send_chunk;
};
//-----======------||||||------~~~~~~-----======------||||||------~~~~~~
#include  "iccpHeader.h"
struct ic_msg
{
  ic_msg_head head ;
  ic_load ld ;                 // for practical reasons  I'll  keep it short
} ;

#pragma pack()


#include "rtsSystems.h"

#endif
