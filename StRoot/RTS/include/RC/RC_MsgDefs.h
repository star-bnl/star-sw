#ifndef RC_MSGDEFS_HH
#define RC_MSGDEFS_HH
// These structures define the message parameters for communication
// with the DAQ RC handler.

// The message structure contains
// Words 1..3 ICCP header
// Words 4... Specific parameters for each command
//
// The structures in this file are only the specific parameters
// Most of these are inherited from MsgParam which contain a 
// param type which should be filled with one of the values below
#include "rc.h"

#ifdef DEFINE_ONLINE
typedef int bool;
#endif

#include <sys/types.h>
#include <iccp.h>

#define RCMSGSIZE 120
#define RCDATASIZE RCMSGSIZE-sizeof(ic_msg_head)

// Supporting enums
enum connect_direction {to_handler, from_handler};
//enum query_type {all=0, inrun=1, none=2};
enum mask_types {literal=1, bits_on=2, bits_off=3};

//----------------------------------------
// Here are the DAQ message structures....
//

// handler --> handler
struct rc_daq_connect             // Used to be ConnectorParam
{                                 // I will separate to DAQ_CONNECT / DAQ_CHASER_CONNECT
  // In all connections
  u_int fd;
  u_int addr;            
  int connection_seq;             // sequence number for the connection
  int port;                       // The port connect
};

struct rc_daq_error               // Used to be ErrorParam
{
  int error;
  int level;
};
#define PORTS_USED 1


// handler --> rc
struct rc_cmd_response        
{ 
  int cheese; 
  int cid;
};
struct rc_drc_send_error
{
  char text[RCDATASIZE];
};

// rc --> handler
struct rc_drc_connect
{
  int pid;                         // connectors pid
  connect_direction direction;
};
struct rc_cmd_nop             { uint dummy; };       //
struct rc_drc_ping            { uint dummy; };       //
struct rc_drc_stophandler     { uint dummy; };       //
struct rc_drc_clear_handler                          //
{ 
  int state; 
};
struct rc_drc_querysystem                            //
{ 
  int system;
  int crates;
  int threads;
  int status;
  int is_caller_cheese;
  bool client_threads;
  bool ping;
};
struct rc_cmd_timeout         { uint dummy; };       //
struct rc_drc_reconfig        { uint dummy; };       //
struct rc_drc_add_component
{
  char daqpath[8];
  int in_run;
  int rb_mask;
  mask_types rb_mask_type;
};
struct rc_cmd_reboot         { uint all; };
struct rc_drc_run_start
{
  int run_number;
  int num_triggers;
};
struct rc_drc_run_stop
{
  int run_number;
  int junk;
};       
struct rc_drc_run_pause
{
  int run_number;  
};      
struct rc_drc_run_resume
{
  int run_number; 
  int num_triggers;
};     
struct rc_drc_set_config
{
  int seq;
  int length;
};     
struct rc_drc_get_config
{
  int seq;
};
struct rc_drc_send_config
{
  int dummy; 
};    
struct rc_drc_set_busy
{
  int run_number;
};       
struct rc_drc_release_busy
{
  int run_number;
};   
struct rc_drc_flush_tokens
{
  int run_number;
  int type;
};   
struct rc_drc_query_tokens
{
  int num_tokens;
}; 
struct rc_drc_get_systems     
{ 
  int seq; 
  int print;
  int all;
};
struct rc_drc_get_threads
{
  int seq;
  int all;
};
struct rc_drc_get_errors
{
  int size;
};
struct rc_drc_clear_errors
{
  int dummy;
};
struct rc_drc_update_query
{
  int dummy;
};

enum cc_command_value { kill_client };
struct rc_drc_client_control
{
  cc_command_value cc_command;
  int cid;
};

struct rc_drc_reconnect
{
  int addr;
  int port;
  int system;
};

struct rc_rts_reconnect
{
  int addr;
  int port;
};

// system --> Handler
struct rc_cmd_ack             { uint dummy; };
struct rc_cmd_ping            { uint dummy; };
struct rc_daq_run_start { 
  uint run_number; 
  int num_triggers;
};

struct rc_daq_run_stop        
{
  int run_number;
  int num_files;
  int num_events;
  int junk;
};

struct rc_daq_run_pause       { uint run_number; };
struct rc_daq_run_resume      
{ 
  uint run_number; 
  int num_triggers;
};
struct rc_daq_send_config     
{ 
  u_int port; 
  u_int handler_id; 
};
struct rc_daq_set_busy        { uint run_number; };
struct rc_daq_release_busy    { uint run_number; };
struct rc_daq_flush_tokens    { uint run_number; };
struct rc_daq_query_tokens   
{ 
  uint num_tokens; 
  uint state;
  uint rb[12];
};
struct rc_mz_emul_fiber
{
  uint ntriggers;
};

struct rc_drc_monitor_send
{
  int gb_tokens_run;
  int bb_tokens_run;
  int rcc_clock;
  char file_name[20];
};

#endif  










