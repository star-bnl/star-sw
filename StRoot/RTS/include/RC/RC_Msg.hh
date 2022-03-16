#ifndef RC_MSG_HEADER
#define RC_MSG_HEADER
 
#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include "iccp.h"
#include "RC_MsgDefs.h"

#ifdef DEFINE_ONLINE
#include <string>
#endif

class RcComInfo;

struct MsgEnvHeader
{
  RcComInfo *origin;
  u_int Seq;            // a sequence number - connection_seq
                        // seq by value! starts out same as from origin
};

MsgEnvHeader msg_env(RcComInfo *info);

class RcMsg
{
public:
  MsgEnvHeader envelope;    // wrapped around external communication
  ic_msg_head head;         // iccp header
  union
  {
    // Generic types
    char data[RCDATASIZE];   
    uchar byte[RCDATASIZE];
    uint dword[RCDATASIZE/4];
    ushort word[RCDATASIZE/2];

    // Message types
    // Internal Handler commands
    rc_daq_connect          daq_connect;
    rc_daq_error            daq_error;

    // handler --> rc
    rc_cmd_response         cmd_response;
    rc_drc_send_error       drc_send_error;

    // rc --> handler
    rc_drc_connect          drc_connect;
    rc_cmd_nop              cmd_nop;
    rc_drc_ping             drc_ping;
    rc_drc_stophandler      drc_stophandler;
    rc_drc_clear_handler    drc_clear_handler;
    rc_drc_querysystem      drc_querysystem;
    rc_drc_get_systems      drc_get_systems;
    rc_drc_get_threads      drc_get_threads;
    rc_cmd_timeout          cmd_timeout;
    rc_drc_reconfig         drc_reconfig;
    rc_drc_add_component    drc_add_component;
    rc_cmd_reboot           cmd_reboot;
    rc_drc_run_start        drc_run_start;
    rc_drc_run_stop         drc_run_stop;
    rc_drc_run_pause        drc_run_pause;
    rc_drc_run_resume       drc_run_resume;
    rc_drc_set_config       drc_set_config;
    rc_drc_get_config       drc_get_config;
    rc_drc_send_config      drc_send_config;
    rc_drc_set_busy         drc_set_busy;
    rc_drc_release_busy     drc_release_busy;
    rc_drc_flush_tokens     drc_flush_tokens;
    rc_drc_query_tokens     drc_query_tokens;
    rc_drc_client_control   drc_client_control;
    rc_drc_reconnect        drc_reconnect;
    rc_drc_get_errors       drc_get_errors;
    rc_drc_clear_errors     drc_clear_errors;
    rc_drc_update_query     drc_update_query;

    // system --> Handler
    rc_cmd_ack              cmd_ack;
    rc_cmd_ping             cmd_ping;
    rc_daq_run_start        daq_run_start;
    rc_daq_run_stop         daq_run_stop;
    rc_daq_run_pause        daq_run_pause;
    rc_daq_run_resume       daq_run_resume;
    rc_daq_send_config      daq_send_config;
    rc_daq_set_busy         daq_set_busy;
    rc_daq_release_busy     daq_release_busy;
    rc_daq_flush_tokens     daq_flush_tokens;
    rc_daq_query_tokens     daq_query_tokens;
    rc_mz_emul_fiber        mz_emul_fiber;
    rc_rts_reconnect        rts_reconnect;
    rc_drc_monitor_send     drc_monitor_send;
  };

  void AddEnvelope(const MsgEnvHeader& env)
    {
      envelope = env;
    };

  void make_LOG_DATA(char *str);
  void Clear(const MsgEnvHeader& env, unsigned char st, 
	     unsigned char dt, unsigned char cmd=0);
  void Write(FILE* fd);
  int Rcv(u_int fd, bool handshake=false) ;
  int Snd(u_int fd, bool handshake=true) ;
  int SockPeekHeader(u_int fd, u_int timeout);   // Peek at a socket (timeout uSec)
  void Dump(FILE* fd);
};

#endif

