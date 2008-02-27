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
