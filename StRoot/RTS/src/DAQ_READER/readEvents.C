#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// This source relies on the RTS "LOG" machinery to print log messages
// of misc. severities...
// If you don't like it, uncomment the following line
//#define NO_RTS_LOG
// This changes the default swwitches when running in the STAR environment
//#define STAR_OFFLINE_ENVIRONMENT

#include <rtsLog.h>

// main include file - resides in daqman.star.bnl.gov:/RTS/include
// which MUST be in your include path....

#include "daqReader.h"

int main(int argc, char *argv[])
{
  rtsLogOutput(RTS_LOG_STDERR);
  rtsLogLevel(NOTE);

  daqReader *reader;

  if(argc > 1) {
    reader = new daqReader(argv[1]);
  }
  else {
    LOG(ERR,"Need a filename...");
    exit(0);
  }

  if(reader == NULL) {
    LOG(ERR, "Error constructing daqReader with %s", argv[1]);
    exit(0);
  }

  if(reader->status) {
    LOG(ERR, "Bad status constructing daqReader with %s",argv[1]);
    exit(0);
  }

  for(int i=1;;i++) {
    char *mem = reader->get(0,0);
    if(mem == NULL) {
      LOG(ERR, "Event %d not valid");
      exit(0);
    }

    if(reader->status == EVP_STAT_EOR) {
      LOG(NOTE, "End of run");
      exit(0);
    }

    if(reader->status != EVP_STAT_OK) {
      LOG(ERR, "Error on %dth event...");
      continue;
    }

    LOG(INFO,"**** Event %d (file: %d): bytes %d, token %d, FILE %s",reader->seq, reader->event_number,reader->bytes,
	reader->token,(int)reader->file_name) ;

    LOG(NOTE,"**** Event %d: bytes %d, token %d, trg_cmd %d, FILE %s",reader->event_number,reader->bytes,
	reader->token,reader->trgcmd,(int)reader->file_name) ;
    
		

    // make humanly readable time from the UNIX time...
    char *stime ;
    stime = ctime((long int *)&reader->evt_time) ;
    *(stime+strlen(stime)-1) = 0 ;
    
    LOG(NOTE,"     Trigger Word 0x%02X, time %u (%s), daqbits 0x%04X, evpgroups 0x%04X",reader->trgword,reader->evt_time,(int)stime,reader->daqbits,reader->evpgroups) ;
    

  }
}
