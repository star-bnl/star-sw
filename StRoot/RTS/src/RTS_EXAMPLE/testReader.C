//******************************************************
//*** Chops events out of .daq file 
//*** 
//*** usage:   daqFileChopper fn.daq arglist
//***
//***          arglist is passed to the function
//***          FilterEvent(), which returns true
//***          if the event is to be saved
//***     
//***          output goes to standard out...
//*****************************************************

#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include <sys/types.h>
#include <stdlib.h>

#include <rtsLog.h>	// for my LOG() call
#include <rtsSystems.h>

// this needs to be always included
#include <DAQ_READER/daqReader.h>
#include <SUNRT/clock.h>

int main(int argc, char *argv[])
{
  rtsLogOutput(RTS_LOG_STDERR) ;
  rtsLogLevel(WARN) ;

  RtsTimer timer;

  char *fn = NULL;
  if(argc > 1) {
    fn = argv[1];
  }
  
  daqReader *evp;
  evp = new daqReader(fn) ;	// create it with the filename argument..

  int good=0;
  int bad=0;

  double t;

  for(;;) {
    timer.reset();
    //char *ret = evp->skip_then_get(4700,0,EVP_TYPE_ANY);
    char *ret = evp->get(0,EVP_TYPE_ANY);
    t = timer.currtime();

    if(ret) {
      if(evp->status) {
	LOG(ERR,"evp status is non-null [0x08X, %d dec]",evp->status,evp->status) ;
	continue ;
      }
      good++;
      LOG("JEFF", "Got event in %lf seconds",t);
    }
    else {    // something other than an event, check what.
      switch(evp->status) {
      case EVP_STAT_OK:   // just a burp!
	LOG("JEFF", "Got empty in %lf seconds",t);
	continue;

      case EVP_STAT_EOR:
	LOG("JEFF", "Got   EOR in %lf seconds", t);
	break;

      case EVP_STAT_EVT:
	bad++;
	LOG("JEFF", "Got   ERR in %lf seconds", t);
	continue;

      case EVP_STAT_CRIT:
	LOG(CRIT,"evp->status CRITICAL (?)") ;
	return -1;
      }
      break;
    }


    printf("evp->seq=%d n=%d\n",evp->seq,evp->event_number);
  }
  
  delete evp ; 
  return 0 ;
}
