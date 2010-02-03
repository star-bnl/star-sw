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
#include <DAQ_READER/daq_dta.h>

void displayHelp()
{
    LOG(ERR,"Usage:  daqFileChopper filename <filterlist>");
    LOG(ERR,"        filterlist -->  -trg xxx yyy zzz...  // list of offline trigger bits");
    LOG(ERR,"                        -dtrg xxx yyy zzz... // list of daq trigger bits");
    LOG(ERR,"                        -eventnum xxx yyy... // list of event numbers");
}

// Example event filter...
//
// This assumes that the argv list is made up from event numbers.
// and passes any event matching the event number.
int FilterEvent(daqReader *rdr, int nargs, char *argv[])
{
  int eventNumber = rdr->seq;

  if(strcmp(argv[0], "-trg")==0) {
    unsigned int bits = rdr->daqbits;
    for(int i=0;i<32;i++) {
      if(bits & (1<<i)) {
	int trgSat = rdr->getOfflineId(i);
      
	for(int j=1;j<nargs;j++) {
	  if(trgSat == atoi(argv[j])) {
	    return 1;
	  }
	}
      }
    }
  }
  else if(strcmp(argv[0], "-dtrg")==0) {
    unsigned int bits = rdr->daqbits;
    for(int i=1;i<nargs;i++) {
      int trg = atoi(argv[i]);
      if(bits & (1<<trg)) return 1;
    } 
  }
  else if(strcmp(argv[0], "-eventnum")==0) {
    for(int i=1;i<nargs;i++) {
      if(eventNumber == atoi(argv[i])) {
	return 1;
      }
    }
  }
  else {
    displayHelp();
    exit(0);
  }
  return 0;
}

int main(int argc, char *argv[])
{
  rtsLogOutput(RTS_LOG_STDERR) ;
  rtsLogLevel(WARN) ;

  if(argc < 3) {
    displayHelp();

    exit(0);
  }

  int filterArgc = argc-2;
  char **filterArgv = &argv[2];
  
  daqReader *evp;
  evp = new daqReader(argv[1]) ;	// create it with the filename argument..

  int good=0;
  int bad=0;

  for(;;) {
    char *ret = evp->get(0,EVP_TYPE_ANY);
    
    if(ret) {
      if(evp->status) {
	LOG(ERR,"evp status is non-null [0x08X, %d dec]",evp->status,evp->status) ;
	continue ;
      }
      good++;
    }
    else {    // something other than an event, check what.
      switch(evp->status) {
      case EVP_STAT_OK:   // just a burp!
	continue;
      case EVP_STAT_EOR:
	LOG(OPER, "Done after scanning %d events (%d bad)",good,bad);
	break;        // file, we're done...
      case EVP_STAT_EVT:
	bad++;
	LOG(WARN, "Problem getting event - skipping [good %d, bad %d]",good,bad);
	continue;
      case EVP_STAT_CRIT:
	LOG(CRIT,"evp->status CRITICAL (?)") ;
	return -1;
      }
    }
    		
    if(evp->status == EVP_STAT_EOR) {
      LOG(INFO,"Done after scanning %d events (%d bad)",good,bad) ;
      break; 
    }

    if(FilterEvent(evp, filterArgc, filterArgv) == 0) {
      continue;
    }
    LOG(INFO, "Keep event #%d (ptr=0x%x sz=%d)",evp->seq,evp->memmap->mem,evp->event_size);
   
    write(STDOUT_FILENO, evp->memmap->mem, evp->event_size);
  }
  
  delete evp ; 
  return 0 ;
}
