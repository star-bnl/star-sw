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

  printf("Hit enter: ");
  char str[120];
  scanf("%s",str);

  for(int i=0;i<1000;i++) {
 
      __sync_synchronize();

      daqReader *rdr =  new daqReader("/net/evb01/a/test/st_physics_18156048_raw_3500024.daq");

      __sync_synchronize();
      //char *ret = rdr->get(0,EVP_TYPE_ANY);


      __sync_synchronize();
      delete rdr;
  }

      printf("Hit enter again before delete: ");
      scanf("%s",str);


}
