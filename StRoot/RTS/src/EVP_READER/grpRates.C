#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
typedef unsigned int UINT32;
#include <SVT/key_map.h>

//#define NO_RTS_LOG
//#define STAR_OFFLINE_ENVIRONMENT

#include <rtsLog.h>
#include <TPC/rowlen.h>
#include <TPC/trans_table.hh>
#include <evpReader.hh>
#include <rtsSystems.h>

extern int sanityCheck(char *datap) ;

char evpgroups[32][32];

void readevpgroups()
{
  memset(evpgroups,0,sizeof(evpgroups));

  FILE *f = fopen("/RTS/conf/handler/evpGroups.txt","r");
  if(!f) return;

  char s[100];
  char *ss;

  while((ss = fgets(s,100,f)) != NULL) {
    if(ss[0] == '#') continue;

    int id;
    char sbuff[100];

    int ret = sscanf(s,"%d %s", &id, sbuff);
    if(ret != 2) continue;
    strcpy(evpgroups[id],sbuff);
  }
}


int main(int argc, char *argv[])
{
  char *mem ;
  int ret ;
  char *datap ;
  class evpReader *evp = NULL ;


  rtsLogLevel(WARN) ;
  rtsLogOutput(RTS_LOG_STDERR) ;



  if(argc != 2) {
    printf("jeff filename\n");
    return 0;
  }

  evp = new evpReader(argv[1]) ;

  // MUST CHECK THE STATUS!!!
  // as well as the variable itself!
  if(evp == NULL) {
    fprintf(stderr,"Constructor for the evpReader failed for file %s!?\n",argv[1]) ;
    return -1 ;
  }

  if(evp->status) {	// error in constructor    
    fprintf(stderr,"Error initializing reader for file %s!\n",argv[1]);
    return -1 ;
  }
  

  // set the evp.star mountpoint i.e. if evp.star is mounted in your local
  // /net/evp.star then mountpoint="/net/evp.star"
  evp->setEvpDisk("/") ;
  

  // The EVENT LOOP!!!!
  
  int end_of_file = 0;

  int ftime = 0;
  int ltime = 0;
  int l0ctrs[32];
  int gctrs[32];

  memset(l0ctrs, 0, sizeof(l0ctrs));
  memset(gctrs, 0, sizeof(gctrs));
  readevpgroups();

  for(int eeee=1;;eeee++) {

    mem = evp->get(0) ;	// get the next event!
    
    //printf("mem=0x%x (%d)",mem,eeee);

    if(mem == NULL) {	// event not valid
      
      LOG(DBG, "mem == NULL %d\n",evp->status);

      switch(evp->status) {
      case EVP_STAT_OK :	// should retry as fast as possible...
	LOG(DBG, "mem=NULL but stat_ok?");
	continue ;
	
      case EVP_STAT_EOR :	// EOR or EOR - might contain token 0!
	LOG(DBG, "End of run");
	end_of_file = 1 ;
	break ;
	
      case EVP_STAT_EVT :
	LOG(WARN,"Problem getting event - skipping") ;
	continue ;
      case EVP_STAT_CRIT :
	LOG(CRIT,"Critical error - halting...",0,0,0,0,0) ;
	return -1 ;
      }
    }

    //printf("end=%d\n",end_of_file);

    if(end_of_file) break ;	// go to next file
    
    
    //       LOG("JEFF", "event_number=%d seq=%d readall_lastevt=%d",
    // 	  evp->event_number, evp->seq, evp->readall_lastevt);
    
    //       LOG(NOTE,"**** Event %d: bytes %d, token %d, trg_cmd %d, FILE %s",evp->event_number,evp->bytes,
    // 	  evp->token,evp->trgcmd,(int)evp->file_name) ;
    
    // make humanly readable time from the UNIX time...
    char *stime ;
    stime = ctime((long int *)&evp->evt_time) ;
    *(stime+strlen(stime)-1) = 0 ;
    
    //       LOG(NOTE,"     Trigger Word 0x%02X, time %u (%s), daqbits 0x%04X, evpgroups 0x%04X",evp->trgword,evp->evt_time,(int)stime,evp->daqbits,evp->evpgroups) ;
    
    //       LOG(DBG,"     Dets: 0x%x/0x%x    evpgroups: 0x%x/0x%x",
    // 	  evp->detectors,evp->detsinrun,evp->evpgroups,evp->evpgroupsinrun,0);
    
//     printf("%s:  evt=%d seq=%d trgs=0x%04x grps=0x%04x\n",
// 	   stime,
// 	   evp->event_number,
// 	   evp->seq,
// 	   evp->daqbits,
// 	   evp->evpgroups);

    if(evp->event_number == 1) {
      ftime = evp->evt_time;
    }

    ltime = evp->evt_time;

    for(int i=0;i<32;i++) {
      if(evp->daqbits & (1<<i)) {
	l0ctrs[i]++;
      }
      if(evp->evpgroups & (1<<i)) {
	gctrs[i]++;
      }
    }
  }

  
  for(int i=0;i<32;i++) {
    if(gctrs[i]) {
      double l0 = l0ctrs[i];
      double g = gctrs[i];
      double t = ltime-ftime;
      
      printf("[%32s (%d)] rate %6.3lf\n",evpgroups[i],i,g/t);

    }
  }
}
