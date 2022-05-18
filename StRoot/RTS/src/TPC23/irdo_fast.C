#include <stdio.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <pthread.h>
#include <math.h>

#include <rtsLog.h>
#include <DAQ1000/rb.hh>

#include <SUNRT/ipcQLib.hh>

#include <TEF_RDO/rb_c.h>

#include "itpc23.h"

#define FEE_40_PHASE	80

// Argh, need this silly class so I can have the right logging before other classes 
// run their constructors
class dummy_c {
public:
	dummy_c() { 
		rtsLogAddDest(RTS_DAQMAN,RTS_LOG_PORT_DET) ;
		rtsLogOutput(RTS_LOG_STDERR|RTS_LOG_NET) ;
	} ;

	~dummy_c() {;} ;
} ;

// MUST be first!
static dummy_c dummy ;


static rb_c *rb ;	// the one!

static u_int fee_mask ;
static int fee_words ;
//static int trg_evts ;
static int occupancy ;

struct wrk_t {
	char *c_addr ;
	int words ;
	int evt ;
} ;

static ipcQClass *que[8] ;
static itpc23 *it[8] ;

static u_int fee_map[16] ;

#if 0
static const char *hwicap_version(u_int v) ;
#endif

// Helpers
static time_t t_mark()
{
        timespec ts ;

        clock_gettime(CLOCK_MONOTONIC,&ts) ;

        return ts.tv_sec*1000000000+ts.tv_nsec ;
}

//returns microseconds
static u_int t_delta(time_t mark)
{
        return (unsigned int)((t_mark() - mark)/1000) ;
}

static void *ana_thread(void *v)
{
	int id = (int)(long)v ;
	struct wrk_t w ;

	LOG(INFO,"Starting ana_thread %d",id) ;

	for(;;) {

		que[id]->receive(&w,sizeof(w),1) ;

		it[id]->evt_start() ;

		it[id]->rdo_scan(w.c_addr,w.words) ;

		it[id]->evt_stop() ;
		
		rb->free(w.c_addr) ;

//		ana_evt(w.evt,w.c_addr,w.words) ;
	}

	return 0 ;
}



static int rd_log(char *s)
{
	int cou = 0 ;
	int tmout = 0 ;

	for(;;) {
		u_int wa ;

		if(rb->rd(&wa,1)==1) {
			switch(wa) {
			case 0x00009800 :
			case 0x00003000 :
				break ;
			default :
				if((wa&0xFFFFFF00)==0xF500) {	// ASCII
					u_char c = wa & 0xFF ;
					cou++ ;

					//LOG(WARN,"c = %c",c) ;

					if(c==0x0A) {
						*s++ = 0 ;
						return cou ;
					}
					else {
						*s++ = c ;
					}				}
				else {
					LOG(ERR,"0x%08X",wa) ;
				}
			}
		}
		else {
			tmout++ ;
			if(tmout==1000000) {
				*s++ = 0 ;
				return cou ;
			}
		}
	}
}

int rb_flush(int us_wait=0)
{
	if(us_wait==0) us_wait=300000 ;

	rb->wr_var("c5\n") ;

	time_t mark = t_mark() ;

	for(int i=0;i<us_wait;i++) {
		char s[256] ;
		if(rd_log(s)) {
			char err = 0 ;

			
			if(strstr(s,"ECHO")) break ;

			if(strstr(s,"ERR")) err = 1;

			if(err) {
				fprintf(stderr,"%s>>> %u: %s%s\n",ANSI_RED,t_delta(mark),s,ANSI_RESET) ;
			}
			else {
				fprintf(stderr,"%s>>> %u: %s%s\n",ANSI_BLUE,t_delta(mark),s,ANSI_RESET) ;
			}
			fflush(stderr) ;
		}
	}
	
	fprintf(stderr,"%s>>> %u: %s%s\n",ANSI_BLUE,t_delta(mark),"rb_flush",ANSI_RESET) ;

	
	return 0 ;
}


int main(int argc, char *argv[])
{
	int c ;
	int rb_ix = 1 ;	// assume 1st RDO
	int events_max = 100 ;
	int busy_evts ;
	int sc_want = 0 ;

	// statics
	fee_mask = 0x0001 ;	// 1 is the default
	fee_words = 0 ;		// assume real SAMPA
	occupancy = 5 ;		// amounts to 940 words/event for 1 FEE, or 35 words10 from SAMPA
	busy_evts = 4 ;		// keep it sane...

	while((c=getopt(argc,argv,"R:e:f:w:p:d:b:S"))!=EOF) {

	switch(c) {
	case 'R' :
		rb_ix = atoi(optarg) ;
		break ;
	case 'e' :
		events_max = atoi(optarg) ;
		break ;
	case 'f' :
		sscanf(optarg,"0x%X",&fee_mask) ;
		break ;
	case 'w' :
		fee_words = atoi(optarg) ;
		break ;
	case 'p' :
		occupancy = atoi(optarg) ;
		break ;
	case 'd' :
		rtsLogLevel(optarg) ;
		break ;
	case 'b' :
		busy_evts = atoi(optarg) ;
		break ;
	case 'S' :
		sc_want = 1 ;
		break ;
	}
	}

	LOG(INFO,"irdo_fast: %s %s",__DATE__,__TIME__) ;
	LOG(INFO,"RDO %d, fee_mask 0x%04X, fee_words %d",rb_ix,fee_mask,fee_words) ;

	int tef_focus = 0 ;

	for(int i=0;i<4;i++) {
		if(i==(rb_ix-1)) {
			rb = new rb_c(tef_focus,i%4,32,1024*1024) ;
			break ;
		}
	}

	it[0] = new itpc23 ;

	it[0]->sector1 = 1 ;
	it[0]->rdo1 = rb_ix ;
	it[0]->fee_words = fee_words ;
	it[0]->fee_mask = fee_mask ;
	it[0]->log_level = 5 ;
	it[0]->run_type = 1 ;
	it[0]->online = 1 ;
	it[0]->fmt = 22 ;

	it[0]->set_rdo(1,rb_ix) ;
	fee_mask = it[0]->fee_mask ;

	que[0] = new ipcQClass(0x10,1,0) ;

	pthread_t tid ;
	pthread_create(&tid,0,ana_thread,(void *)0) ;

	// init
	rb->disable = 0 ;
	rb->realtime = 1 ;
	rb->sector_id = 1 ;	// make it non-trivial
	rb->rdo_id = rb_ix ;

	memset(fee_map,0,sizeof(fee_map)) ;
	FILE *f = fopen("fee_map.txt","r") ;
	if(f==0) {
		LOG(ERR,"Can't open fee_map.txt") ;
	}
	else {
		while(!feof(f)) {
			int port, wire1, padplane ;

			fscanf(f,"%d 0x%X %d",&port,&wire1,&padplane) ;

			fee_map[port] = wire1 ;
		}
		fclose(f) ;
	}

	for(int i=0;i<16;i++) {
		LOG(TERR,"fee_map: port %2d = 0x%08X",i,fee_map[i]) ;
	}

	LOG(INFO,"Started on RDO %d",rb_ix) ;

	rb->gtp_clear_link() ;
	rb->gtp_status(0) ;

	rb->wr_var("mw 0xD,0x%X\n",0) ;	// stop previous run, if any

	rb->gtp_start(1) ;	// start ASCII

	rb->wr_var("cE 1\nc4 0\n") ;	// no echo, no prompt


	rb->wr_var("b\n") ;			// get something out of the iRDO
	rb_flush() ;
	
	rb->wr_var("cr 1\n") ;	// use TCD clock
	rb->wr_var("cT 2\n") ;	// TCD triggers
	rb->wr_var("mw 0x6,0x%X\n",fee_words) ;	// words per FEE for simulated: before FEE machinations!
	rb_flush() ;

	// load FEEs
	rb->wr_var("mc 0x3,0x%X\n",12) ;	// clear FEE XOFF state to OFF

	rb->wr_var("cF 0x%X\n",0xFFFF) ;	// set FEE mask to all 0xFFFF
	rb->wr_var("Fp\n") ;			// power up FEEs
	rb->wr_var("Yy %d\n",FEE_40_PHASE) ;	// FEE phase: usually 50
	rb->wr_var("Y5 %d\n",0) ;		// RHICx5 phase
	rb_flush() ;

	rb->wr_var("Fy 1\n") ;			// print FEE load stages
	rb->wr_var("Fc\n") ;			// load FEEs
	rb->wr_var("Fs\n") ;			// check POK etc
	rb->wr_var("FS\n") ;			// fee mask MUST be 0xFFFF before FS!
	rb_flush() ;

	LOG(INFO,"Setting FEE mask via cF to 0x%04X",fee_mask) ;

	rb->wr_var("cF 0x%X\n",fee_mask) ;	// now set the correct FEE mask

	rb->wr_var("c\n") ;	// check...
	rb_flush() ;

	rb->wr_var("mp 0x01,0x0\n") ;	// clear Trigger FIFO
	rb->wr_var("mc 0x01,0x7\n") ;	// clear LOCKstep
	rb->wr_var("mc 0x01,0x5\n") ;	// clear soft-busy
	rb->wr_var("ms 0x01,0x6\n") ;	// enable BUSY to TCD

	rb->wr_var("mw 0xC,0x%X\n",16<<4) ;	// local busy counter

	rb->wr_var("mp 0x01,0x8\n") ;		// clear busy event counter
	for(int i=0;i<busy_evts;i++) {		// and count up to required events e.g. 4
		rb->wr_var("ms 0x01,0x9\n") ;
		rb->wr_var("mc 0x01,0x9\n") ;
	}


	// dump registers
	for(int i=0;i<32;i++) {
		rb->wr_var("mr 0x%X\n",i) ;
		
	}
	rb_flush(1000000) ;

	// asign ports
	for(int i=0;i<16;i++) {
		if(fee_mask & (1<<i)) ;
		else continue ;
		
		rb->wr_var("fi1 0x%08X\n",fee_map[i]) ;	// enable access
		rb->wr_var("fcP %d\n",i) ;		// asign port
		rb->wr_var("fi+\n") ;			// done...
		
	}


	// clear ALL FEE FIFOs
	rb->wr_var("mw 0x22,0x2222\n") ;
	rb->wr_var("mw 0x23,0x2222\n") ;
	rb->wr_var("mw 0x24,0x2222\n") ;
	rb->wr_var("mw 0x25,0x2222\n") ;
	rb->wr_var("mw 0x22,0x0\n") ;
	rb->wr_var("mw 0x23,0x0\n") ;
	rb->wr_var("mw 0x24,0x0\n") ;
	rb->wr_var("mw 0x25,0x0\n") ;
	rb->wr_var("mp 0x2,0x5\n") ;	// FIFO_RST to the FEE part...

	rb->wr_var("ms 0x00,0x9\n") ;	// set running: just LEDs -- who cares
	rb_flush(100000) ;

	// configure FEE
	rb->wr_var("fcT 2\n") ;		// RDO trigger
	rb->wr_var("fcs 0\n") ;		// no standalone
	rb->wr_var("fcm 1\n") ;		// printout mode: binary
	rb->wr_var("fcf 0\n") ;		// switch off FPD
	rb->wr_var("fcd 1\n") ;		// dpcfg=1 : want ped subtstraction

	rb->wr_var("fcc %d\n",occupancy) ;	// setup SAMPA to fire 10 percent
	rb->wr_var("fcY 16\n") ;		// xoff backoff; default was 512(!)

	rb->wr_var("fsc\n") ;		// send config to FEE; will show up in the 1st triggered event!
	usleep(300000) ;
	rb_flush() ;


	rb->wr_var("fPP\n") ;		// ... and load percentage 
	usleep(300000) ;
	rb_flush() ;

	if(sc_want) ;
	else {
		rb->wr_var("mp 0x2,0x5\n") ;	// FIFO_RST to the FEE part...
	}

	rb->wr_var("fss\n") ;		// run start to FEE
	usleep(300000) ;
	rb_flush() ;

#if 0
	// clear ALL FEE FIFOs
	rb->wr_var("mw 0x22,0x2222\n") ;
	rb->wr_var("mw 0x23,0x2222\n") ;
	rb->wr_var("mw 0x24,0x2222\n") ;
	rb->wr_var("mw 0x25,0x2222\n") ;
	rb->wr_var("mw 0x22,0x0\n") ;
	rb->wr_var("mw 0x23,0x0\n") ;
	rb->wr_var("mw 0x24,0x0\n") ;
	rb->wr_var("mw 0x25,0x0\n") ;
	rb->wr_var("mp 0x2,0x5\n") ;	// FIFO_RST to the FEE part...
#endif

	rb->wr_var("ms 0x3,0x%X\n",12) ;	// set FEE XOFF state to ON: no more data sent to FEE

	// GO
	rb->wr_var("mw 0xD,0x%X\n",(rb->sector_id<<8|rb->rdo_id<<4)|0x5) ;	// start running
	rb->gtp_start(0) ;	// start binary

	int evts = 0 ;

	double words_per_sec = 0 ;
	double evts_per_sec = 0 ;
	unsigned long mark = t_mark() ;

	it[0]->run_start() ;

	for(;;) {
		int words ;
		char *c_addr ;
		u_int status ;
		int fifo_ix ;

		words = rb->get_priority_fast(&c_addr,&status,&fifo_ix) ;

		if(words) {
			wrk_t w ;

			evts++ ;


			words_per_sec += words ;
			evts_per_sec++ ;


			u_int delta = t_delta(mark) ;

			if(delta>1000000) {
				words_per_sec *= 4 ;	// turn it into bytes
				words_per_sec /= 1000000.0 ;	// into MB

				words_per_sec /= (delta*0.000001) ;
				evts_per_sec /= (delta*0.000001) ;

				LOG(INFO,"%d/%d events: %.1f MB/s, %.1f Hz",it[0]->evt_trgd,evts,words_per_sec,evts_per_sec) ;
				words_per_sec = 0 ;
				evts_per_sec = 0 ;
				mark = t_mark() ;
			}

			//if((evts%100)==1) LOG(TERR,"evt %d: fifo %d: words %d, status 0x%08X",evts,fifo_ix,words,status) ;
			//LOG(TERR,"evt %d: fifo %d: words %d, status 0x%08X",evts,fifo_ix,words,status) ;

			w.evt = evts ;
			w.c_addr = c_addr ;
			w.words = words ;

			que[0]->send(&w,sizeof(w),1) ;
			//ana_evt(evts,c_addr,words) ;

			//u_int *d = (u_int *)c_addr ;
			//for(int i=0;i<words;i++) {
			//	printf("%d/%d = 0x%08X\n",i,words,d[i]) ;
			//}

			//if(evts==40) {
			//	LOG(WARN,"Firing fss") ;
			//	rb->wr_var("fss\n") ;
			//}
			

			if(evts==events_max) {
				LOG(WARN,"Issuing stop.") ;
				rb->wr_var("mw 0xD,0\n") ;
				break ;
			}
		}
		else usleep(1000) ;
	}
	
	LOG(INFO,"Done after %d events",evts) ;
	usleep(10000) ;

	rb->gtp_start(1) ;
	rb->wr_var("b\n") ;
	rb_flush() ;

	return 0 ;
}

#if 0
const char *hwicap_version(u_int v)
{	
	static char ascii[64] ;

        int s = v & 0x3F ;
        int m = (v>>6)&0x3F ;
        int h = (v>>12)&0x1F ;
        int y = ((v>>17)&0x3F) ;
        int mo = (v>>23)&0xF ;
        int d = (v>>27)&0x1F ;


        sprintf(ascii,"%02d-%02d-%02d %02d:%02d:%02d",
                mo,d,y,h,m,s) ;

	return ascii ;
}

#endif
