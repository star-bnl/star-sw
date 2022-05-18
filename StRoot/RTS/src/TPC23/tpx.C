#include <stdio.h>
#include <sys/types.h>
#include <getopt.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>


#include <rtsLog.h>

#include <SUNRT/ipcQLib.hh>

#include <rtsSystems.h>
#include <DAQ1000/rdo_cmds.h>
#include <DAQ1000/ddl_struct.h>

//#include <DDB/rb_main_c.h>

#include <DDB/rb_tpx_c.h>

#include "tpx23.h"



struct ddb_tt {
	char sector ;
	char use_local_clock ;
	char run_type ;
	char rb_max_count ;

	u_short rb_mask ;
	u_short rb_fifo_cou ;
	u_short phy_mask ;

	u_short node_id ;

	u_short wrkr_cou ;

	u_int run ;

	const char *rts_name ;

	rb_c *crb[8] ;
} ddb ;


static tpx23 *it[16] ;

static u_int tpx_rdo_run_start(int rdo1)
{
	u_int ret ;
	rb_tpx_c *rb = (rb_tpx_c *)ddb.crb[rdo1-1] ;

	LOG(INFO,"%d: rdo_run_start",rdo1) ;
	ret = rb->start() ;

	return ret ;
} 

static void *tpx_rdo_config(void *v)
{
	int rdo1 = (int)(long)v ;
	int r = rdo1 - 1 ;
	rb_tpx_c *rb = (rb_tpx_c *) ddb.crb[r] ;

	LOG(INFO,"%d: rdo_config",rdo1) ;

	u_int err = rb->get_link_status(0) ;

	if(err) {
		LOG(ERR,"link_status 0x%08X",err) ;
		return (void *)-1 ;
	}
	else {
		LOG(INFO,"link_status 0x%08X",err) ;
	}

	err = rb->wr_var("get id") ;

	int rdo = (err >> 12)&0xFF ;
	int sec = (err >> 20)&0xFF ;

	LOG(INFO,"get id ret: 0x%08X: sector %d, rdo %d",err,sec,rdo) ;
	if(err & 0x80000000) return 0 ;


	err = rb->wr_var("set reconfig 0xabc0") ;
	usleep(3000000) ;
	LOG(INFO,"set reconfig: ret 0x%08X",err) ;

	err = rb->wr_var("get id") ;

	rdo = (err >> 12)&0xFF ;
	sec = (err >> 20)&0xFF ;

	LOG(INFO,"get id: ret 0x%08X: sector %d, rdo %d",err,sec,rdo) ;

	err = rb->wr_var("set reconfig 1") ;
	usleep(7000000) ;
	LOG(INFO,"set reconfig: ret 0x%08X",err) ;

	err = rb->wr_var("get id") ;

	rdo = (err >> 12)&0xFF ;
	sec = (err >> 20)&0xFF ;

	LOG(INFO,"get id: ret 0x%08X: sector %d, rdo %d",err,sec,rdo) ;
	if(err & 0x80000000) return 0 ;

	err = rb->wr_var("set id %d %d",rb->sector_id,rb->rdo_id) ;
	LOG(INFO,"set id: ret 0x%08X",err) ;

	err = rb->wr_var("get id") ;

	rdo = (err >> 12)&0xFF ;
	sec = (err >> 20)&0xFF ;

	LOG(INFO,"get id: ret 0x%08X: sector %d, rdo %d",err,sec,rdo) ;
	if(err & 0x80000000) return 0 ;
	

	err = rb->wr_var("read log") ;
	LOG(INFO,"read log: ret 0x%08X",err) ;


//	err = rb->wr_var("test rdo %d",64) ;
//	LOG(INFO,"test rdo: ret 0x%08X",err) ;



	const int TPX_MAX_TB = 420 ;

	int asic_seq_lo = 2 ;	// from RC
	int asic_thr_lo = 4 ;	// from RC
	const int bc2_enabled = 0 ;	// NEVER! Buggy!
	int tail_cancellation = (ddb.run_type==1)?0:1 ;
	int dpcfg_style = (ddb.run_type==1)?0:1;	// need to look this up...
	int dpcfg_tmp = (ddb.run_type==1)?0:1 ;		// need to look this up...

	char config_buffer[128*1024] ;
	u_int *config_now = (u_int *)config_buffer ;

	config_now++ ;	// leave SPACE for the DDL command!

	// bad FEEs; 1st datum is their count, could be 0
	// followed by FEE IDs, one per u_int
//	u_int *bad_fee_cou  = config_now ;
//	config_now++ ;

//	*config_now++ = 112 ;	// bad FEE
//	*bad_fee_cou = (config_now - bad_fee_cou)-1 ;

//	LOG(WARN,"we have %d bad fees",*bad_fee_cou) ;

	*config_now++ = 0 ;	// 0 bad fees
	*config_now++ = 0xFFFFFFFF ;
	*config_now++ = TPX_MAX_TB+30 ;
	*config_now++ = 1 ;

	*config_now++ = 10 ;	// mon_wait: 10 (60 in tpx.C)
	*config_now++ = 200000 ;	// timeout_loops: 200000 (1000 in tpx.C)
	*config_now++ = 500 ;	// hearbeat_ticks: 100 (5000 in tpx.C)

	*config_now++ = ddb.run_type ;
	*config_now++ = 0xA ; //(ddb.run_type==1)?3:10 ;	// run_style ;
	*config_now++ = ddb.use_local_clock ;	//0:TCD, 1=local, 2=emulated
	*config_now++ = 4 ;	// daq10k flags

	// 32 ALTRO params
	u_int *altro = config_now ;
	memset(altro,0,32*4) ;

	altro[ALTRO_BCTHR] = (4<<10)|4 ;
	altro[ALTRO_TRCFG] = TPX_MAX_TB ;
	altro[ALTRO_DPCFG] = (dpcfg_tmp << 19) | (0<<17) | (0<<14) | (asic_seq_lo<<12) | (bc2_enabled<<11) | (0<<7) | (0<<5) | (0<<4) | dpcfg_style ;
	altro[ALTRO_ZSTHR] = asic_thr_lo ;

	altro[ALTRO_DPCF2] = (tail_cancellation<<5)|(1<<4)|0xF ;

	altro[ALTRO_K1] = 57158 ;
	altro[ALTRO_K2] = 21787 ;
	altro[ALTRO_K3] = 26699 ;
	altro[ALTRO_L1] = 7449 ;
	altro[ALTRO_L2] = 37911 ;
	altro[ALTRO_L3] = 58775 ;

	config_now += 32 ;

	err = rb->wr_var("write config %p %d",config_buffer,(char *)config_now-config_buffer) ;
	LOG(INFO,"write config: ret 0x%08X",err) ;

	sleep(2) ;	// needs time

	err = rb->wr_var("read config") ;
	LOG(INFO,"read config: ret 0x%08X",err) ;

	sleep(1) ;

	rb->wr_var("read log") ;

	rb->wr_var("get id") ;

	sleep(1) ;

//	char ped_buffer[1152*(512+4)] ;


	return 0 ;

}

int tpx_pre_clock_send_config()
{
	return 0 ;
}

int tpx_post_clock_send_config()
{
	for(int r=0;r<ddb.rb_max_count;r++) {
		if(ddb.rb_mask & (1<<r)) ;
		else continue ;

		tpx_rdo_config((void *)(long)(r+1)) ;
	}

	return 0 ;

}

int tpx_startup()
{
	ddb.rts_name = "TPX" ;
	ddb.node_id = TPX_NODES(ddb.sector) ;


	ddb.phy_mask = 0x1 ;
	ddb.rb_max_count = 1 ;
	ddb.rb_fifo_cou = 64 ;

	for(int r=0;r<ddb.rb_max_count;r++) {
		if(ddb.phy_mask & (1<<r)) ;
		else continue ;

		ddb.crb[r] = new rb_tpx_c(r/2,r%2,ddb.rb_fifo_cou,1152*1024) ;

		ddb.crb[r]->sector_id = ddb.sector ;
		ddb.crb[r]->rdo_id = r+1 ;
		ddb.crb[r]->disable = 0 ;

		ddb.crb[r]->clr_link() ;
	}

	for(int i=0;i<ddb.wrkr_cou;i++) {	// workers!
		it[i] = new tpx23 ;
		it[i]->sector1 = ddb.sector ;
		it[i]->rdo1 = 1 ;		// to have something
		it[i]->id = i ;
	}

	return 0 ;
}

int tpx_run_start()
{
	LOG(INFO,"tpx_run_start") ;

	for(int r=0;r<ddb.rb_max_count;r++) {
		if(ddb.rb_mask & (1<<r)) ;
		else continue ;

		tpx_rdo_run_start(r+1) ;
	}

	for(int i=0;i<ddb.wrkr_cou;i++) {	// workers!
		it[i]->run_start();
	}

	return 0 ;
}

int tpx_run_stop()
{
	LOG(INFO,"tpx_run_stop") ;

	for(int r=0;r<ddb.rb_max_count;r++) {
		if(ddb.rb_mask & (1<<r)) ;
		else continue ;

		rb_tpx_c *rb = (rb_tpx_c *)ddb.crb[r] ;

		rb->stop() ;
	}

	for(int i=0;i<ddb.wrkr_cou;i++) {	// workers!
		it[i]->run_stop();
	}

	return 0 ;
}

int tpx_rdo_scan(char *c_addr, u_int words)
{
	it[0]->rdo1 = 1 ;
	return it[0]->rdo_scan(c_addr,words) ;
}

static ipcQClass *que[8] ;
struct wrk_t {
	char *c_addr ;
	int words ;
	int evt ;
	int fifo_ix ;
} ;

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

		LOG(TERR,"received words %d from fifo %d",w.words,w.fifo_ix) ;

		it[id]->rdo1 = 1 ;
//		it[id]->rdo_scan(w.c_addr,w.words) ;

		ddb.crb[0]->ret_fifo(w.fifo_ix) ;
	}

	return 0 ;
}



int main(int argc, char *argv[])
{
	rtsLogOutput(RTS_LOG_STDERR) ;
//	rtsLogLevel(NOTE) ;
	int c ;

	que[0] = new ipcQClass(0x10,1,0) ;
	pthread_t tid ;
	pthread_create(&tid,0,ana_thread,(void *)0) ;


	memset(&ddb,0,sizeof(ddb)) ;

	ddb.sector = 1 ;
	ddb.wrkr_cou = 1 ;

	tpx_startup() ;

	ddb.run_type = 1 ;
	ddb.use_local_clock = 0 ;
	ddb.run = 11000000 ;
	ddb.rb_mask = 0x1 ;

	int max_evts = 1000 ;

	while((c=getopt(argc,argv,"e:"))!=EOF) {
	switch(c) {
	case 'e' :
		max_evts = atoi(optarg) ;
		break ;
	}
	}
	

	tpx_pre_clock_send_config() ;
	tpx_post_clock_send_config() ;
	tpx_run_start() ;

	int evts = 0 ;
	u_int loops = 0 ;
	struct wrk_t w ;

	double words_per_sec = 0 ;
	double evts_per_sec = 0 ;
	unsigned long mark = t_mark() ;

	for(;;) {
		loops++ ;
		for(int f=0;f<ddb.rb_fifo_cou;f++) {
			for(int r=0;r<ddb.rb_max_count;r++) {
				if(ddb.rb_mask & (1<<r)) ;
				else continue ;

				u_int words = ddb.crb[r]->get_fifo_status(f) ;

				//LOG(TERR,"R%d:F%d: words 0x%X",r,f,words) ;

				if(words && (words<0x0F000000)) {
					LOG(TERR,"R%d:F%d: evt %d, words 0x%X",r,f,evts,words) ;

					char *c_addr = ddb.crb[r]->get_fifo_addr(f) ;
					ddb.crb[r]->set_fifo_status(f,0xDDDDDDDD) ;

					evts++ ;

					words_per_sec += words ;
					evts_per_sec++ ;

					u_int delta = t_delta(mark) ;

					if(delta>1000000) {
						words_per_sec *= 4 ;	// into bytes
						words_per_sec /= 1000000.0 ; // into MB

						words_per_sec /= (delta*0.000001) ;
						evts_per_sec /= (delta*0.000001) ;

						LOG(INFO,"evts %d: %.1f MB/s, %.1f Hz",evts,words_per_sec, evts_per_sec) ;

						words_per_sec = 0 ;
						evts_per_sec = 0 ;
						mark = t_mark() ;
					}


					//if(f>60) {
					//	LOG(WARN,"FIFO ix %d",f) ;
					//}

					
					w.evt = evts ;
					w.c_addr = c_addr ;
					w.words = words ;
					w.fifo_ix = f ;

					LOG(TERR,"words %d, fifo %d",words,f) ;

					que[0]->send(&w,sizeof(w),1) ;
					//ddb.crb[r]->ret_fifo(f) ;

					loops = 0 ;
					if(evts==max_evts) {
						LOG(INFO,"tpx_run_stop") ;
						tpx_run_stop() ;
						//goto run_done;
					}

				}
			}
		}
		if(loops==10000000) {
			LOG(WARN,"breaking out") ;
			break ;
		}

	}

//	run_done:;

	LOG(INFO,"stopping run") ;

//	tpx_run_stop() ;

	ddb.crb[0]->wr_var("read log") ;

	return 0 ;
}

	
