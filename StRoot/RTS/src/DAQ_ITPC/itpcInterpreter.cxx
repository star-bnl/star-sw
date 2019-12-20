#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>
#include <math.h>
#include <stdlib.h>

#include <rtsLog.h>
#include <DAQ_READER/daq_dta.h>

#include <I386/atomic.h>

#include "itpcInterpreter.h"
#include "itpcPed.h"

static void hammingdecode(unsigned int buffer[2], bool& error, bool& uncorrectable, bool fix_data) ;

struct itpcInterpreter::itpc_config_t itpcInterpreter::itpc_config[25] ;


int itpcInterpreter::itpc_fee_map[24][4][16] = {
{//S1 checked
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},   //ok 
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}  
},
{//S2 checked
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
//usual	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
//	{ 7, 1,17,12,24, 0,13, 8,28, 2,19,20,29,25,21, 3}, // moved #6 to #11 
	{ 7, 1, 0,12,24,17,13, 8,28, 2,19,20,29,25,21, 3}, // moved #6 to #11; and #3 to #6
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S3 checked
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S4 checked
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S5 checked
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S6 checked
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S7 checked
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S8 mess with mapping
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S9 mess with mapping
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S10 checked
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S11
//	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, // usual
	{49,52,46,47, 0, 54,0, 0, 0,50, 0,55,48, 0,51,53}, // new: bad port #8 on RDO ID 0x0052F5EA, moved to #4
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S12
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S13
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S14
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S15
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S16
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S17
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S18
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S19
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S20
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S21
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S22
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S23
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S24
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
} ;

int itpcInterpreter::fee_map_check()
{
	LOG(TERR,"fee_map_check") ;

	for(int s=0;s<24;s++) {
		u_int padplane[64] ;

		memset(padplane,0xFF,sizeof(padplane)) ;

		for(int r=0;r<4;r++) {
			for(int p=0;p<16;p++) {
				int pd = itpc_fee_map[s][r][p] ;

				if(pd==0) continue ;	// no FEE connected

				if(padplane[pd] == 0xFFFFFFFF) {
					padplane[pd] = ((r+1)<<8)|(p + 1);
				}
				else {
					int rdo = padplane[pd]>>8 ;
					int port = padplane[pd]&0xFF ;
					LOG(ERR,"S%02d:%d -- port %d, padplane %2d: taken padlane by port %d:%02d",s+1,r+1,p+1,pd,rdo,port) ;
				}
			}
		}

		for(int p=0;p<64;p++) {
			u_int pd = padplane[p]  ;

			if((p==0)||(p>55)) {
				if(pd != 0xFFFFFFFF) {
					LOG(ERR,"S%02d: illegal padplane %2d",s+1,p) ;
				}
			}
			else {
				if(pd==0xFFFFFFFF) {
					LOG(ERR,"S%02d: padplane %2d missing",s+1,p) ;
				}
			}
		}

	}

	return 0 ;
}

u_int itpcInterpreter::ifee_mask(int sec1, int rdo1)
{
	u_int mask = 0 ;

	for(int i=0;i<16;i++) {
		if(itpc_fee_map[sec1-1][rdo1-1][i]) mask |= (1<<i) ;
	}

	return mask ;

#if 0
	switch(rdo1) {
	case 1 :
		return 0xAE9B ;	// was 0x9D9B in FY18
	case 2 :
		return 0xFEBF ; // was 0xFE7F in FY18
	case 3 :
		return 0xFFFD ;
	case 4 :
		return 0xFFFF ;
	}
	
	LOG(ERR,"WHAT %d???",rdo1) ;
	return 0xFFFF ;
#endif

}



int itpcInterpreter::parse_default() 
{
	fee_map_check() ;

	memset(&itpc_config,0,sizeof(itpc_config)) ;

	for(int sec=1;sec<=24;sec++) {	
	for(int rb=0;rb<4;rb++) {
		u_int mask = ifee_mask(sec,rb+1) ;

		itpc_config[sec].rdo[rb].rdo_id = rb+1 ;
		itpc_config[sec].rdo[rb].wire1 = 0 ;
		itpc_config[sec].rdo[rb].fee_mask = mask ;
		itpc_config[sec].rdo[rb].phase = 90 ;

		for(int i=0;i<16;i++) {
			if(mask & (1<<i)) itpc_config[sec].rdo[rb].fee_count++ ;
		}
		
		//LOG(TERR,"S%02d:%d: fee mask 0x%04X, fee_count %d",sec,rb+1,mask,itpc_config[sec].rdo[rb].fee_count) ;
		
		for(int port=0;port<16;port++) {
			itpc_config[sec].rdo[rb].fee[port].wire1 = 0 ;
			itpc_config[sec].rdo[rb].fee[port].padplane_id = itpc_fee_map[sec-1][rb][port] ;
			itpc_config[sec].rdo[rb].fee[port].sampa_version = 3 ;	// assume V3
		}
	}
	}

	return 0 ;
	
}

int itpcInterpreter::parse_config(const char *fname) 
{
	LOG(ERR,"Should not use parse_config") ;
	return -1 ;

	if(fname==0) fname = "/RTS/conf/itpc/itpc_config.txt" ;

	FILE *f = fopen(fname,"r") ;
	if(f==0) {
		LOG(ERR,"%s: %s [%s]",__PRETTY_FUNCTION__,fname,strerror(errno)) ;
		return -1 ;
	}

	memset(&itpc_config,0,sizeof(itpc_config)) ;

	while(!feof(f)) {
		char line[128] ;
		int sec,rdo,rb,phase,port,padplane ;
		u_int wire1, mask ;
		int ret ;

		if(fgets(line,sizeof(line),f)==0) continue ;

		if(strncmp(line,"R ",2)==0) {	// RDO section
			ret = sscanf(line,"R %d %d %d %X %X %d",&sec,&rdo,&rb,&wire1,&mask,&phase) ;
			if(ret != 6) {
				LOG(ERR,"%s: malformed line [%s]",fname,line) ;
				continue ;
			}

			itpc_config[sec].rdo[rb].rdo_id = rdo ;
			itpc_config[sec].rdo[rb].wire1 = wire1 ;
			itpc_config[sec].rdo[rb].fee_mask  = mask ;
			itpc_config[sec].rdo[rb].phase = phase ;

			for(int i=0;i<16;i++) {
				if(mask & (1<<i)) itpc_config[sec].rdo[rb].fee_count++ ;
			}
			LOG(TERR,"RDO %d: fee mask 0x%04X, fee_count %d",rb+1,mask,itpc_config[sec].rdo[rb].fee_count) ;

		}
		else if(strncmp(line,"F ",2)==0) {	// FEE section
			int v ;

			ret = sscanf(line,"F %d %d %X %d %d",&sec,&rdo,&wire1,&port,&padplane) ;
			if(ret != 5) {
				LOG(ERR,"%s: malformed line [%s]",fname,line) ;
				continue ;
			}
			
			// special deal for SAMPA version
			if(strstr(line,"V3")) v = 3 ;
			else if(strstr(line,"V4")) v = 4 ;
			else v = 2 ;

			for(int i=0;i<4;i++) {
				if(itpc_config[sec].rdo[i].rdo_id == rdo) {
					itpc_config[sec].rdo[i].fee[port].wire1 = wire1 ;
					itpc_config[sec].rdo[i].fee[port].padplane_id = padplane ;
					itpc_config[sec].rdo[i].fee[port].sampa_version = v ;
					if(v != 2) {
						LOG(WARN,"S%02d:%2d:#%02d: 0x%X SAMPA is V%d",sec+1,i+1,port,wire1,v) ;
					}
				}
			}
					

		}
	}

	fclose(f) ;

	return 0 ;
}


itpcInterpreter::itpcInterpreter()
{
	id = 0 ;	// not valid

	evt_ix = 0 ;
	realtime = 0 ;
	dbg_level = 0 ;

	fee_evt_cou = 0 ;
	run_number = 0 ;

	fout = 0 ;

	fpga_fee_v_all = 0 ;

	fee_version = 0 ;	// original pre-Mar 2018
	rdo_version = 0 ;

	expected_rdo_version = -1 ;	// uknown; don't check
	expected_fee_version = -1 ;	// unknown; don't check


	memset(fee,0,sizeof(fee)) ;
	memset(itpc_config,0,sizeof(itpc_config)) ;

	ped_c = 0 ;
}

atomic_t itpcInterpreter::run_errors[4][16] ;

void itpcInterpreter::run_start(u_int run)
{
	run_number = run ;
	evt_ix = 0 ;
	fee_evt_cou = 0 ;

	if(fout) {
		fprintf(fout,"***** RUN_START: %08u\n",run_number) ;
		fflush(fout) ;
	}

	if(id==1) memset(run_errors,0,sizeof(run_errors)) ;

	LOG(DBG,"Starting run %08u",run_number) ;
}

void itpcInterpreter::run_err_add(int rdo1, int type)
{
	atomic_inc(&run_errors[rdo1-1][type]) ;
}

void itpcInterpreter::run_stop()
{

	if(fout) {
		fprintf(fout,"***** RUN_STOP: %08u\n",run_number) ;
		fflush(fout) ;
		fclose(fout) ;
		fout = 0 ;
	}

	LOG(INFO,"%d: stopping run %08u after %d/%d events",id,run_number,fee_evt_cou,evt_ix) ;

	if(id==1) {
		for(int i=0;i<4;i++) {
			for(int j=0;j<16;j++) {
				if(atomic_read(&run_errors[i][j])) {
					LOG(ERR,"RDO %d: error type %d = %u",i+1,j,atomic_read(&run_errors[i][j])) ;
				}
			}
		}
	}
}


void itpcInterpreter::start_event(u_int bytes)
{
	evt_ix++ ;	//events will start from 1

	evt_bytes = bytes ;	//store length

	//zap stuff
	word_ix = 0 ;

	status = 0 ;

	state = S_IDLE ;

	fee_port = -1 ;
	
	d_cou = -1 ;

	sampa_bx = -1 ;

	ascii_cou = 0 ;

	memset(evt_err,0,sizeof(evt_err)) ;
}

void itpcInterpreter::stop_event()
{
	for(int i=0;i<8;i++) {
		if(evt_err[i]) LOG(ERR,"%d: event errors[%d] = %u",rdo_id,i,evt_err[i]) ;
	}
	
}



static inline u_int sw16(u_int d)
{
	u_int tmp = d ;

	d >>= 16 ;

	d |= (tmp & 0xFFFF)<<16 ;

	return d ;
}



/*
	This is the pre-April 2018 version for rdo_version = fee_version = 0 ;
*/
/* We start with 0x980000008 */
u_int *itpcInterpreter::fee_scan(u_int *start, u_int *end)
{
	u_int *d = start ;
	u_int dd ;
	int hdr ;
	int trg ;
	int cfg ;

//	LOG(TERR,"len %d: 0x%X 0x%X 0x%X",end-start,d[0],d[1],d[2]) ;

	d++ ;	// skip over 0x98000008 ;

	// next is FEE port (from 0)
	fee_port = *d++ ;	// fee port
	fee_port++ ;		// start from 1

	fee_id = -1 ;
	trg = 0 ;	// event is triggered
	hdr = 0 ;
	cfg = 0 ;
	ascii_cou = 0 ;

	// We are at the start of FEE data:
	u_int dd_x = d[0] & 0xFFC000FF ;	// command

	// I should now point at 0x80xx0001
	if(dd_x != 0x80000001) {
		// HAPPENS
		//LOG(WARN,"Missing start 0x%08X",d[0]) ;

		//for(int i=0;i<32;i++) {
		//	LOG(TERR,"%d = 0x%08X",i-8,d[i-8]) ;
		//}

		fee_bx = 0 ;	// dummy

		cfg++ ;
		trg++ ;
	}

	while(d<end) {
		dd = *d++ ;
		dd_x = dd & 0xFFC000FF ;	// just commands
		u_int dd_a ;

		LOG(NOTE,"FEE #%d: %u 0x%08X",fee_port,d-start,dd) ;
		
		// let's get non-FEE headers out of the way first
		switch(dd_x) {
		case 0x980000F8 :	// End-of-FEE status (from RDO)
			//LOG(TERR,"0x%08X 0x%08X 0x%08X 0x%08X 0x%08X",dd,d[0],d[1],d[-1],d[-2]) ;

			if(d[1]) {	// fee_status (from RDO)
				LOG(ERR,"%d: FEE #%d[%d] END: 0x%08X 0x%08X 0x%08X 0x%08X 0x%08X 0x%08X 0x%08X",rdo_id,fee_port,fee_id,
				    d[0],d[1],d[2],d[3],d[4],d[5],d[6]) ;
			}
			else {
				LOG(NOTE,"FEE #%d[%d] END: 0x%08X 0x%08X 0x%08X 0x%08X 0x%08X 0x%08X 0x%08X",fee_port,fee_id,
				    d[0],d[1],d[2],d[3],d[4],d[5],d[6]) ;
			}

			// check word _before_ the RDO header; should come from a FEE
			if((d[-2]&0xFFC00000) != 0x40000000) {
				// might fire due to a bug in readout
				if((d[-2]&0xFFC00000) != 0x80000000) {				
					//LOG(ERR,"%d: FEE #%d: Before END 0x%08X",rdo_id,fee_port,d[-2]) ;
				}
				else {
					//OFTEN
					//LOG(WARN,"%d: FEE #%d: Before END 0x%08X",rdo_id,fee_port,d[-2]) ;
				}
			}

			// search for RDO-END-FEE marker
			for(int i=0;i<16;i++) {
				dd = *d++  ;
				if(dd == 0x58000009) {
					dd-- ;	// go back one
					goto stop_loop ;
				}
			}

			LOG(ERR,"%d: FEE #%d: can't find RDO-END",rdo_id,fee_port) ;
			goto stop_loop ;

		}


		
		u_int t_fee_id = (dd >> 16) & 0x3F ;
		if(fee_id<0) {
			if((dd_x & 0xFF000000)==0x80000000) fee_id = t_fee_id ;
		}
		else {
			if(fee_id != t_fee_id) {
				evt_err[ERR_DIFFERENT_FEE_IDS]++ ;
				LOG(NOTE,"FEE #%d: IDs differ: expect %d, is %d, 0x%08X",fee_port,fee_id,t_fee_id,dd) ;
			}
		}

		switch(dd_x) {
		case 0xA00000A0 :	// ASCII start
			break ;
		case 0x600000A0 :	// ASCII end
			break ;
		case 0xA00000B0 :	// binary start
			LOG(ERR,"%d: FEE #%d: Unexpected 0x%08X",rdo_id,fee_port,dd) ;
			break ;
		case 0x600000B0 :	// binary end
			LOG(ERR,"%d: FEE #%d: Unexpected 0x%08X",rdo_id,fee_port,dd) ;
			break ;
		case 0x80000001 :	// START EVENT (also hdr)
			// this can be missing! and can also show up at the very end!
			{
				u_int bx_lo = d[4] & 0xFFFF ;
				u_int bx_hi = d[5] & 0xFFFF ;

				bx_lo |= (bx_hi<<16) ;

				fee_bx = bx_lo ;
				//LOG(TERR,"BX %u 0x%08X",bx_lo&0xFFFFF,bx_lo) ;
			}

			//for(int jj=-2;jj<6;jj++) {
			//	LOG(TERR,"... %d = 0x%08X",jj,d[jj]) ;
			//}
			cfg++ ;
			trg++ ;	
			break ;
		case 0x60000001 :	// end event hdr
			//LOG(TERR,"END event %d %d",cfg,trg) ;
			cfg = 0 ;
			if(trg==1) {
				fee_id = (dd>>16)&0x3F ;
				//LOG(TERR,"FEE #%d(%d) start SAMPA: FEE BX %u (0x%08X)",fee_port,fee_id,fee_bx&0xFFFFF,fee_bx) ;

				for(int i=0;i<4;i++) {
					u_int expect_mask ;
					//u_int *cur_d = d ;

					switch(i) {
					case 0 :
					case 2 :
						expect_mask = 0x0000FFFF ;
						break ;
					default :
						expect_mask = 0xFFFF0000 ;
						break ;
					}

					//if(i==0 || i==2) found_ch_mask = 0 ;
					found_ch_mask = 0 ;

					d = sampa_lane_scan(d,end) ;
					//LOG(TERR,"fee_port %d: lane %d was %d words",fee_port,i,d-cur_d) ;
					
					//if(i==1 || i==3) {
						if(found_ch_mask != expect_mask) {
							//LOG(ERR,"expect 0x%08X",expect_mask) ;
							LOG(ERR,"%d: fee_port %d: missing ch after lane %d: 0x%08X",
							    rdo_id,fee_port,i,found_ch_mask) ;
						}
					//}


				}
				break ;
			}
			LOG(ERR,"%d: FEE #%d: %u 0x%08X -- odd",rdo_id,fee_port,d-start,dd) ;
			break ;
		case 0xA0000001 :	// start event trailer 
			cfg++ ;
			break ;
		case 0x40000001 :	// END EVENT (also trailer)
			if(dd & 0xFF00) {
				LOG(ERR,"%d: FEE #%d: EVENT error 0x%02X",rdo_id,fee_port,(dd>>8)&0xFF) ;
				fee[fee_port].event_errs++ ;
			}
			cfg = 0 ;
			break ;
		case 0x80000002 :	// START SEND_CONFIG (also header)
			hdr++ ;
			cfg++ ;
			break ;
		case 0x60000002 :	// end send_config header
			cfg = 0 ;
			break ;
		case 0xA00000FA :	// start config data
			cfg++ ;
			break ;
		case 0x600000FA :	// end config data
			cfg = 0 ;
			break ;
		case 0xA00000EC :	// start monitoring header
			break ;
		case 0x600000EC :	// end monitoring header
			break ;
		case 0xA00000ED :	// start monitoring trailer
			break ;
		case 0x600000ED :	// end monitoring trailer
			break ;
		case 0xA0000002 :	// start send_config trailier
			cfg++ ;
			break ;
		case 0x40000002 :	// END SEND_CONFIG event/trailer
			cfg = 0 ;
			if(dd & 0xFF00) {
				fee[fee_port].send_config_errs++ ;
				LOG(ERR,"FEE #%d: SEND_CONFIG error 0x%02X",fee_port,(dd>>8)&0xFF) ;
			}
			break ;
		case 0x80000003 :	// START of pedestal confirm
			//LOG(TERR,"Start Ped, fee %d",fee_port) ;
			{
				u_int delta = ((d[3]&0xFFFF)<<16)|(d[2]&0xFFFF) ;
				int for_me = (d[0] & 0x8000)?1:0 ;
				int timebins = d[0] & 0x3FF ;
				int ch = (d[0]>>10)&0x3F ;
				LOG(TERR,"FEE #%d: ped: for me %d, ch %d, timebins %d, chsum %u, delta %u",fee_port,for_me,ch,timebins,d[1]&0xFFFF,delta) ;
			}
			d += 5 ;

			break ;
		case 0x40000003 :	// END of pedestal confirm event
			if(dd & 0xFF00) {
				fee[fee_port].pedestal_errs++ ;
				LOG(ERR,"FEE #%d: PEDESTAL error 0x%02X",fee_port,(dd>>8)&0xFF) ;
			}

			LOG(ERR,"Shouldn't be 0x%X",dd) ;
			break ;
		case 0x80000004 :
			break ;
		case 0x40000004 :
			break ;
		default :	// none of the above...

			dd_a = dd & 0xFFC0FF00 ;	// for ASCII
			if(dd_a == 0x0000F500 || dd_a==0x00800000) {	// ASCII char
				u_char c = dd & 0xFF ;
				//LOG(TERR,"... [0x%08X]",dd) ;

				if(isprint(c) || isspace(c)) ;
				else c = '?' ;

				ascii_dta[ascii_cou++] = c ;
				if(c=='\n') {
					int last ;

					ascii_dta[ascii_cou++] = 0 ;
					if(fout) {
						fprintf(fout,"#%02d: %s",fee_port,ascii_dta) ;
						fflush(fout) ;
					}
					

					last = strlen(ascii_dta) - 1 ;
					if(ascii_dta[last]=='\n') ascii_dta[last] = 0 ;
					if(last > 1) {
						LOG(TERR,"FEE_asc %d:#%02d: \"%s\"",rdo_id,fee_port,ascii_dta) ;
					}

					if(fee_port > 0) {	// this must be!
						u_int id1 = 0 ;
						if(strncmp(ascii_dta,"1Wire:",6)==0) {
							char *id = strstr(ascii_dta,"ID") ;
							if(id) {
								if(sscanf(id,"ID 0x%X",&id1)==1) {
									//printf("********** port %d, WIRE 0x%08X\n",fee_port,id1) ;
									fee[fee_port].wire1_id = id1 ;
								}
							}
						}
						else if(strncmp(ascii_dta,"Padplane ",9)==0) {
							if(sscanf(ascii_dta,"Padplane %d",&id1)==1) {
								//printf("********** port %d, PADPLANE %d\n",fee_port,id1) ;
								fee[fee_port].padplane_id = id1 ;
							}
						}
						   
						if(strstr(ascii_dta,"ERROR")) {
							last = strlen(ascii_dta) - 1 ;
							if(ascii_dta[last]=='\n') ascii_dta[last] = 0 ;
							LOG(ERR,"%d: FEE #%d: [%s]",rdo_id,fee_port,ascii_dta) ;
						}

					}


					ascii_cou = 0 ;
				}
				break ;
			}
			if(cfg) break ;

			evt_err[ERR_UNKNOWN]++ ;
			LOG(NOTE,"FEE #%d: %u 0x%08X -- unkown",fee_port,d-start,dd) ;
			break ;
		}

	}

	stop_loop: ; 


	if(fee_id < 0) {	// Hm, never seen any FEE data really...
//		LOG(ERR,"%d: fee_id %d, FEE #%d [0x%08X]: format error [%u 0x%08X]",rdo_id,fee_id,fee_port,(u_int)fee_port,d-start,*d) ;
		return d ;
	}


	return d ;
}

// Used when not running FCF
int itpcInterpreter::sampa_ch_scan()
{
	int err = 0 ;
	int t_stop_last = -1 ;
	int s,r,p ;
	
//	LOG(TERR,"sampa_ch_scan") ;


	s = sector_id - 1 ;	// from 0
	r = rdo_id - 1 ;
	p = fee_port - 1 ;

	if(ped_c) {
		ped_c->sector = s  ;
		ped_c->rdo = r  ;
		ped_c->port = p  ;
		ped_c->fee_id = fee_id ;

		ped_c->ch_start(fee_ch) ;
	}


	for(int i=0;i<tb_cou;) {
//		LOG(TERR,"i   %d",i) ;

		int t_cou = tb_buff[i++] ;
		int t_start = tb_buff[i++] ;



		int t_stop = t_start + t_cou - 1 ;

//		LOG(TERR,"...%d %d %d",t_start,t_cou,t_stop) ;

		if(t_start <= t_stop_last) {
                        LOG(ERR,"%d: t_start %d, t_cou %d, t_stop %d, t_stop_last %d",rdo_id,t_start,t_cou,t_stop,t_stop_last) ;
			err = -1 ;
			break ;
		}

		if(t_stop > 512) {
                        LOG(ERR,"%d: t_start %d, t_cou %d, t_stop %d, t_stop_last %d",rdo_id,t_start,t_cou,t_stop,t_stop_last) ;
			err = -2 ;
			break ;
		}

		t_stop_last = t_stop ;

		if(ped_c) {
			for(int t=t_start;t<=t_stop;t++) {
				u_short adc = tb_buff[i++] ;

				ped_c->accum(s,r,p,fee_id,fee_ch,t,adc) ;
				//ped_c->accum(t,adc) ;
			}
		}
		else {
			i += t_stop - t_start + 1 ;
		}
	}


	if(ped_c) ped_c->ch_done(err) ;

	if(err) {
		//run_err_add(rdo_id,6) ;

//		for(int i=0;i<tb_cou;i++) {
//			LOG(NOTE,"%d/%d = %u",i,tb_cou,tb_buff[i]) ;
//		}
	}

	return err ;
}

u_int *itpcInterpreter::sampa_ch_hunt(u_int *start, u_int *end) 
{
	// I expect to be pointed to the SAMPA channel header!
	u_int *data = start ;
	u_int *data_start ;

	u_int d ;
	u_int err  ;
	u_int h[6] ;
	bool hamming_err ;
	u_int l_sampa_bx ;
	u_int type, words ;
	unsigned long long hc ;
	u_int hh[2] ;
	bool uncorrectable ;
	int p_cou ;
	u_int hdr[2] ;

	u_int sampa_id, sampa_ch ;	// shadow class variables on purpose!


	retry:;

	err = 0 ;
	hdr[0] = hdr[1] = 0xFFFFEEEE ;

	data_start = data ;

	if(data>=end) goto err_end ;
	d = *data++ ;
	hdr[0] = d ;

	if(d & 0xC0000000) {
		err |= 0x1 ;
		//LOG(ERR,"%d: Bad Hdr 1",rdo_id) ;
		goto err_ret ;
	}

	h[0] = (d >> 20) & 0x3FF ;
	h[1] = (d >> 10) & 0x3FF ;
	h[2] = d & 0x3FF ;
	
	if(data>=end) goto err_end ;
	d = *data++ ;
	hdr[1] = d ;

	if(d & 0xC0000000) {
		err |= 0x2 ;
		//LOG(ERR,"%d: Bad Hdr 2",rdo_id) ;
		goto err_ret ;
	}

	h[3] = (d >> 20) & 0x3FF ;
	h[4] = (d >> 10) & 0x3FF ;

	h[5] = d & 0x3FF ;

//	if(h[5] != 0xAB) {
//		err |= 0x4 ;
//		goto err_ret ;
//	}


	type = h[0] >> 7 ;
	words = h[1] ;
	sampa_id = h[2] & 0xF ;
	sampa_ch = (h[2]>>4) & 0x1F ;



	l_sampa_bx = (h[2]&0x200) >> 9 ;
	l_sampa_bx |= (h[3]<<1) ;
	l_sampa_bx |= (h[4]&0x1FF)<<11 ;

//	LOG(WARN,"....... %d %d %u",sampa_id,sampa_ch,l_sampa_bx) ;

	// check parity
	p_cou = 0 ;
	for(int i=0;i<=4;i++) {
		for(int j=0;j<10;j++) {
			if(h[i] & (1<<j)) p_cou++ ;
		}
	}

	if(p_cou&1) {	// parity error
		err |= 0x11 ;
		goto err_ret ;
	}


	hc = ((long long)h[4]<<40)|((long long)h[3]<<30)|(h[2]<<20)|(h[1]<<10)|h[0];
	hh[0] = hc & 0x3FFFFFFF ;
	hh[1] = (hc>>30) ;


	hammingdecode(hh, hamming_err,uncorrectable,0) ;


	if(hamming_err) {
		//LOG(ERR,"%d:%d: Type %d, words %d, SAMPA %d:%d, BX %u, errors %d:%d",rdo_id,fee_port,type,words,sampa_id,sampa_ch,l_sampa_bx,parity_err,hamming_err) ;
		// and I should do something here!
		err |= 0x10 ;
		goto err_ret ;
	}


	switch(type) {
	case 0 :	// heartbeat
		if(words != 0) {
			err |= 0x20 ;
			goto err_ret ;
		}
		if(sampa_ch != 21) {
			err |= 0x21 ;
			goto err_ret ;
		}
		break ;
	case 4 :	// physics
		break ;
	default :
		//LOG(ERR,"%d: Type %d, words %d, SAMPA %d:%d, BX %u",rdo_id,type,words,sampa_id,sampa_ch,l_sampa_bx) ;
		err |= 0x40 ;
		goto err_ret ;
	}


	err_ret:;
	if(err) {
//		LOG(WARN,"... 0x%08X 0x%08X",hdr[0],hdr[1]) ;
		goto retry ;
	}

	LOG(WARN,"Found SAMPA %d:%d [0x%08X 0x%08X]",sampa_id,sampa_ch,hdr[0],hdr[1]) ;
	return data_start ;	// got one!

	err_end:;

	LOG(ERR,"Found no SAMPA channels") ;
	return 0 ;
}

u_int *itpcInterpreter::sampa_lane_scan(u_int *start, u_int *end)
{
	u_int d ;
	u_int h[6] ;
	u_int type, words ;
	u_int word32 ;
	u_char lane ;
	u_int err = 0 ;
	u_int *data ;
	int ch_loop_cou = 0 ;
	int l_sampa_bx ;
	u_int lane_hdr ;
	bool parity_err ;
	bool hamming_err ;
	unsigned long long hc ;
	u_int hh[2] ;
	bool uncorrectable ;
	int p_cou ;
	u_int hdr[2] ;
	u_int first_b ;

	data = start ;


	// first datum is the 0xB....
	d = *data++ ;
	lane_hdr = d ;

	first_b = d ;

	fee_id = (d >> 16) & 0x3F ;
	lane = (d>>24) & 0x3 ;

	if((first_b & 0xFC000000)!=0xB0000000) {
		LOG(ERR,"SAMPA FIFO overwrite 0x%08X!",first_b) ;
	}

//	LOG(TERR,"SAMPA lane: FEE %3d, lane %d [0x%08X]",fee_id,lane,d) ;

	new_ch:;		// start of channel data

	ch_loop_cou++ ;		// count how many channels we found

	// data is now at the SAMPA header
	d = *data++ ;
	hdr[0] = d ;

	if(d & 0xC0000000) {
		hdr[1] = 0xAABBCCDD ;
		memset(h,0xAB,sizeof(h)) ;

		err |= 0x100 ;
		LOG(ERR,"%d: Bad Hdr 1",rdo_id) ;
		goto err_ret ;
	}

	h[0] = (d >> 20) & 0x3FF ;
	h[1] = (d >> 10) & 0x3FF ;
	h[2] = d & 0x3FF ;
	
	d = *data++ ;
	hdr[1] = d ;

	if(d & 0xC0000000) {
		h[3] = h[4] = h[5] = 0x11223344 ;
		err |= 0x200 ;
		LOG(ERR,"%d: Bad Hdr 2",rdo_id) ;
		goto err_ret ;
	}

	h[3] = (d >> 20) & 0x3FF ;
	h[4] = (d >> 10) & 0x3FF ;
	h[5] = d & 0x3FF ;

//	if(h[5] != 0xAB) {
//		err |= 1 ;
//		goto err_ret ;
//	}


	type = h[0] >> 7 ;
	words = h[1] ;
	sampa_id = h[2] & 0xF ;
	sampa_ch = (h[2]>>4) & 0x1F ;


	switch(lane) {
	case 0 :
	case 1 :
		if((sampa_id&1)) {
			LOG(ERR,"sampa_id %d, lane %d",sampa_id,lane) ;
		}
		break ;
	case 2 :
	case 3 :
		if(!(sampa_id&1)) {
			LOG(ERR,"sampa_id %d, lane %d",sampa_id,lane) ;
		}
		break ;
	}
		

// I don't really need this check; it's handled by missing channel later on
#if 0
	if(ch_loop_cou==1) {
		switch(lane) {
		case 0 :
		case 2 :
			if(sampa_ch != 0) {
				// this happens if I'm missing a channel
				LOG(WARN,"first in lane sampa_ch is %d not 0?",sampa_ch) ;
			}
			break ;
		case 1 :
		case 3 :
			if(sampa_ch != 16) {
				// this happens if I'm missing a channel
				LOG(WARN,"first in lane sampa_ch is %d not 16?",sampa_ch) ;
			}
			break ;
		}
	}
#endif

	if(sampa_id & 1) fee_ch = sampa_ch + 32 ;
	else fee_ch = sampa_ch ;

	l_sampa_bx = (h[2]&0x200) >> 9 ;
	l_sampa_bx |= (h[3]<<1) ;
	l_sampa_bx |= (h[4]&0x1FF)<<11 ;


//	LOG(TERR,"+++ %d %d %u",sampa_id,sampa_ch,l_sampa_bx) ;

	// check parity
	p_cou = 0 ;
	for(int i=0;i<=4;i++) {
		for(int j=0;j<10;j++) {
			if(h[i] & (1<<j)) p_cou++ ;
		}
	}

	if(p_cou&1) {
		parity_err = 1 ;
	}
	else {
		parity_err = 0 ;
	}


	hc = ((long long)h[4]<<40)|((long long)h[3]<<30)|(h[2]<<20)|(h[1]<<10)|h[0];

	hh[0] = hc & 0x3FFFFFFF ;
	hh[1] = (hc>>30) ;


	hammingdecode(hh, hamming_err,uncorrectable,0) ;


	if(parity_err || hamming_err) {
		LOG(ERR,"%d:%d: Type %d, words %d, SAMPA %d:%d, BX %u, errors %d:%d",rdo_id,fee_port,type,words,sampa_id,sampa_ch,l_sampa_bx,parity_err,hamming_err) ;
		// and I should do something here!
		err |= 1 ;
		goto err_ret ;
	}
	else {
//		LOG(TERR,"Type %d, words %d, SAMPA %d:%d, BX %u, errors %d:%d, fee_port %d",type,words,sampa_id,sampa_ch,l_sampa_bx,parity_err,hamming_err,fee_port) ;
	}


	switch(type) {
	case 0 :	// heartbeat
		LOG(WARN,"%d: Type %d, words %d, SAMPA %d:%d, BX %u, errors %d:%d, fee_port %d, lane_hdr 0x%08X",
		    rdo_id,type,words,sampa_id,sampa_ch,l_sampa_bx,parity_err,hamming_err,fee_port,lane_hdr) ;

		if(words != 0) {
			err |= 2 ;
			goto err_ret ;
		}
		if(sampa_ch != 21) {
			err |= 4 ;
			goto err_ret ;
		}
		break ;
	case 4 :	// physics
		//if(dbg_level) LOG(WARN,"Type %d, words %d, SAMPA %d:%d, BX %u",type,words,sampa_id,sampa_ch,l_sampa_bx) ;
		break ;
	case 1 :	// trigger overrun
		LOG(ERR,"%d: Type %d, words %d, SAMPA %d:%d, BX %u [lane_hdr 0x%08X],fee_port %d",rdo_id,type,words,sampa_id,sampa_ch,l_sampa_bx,lane_hdr,fee_port) ;
		err |= 8 ;
		goto err_ret ;
		break ;
	default :
		LOG(ERR,"%d: Type %d, words %d, SAMPA %d:%d, BX %u [lane_hdr 0x%08X]",rdo_id,type,words,sampa_id,sampa_ch,l_sampa_bx,lane_hdr) ;
		err |= 8 ;
		goto err_ret ;
	}


	if(sampa_bx < 0) {
		sampa_bx = l_sampa_bx ;
	}
	else if(sampa_bx != l_sampa_bx) {
		if(abs(sampa_bx - l_sampa_bx)>1) {
			// I could have a difference of 1 here...
			//LOG(WARN,"%d:#%02d:%d expect %u, got %u",rdo_id,fee_port,fee_ch,sampa_bx,l_sampa_bx) ;
		}
	}


	//now go for the data...
	word32 = words / 3 + (words%3?1:0) ;	

	//LOG(TERR,"words %d, word32 %d",words,word32) ;
	tb_cou = words ;

	if(ped_c && ped_c->want_data) {	// I will handle my own data; FCF

		ped_c->sector = sector_id ;
		ped_c->rdo = rdo_id ;
		ped_c->port = fee_port ;

		if(ped_c->do_ch(fee_id, fee_ch, data, words)) {
			err |= 0x400 ;
		}

		data += word32 ;
	}
	else {				// done in pedestal runs

		int t_cou = 0 ;	// local
		for(u_int i=0;i<word32;i++) {
			d = *data++ ;

			if((d&0xC0000000)) {
				LOG(ERR,"%d:%d: %d:%d sampa data word %d/%d = 0x%08X",rdo_id,fee_port,sampa_id,sampa_ch,i,word32,d) ;
				err |= 0x10 ;
				i-- ;

				if(d==0x980000F8) {	//end of event!!!
					LOG(ERR,"Returning here") ;
					return data-1 ;
				}
				continue ;
			}

			//LOG(TERR,"... 0x%08X",d) ;

			tb_buff[t_cou++] = (d>>20) & 0x3FF ;
			tb_buff[t_cou++] = (d>>10) & 0x3FF ;
			tb_buff[t_cou++] = d & 0x3FF ;
		}

		if(sampa_ch_scan()<0) {
			err |= 0x400 ;
		}
	}

	//note this hack
	//tb_cou = words ;

	

	if(err) {
		if(err & 0x400) {	// various timebin errors
			run_err_add(rdo_id,6) ;
		}
		LOG(ERR,"%d: Last SAMPA: FEE #%d: %d:%d = 0x%08X [err 0x%0x]",rdo_id,fee_port,sampa_id,sampa_ch,*data,err) ;
	}
	else {
		if(found_ch_mask & (1<<sampa_ch)) {
			LOG(ERR,"SAMPA ch %d already found!",sampa_ch) ;
		}
		found_ch_mask |= (1<<sampa_ch) ;
		LOG(NOTE,"Last SAMPA %d:%d = 0x%08X [err 0x%0x]",sampa_id,sampa_ch,*data,err) ;
	}
		
	err = 0 ;	// clear error before we go into a new channel


//	LOG(TERR,"Data at end is now 0x%08X",*data) ;

	if(*data & 0xC0000000) {
		// this must be 0x7xxxxxxx
		if((data[0] & 0xFC000000)!=0x70000000) {
			LOG(ERR,"bad end 0x%08X",data[0]) ;
		}
		//if((data[0] & 0x0FFFFFFF)!=(first_b & 0x0FFFFFFF)) {
		//	LOG(ERR,"bad start/stop 0x%08X 0x%08X",first_b,data[0]) ;
		//}
		if(ch_loop_cou != 16) {	// this is a SAMPA2 bug!!!
			//LOG(ERR,"Found only %d channels; datum is 0x%08X 0x%08X, last ch is %d, bx %u",ch_loop_cou,data[0],data[1],sampa_ch,sampa_bx) ;
			//LOG(ERR,"   first_b 0x%08X, 0x%08X 0x%08X",first_b,data[-2],data[-1]) ;
		}
		data++ ;	// move to the 0xB... of the next lane!
		return data ;	//keep the signature
	}
	else {
		goto new_ch ;
	}

	err_ret:

	LOG(ERR,"%d: ERR 0x%X: 0x%03X 0x%03X 0x%03X 0x%03X 0x%03X 0x%03X",rdo_id,err,
	    h[0],h[1],h[2],h[3],h[4],h[5]) ;
	LOG(ERR,"   0x%08X 0x%08X",hdr[0],hdr[1]) ;

#if 0
	u_int *n_data = sampa_ch_hunt(data,end) ;
	if(n_data) {
		err = 0 ;
		data = n_data ;
		goto new_ch ;
	}
#endif

	return data ;
}


void itpcInterpreter::fee_dbase(const char *fname) 
{
	FILE *db ;
	char hname[128] ;
	char uname[128] ;

	//date,time
	
	//hostname
	strcpy(hname,"host-unknown") ;
	gethostname(hname,sizeof(hname)) ;

	//user
	strcpy(uname,"user-unknown") ;
	getlogin_r(uname,sizeof(uname)) ;


	if(fname==0) {
		fname = "/RTScache/fee_dbase.csv" ;

	}

	db = fopen(fname,"a") ;

	if(db==0) {
		LOG(ERR,"%s: %s [%s]",__PRETTY_FUNCTION__,fname,strerror(errno)) ;
		return ;
	}

	time_t tm = time(0) ;

	char *ct = ctime(&tm) ;
	ct[strlen(ct)-1] = 0 ;

//	LOG(TERR,"ped_c %p: %d %d",ped_c,sector_id,rdo_id) ;

	for(int i=1;i<=16;i++) {
		char err_str[32] ;
		if(fee[i].wire1_id == 0) continue ;	//skip non-FEEs

		
		itpcPed *p_c = (itpcPed *) ped_c ;
		int err = 0 ;
		int flag = 0 ;
		int bad_pin = 0 ;
		int reflow = 0 ;

		for(int c=0;c<64;c++) {
			if(p_c && p_c->fee_err[sector_id-1][rdo_id-1][i-1][c]) {
				err++ ;

				int fl = p_c->fee_err[sector_id-1][rdo_id-1][i-1][c] ; 
				flag |= fl ;


				if(fl & 3) {
					bad_pin++ ;
				}
				else if(fl & 4) {
					if(c<=31) reflow |= 1 ;
					else reflow |= 2 ;
				}



			}
			else {

			}
		}

		if(err) {
			err_str[0] = 0 ;

			if(bad_pin <= 1) ;
			else {
				sprintf(err_str,"PINS_%d ",bad_pin) ;
			}

			
			if(reflow) {
				strcat(err_str,"REFLOW SAMPA ") ;
			}
			else if(bad_pin <= 1) {
				goto skip_err ;
			}

			if(reflow & 1) {
				strcat(err_str,"0 ") ;
			}
			if(reflow & 2) {
				if(reflow & 1) {
					strcat(err_str,"and 1") ;
				}
				else {
					strcat(err_str,"1") ;
				}
			}		

			LOG(ERR,"FEE 0x%08X, port %2d: BAD: %s",fee[i].wire1_id,i,err_str) ;

			skip_err:;
		}

		if(err) {
			sprintf(err_str,"errs%d=0x%X",err,flag) ;
		}
		else {
			sprintf(err_str,"NC") ;		
		}

		//1wire1, padplane, port, RDO 1wire, RDO #, host, uname,date, comment
		fprintf(db,"0x%08X,%2d,%2d,0x%08X,%d,%s,%s,%s,%s\n",fee[i].wire1_id,fee[i].padplane_id,i,
			rdo_wire1_id,rdo_id,uname,hname,ct,err_str) ;



	}
	fprintf(db,"\n") ;	//one more NL

	fclose(db) ;

}

int itpcInterpreter::ana_send_config(u_int *data, u_int *data_end)
{
	u_int d ;

	data++ ;	// skip FD71

	data++ ;	// skip "version"??

	if(*data != 0x98000066) {
		LOG(ERR,"data is 0x%08X",*data) ;
		return -1 ;
	}

	data++ ;	// skip 0066

	ascii_cou = 0 ;
	while(data<data_end) {
		d = *data++ ;

		if((d & 0xFFFFFF00)==0x9800F500) {	//ASCII
			int c = d & 0xFF ;
			if(c=='\n' || ascii_cou==120) {
				ascii_dta[ascii_cou++] = 0 ;

				LOG(INFO,"%d: \"%s\"",rdo_id,ascii_dta) ;
				ascii_cou = 0 ;
			}
			else {
				ascii_dta[ascii_cou++] = c ;
			}
		}
		else {
			if(d==0x58000067) ;
			else LOG(ERR,"end at 0x%08X [%d]",d,data_end-data) ;

			break ;
		}
	}

	// I should now be at the RDO configuration
	d = *data++ ;

	if(d != 0x980000FA) {
		LOG(ERR,"%d: Bad FA 0x%08X",rdo_id,d) ;
		return -1 ;
	}
	
//	for(int i=0;i<16;i++) {
//		LOG(TERR,"FA: %d: 0x%08X",i,data[i]) ;
//	}


	rdo_wire1_id = data[7] ;

	data += 10 ;

	d = *data++ ;

	// I should ne at the end of RDO configuraion
	if(d != 0x580000FB) {
		LOG(ERR,"%d: Bad FB 0x%08X",rdo_id,d) ;
		return -1 ;
	}

	d = *data++ ;

	// I should be now at start of RDO monitoring
	if(d != 0x980000FC) {
		LOG(ERR,"%d: Bad FC 0x%08X",rdo_id,d) ;
		return -1 ;
	}

	ascii_cou = 0 ;
	while(data<data_end) {
		d = *data++ ;

		if((d & 0xFFFFFF00)==0x9800F500) {	//ASCII
			int c = d & 0xFF ;
			if(c=='\n' || ascii_cou==120) {
				ascii_dta[ascii_cou++] = 0 ;

				LOG(INFO,"%d: \"%s\"",rdo_id,ascii_dta) ;
				ascii_cou = 0 ;
			}
			else {
				ascii_dta[ascii_cou++] = c ;
			}
		}
		else {
			if(d==0x580000FD) ;
			else LOG(ERR,"end at 0x%08X [%d]",d,data_end-data) ;

			break ;
		}
	}

#if 0
	// I should now be at the start of FEE stuff
	d = *data++ ;

	if(d != 0x98000018) {
		LOG(ERR,"%d: Bad 18 0x%08X",rdo_id,d) ;
		return -1 ;
	}

	// now the data is from FEE e.g. 0xA03600A0 
	d = *data++ ;

	if((d & 0xFF00FFFF)!=0xA00000A0) {
		LOG(ERR,"%d: bad FEE ASCII 0x%08X",rdo_id,d) ;
	}
#endif

	fee_port = -1 ;
	ascii_cou = 0 ;
	
	while(data<data_end) {
		u_int dd_a, dd_b, dd_c ;
		//u_int fee_id ;

		d = *data++ ;

		//fee_id = (d>>16) & 0xFF ;

		dd_a = d & 0xFFC0FF00 ;
		dd_b = d & 0xFFC000FF ;
		dd_c = d & 0xFFC0000F ;

		if(dd_c==0x98000008) {	// start of FEE in fee_dump() ;
			fee_port = (d >> 4) & 0xFF ;
			//LOG(WARN,"fee_port %d",fee_port) ;
		}
		else if(dd_a==0x0000F500 || dd_a==0x00800000) {	//ASCII
			int c = d & 0xFF ;
			if(c=='\n' || ascii_cou==120) {
				ascii_dta[ascii_cou++] = 0 ;


				u_int id1 = 0 ;
				if(strncmp(ascii_dta,"1Wire:",6)==0) {
					char *id = strstr(ascii_dta,"ID") ;
					if(id) {
						if(sscanf(id,"ID 0x%X",&id1)==1) {
							fee[fee_port].wire1_id = id1 ;
						}
					}
				}
				else if(strncmp(ascii_dta,"Padplane ",9)==0) {
					if(sscanf(ascii_dta,"Padplane %d",&id1)==1) {
						fee[fee_port].padplane_id = id1 ;
					}
				}
				else if(strncmp(ascii_dta,"V: all",6)==0) {
					if(sscanf(ascii_dta,"V: all 0x%X",&id1)==1) {	
						fpga_fee_v_all = id1 ;
						//LOG(WARN,"FEE_asc %d:#%02d: FPGA 0x%08X",rdo_id,fee_port,id1) ;
					}
				}

	
				if(strstr(ascii_dta,"ERROR")) {
					LOG(ERR,"FEE_asc %d:#%02d: \"%s\"",rdo_id,fee_port,ascii_dta) ;
				}
				else {
					LOG(TERR,"FEE_asc %d:#%02d: \"%s\"",rdo_id,fee_port,ascii_dta) ;
				}


				ascii_cou = 0 ;
			}
			else {
				ascii_dta[ascii_cou++] = c ;
			}
		}
		else if(dd_b==0xA00000A0 || dd_b==0x600000A0) {	// ASCII start/stop

		}
		else {
			//LOG(ERR,"end at 0x%08X [%d]",d,data_end-data) ;

			//break ;
		}
	}
		

	if((data_end-data)==0) ;
	else LOG(ERR,"at end 0x%08X [%d]",*data,data_end-data) ;

	return 0 ;
}


int itpcInterpreter::ana_triggered(u_int *data, u_int *data_end)
{
	u_int trg ;
	u_int err = 0 ;
	u_int soft_err = 0 ;
	int fee_cou = 0 ;

	int expect_fee_cou = itpc_config[sector_id].rdo[rdo_id-1].fee_count ;

	fee_port = 0 ;

	//data[0] is 0x98000vv4

	trg = data[1] ;		// trigger_fired
	if(trg==0) return 0 ;	// no triggers

	// start of FEE is at data[2] ;
	data += 2 ;
	

	fee_start:;

	// start of FEE is 0x80ff0010 ;

	fee_version = 0 ;

	if(data[0] == 0x98001000) return 0 ;	// no FEEs; 

	if(fee_cou==0) fee_evt_cou++ ;

	fee_port = 0 ;	// claim unknown
	fee_id = 0 ;	// claim unknown
	fee_cou++ ;	// so it starts from 1


	// at FEE start; FEE header
	//for(int i=0;i<12;i++) {
	//	LOG(TERR,"%d: FEE header %d: 0x%08X",rdo_id,i,data[i]) ;
	//}

#if 0
	// new on Feb 7, 2019 -- added dummy at the very start of run
	// nah, didn't work
	if(data[0]==0x0FEEC0DE) {
		data++ ;
	}
	else if((data[0]&0xFF00FFFF)==0x80000010) {	// normal...

	}
	else {
		// the dummy can get corrupted
		LOG(ERR,"%d: bad first FEE datum 0x%08X",rdo_id,data[0]) ;
		if((data[0]&0x0000FFFF)==0xC0DE) {	// good enough
			data++ ;
		}
	}
#endif

	if((data[0] & 0xFFC0FFFF)==0x80000001) {
		fee_version = 0 ;
		fee_id = (data[0]>>16) & 0xFF ;
	}
	else {	
		u_int f_id[3], d_x[3] ;
		u_int f_ok = 0 ;

		// gotta be fee_version 1
		fee_version = 1 ;

		// I have to have
		//   0x80ff0010
		//   0x00ff4321
		//   0x00ff8765 ;

		// get fee_ids
		f_id[0] = (data[0]>>16) & 0x3F ;
		f_id[1] = (data[1]>>16) & 0x3F ;
		f_id[2] = (data[2]>>16) & 0x3F ;

		// get decoded
		d_x[0] = data[0] & 0xFFC0FFFF ;
		d_x[1] = data[1] & 0xFFC0FFFF ;
		d_x[2] = data[2] & 0xFFC0FFFF ;

		if(d_x[0]==0x80000010) {
			f_ok |= 1 ;
			if(d_x[1]==0x00004321) f_ok |= 2 ;
			if(d_x[2]==0x00008765) f_ok |= 4 ;

			if(f_id[0]==f_id[1]) f_ok |= 8 ;
			if(f_id[1]==f_id[2]) f_ok |= 0x10 ;
			if(f_id[0]==f_id[2]) f_ok |= 0x20 ;
		}

		u_int w = data[0]>>28 ;

		switch(w) {
		case 0 :
		case 8 :
			break ;
		default :
			LOG(ERR,"%d: 0x%08X 0x%08X 0x%08X [0x%02X] (not an error)",rdo_id,data[0],data[1],data[2],f_ok) ;
			LOG(ERR,"%d: 0x%08X 0x%08X 0x%08X [0x%02X] (not an error)",rdo_id,data[-3],data[-2],data[-1],f_ok) ;
			break ;
		}

		if(f_ok!=0x3F) {	// all was NOT OK; hm, this happens ALL the time
			// I see e.g.
			// 
			//LOG(ERR,"%d: 0x%08X 0x%08X 0x%08X [0x%02X] (not an error)",rdo_id,data[0],data[1],data[2],f_ok) ;
			run_err_add(rdo_id,0) ;
		}


		for(int i=0;i<3;i++) {	// hunt for 0x4321
			if(d_x[i]==0x00004321) {
				data = data+i ;
			}
		}
	}
	
	if(expected_fee_version >= 0) {
		if(fee_version != expected_fee_version) {	
			LOG(ERR,"FEE version %d, expected %d",fee_version,expected_fee_version) ;
		}
	}
	
	//at this point data[0]==0x00004321 
	// UNLESS there's a complete screwup which needs recovery!!!
	// The screwup usually happens when the FEE has a FIFO overrun for some reason

	fee_id = (data[0]>>16) & 0x3F ;
		
	switch(fee_version) {
	case 1 :
		if(data[0] != ((fee_id<<16)|0x4321)) err |= 0x10000 ;
		if(data[1] != ((fee_id<<16)|0x8765)) err |= 0x20000 ;

		if((u_int)fee_version != (data[2] & 0xFFFF)) err |= 0x40000 ;

		if(data[3]&0xFFF0) err |= 0x40000 ; 
		fee_port = (data[3] & 0xF) + 1 ;
		//data[4] & data[5] are BX

		if(data[6] != ((fee_id<<16)|0x60000010)) err |= 0x80000 ;	// end of FEE hdr

		if(err) {	// this is a complete screwup!
			run_err_add(rdo_id,3) ;
			goto done ;
		}

		break ;
	default :
		break ;
	}

//	for(int i=0;i<10;i++) {
//		LOG(TERR,"... fee V%d hdr: %d = 0x%08X",fee_version,i,data[i]) ;
//	}
	
	// I need fee_port here!!!

//	LOG(TERR,"... %d: port %d, id %d",fee_cou,fee_port,fee_id) ;


	//data[7] is the start of lane data!!!
	data += 7 ;

//	LOG(TERR,"into sampa_lane_scan: fee_id %d",fee_id) ;

	
	for(int i=0;i<4;i++) {
		u_int expect_mask ;

		if((*data & 0xFC000000) != 0xB0000000) {
			// bits 0x1011abxx e.g. 0xB4
			// a is sync_fifo_overflow
			// b is fifo_overflow and once they latch I _must_ auto-recover!

			if((*data & 0xF0000000) == 0xB0000000) {
				
				run_err_add(rdo_id,1) ;
				LOG(ERR,"%d:#%02d: lane %d: FIFO overflow 0x%08X",rdo_id,fee_port,i,*data) ;

			}
			else {
				run_err_add(rdo_id,4) ;
				LOG(ERR,"%d:#%02d: lane %d: bad sig 0x%08X",rdo_id,fee_port,i,*data) ;
			}

			err |= 0x100 ;
			goto done ;
		}

		switch(i) {
		case 0 :
		case 2 :
			expect_mask = 0x0000FFFF ;
			break ;
		default :
			expect_mask = 0xFFFF0000 ;
			break ;
		}

		found_ch_mask = 0 ;
		data = sampa_lane_scan(data,data_end) ;

		if(found_ch_mask != expect_mask) {	// SAMPA2 error!!!
			run_err_add(rdo_id,2) ;
			//dbg_level = 1 ;
			LOG(ERR,"%d: evt %d: fee_port %d: missing channels in lane %d: expect 0x%08X, got 0x%08X",
				rdo_id,evt_ix,fee_port,i,expect_mask,found_ch_mask) ;

			// enumarte in FEE port
#if 0
			for(int j=0;j<32;j++) {
				if(expect_mask & (1<<j)) {
					if(!(found_ch_mask & (1<<j))) {
						int ch ;

						switch(i) {
						case 0 :
						case 1 :
							ch = j ;
							break ;
						case 2 :
						case 3 :
							ch = j + 32 ;
							break ;
						}

						LOG(ERR,"%d:#%d ch %d missing",rdo_id,fee_port,ch) ;
					}
				}
			}
#endif
			//soft_err |= 0x100 ;
			//err |= 0x400 ;
			//goto done ;
		}

		if(data==0) {
			LOG(ERR,"data 0!!!!") ;
			err |= 0x200 ;
			goto done ;
		}
	}


//	for(int i=0;i<10;i++) {
//		LOG(TERR,"fee trl %d = 0x%08X",i,data[i]) ;
//	}

	// I expect 8 words of end-of-event from the FEE
	switch(fee_version) {
	default :
		break ;
	case 1 :
		// I can have a normal 0xA0000010 trailer or
		// monitoring 0xA00000EC header
		if(data[0] == ((fee_id<<16)|0xA00000EC)) {	// monitoring
			//LOG(WARN,"Monitoring...") ;

			int found_end = 0 ;
			while(data<data_end) {
				if(*data == ((fee_id<<16)|0xA0000010)) {
					found_end = 1 ;
					break ;
				}
					
				u_int dd_a = data[0] & 0xFFC0FF00 ;

				if(dd_a==0x0000F500 || dd_a==0x00800000) {	//ASCII				
					int c = data[0] & 0xFF ;

					if(c=='\n' || ascii_cou==120) {
						ascii_dta[ascii_cou++] = 0 ;

						if(strstr(ascii_dta,"ERROR")) {
							LOG(ERR,"FEE_asc %d:#%02d: \"%s\"",rdo_id,fee_port,ascii_dta) ;
						}
						else {
							LOG(TERR,"FEE_asc %d:#%02d: \"%s\"",rdo_id,fee_port,ascii_dta) ;
						}


						ascii_cou = 0 ;
					}
					else {
						ascii_dta[ascii_cou++] = c ;
					}
				}
				data++ ;
			}

			if(found_end) {
				//LOG(WARN,"monitoring end") ;
			}
			else {
				LOG(ERR,"No monitoring end?") ;
				err |= 0x100000 ;
			}
					
		}


		// at end of FEE data; FEE trailer
		//for(int i=0;i<12;i++) {
		//	LOG(TERR,"%d: FEE trailer: %d: 0x%08X",rdo_id,i,data[i]) ;
		//}

		if(data[0] != ((fee_id<<16)|0xA0000010)) {
			err |= 0x10 ;
			LOG(ERR,"%d:#%02d: hdr FEE word 0x%08X",rdo_id,fee_port,data[0]) ;
		}

//		if(data[7] != ((fee_id<<16)|0x40000010)) {
//			for(int i=0;i<8;i++) {
//				LOG(TERR,"%d: %d: 0x%08X",rdo_id,i,data[i]) ;
//			}
//		}
		
		if(data[7] != ((fee_id<<16)|0x40000010)) {
			LOG(ERR,"%d:#%02d: last FEE word 0x%08X not 0x40..",rdo_id,fee_port,data[7]) ;
			err |= 0x20 ;	// this is the last guy and can misfire
		}

		if((data[1] & 0xFFFF)||(data[2]&0xFFFF)||(data[3]&0xFFFF)||(data[4]&0xFFFF)) {
			LOG(ERR,"%d:#%02d: FEE event errors: 0x%X 0x%X 0x%X 0x%X",rdo_id,fee_port,
			    data[1],data[2],data[3],data[4]) ;
		}

		if(err) {
			run_err_add(rdo_id,5) ;
			goto done ;
		}
		break ;
	}



	data += 8 ;	// start of new FEE


	if(data > data_end) goto done ;

	if(data[0]==0x98001000) goto done ;	// end of FEE section marker!!!

	if(data[1]==0x98001000) {	// still happens!
		//LOG(ERR,"%d:#%d, fee_count %d -- delayed end-event marker [0x%08X]?",rdo_id,fee_port,fee_cou,data[0]) ;
		data++ ;	// advance so not to confuse further checks
		goto done ;	// occassionally a "delayed FEE" is the last one
	}

	if(data[0]==0x980000FC) goto done ;	// RDO-mon start

	if(itpc_config[sector_id-1].rdo[rdo_id-1].fee_count && (fee_cou>itpc_config[sector_id-1].rdo[rdo_id-1].fee_count)) {
		LOG(ERR,"RDO %d: fee_count %d",rdo_id,fee_cou) ;
		goto done ;
	}

	// a bug which causes the 1st FEE datum 0x80xx0010 to seep in
	if((data[0]&0xFF00FFFF)==0x80000010 && fee_cou==expect_fee_cou) {
//		LOG(ERR,"%d: what? 0x%08X",rdo_id,data[0]) ;
		if(data[1]==0x980000FC) {
			data++ ;
			goto done ;
		}
	}

	goto fee_start ;


	done:;

	// End of FEE section; at RDO trailer

	//for(int i=0;i<8;i++) {
	//	LOG(TERR,"%d: RDO trailer check %d: 0x%08X",rdo_id,i,data[i]) ;
	//}


	

	if(err || soft_err) {
		run_err_add(rdo_id,7) ;

		LOG(ERR,"%d:#%02d(id %d,cou %d) evt %d: error 0x%X 0x%X",
		    rdo_id,fee_port,fee_id,fee_cou,evt_ix,err,soft_err) ;

		for(int i=-4;i<8;i++) {
			LOG(ERR,".... %d = 0x%08X",i,data[i]) ;	
		}
		
		if(err || soft_err) return -1 ;
	}

	// this only works online
	if(realtime) {
		// needs more work in case of a masked fee
//		if(fee_cou != expect_fee_cou) {
//			LOG(ERR,"%d: fees found %d, expect %d",rdo_id,fee_cou,expect_fee_cou) ;
//		}
	}

	after_fee:;

	// I can either have the trailer with the trigger data
	// OR I can have the monitoring event following...

	switch(data[0]) {
	case 0x98001000 :	// start of trailer
		if(data[1]!=0) {	// event status
			run_err_add(rdo_id,8) ;
			LOG(ERR,"RDO %d: bad event status 0x%08X",rdo_id,data[1]) ;
		}

		{
		int trg_cou = data[2] ;
		
		data += 2+trg_cou+1 ;
		}

		if(data[0] != 0x58001001) {
			LOG(ERR,"RDO %d: no end-of-trailer 0x%08X",rdo_id,data[0]) ;
		}

		data++ ;

		//data[0] & data[1] are the FEE stop,start pairs (hi,lo) 
		//for(int i=0;i<8;i++) {
		//	LOG(TERR,"%d: %d: 0x%08X",rdo_id,i,data[i]) ;
		//}


		break ;
	case 0x980000FC :	// RDO_mon
		data++ ;
		ascii_cou = 0 ;
		while(data<data_end) {
			u_int d = *data++ ;

			if((d&0xFFFFFF00)==0x9800F500) {
				int c = d & 0xFF ;

				if(c=='\n' || ascii_cou==120) {
					ascii_dta[ascii_cou++] = 0 ;

					LOG(INFO,"RDO_asc %d: \"%s\"",rdo_id,ascii_dta) ;

					ascii_cou = 0 ;
				}
				else {
					ascii_dta[ascii_cou++] = c ;
				}

			}
			else if(d==0x580000FD) goto after_fee ;
			else LOG(WARN,".... %d 0x%08X",data_end-data,d) ;
		}
	

		break ;
	default :	
		for(int i=0;i<16;i++) {
			LOG(ERR,"After FEE: %d [%d] = 0x%08X",i,data_end-data,*data) ;
			data++ ;
		}
		break ;
	}

	return 0 ;
}

int itpcInterpreter::ana_pedestal(u_int *data, u_int *data_end)
{
	int fee_port = 0 ;

	data++ ;	// skip 9800FD60
	
	while(data<data_end) {
		u_int dd_f = data[0] & 0xFF00FFFF ;
		u_int dd_r = data[0] & 0xFF00000F ;

		if(data[0]==0x5800FD61) {
			LOG(INFO,"pedestal packet done") ;
			break ;
		}

		if((data[0] & 0xFFFFFF00)==0x98AAAA00) {
			LOG(TERR,"-> Done port %d",(data[0]&0xFF)+1) ;
		}
		else if((data[0] & 0xFFFFFF00)==0x98BBBB00) {
			LOG(TERR,"--> Done channel %d",data[0]&0xFF) ;
		}
		else if(dd_r==0x98000008) {
			fee_port = ((data[0])>>4)&0x1F ;
		}
		else if(dd_r==0x58000009) {

		}
		else if(dd_f==0x80000003) {
			u_int s[7] ;
			
			int fee_id = (data[0] >> 16) & 0xFF ;
			
			for(int i=0;i<7;i++) {
				s[i] = data[1+i] & 0xFFFF ;
			}

			int for_me = s[0]&1 ;
			int my_port = (s[0]>>8) & 0xFF ;

			int len = s[1] & 0x3FF ;
			int ch = (s[1]>>10)&0x3F ;

			u_int ticks1 = (s[4]<<16)|s[3] ;
			u_int ticks2 = (s[6]<<16)|s[5] ;
			LOG(TERR,"#%02d(%02d): fee_id %02d: for_me %d: ch %2d, len %3d",fee_port,my_port,fee_id,for_me,ch,len) ;
			LOG(TERR,"       chsum 0x%04X, ticks %u %u",s[2],ticks1,ticks2) ;
			


			data += 7 ;			
	
		}
		else {
			LOG(TERR,"... 0x%08X",data[0]) ;
		}		
		data++ ;
	}

	return 0 ;
}

int itpcInterpreter::rdo_scan_top(u_int *data, int words)
{
	u_int *data_end = data + words ;
	u_int *data_start = data ;
	u_int d ;
	int ret = 0 ;

	//some preliminaries which used to be in start_event
	evt_ix++ ;
	evt_bytes = words*4 ;
	word_ix = 0 ;
	status = 0 ;
	state = S_IDLE ;
	fee_port = -1 ;
	d_cou = 01 ;
	sampa_bx = -1 ;
	ascii_cou = 0 ;
	memset(evt_err,0,sizeof(evt_err)) ;

	// move forward until I hit start-comma
	int w_cou = (words<16)?words:16 ;

	// the data is already SWAPPED if processed in the sector brokers!!!
	for(int i=0;i<w_cou;i++) {
		LOG(NOTE,"...%d/%d = 0x%08X",i,words,data[i]) ;

		if((data[i] == 0xCCCC001C)||(data[i] == 0x001CCCCC)) {
			data = data + i ;
			break ;
		}
	}

	w_cou = data_end - data ;

	if(data[0]==0xCCCC001C) {	// need swapping!!!!
		LOG(NOTE,"swapping") ;
		for(int i=0;i<w_cou;i++) {
			data[i] = sw16(data[i]) ;
		}
	}

	d = *data++ ;	// now at start comma

	switch(d) {
	case 0xFFFF001C :
	case 0x001CCCCC :
		break ;
	default :
		LOG(ERR,"%d: First word is not a START comma!? [0x%08X 0x%08X 0x%08X]",rdo_id,data[-1],data[0],data[1]) ;
		return -1 ;
	}

	
//	for(int i=0;i<16;i++) {
//		LOG(TERR,"%d/%d = 0x%08X",i,words,data[i]) ;
//	}

	d = *data ;	// now at start packet word from RDO

	switch(d) {
	case 0x9800FD71 :	// send config packet
		// dump to special handler
		// but fish the rdo_version, eh?
		ret = ana_send_config(data,data_end) ;
		break ;
	case 0x9800FD80 :	// start run
		// check some basics immediatelly here
		LOG(TERR,"%d: run_start packet: 0x%08X 0x%08X 0x%08X",rdo_id,data[0],data[1],data[2]) ;

		if(data[2] != 0x11223344) {
			LOG(ERR,"%d: run_start: bad signature 0x%08X",rdo_id,data[2]) ;
			goto err_ret ;
		}

		if(data[1]) {
			LOG(ERR,"%d: run_start: already bad status 0x%08X",rdo_id,data[1]) ;
			goto err_ret ;
		}

		return 0 ;

		break ;
	case 0x9800FD60 :	// pedestal response from FEE
		ret = ana_pedestal(data,data_end) ;
		break ;
	case 0x5800FD81 :	// end of run???
		LOG(WARN,"End of run 0x%08X",d) ;
		break ;
	default :
		if((d & 0xFF00000F)==0x98000004) {	// triggered event
			rdo_version = (d >> 4) & 0xFF ;

			LOG(NOTE,"rdo_version %d",rdo_version) ;

			if(rdo_version==0) {	// used the old code (which remains frozen)
				ret = rdo_scan(data_start,words) ;				
			}
			else {
				ret = ana_triggered(data,data_end) ;
			}
			break ;
		}
		LOG(ERR,"Unknown packet 0x%08X",d) ;
		goto err_ret ;
	}


//	done:;

	// from stop_event
	for(int i=0;i<8;i++) {
		if(evt_err[i]) LOG(ERR,"%d: event errors[%d] = %u",rdo_id,i,evt_err[i]) ;
	}


	return ret ;	


	err_ret:;

	for(int i=0;i<16;i++) {
		LOG(TERR,".... bad evt: %d = 0x%08X",i,data[i]) ;
	}

	return -1 ;
}

/*
	This is the frozen, pre-April 2018 unpacker for rdo_version=0
*/
int itpcInterpreter::rdo_scan(u_int *data, int words)
{
//	int status = 0 ;
	int log_start = 0 ;

	u_int *data_end = data + words ;
	u_int *data_start = data ;
	u_int rh_xing_start = 0 ;
	u_int flags = 0 ;

	u_int d  ;

	char mon_string[512] ;
	int mon_cou = 0 ;

	if(data[0] != 0xDDDDDDDD) {
		LOG(ERR,"%d: words %d = 0x%08X",rdo_id,words,data[0]) ;
	}

//	for(int i=(words-16);i<(words+32);i++) {
//		LOG(TERR,"E %d/%d = 0x%08X",i,words,data[i]) ;
//	}

	// the data is already SWAPPED if processed in the sector brokers!!!
	for(int i=0;i<16;i++) {
		LOG(NOTE,"...%d/%d = 0x%08X",i,words,data[i]) ;

		if((data[i] == 0xCCCC001C)||(data[i] == 0x001CCCCC)) {
			data = data + i ;
			words-- ;
			break ;
		}
	}


	if(words<=0) {
		LOG(ERR,"%d: words %d = 0x%08X",rdo_id,words,data[0]) ;
	}

	if((data[0] == 0xCCCC001C)||(data[0] == 0x001CCCCC)) ;	//as it should be
	else {
		LOG(ERR,"%d: words %d = 0x%08X",rdo_id,words,data[0]) ;
	}

	if(data[0]==0xCCCC001C) {	// need swapping!!!!
		LOG(NOTE,"swapping") ;
		for(int i=0;i<words;i++) {
			data[i] = sw16(data[i]) ;
		}
	}
	else {
		LOG(NOTE,"%d: words %d = 0x%08X",rdo_id,words,data[0]) ;
	}

	d = *data++ ;

	switch(d) {
	case 0xFFFF001C	:	//old style
	case 0x001CCCCC :	// new style
		flags |= 1 ;
		break ;
	default:
		LOG(ERR,"%d: First word is not a START comma!? [0x%08X 0x%08X 0x%08X]",rdo_id,data[-1],data[0],data[1]) ;
	}

//	LOG(TERR,"RDO scan") ;

	while(data<data_end) {
		u_int hdr ;

		word_ix = data - data_start ;

		d = *data++ ;

		LOG(NOTE,"%d/%d = 0x%08X",word_ix,words,d) ;

		switch(d) {
		//case 0x5800FD71 :	// end of send_config 
		//case 0x5800FD81 :	// end of run
		case 0x5800FD61 :	// end of manual "Fd"
			LOG(INFO,"END of something [0x%08X]",d) ;
			return 0 ;
		}

		if(log_start) {
			LOG(TERR,"... %d = 0x%08X",word_ix,d) ;
		}

		if(d==0x5800FD01) {
			LOG(WARN,"End of run (but still %d words)",data_end-data) ;
			return -1 ;
			//goto re_loop ;
		}

		switch(state) {
		case S_FEE_ASCII :
			if((d&0xFFFF0000)==0x00AA0000) {
				u_char c = d & 0xFF ;

				if(isprint(c) || isspace(c)) ;
				else c = '?' ;

				ascii_dta[ascii_cou++] = c ;
				if(c=='\n') {
					ascii_dta[ascii_cou++] = 0 ;
					LOG(ERR,"WTF?") ;
					printf("HERE #%02d: %s",fee_port,ascii_dta) ;

					if(strncmp(ascii_dta,"1Wire ",6)) {
						u_int id1 ;
						char *id = strstr(ascii_dta,"ID") ;
						sscanf(id,"ID 0x%X",&id1) ;
						printf("=====> 0x%08X",id1) ;
					}
					fflush(stdout) ;
					ascii_cou = 0 ;
				}
			}
			else {
				LOG(ERR,"Bad: %d = 0x%08X",word_ix,d) ;
			}
			state = S_FEE_ASCII_END ;
			goto re_loop ;

		case S_FEE_ASCII_END :
			if((d&0xF80000FF)!=0x600000A1) {
				LOG(ERR,"Bad %d = 0x%08X",word_ix,d) ;
			}
			state = S_IDLE ;
			goto re_loop ;
		case S_FEE_PORT :
			fee_port = (d&0xFF)+1 ;
			LOG(NOTE,"FEE port #%02d",fee_port) ;
			state = S_IDLE ;
			goto re_loop ;
		case S_TRIGGER :
			LOG(TERR,"Trg %2d = 0x%08X",d_cou,d) ;
			d_cou++ ;
			if((d&0xF80000FF)==0x58000005) {
				state = S_IDLE ;
			}
			goto re_loop ;
		case S_IDLE :
		default :
			break ;
		}

		if((d & 0xF80000FF)==0xA00000A0) {
			state = S_FEE_ASCII ;
			goto re_loop ;
		}


		hdr = d & 0xF800FFFF ;

		switch(hdr) {
		case 0x9800FD70 :
			LOG(INFO,"SEND_CONFIG: start") ;
			break ;
		case 0x5800FD71 :
			if(fout) {
				fprintf(fout,"%d: 1-wire Id 0x%08X\n",rdo_id,rdo_wire1_id) ;
			}

			LOG(INFO,"SEND_CONFIG: end for RDO ID 0x%08X",rdo_wire1_id) ;
			for(int i=1;i<=16;i++) {
				LOG(INFO,"   FEE #%02d: Padplane %02d, 1Wire 0x%08X",i,fee[i].padplane_id,fee[i].wire1_id) ;
				if(fout) {
					fprintf(fout,"   FEE #%02d: Padplane %02d, 1Wire 0x%08X\n",i,fee[i].padplane_id,fee[i].wire1_id) ;
				}
			}

			if(fout) fflush(fout) ;

			//fee_dbase() ;

			return 2 ;
			break ;
		case 0x9800FD80 :
			LOG(INFO,"RUN_START: start") ;
			break ;
		case 0x5800FD81 :
			LOG(INFO,"RUN_START: stop, events %u",evt_ix) ;
			return 0 ;
			break ;
		case 0x98000008 :
			// sometimes I get ASCII immediatelly
			if((data[1]&0xFFC00000)!=0x80000000) {
				if((data[1]&0xFFC0FFFF)!=0xA00000A0) {
					// I read e.g. 0x002E0000 instread of 0x802E0001
					//LOG(ERR,"%d: evt %d: After start 0x%08X, FEE #%d",rdo_id,evt_ix,data[1],data[0]+1) ;
				}
			}


			data = fee_scan(data-1, data_end) ;
#if 0
			{
			LOG(TERR,"Left fee_port %d: %d",fee_port,data_end-data) ;
			if(fee_port==16) {
				u_int *d = data ;

				while(d<=data_end) {
					LOG(TERR,"%d = 0x%08X",data_end-d,*d) ;
					d++ ;
				}
			}
			}
#endif
#if 0
			fee_port = *data++ ;
			fee_port &= 0xFF ;
			fee_port++ ;
			LOG(NOTE,"RDO FEE data: START: fee_port %d",fee_port) ;
#endif
			break ;
		case 0x58000009 :
			LOG(NOTE,"RDO FEE data: END: fee_port %d",fee_port) ;
			break ;
		case 0x98000004 :
			if(flags & 0x6) {
				LOG(ERR,"Duplicate hdr") ;
			}

			flags |= 2 ;
			LOG(NOTE,"RDO Event Header: START") ;
			{
				u_int *d_now = data ;

				for(int i=0;i<10;i++) {
					
					LOG(NOTE,"Event Header %2d = 0x%08X",i,d) ;

					if(d==0x58000005) {
						flags |= 4 ;
						break ;
					}
					else {
						d = *data++ ;
					}
				}

				if((data-d_now)!=8) {
					LOG(ERR,"RDO Event Header corrupt %d",data-d_now) ;
				}

			}
			break ;
		case 0x58000005 :
			LOG(ERR,"RDO Event Header: END (could be spurious)") ;
			break ;
		case 0x98000006 :
			fee_evt_cou++ ;
			LOG(NOTE,"RDO FEE complement: START") ;
			break ;
		case 0x58000007 :
			LOG(NOTE,"RDO FEE complement:   END") ;
			break ;
#if 0
		case 0x98000044 :
			d = *data++ ;
			if(d) {
				LOG(ERR,"RDO FEE Readout Status: START: status 0x%08X",d) ;
			}
			else {
				LOG(NOTE,"RDO FEE Readout Status: START: status 0x%08X",d) ;
			}
			break ;
		case 0x58000045 :
			LOG(NOTE,"RDO FEE Readout Status:   END") ;
			break ;
#endif

		case 0x98000066 :
			LOG(NOTE,"RDO Boottext: START: %d = 0x%08X",word_ix,d) ;
			for(;;) {
				d = *data++ ;
				if((d & 0xFFFFFF00)==0x9800F500) {
					int c = d & 0xFF ;
					if(c=='\n') {
						mon_string[mon_cou++] = 0 ;

						if(fout) {
							fprintf(fout,"%d: %s\n",rdo_id,mon_string) ;
							fflush(fout) ;
						}
						LOG(INFO,"%d: \"%s\"",rdo_id,mon_string) ;
						mon_cou = 0 ;
					}
					else {
						mon_string[mon_cou++] = c ;
					}
					//printf("%c",d&0xFF) ;
				}
				else {
					LOG(NOTE,"RDO Boottext: END: %d = 0x%08X",word_ix,d) ;	
					mon_cou = 0 ;
					break ;
				}
			}
			fflush(stdout) ;
			break ;
		case 0x58000067 :	// should not see it here...
			LOG(ERR,"RDO Boottext:  END: %d = 0x%08X",word_ix,d) ;
			break ;

		case 0x980000FC :
			LOG(NOTE,"RDO Monitoring: START: %d = 0x%08X",word_ix,d) ;
			for(;;) {
				d = *data++ ;
				if((d & 0xFFFFFF00)==0x9800F500) {
					int c = d & 0xFF ;
					if(c=='\n') {
						mon_string[mon_cou++] = 0 ;
						LOG(INFO,"%d: \"%s\"",rdo_id,mon_string) ;
						if(fout) {
							fprintf(fout,"%d: \"%s\"\n",rdo_id,mon_string) ;
							fflush(fout) ;
						}
						mon_cou = 0 ;
					}
					else {
						mon_string[mon_cou++] = c ;
					}
					//printf("%c",d&0xFF) ;
				}
				else {
					LOG(NOTE,"RDO Monitoring: END: %d = 0x%08X",word_ix,d) ;
					mon_cou =0 ;
					break ;
				}
			}
			fflush(stdout) ;
			break ;
		case 0x580000FD :	// should not see it here...
			LOG(ERR,"RDO Monitoring:  END: %d = 0x%08X",word_ix,d) ;
			break ;
		case 0x98001000 :
			if(flags & 0x18) {
				LOG(ERR,"RDO %d: duplicate trailer",rdo_id) ;
			}
			flags |= 8 ;
			LOG(NOTE,"RDO: Event Trailer Start") ;
			{
				u_int trg_cou ;
				u_int *d_now = data ;
				int suspect = 0 ;

				if(data[0] != 0xABCD0000) {
					suspect = 1 ;
					LOG(ERR,"RDO %d: Event Trailer %u/%u = ABCD 0x%08X",rdo_id,word_ix,words,data[0]) ;
				}

				if(data[1] != 0) {
					suspect = 1 ;
					//often!
					//LOG(ERR,"RDO %d: Event Trailer %u/%u = status 0x%08X",rdo_id,word_ix,words,data[1]) ;
				}

				trg_cou=data[2] ;
				if(trg_cou>100) {
					suspect = 1 ;
					//often
					//LOG(ERR,"RDO %d: Event Trailer %u/%u = trg_cou 0x%08X",rdo_id,word_ix,words,data[2]) ;
					trg_cou = 0 ;
				}

				if(suspect) {
					// almost always 0xF which is correct.
					if(flags != 0xF) LOG(ERR,"flags 0x%X",flags) ;
				}

				if((data[3+trg_cou+2]&0xF800FFFF)!=0x58001001) suspect = 1 ;	// very often


				//LOG(TERR,"END 0x%08X",data[3+trg_cou+2]) ;

				// data[0] = 0xabcd0000 ;
				// data[1] = status (must be 0)
				// data[2] = trigger count
				// data[3..] = triggers
				// data[x] = yada ;
				// data[x+1] = yada
				// data[x+2] = 0x58001001 ;

				for(int i=0;i<100;i++) {
					//if(i==2 && d) {	//status
					//	LOG(ERR,"FEE #%d: Event Trailer %u = 0x%08X",fee_port,word_ix,d) ;
					//}

					LOG(NOTE,"Event Trailer %2d = 0x%08X",i,d) ;

					if((d&0xF800FFFF)==0x58001001) {
						flags |= 0x10 ;
						break ;
					}
					else {
						d = *data++ ;
						if(data>data_end) {
							flags |= 0x1000 ;
							suspect = 2 ;
							break ;
						}
					}
				}

				// unfortuntatelly this is not a fixed value... THINK!
				// ALSO -- it seems to be 100 easily which means that I am missing the end of event!
				int t_len = data - d_now ;
				if(0) {
				//if(suspect) {
					LOG(ERR,"RDO %d: Event Trailer Suspect %d - %d",rdo_id,suspect,t_len) ;
					for(int i=0;i<16;i++) {
						LOG(TERR,"%d/%d = 0x%08X",d_now-data_start,words,*d_now) ;
						d_now++ ;
					}
					data = data_end + 1 ;	// to make sure it's over
				}
			}

			break ;
		case 0x58001001 :	// should not see it here
			LOG(ERR,"RDO: Event Trailer End") ;
			break ;

		case 0x980000FA :
			LOG(NOTE,"RDO Configuration: START: %d = 0x%08X",word_ix,d) ;
			//LOG(TERR,"RDO wire1_id 0x%08X",data[7]) ;
			rdo_wire1_id = data[7] ;
			data += 10 ;
			break ;
		case 0x580000FB :
			LOG(NOTE,"RDO Configuration:   END: %d = 0x%08X",word_ix,d) ;
			break ;
		case 0x9800FD60:
			LOG(NOTE,"RDO FEE DUMP : START") ;
			break ;
		case 0x980000F8 :
			if(data[1]) {
				LOG(ERR,"%d: FEE #%d: stat 0x%08X 0x%08X 0x%08X 0x%08X",rdo_id,fee_port,data[1],data[2],data[3],data[4]) ;
			}
			else {
				LOG(TERR,"%d: FEE #%d: stat 0x%08X 0x%08X 0x%08X 0x%08X",rdo_id,fee_port,data[1],data[2],data[3],data[4]) ;
			}
			//log_start = 1 ;
			break ;


		// FEE headers
		// 0x8 is start triggered
		//  0x4 is end triggred
		// 0xA is start header
		//  0x6 is end header

		case 0x80000010 :
			{
				
				u_int h, l ;
				u_int glo_status ;
				
				l = (*data++) & 0xFFFF ;
				h = (*data++) & 0xFFFF ;
			
				glo_status = (h<<16) | l ;

				if(glo_status) {
					LOG(ERR,"FEE SEND_CONFIG: START 0: status 0x%08X",glo_status) ;
				}
				else {
					LOG(NOTE,"FEE SEND_CONFIG: START 0: status 0x%08X",glo_status) ;
				}
			}
			break ;
		case 0x60000011 :
			LOG(NOTE,"FEE SEND_CONFIG:   END 0") ;
			break ;

		case 0xA00000FA :
			LOG(NOTE,"FEE Configuration: START") ;
			break ;
		case 0x600000FB :
			LOG(NOTE,"FEE Configuration:   END") ;
			break ;

		case 0xA00000EC :
			LOG(NOTE,"FEE Monitoring: START 0") ;
			break ;
		case 0x600000ED :
			LOG(NOTE,"FEE Monitoring:   END 0") ;
			break ;
		case 0xA00000FC :
			LOG(NOTE,"FEE Monitoring: START 1") ;
			break ;
		case 0x600000FD :
			LOG(NOTE,"FEE Monitoring:   END 1") ;
			break ;

		case 0xA0000020 :
			{
				
				u_int h, l ;
				u_int glo_status ;
				
				l = (*data++) & 0xFFFF ;
				h = (*data++) & 0xFFFF ;
			
				glo_status = (h<<16) | l ;

				if(glo_status) {
					LOG(ERR,"FEE SEND_CONFIG: START 1: status 0x%08X",glo_status) ;
				}
				else {
					LOG(NOTE,"FEE SEND_CONFIG: START 1: status 0x%08X",glo_status) ;
				}
			}

			break ;
		case 0x40000021 :
			LOG(NOTE,"FEE SEND_CONFIG:   END 1 (all)") ;
			break ;


		case 0x80000200 :
			LOG(NOTE,"FEE Trigger: START: TRIGGERED") ;
			{
			u_int h, l ;
			u_int bx_xing ;
			u_int glo_status ;
			u_int fee_evt ;
			u_int type ;

			l = (*data++) & 0xFFFF ;
			h = (*data++) & 0xFFFF ;
			
			glo_status = (h<<16) | l ;


			l = (*data++) & 0xFFFF ;
			h = (*data++) & 0xFFFF ;
			
			rh_xing_start = (h<<16) | l ;


			l = (*data++) & 0xFFFF ;
			h = (*data++) & 0xFFFF ;
			
			bx_xing = (h<<16) | l ;

			
			l = (*data++) & 0xFFFF ;
			h = (*data++) & 0xFFFF ;
			
			type = (h<<16) | l ;

			l = (*data++) & 0xFFFF ;
			h = (*data++) & 0xFFFF ;
			
			fee_evt = (h<<16) | l ;


			int fee_id = (d>>16)&0x3F ;

			if(glo_status) {
				LOG(ERR,"FEE %d(#%d) START Event %d: type 0x%08X, xing %u, 20bit %u, RHIC %u, glo 0x%08X",fee_id,fee_port,
				    fee_evt,type,bx_xing,bx_xing&0xFFFFF,rh_xing_start,glo_status) ;
			}
			else {
				LOG(TERR,"FEE %d(#%d) START Event %d: type 0x%08X, xing %u, 20bit %u, RHIC %u, glo 0x%08X",fee_id,fee_port,
				    fee_evt,type,bx_xing,bx_xing&0xFFFFF,rh_xing_start,glo_status) ;
			}

			}
			break ;
		case 0x60000201 :
			LOG(NOTE,"FEE Trigger:   END: hdr") ;
			break ;
		case 0xA0000300:
			{
				u_int a, b ;
				u_int status ;
				u_int xing ;
				u_int glo_status ;
				u_int fee_evt ;

				a = (*data++) & 0xFFFF ;	//lo
				b = (*data++) & 0xFFFF ;	//ji

				glo_status = (b<<16) | a ;


				a = (*data++) & 0xFFFF ;	//lo
				b = (*data++) & 0xFFFF ;	//ji

				status = (b<<16) | a ;

				a = (*data++) & 0xFFFF ;	//lo
				b = (*data++) & 0xFFFF ;	//ji
				
				xing = (b<<16) | a ;

				a = (*data++) & 0xFFFF ;	//lo
				b = (*data++) & 0xFFFF ;	//ji
				
				fee_evt = (b<<16) | a ;

				if(glo_status || status) {
					LOG(ERR,"FEE END Event %d: glo 0x%08X, status 0x%08X, xing %u, delta %u",
					    fee_evt,glo_status,status, xing, xing-rh_xing_start) ;
				}
				else {
					LOG(TERR,"FEE END Event %d: glo 0x%08X, status 0x%08X, xing %u, delta %u",
					    fee_evt,glo_status,status, xing, xing-rh_xing_start) ;
				}
			}
			break ;
		case 0x40000301:
			LOG(WARN,"FEE Event:   END") ;
			break ;
		default :	// all other cases

			if(words==(data-data_start)) {	// last word
				if((d & 0xFFFF0000)==0x005C0000) break ;	// last word and it's a end-comma -- normal

				//last word but it's some gibberish -- it can happen occassionally, don't know why
				//LOG(WARN,"%d: last huh 0x%08X at %d/%d",rdo_id,d,word_ix,words) ;
				break ;
			}	
				

			//LOG(WARN,"%d: huh 0x%08X at %d/%d",rdo_id,d,word_ix,words) ;

#if 0
			if((d & 0xFFFF0000)==0x005C0000) ;	// stop-comma is OK here
			else {
				if(words==(data-data_start)) ;	// OK for the last word
				else {
					// junk at the end of event
					//LOG(WARN,"%d: datum huh 0x%08X at %d/%d",rdo_id,d,data-data_start,words) ;
				}
			}
#endif

			break ;
		}


		if(flags & 0x10) {
			//LOG(TERR,"flags 0x%X",flags) ;
			break ;
		}
		re_loop: ;

	}



//	if((flags != 0x1F) || ((data_end-data) != 0)) {
	if((flags != 0x1F)) {
		LOG(ERR,"At end: %d, flags 0x%X",data_end-data,flags) ;
		for(int i=0;i<32;i++) {
			LOG(WARN,"... %2d = 0x%08X",i,data_end[16-i]) ;
		}
	}

	if((data[-1] & 0xFFFF0000)==0x005C0000) {
		log_start = 0 ;
		//LOG(WARN,"Stop Comma at the very end") ;
		return 1 ;
	}
	else LOG(NOTE,"%d: no end-comma %d %d = 0x%08X!",rdo_id,data_end-data,words,data[-1]) ;

	return 1 ;



}



/* Tonko: received from Arild on 16 Nov 2017

buffer[0] -- first 30 bits of 50 bit word
buffer[1] -- last 20 bits of 50 bit word

*/

typedef unsigned char uint8_t ;

static void hammingdecode(unsigned int buffer[2], bool& error, bool& uncorrectable, bool fix_data) // Least efficient hamming decoder ever
{

  // header split
  bool parityreceived[6];
  bool data_in[43];
  bool overallparity;

  for (int i = 0; i < 6; i++)
    parityreceived[i] = (buffer[0] >> i) & 0x1;

  overallparity = (buffer[0] >> 6) & 0x1;

  //for (int i = 0; i < 43; i++)
  //  data_in[i] = (header_in >> (i + 7)) & 0x1;

  for (int i = 7; i < 30; i++)
    data_in[i-7] = (buffer[0] >> i) & 0x1;

  for (int i = 30; i < 50; i++)
    data_in[i-7] = (buffer[1] >> (i - 30)) & 0x1;

  //calculated values
  bool corrected_out[43];
  bool overallparitycalc = 0;
  bool overallparity_out = 0;
  bool paritycalc[6];
  bool paritycorreced_out[6];

  ////////////////////////////////////////////////////////////////////////////////////////////////
  // calculate parity
  paritycalc[0]   =   data_in[0]  ^ data_in[1]  ^ data_in[3]  ^ data_in[4]  ^ data_in[6]  ^ 
                      data_in[8]  ^ data_in[10] ^ data_in[11] ^ data_in[13] ^ data_in[15] ^ 
                      data_in[17] ^ data_in[19] ^ data_in[21] ^ data_in[23] ^ data_in[25] ^ 
                      data_in[26] ^ data_in[28] ^ data_in[30] ^ data_in[32] ^ data_in[34] ^ 
                      data_in[36] ^ data_in[38] ^ data_in[40] ^ data_in[42];

  paritycalc[1]   =   data_in[0]  ^ data_in[2]  ^ data_in[3]  ^ data_in[5]  ^ data_in[6]  ^ 
                      data_in[9]  ^ data_in[10] ^ data_in[12] ^ data_in[13] ^ data_in[16] ^ 
                      data_in[17] ^ data_in[20] ^ data_in[21] ^ data_in[24] ^ data_in[25] ^ 
                      data_in[27] ^ data_in[28] ^ data_in[31] ^ data_in[32] ^ data_in[35] ^ 
                      data_in[36] ^ data_in[39] ^ data_in[40] ;

  paritycalc[2]   =   data_in[1]  ^ data_in[2]  ^ data_in[3]  ^ data_in[7]  ^ data_in[8]  ^ 
                      data_in[9]  ^ data_in[10] ^ data_in[14] ^ data_in[15] ^ data_in[16] ^ 
                      data_in[17] ^ data_in[22] ^ data_in[23] ^ data_in[24] ^ data_in[25] ^ 
                      data_in[29] ^ data_in[30] ^ data_in[31] ^ data_in[32] ^ data_in[37] ^
                      data_in[38] ^ data_in[39] ^ data_in[40] ;

  paritycalc[3]   =   data_in[4]  ^ data_in[5]  ^ data_in[6]  ^ data_in[7]  ^ data_in[8]  ^ 
                      data_in[9]  ^ data_in[10] ^ data_in[18] ^ data_in[19] ^ data_in[20] ^ 
                      data_in[21] ^ data_in[22] ^ data_in[23] ^ data_in[24] ^ data_in[25] ^
                      data_in[33] ^ data_in[34] ^ data_in[35] ^ data_in[36] ^ data_in[37] ^
                      data_in[38] ^ data_in[39] ^ data_in[40] ;

  paritycalc[4]   =   data_in[11] ^ data_in[12] ^ data_in[13] ^ data_in[14] ^ data_in[15] ^ 
                      data_in[16] ^ data_in[17] ^ data_in[18] ^ data_in[19] ^ data_in[20] ^ 
                      data_in[21] ^ data_in[22] ^ data_in[23] ^ data_in[24] ^ data_in[25] ^
                      data_in[41] ^ data_in[42] ;

  paritycalc[5]   =   data_in[26] ^ data_in[27] ^ data_in[28] ^ data_in[29] ^ data_in[30] ^
                      data_in[31] ^ data_in[32] ^ data_in[33] ^ data_in[34] ^ data_in[35] ^
                      data_in[36] ^ data_in[37] ^ data_in[38] ^ data_in[39] ^ data_in[40] ^
                      data_in[41] ^ data_in[42] ;
  ////////////////////////////////////////////////////////////////////////////////////////////////

  uint8_t syndrome = 0;

  for (int i = 0; i < 6; i++)
   syndrome |= (paritycalc[i]^parityreceived[i]) << i;

  bool data_parity_interleaved[64];
  bool syndromeerror;

  //data_parity_interleaved[0]          =  0;
  data_parity_interleaved[1]          =  parityreceived[0];
  data_parity_interleaved[2]          =  parityreceived[1];
  data_parity_interleaved[3]          =  data_in[0];
  data_parity_interleaved[4]          =  parityreceived[2];
  for (int i = 1; i <= 3; i++)
    data_parity_interleaved[i+5-1]    =  data_in[i];
  data_parity_interleaved[8]          =  parityreceived[3];
  for (int i = 4; i <= 10; i++)
    data_parity_interleaved[i+9-4]    =  data_in[i];
  data_parity_interleaved[16]         =  parityreceived[4];
  for (int i = 11; i <= 25; i++)
    data_parity_interleaved[i+17-11]  =  data_in[i];
  data_parity_interleaved[32]         =  parityreceived[5];
  for (int i = 26; i <= 42; i++)
    data_parity_interleaved[i+33-26]  =  data_in[i];
  //for (int i = 50; i <= 63; i++)
  //  data_parity_interleaved[i]        =  0;

  data_parity_interleaved[syndrome] =  !data_parity_interleaved[syndrome]; // correct the interleaved

  paritycorreced_out[0] = data_parity_interleaved[1];
  paritycorreced_out[1] = data_parity_interleaved[2];
  corrected_out[0]        = data_parity_interleaved[3];
  paritycorreced_out[2]   = data_parity_interleaved[4];
  for (int i = 1; i <= 3; i++)
    corrected_out[i]      = data_parity_interleaved[i+5-1];
  paritycorreced_out[3]   = data_parity_interleaved[8];
  for (int i = 4; i <= 10; i++)
    corrected_out[i]     = data_parity_interleaved[i+9-4];
  paritycorreced_out[4]   = data_parity_interleaved[16];
  for (int i = 11; i <= 25; i++)
    corrected_out[i]    = data_parity_interleaved[i+17-11];
  paritycorreced_out[5]   = data_parity_interleaved[32];
  for (int i = 26; i <= 42; i++)
    corrected_out[i]    = data_parity_interleaved[i+33-26];

    // now we have the "corrected" data -> update the flags

  bool wrongparity;
  for (int i = 0; i < 43; i++)
    overallparitycalc ^=data_in[i];
  for (int i = 0; i < 6; i++)
    overallparitycalc ^= parityreceived[i];
  syndromeerror = (syndrome > 0) ? 1 : 0; // error if syndrome larger than 0
  wrongparity = (overallparitycalc != overallparity);
  overallparity_out = !syndromeerror &&  wrongparity ? overallparitycalc : overallparity; // If error was in parity fix parity
  error = syndromeerror |  wrongparity;
  uncorrectable = (syndromeerror && (!wrongparity));

  
  //header_out = 0;
  //for (int i = 0; i < 43; i++)
  //  header_out |= corrected_out[i] << (i + 7);
  //header_out |= overallparity_out << 6;
  //for (int i = 0; i < 6; i++)
  //  header_out |= paritycorreced_out[i] << i;
  if (fix_data)
  {
    for (int i = 0; i < 6; i++)
      buffer[0] = (buffer[0] & ~(1 << i)) | (paritycorreced_out[i] << i);
    buffer[0] = (buffer[0] & ~(1 << 6)) | (overallparity_out << 6);
    for (int i = 7; i < 30; i++)
      buffer[0] = (buffer[0] & ~(1 << i)) | (corrected_out[i - 7] << i);
    for (int i = 30; i < 50; i++)
      buffer[1] = (buffer[1] & ~(1 << (i - 30))) | (corrected_out[i - 7] << (i - 30));
  }
}
