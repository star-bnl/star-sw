#include <assert.h>
#include <sys/types.h>
#include <errno.h>
#include <math.h>
#include <time.h>

#include <rtsLog.h>
#include <rtsSystems.h>
#include <daqFormats.h>

#include <SFS/sfs_index.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>


#include "daq_fcs.h"
#include "fcs_data_c.h"

const char *daq_fcs::help_string = "\
\n\
FCS Help: \n\
Supported Banks: \n\
	raw	returns=ptr of start of DDL data; c1=sector[1..1]; c2=rdo[1..4]; \n\
\n\
\n\
" ;

class daq_det_fcs_factory : public daq_det_factory
{
public:
        daq_det_fcs_factory() {
		LOG(DBG,"%s",__PRETTY_FUNCTION__) ;
                daq_det_factory::det_factories[FCS_ID] = this ;
        }

        daq_det *create() {
		LOG(DBG,"%s",__PRETTY_FUNCTION__) ;
                return new daq_fcs ;
        }
} ;

static daq_det_fcs_factory fcs_factory ;



daq_fcs::daq_fcs(daqReader *rts_caller) 
{
	LOG(DBG,"%s",__PRETTY_FUNCTION__) ;

	rts_id = FCS_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "fcs" ;
	caller = rts_caller ;

	if(caller) caller->insert(this, rts_id) ;

	raw = new daq_dta ;
	adc = new daq_dta ;
	zs = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_fcs::~daq_fcs() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete raw ;
	delete adc ;
	delete zs ;

	return ;
}



daq_dta *daq_fcs::get(const char *bank, int sec, int raw, int pad, void *p1, void *p2) 
{
	Make() ;

	if(present==0) return 0 ;

	LOG(DBG,"%s: looking for bank %s",name,bank) ;

	if(strcmp(bank,"*")==0) bank = "raw" ;
		


	if(strcasecmp(bank,"raw")==0) {
		if((present & DET_PRESENT_SFS)==0) return 0 ;		// no DDL
		return handle_raw() ;		// actually sec, rdo; r1 is the number of bytes
	}
	else if(strcasecmp(bank,"adc")==0) {
		if((present & DET_PRESENT_SFS)==0) return 0 ;		// no DDL
		return handle_adc() ;		// actually sec, rdo; r1 is the number of bytes
	}
	else if(strcasecmp(bank,"zs")==0) {
		if((present & DET_PRESENT_SFS)==0) return 0 ;		// no DDL
		return handle_zs() ;		// actually sec, rdo; r1 is the number of bytes
	}
	else {
		LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	}

	return 0 ;
}

daq_dta *daq_fcs::handle_raw()
{
	char str[128] ;
	int min_rdo = 1 ;
	int max_rdo = 8 ;
	char *full_name ;
	int got_any = 0 ;

	// bring in the bacon from the SFS file....
	assert(caller) ;

	
	sprintf(str,"%s/sec01/rb01/raw",sfs_name) ;
	full_name = caller->get_sfs_name(str) ;
	
	LOG(DBG,"%s: trying sfs on \"%s\"",name,str) ;
	if(full_name) {	// FY17 version
		min_rdo = 1 ;
		max_rdo = 1 ;
		version = 2017 ;
	}

	raw->create(1024,"fcs_raw",rts_id,DAQ_DTA_STRUCT(u_char)) ;

	for(int r=min_rdo;r<=max_rdo;r++) {
		if(full_name==0) {
			sprintf(str,"%s/sec01/rdo%d/raw",sfs_name,r) ;
			full_name = caller->get_sfs_name(str) ;

			LOG(NOTE,"str %s, full_name %s",str,full_name) ;
			if(full_name) version = 2018 ;
		}


		if(full_name==0) continue ;
		
		int size = caller->sfs->fileSize(full_name) ;	// this is bytes

		LOG(DBG,"S%d:%d: Got size %d",1,r,size) ;
		if(size <= 0) {
			LOG(NOTE,"%s: %s: not found in this event",name,str) ;
			return 0 ;
		}

		got_any = 1 ;

		char *st = (char *) raw->request(size) ;

		caller->sfs->read(full_name, st, size) ;

		LOG(DBG,"sfs read succeeded") ;

		raw->finalize(size,1,r,0) ;

		full_name = 0 ;
	}

	raw->rewind() ;

	if(got_any) {
		return raw ;
	}
	else return 0 ;

}

daq_dta *daq_fcs::handle_zs()
{
	char str[128] ;
	u_int min_rdo = 1 ;
	u_int max_rdo = 8 ;
	char *full_name ;
	int got_any = 0 ;
//	int bytes = 0 ;
//	char *st ;

	// bring in the bacon from the SFS file....
	assert(caller) ;



	// first check the global zs (new in May 2019)
	sprintf(str,"%s/sec01/zs",sfs_name) ;
	full_name = caller->get_sfs_name(str) ;


	if(full_name) {
		char *st, *m_st ;
		u_short *zs_start ;

		int sec, rdo ;
		int bytes ;

		zs->create(8*1024,"fcs_zs",rts_id,DAQ_DTA_STRUCT(daq_adc_tb)) ;

		bytes = caller->sfs->fileSize(full_name) ;	// this is bytes
		
		m_st = st = (char *)malloc(bytes) ;
		caller->sfs->read(full_name,st,bytes) ;

		u_int *zs_int = (u_int *)st ;

		int bytes_data = zs_int[0] & 0x0FFFFFFF ;

		LOG(NOTE,"zs first 0x%X, bytes data %d",zs_int[0],bytes_data) ;

		zs_start = (u_short *)st ;
		for(int j=0;j<16;j++) LOG(DBG,"%d = 0x%04X",j,zs_start[j]) ;



		zs_int++ ;

		bytes_data -= 4 ;
		st += 4 ;

		while(bytes_data) {


		zs_start = (u_short *)st ;
		u_short *zs_dta = zs_start ;
		u_int *zs_int = (u_int *)st ;

		LOG(NOTE,"... board_id 0x%08X, shorts %d, bytes_data %d",zs_int[0],zs_int[1],bytes_data) ;




//		if(zs_int[0] != r) {
//			LOG(ERR,"Expect %d, read %d",r,zs_int[0]) ;
//			free(st) ;
//			continue ;
//		}


		//sec is the full board_id
		//rdo is just the dep board
		if(zs_int[0] & 0xF0000000) {
			sec = zs_int[0] & 0xFFFF ;
			rdo = zs_int[0] & 0x1F ;
		}
		else {
			sec = 0 ;
			rdo = 0 ;
		}

		//bytes_data -= 2*4 ;
		//st += 2*4 ;

		if(zs_int[1] == 0) {	// number of shorts
			LOG(WARN,"0 shorts??") ;
			continue ;
		}
		
		st += zs_int[1]*2 ;
		bytes_data -= zs_int[1]*2 ;

		LOG(DBG,"S%d:%d - bytes_data %d",(sec>>11)+1,((sec>>8)&0x7)+1,bytes_data) ;

//		LOG(TERR,"... 0x%X : 0x%X %d",zs_int[0],sec,rdo) ;

		u_short *zs_end = zs_dta + zs_int[1] ;

		zs_dta += 2*2 ;	// to skip the 2 ints

		int ch_cou = 0 ;

		while(zs_dta < zs_end) {
			int ch = *zs_dta++ ;
			int seq_cou = *zs_dta++ ;
			int a_cou = 0 ;

			LOG(DBG,"Ch %d(%d), seq %d: %d",ch,ch_cou,seq_cou,zs_end-zs_dta) ;
			ch_cou++ ;

			if(seq_cou==0) continue ;

			got_any = 1 ;
			
			daq_adc_tb *a_t = (daq_adc_tb *) zs->request(8*1024) ;

			for(int i=0;i<seq_cou;i++) {
				int t_start = *zs_dta++ ;
				int t_cou = *zs_dta++ ;
				int t_end = t_start + t_cou ;

				LOG(DBG,"..... t_start %d, t_cou %d",t_start,t_cou) ;

				for(int t=t_start;t<t_end;t++) {
					u_short d = *zs_dta++ ;

					a_t[a_cou].adc = d ;
					a_t[a_cou].tb = t ;
					a_cou++ ;
				}

			}
			
			zs->finalize(a_cou,sec,rdo,ch) ;
		}

		//st += zs_int[1]*2 ;
		//bytes_data -= zs_int[1]*2 ;

		}

		if(m_st) free(m_st) ;

		zs->rewind() ;

		if(got_any) {
			return zs ;
		}
		else return 0 ;

	}

	for(u_int r=min_rdo;r<=max_rdo;r++) {
		int sec, rdo ;

		sprintf(str,"%s/sec01/rdo%d/zs",sfs_name,r) ;
		full_name = caller->get_sfs_name(str) ;

		LOG(DBG,"full %s, str %s",full_name,str) ;

		if(full_name==0) continue ;
		
		int bytes = caller->sfs->fileSize(full_name) ;	// this is bytes

		LOG(DBG,"S%d:%d: Got size %d",1,r,bytes) ;
		if(bytes <= 0) {
			LOG(NOTE,"%s: %s: not found in this event",name,str) ;
			continue ;
		}



		char *st = (char *)malloc(bytes) ;

		caller->sfs->read(full_name, st, bytes) ;

		LOG(DBG,"sfs read succeeded") ;

		u_short *zs_start = (u_short *)st ;
		u_short *zs_dta = zs_start ;
		u_int *zs_int = (u_int *)st ;

		LOG(NOTE,"... board_id 0x%08X, shorts %d",zs_int[0],zs_int[1]) ;

//		if(zs_int[0] != r) {
//			LOG(ERR,"Expect %d, read %d",r,zs_int[0]) ;
//			free(st) ;
//			continue ;
//		}


		//sec is the full board_id
		//rdo is just the dep board
		if(zs_int[0] & 0xF0000000) {
			sec = zs_int[0] & 0xFFFF ;
			rdo = zs_int[0] & 0x1F ;
		}
		else {	// early 2019, I'll do it by hand
			if(r==1) {
				sec = ((1-1)<<11)|((r-1)<<8)|(2<<6)|(1<<5)|0 ;
			}
			else {
				sec = ((1-1)<<11)|((r-1)<<8)|(0<<6)|(1<<5)|0 ;
			}

			rdo = 0 ;
		}

		if(zs_int[1] == 0) {	// number of shorts
			free(st) ;
			continue ;
		}


//		LOG(TERR,"... 0x%X : 0x%X %d",zs_int[0],sec,rdo) ;

		u_short *zs_end = zs_dta + zs_int[1] ;

		zs_dta += 2*2 ;

		while(zs_dta < zs_end) {
			int ch = *zs_dta++ ;
			int seq_cou = *zs_dta++ ;
			int a_cou = 0 ;

			LOG(DBG,"Ch %d, seq %d: %d",ch,seq_cou,zs_end-zs_dta) ;

			if(seq_cou==0) continue ;

			got_any = 1 ;
			
			daq_adc_tb *a_t = (daq_adc_tb *) zs->request(8*1024) ;

			for(int i=0;i<seq_cou;i++) {
				int t_start = *zs_dta++ ;
				int t_cou = *zs_dta++ ;
				int t_end = t_start + t_cou ;

				LOG(DBG,"..... t_start %d, t_cou %d",t_start,t_cou) ;

				for(int t=t_start;t<t_end;t++) {
					u_short d = *zs_dta++ ;

					a_t[a_cou].adc = d ;
					a_t[a_cou].tb = t ;
					a_cou++ ;
				}

			}
			
			zs->finalize(a_cou,sec,rdo,ch) ;
		}
		
		free(st) ;
	}

	zs->rewind() ;

	if(got_any) {
		return zs ;
	}
	else return 0 ;

}

daq_dta *daq_fcs::handle_adc()
{
	daq_dta *dta ;

	// bring in the bacon from the SFS file....
	assert(caller) ;

	dta = handle_raw() ;

	if(dta==0) return 0 ;

	adc->create(1000,"adc",rts_id,DAQ_DTA_STRUCT(u_short)) ;

	
	while(dta && dta->iterate()) {
		u_short *ptr = (u_short *) dta->Void ;
		
		//LOG(TERR,"Hello %d",dta->ncontent) ;

		fcs_data_c fcs_c ;


		fcs_c.sector = dta->sec ;
		fcs_c.set_rdo(dta->rdo) ;

		fcs_c.start(ptr,dta->ncontent/2) ;

		//LOG(TERR,"Hello again %d %d %d = %d %d",fcs_c.sector,fcs_c.rdo, fcs_c.ch,dta->sec,dta->rdo) ;

		while(fcs_c.event()) {

			//LOG(TERR,"Request %d",fcs_c.tb_cou) ;

			u_short *at = (u_short *)adc->request(fcs_c.tb_cou) ;

			for(int i=0;i<fcs_c.tb_cou;i++) {
				at[i] = fcs_c.adc[i] ;
			}

			//if(fcs_c.first_rhic_strobe_tick!=0 || fcs_c.trigger_tick != 142) {
			//	LOG(WARN,"RHIC %d, Trg %d",fcs_c.first_rhic_strobe_tick,fcs_c.trigger_tick) ;
			//}

			
			int sec = fcs_c.hdr_board_id ;	// all sorts of things
			int rdo = fcs_c.hdr_board_id & 0x1F ;	// dep id

			adc->finalize(fcs_c.tb_cou, sec, rdo, fcs_c.ch) ;			
		}
	}

	adc->rewind() ;

	return adc ;

}

// knows how to get the token out of an event...
int daq_fcs::get_token(char *addr, int words)
{
	LOG(ERR,"get_token") ;

	int cou ;
	struct daq_trg_word trg[128] ;

	cou = get_l2(addr,words,trg,1) ;

	if(cou==0) return -1000 ;	// special marker...
	if(trg[0].t==0) return -ENOSYS ;

	return trg[0].t ;
}

static inline u_int sw16(u_int d)
{
        u_int tmp = d ;

        d >>= 16 ;

        d |= (tmp & 0xFFFF)<<16 ;

        return d ;
}



// knows how to get a/the L2 command out of the event...
int daq_fcs::get_l2(char *addr, int words, struct daq_trg_word *trg, int rdo)
{
	int t_cou = 0 ;
	u_int *d = (u_int *)addr ;
	u_short *d16  ;
	u_int trg_word ;
	u_int hdr ;

	int trg_cmd, daq_cmd ;
	int t_hi, t_mid, t_lo ;

//	LOG(TERR,"get_l2: %p %d %p %d",addr,words,trg,rdo) ;
	if(addr==0) return 0 ;

	//LOG(WARN,"get_l2") ;
//	for(int i=0;i<16;i++) {
//		LOG(TERR,"... %d/%d = 0x%08X",i,words,d[i]) ;
//	}

	d += 4 ;	// skip GTP header

	d16 = (u_short *)d ;

	if(d[0] != 0xCCCC001C) {
		LOG(ERR,"Comma word 0x%08X bad, words %d",d[0],words) ;
		goto err_end ;
	}

	hdr = sw16(d[1]) >> 16 ;


	switch(hdr) {
	case 0x9800 :	// FY17
		break ;
	case 0x9801 :	// FY18
		break ;
	case 0x9802 :	// FY19
		break ;
	default :
		LOG(ERR,"Unexpected event 0x%04X",hdr) ;
		goto err_end ;
	}
	
//	LOG(TERR,"%d: hdr 0x%X: 0x%X 0x%X 0x%X",rdo,hdr,d[3],d[4],d[5]) ;

	switch(hdr) {
	case 0x9801 :
		trg_word = (d16[4]<<16) | d16[3] ;
		break ;
	case 0x9802 :
		trg_word = (d16[5]<<16) | d16[4] ;
		break ;
	default :
		trg_word = sw16(d[3]) ;	// trigger
		break ;
	}


	trg_cmd = trg_word & 0xF ;
	daq_cmd = (trg_word >> 4) & 0xF ;
	t_hi = (trg_word >> 8) & 0xF ;
	t_mid = (trg_word >> 12) & 0xF ;
	t_lo = (trg_word >> 16) & 0xF ;

	t_lo |= (t_hi<<8) | (t_mid << 4) ;

//	LOG(TERR,"%d: words %d: trg_word 0x%08X: trg_cmd 0x%X, daq_cmd 0x%X, token %d",rdo,words,
//	    trg_word,trg_cmd,daq_cmd,t_lo) ;

	if(trg_cmd==0) {
		//LOG(WARN,"trg_cmd=0 in event type 0x%04X",hdr) ;
		// Monitoring event...
		goto err_end ;
	}
	else {
		if(t_lo==0) {
			if(trg_cmd==5) {	// allowed for self-triggered!
				t_lo = 4095 ;	// use this particular token!
			}
			else {
				LOG(ERR,"Token-0 in triggered event 0x%04X: trg_cmd 0x%05X",hdr,trg_word) ;

				u_short *d16 = (u_short *)d ;
				for(int i=0;i<16;i++) {
					LOG(TERR,"... %d = 0x%04X",i,d16[i]) ;
				}

				goto err_end ;
			}
		}
		else {
			switch(trg_cmd) {
			case 4 :
			case 5 :
			case 6 :	// local trigger
			case 10 :	// pulser
				break ;
			default :
				LOG(WARN,"Unusual trg_cmd=0x%X in event 0x%04X",trg_cmd,hdr) ;
				break ;
			}
		}
	}

	if(0) {
		u_short *d16 = (u_short *)d ;
		for(int i=0;i<16;i++) {
			LOG(TERR,"... %d = 0x%04X",i,d16[i]) ;
		}
	}

	trg[t_cou].t = t_lo ;
	trg[t_cou].trg = trg_cmd ;
	trg[t_cou].daq = daq_cmd ;
	trg[t_cou].rhic = 0 ;
	trg[t_cou].rhic_delta = 0 ;
	t_cou++ ;


	return t_cou ;

	err_end:;

	trg[0].t = 4096 ;
	trg[0].trg = 0 ;
	trg[0].daq = 0 ;

	return 1 ;

}



