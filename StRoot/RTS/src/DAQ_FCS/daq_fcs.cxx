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

	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_fcs::~daq_fcs() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete raw ;
	delete adc ;

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
	else {
		LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	}

	return 0 ;
}

daq_dta *daq_fcs::handle_raw()
{
	char str[128] ;
	int min_rdo = 1 ;
	int max_rdo = 2 ;
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
			if(full_name) version = 2018 ;
		}


		if(full_name==0) continue ;
		
		int size = caller->sfs->fileSize(full_name) ;	// this is bytes

		LOG(DBG,"Got size %d",size) ;
		if(size <= 0) {
			LOG(DBG,"%s: %s: not found in this event",name,str) ;
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

		fcs_c.start(ptr,dta->ncontent/2) ;

		//LOG(TERR,"Hello again") ;

		while(fcs_c.event()) {

			//LOG(TERR,"Request %d",fcs_c.tb_cou) ;

			u_short *at = (u_short *)adc->request(fcs_c.tb_cou) ;

			for(int i=0;i<fcs_c.tb_cou;i++) {
				at[i] = fcs_c.adc[i] ;
			}

			//if(fcs_c.first_rhic_strobe_tick!=0 || fcs_c.trigger_tick != 142) {
			//	LOG(WARN,"RHIC %d, Trg %d",fcs_c.first_rhic_strobe_tick,fcs_c.trigger_tick) ;
			//}

			adc->finalize(fcs_c.tb_cou, fcs_c.sector, fcs_c.rdo, fcs_c.ch) ;			
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
	u_int err = 0 ;
	int t_cou = 0 ;
	u_int *d = (u_int *)addr ;
	u_short *d16  ;
	u_int trg_word ;
	u_int hdr ;

	int trg_cmd, daq_cmd ;
	int t_hi, t_mid, t_lo ;

	//LOG(WARN,"get_l2") ;
//	for(int i=0;i<16;i++) {
//		LOG(TERR,"... %d/%d = 0x%08X",i,words,d[i]) ;
//	}

	d += 4 ;	// skip GTP header

	d16 = (u_short *)d ;

	if(d[0] != 0xCCCC001C) {
		LOG(ERR,"First word 0x%X bad",d[0]) ;
		err |= 1 ;
		goto err_end ;
	}

	hdr = sw16(d[1]) ;

	if(hdr != 0x98000004) {
		if((hdr & 0xFFFF0000)!=0x98010000) {
			LOG(WARN,"Not a triggered event 0x%08X",sw16(d[1])) ;
			trg[0].t = 4096 ;
			trg[0].trg = 0 ;
			trg[0].daq = 0 ;

			return 1 ;
		}
	}

	if((hdr & 0xFFFF0000)==0x98010000) {
		//for(int i=0;i<16;i++) {
		//	LOG(TERR,"... %2d = 0x%04X",i,d16[i]) ;
		//}
		
		trg_word = (d16[4]<<16) | d16[3] ;
	}
	else {			
		trg_word = sw16(d[3]) ;	// trigger
	}

//	LOG(TERR,"Trg 0x%08X 0x%08X 0x%08X 0x%08X",
//	    sw16(d[2]),trg_word,sw16(d[4]),sw16(d[5])) ;



	trg_cmd = trg_word & 0xF ;
	daq_cmd = (trg_word >> 4) & 0xF ;
	t_hi = (trg_word >> 8) & 0xF ;
	t_mid = (trg_word >> 12) & 0xF ;
	t_lo = (trg_word >> 16) & 0xF ;

	t_lo |= (t_hi<<8) | (t_mid << 4) ;

//	if(trg_word & 0x00A00000) {	//L0 fired
		trg[t_cou].t = t_lo ;
		trg[t_cou].trg = trg_cmd ;
		trg[t_cou].daq = daq_cmd ;
		trg[t_cou].rhic = 0 ;
		trg[t_cou].rhic_delta = 0 ;
		t_cou++ ;

		//LOG(INFO,"T %4d, trg_cmd %d, daq_cmd %d [%s]",t_lo,trg_cmd,daq_cmd,trg_word&0x00A00000?"FIRED":"Not fired") ;
//	}

	return 1 ;

	err_end:;

	trg[0].t = 4096 ;
	trg[0].trg = 0 ;
	trg[0].daq = 0 ;

	return 1 ;

}


/*******************************/
int fcs_data_c::start(u_short *d16, int shorts)
{
	u_int *d ;
	dta_p = d16 ;
	dta_stop = d16 + shorts ;

	d = (u_int *)d16 ;

	rhic_start = 0;

	//check version
	if(d[0]==0xDDDDDDDD) {	// new FY18 data!
		d += 4 ;	// skip GTP header
		d16 += 8 ;

		//for(int i=0;i<64;i++) {
		//	printf("--- %d = 0x%04X\n",i,d16[i]) ;
		//	//LOG(TERR,"...%d = 0x%08X",i,d[i]) ;
		//}

		// d[0] is start comma

		version = sw16(d[2]) ;
		
		switch(version) {
		case 0x12340000 :	// pre-May-15-2018
			d += 12 ;	// skip event header to go to ADC data
			break ;
		default :		// nre
			//rhic_start = sw16(d[4]) ;
			//d += 5 ;

			dta_p = ((u_short *)d)+6 ;
			return 1 ;

			break ;
		}

		dta_p = (u_short *) d ;


		//for(int i=0;i<16;i++) {
		//	LOG(TERR,"...%d = 0x%04X",i,dta_p[i]) ;
		//}

		return 1 ;
	}

	// old 2017 format here
	//LOG(TERR,"start: 0x%08X 0x%08X",d[0],d[1]) ;

	//move to start-of-ADC marker
	while(dta_p < dta_stop) {
		if(*dta_p++ == 0xFD06) {
			//for(int i=0;i<16;i++) {
			//	LOG(TERR,"...%d = 0x%04X",i,dta_p[i]) ;
			//}


			return 1 ;
		}
	}


	return -1 ;
}


int fcs_data_c::event()
{
	tb_cou = 0 ;
	ch = -1 ;

	trigger_tick = -1 ;
	first_rhic_strobe_tick = -1 ;

	while(dta_p<dta_stop) {

#if 0
		u_short h[3] ;


		for(int i=0;i<128;i++) printf("%d 0x%04X\n",i,dta_p[i]) ;

		

		h[0] = *dta_p++ ;	// adc_single ID

		if(h[0]==0xFD07 || h[0]==0x5800) {	//end of adc_single stream at 0x580000007
			break ;
		}

		if(version==0x28010518) dta_p++ ;	// the ID is doubled always...

		h[1] = *dta_p++ ;	// adc_single token
		h[2] = *dta_p++ ;	// adc_single rhic

		ch = h[0] & 0xF ;
#else
		//printf("+++ 0x%04X 0x%04X\n",dta_p[0],dta_p[1]) ;
		if((dta_p[0]==0xFD07) || (dta_p[0]==0x5800)) break ;
		if((dta_p[0]==0x0066) && (dta_p[1]==0x7788)) break ;


		ch = *dta_p & 0xF ;
		dta_p += 1 ;

		// and also skip the token for now
		dta_p += 2 ;
#endif
		//LOG(TERR,"H 0x%X 0x%X 0x%X (ch %2d)",h[0],h[1],h[2],ch) ;

		while(dta_p<dta_stop) {
			u_short d = *dta_p++ ;

			//printf("... %d = 0x%04X [%u]\n",tb_cou,d,d) ;

			//LOG(TERR,".... %d = 0x%X",tb_cou,d) ;

			if(d==0xFFFF) {		// last item of adc_single
				//LOG(TERR,"... tb_cou %d",tb_cou) ;
				break ;
			}

			if(d & 0x2000) {
				if(first_rhic_strobe_tick < 0) {
					first_rhic_strobe_tick = tb_cou ;
					//LOG(TERR,"... first rhic strobe at %d",tb_cou) ;
				}
			}
			if(d & 0x8000) {
				if(trigger_tick < 0) {
					trigger_tick = tb_cou ;
					//LOG(TERR,"... trigger tick at %d",tb_cou) ;
				}
			}

//			accum(ch,tb_cou,d&0xFFF) ;
			if(accum(ch,tb_cou,d)<0) {
				LOG(ERR,"Event too big, ch %d, tb %d",ch,tb_cou) ;
				return 0 ;
			}
			tb_cou++ ;
		}

		//LOG(TERR,"0x%08X 0x%08X 0x%08X",dta_p[0],dta_p[1],dta_p[2]) ;

		//LOG(TERR,"Ch %d, %d ADCs",ch,tb_cou) ;
		return 1 ;
	}

//	u_int rhic_end = (dta_p[1]<<16)|dta_p[2] ;
//	LOG(TERR,"RHIC ticks %u",rhic_end-rhic_start) ;

	//LOG(TERR,"0x%08X 0x%08X 0x%08X: 0x%08X",dta_p[0],dta_p[1],dta_p[2],rhic_end) ;	

	return 0 ;
}

int fcs_data_c::accum(int ch, int tb, u_short sadc)
{
	if((u_int)tb>=sizeof(adc)/sizeof(adc[0])) {
		return -1 ;
	}

	adc[tb] = sadc ;	//but store the full data, with flags

	sadc &= 0xFFF ;	//zap the flags

	if(ped_run) {
		//if(tb==0) LOG(TERR,"Accum: ch %d = %d",ch,sadc) ;

		ped.mean[ch] += (double)sadc ;
		ped.rms[ch] += (double)sadc * (double)sadc ;
		ped.cou[ch]++ ;
	}

	return 0 ;

}



void fcs_data_c::ped_start()
{
	memset(&ped,0,sizeof(ped)) ;
}


void fcs_data_c::ped_stop()
{
	for(int c=0;c<16;c++) {
		if(ped.cou[c]) {
			ped.mean[c] /= ped.cou[c] ;
			ped.rms[c] /= ped.cou[c] ;

			ped.rms[c] = sqrt(ped.rms[c]-ped.mean[c]*ped.mean[c]) ;
		}

	}


	if(ped_run) {
		//pedestal dump...
		FILE *pedf ;

		time_t now = time(0) ;
		struct tm *tm = localtime(&now) ;

		char fname[128] ;

		if(run_number) {
			sprintf(fname,"/RTScache/fcs_pedestals_%08u.txt",run_number) ;
		}
		else {
			
			sprintf(fname,"/RTScache/fcs_pedestals_%d_%d_%d_%d_%d.txt",
				tm->tm_year+1900,
				tm->tm_mon+1,
				tm->tm_mday,
				tm->tm_hour,
				tm->tm_min) ;
		}

		pedf = fopen(fname,"w") ;

		fprintf(pedf,"#RUN %u\n",run_number) ;
		fprintf(pedf,"#TIME %u\n",(unsigned int)now) ;
		char *ctm = ctime(&now) ;
		fprintf(pedf,"#DATE %s",ctm) ;
		fprintf(pedf,"\n") ;

		for(int c=0;c<16;c++) {
			LOG(TERR,"PEDs: %2d %f %f %.3f %.3f %.3f",c,ped.mean[c],ped.rms[c],
				fee_currents[c][0],fee_currents[c][1],fee_currents[c][2]) ;


			fprintf(pedf,"%2d %f %f %.3f %.3f %.3f\n",c,ped.mean[c],ped.rms[c],
				fee_currents[c][0],fee_currents[c][1],fee_currents[c][2]) ;

		}

		fclose(pedf) ;
	}

}
