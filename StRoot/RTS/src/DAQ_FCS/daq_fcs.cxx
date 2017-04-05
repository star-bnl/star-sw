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



daq_dta *daq_fcs::get(const char *bank, int sec, int row, int pad, void *p1, void *p2) 
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

	// bring in the bacon from the SFS file....
	assert(caller) ;

	
	sprintf(str,"%s/sec01/rb01/raw",sfs_name) ;
	char *full_name = caller->get_sfs_name(str) ;
	
	LOG(DBG,"%s: trying sfs on \"%s\"",name,str) ;
	if(full_name == 0) return 0 ;

	int size = caller->sfs->fileSize(full_name) ;	// this is bytes

	LOG(DBG,"Got size %d",size) ;
	if(size <= 0) {
		LOG(DBG,"%s: %s: not found in this event",name,str) ;
		return 0 ;
	}

	raw->create(size,"fcs_raw",rts_id,DAQ_DTA_STRUCT(u_char)) ;
	char *st = (char *) raw->request(size) ;

	caller->sfs->read(full_name, st, size) ;

	LOG(DBG,"sfs read succeeded") ;

        raw->finalize(size,1,1,0) ;

	raw->rewind() ;

	return raw ;

}

daq_dta *daq_fcs::handle_adc()
{
	char str[128] ;

	// bring in the bacon from the SFS file....
	assert(caller) ;

	
	sprintf(str,"%s/sec01/rb01/raw",sfs_name) ;
	char *full_name = caller->get_sfs_name(str) ;
	
	LOG(DBG,"%s: trying sfs on \"%s\"",name,str) ;
	if(full_name == 0) return 0 ;

	int size = caller->sfs->fileSize(full_name) ;	// this is bytes

	LOG(DBG,"Got size %d",size) ;
	if(size <= 0) {
		LOG(DBG,"%s: %s: not found in this event",name,str) ;
		return 0 ;
	}

	char *ptr = (char *) malloc(size) ;
	LOG(DBG,"Malloc at %p",ptr) ;

	caller->sfs->read(full_name, ptr, size) ;

	LOG(DBG,"sfs read succeeded") ;

	adc->create(1000,"adc",rts_id,DAQ_DTA_STRUCT(u_short)) ;

	fcs_data_c fcs_c ;
	fcs_c.start((u_short *)ptr,size/2) ;

	
	while(fcs_c.event()) {
		
		u_short *at = (u_short *)adc->request(fcs_c.tb_cou) ;

		for(int i=0;i<fcs_c.tb_cou;i++) {
			at[i] = fcs_c.adc[i] ;
		}

		adc->finalize(fcs_c.tb_cou, fcs_c.sector, fcs_c.rdo, fcs_c.ch) ;
	}

	free(ptr) ;

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



// knows how to get a/the L2 command out of the event...
int daq_fcs::get_l2(char *addr, int words, struct daq_trg_word *trg, int rdo)
{
	int err = 0 ;
	int t_cou = 0 ;

	
	u_short *d = (u_short *)addr ;
//	u_short s_words = words/2 ;

	if(d[0] != 0xFD04) {
		LOG(ERR,"Trigger error",d[0]) ;
		err |= 1 ;
	}


	int trg_word ;
	int trg_cmd, daq_cmd ;
	int t_hi, t_mid, t_lo ;

	trg_word = d[2]<<16 | d[1] ;

	trg_cmd = trg_word & 0xF ;
	daq_cmd = (trg_word >> 4) & 0xF ;
	t_hi = (trg_word >> 8) & 0xF ;
	t_mid = (trg_word >> 12) & 0xF ;
	t_lo = (trg_word >> 16) & 0xF ;

	t_lo |= (t_hi<<8) | (t_mid << 4) ;

	if(trg_word & 0x00A00000) {	//L0 fired
		trg[t_cou].t = t_lo ;
		trg[t_cou].trg = trg_cmd ;
		trg[t_cou].daq = daq_cmd ;
		trg[t_cou].rhic = 0 ;
		trg[t_cou].rhic_delta = 0 ;
		t_cou++ ;

		//LOG(TERR,"T %4d, trg_cmd %d, daq_cmd %d",t_lo,trg_cmd,daq_cmd) ;
	}

	if(d[3] != 0xFD05) {
		LOG(ERR,"Trigger error 0x%04X",d[3]) ;
		err |= 2 ;
	}


	if(err) {
		trg[0].t = 4097 ;
		trg[0].trg = 4 ;
		trg[0].daq = 0 ;
	}


	return t_cou ;
}


/*******************************/
int fcs_data_c::start(u_short *d16, int shorts)
{
	dta_p = d16 ;
	dta_stop = d16 + shorts ;

	//move to start-of-ADC marker
	while(dta_p < dta_stop) {
		if(*dta_p++ == 0xFD06) return 1 ;
	}

	return -1 ;
}


int fcs_data_c::event()
{
	tb_cou = 0 ;
	ch = -1 ;

	while(dta_p<dta_stop) {
		u_short h[3] ;


		h[0] = *dta_p++ ;
		if(h[0]==0xFD07) {	//end of event
			return 0 ;
		}

		h[1] = *dta_p++ ;
		h[2] = *dta_p++ ;

		ch = h[0] & 0xF ;

		while(dta_p<dta_stop) {
			u_short d = *dta_p++ ;
			if(d==0xFFFF) break ;

			accum(ch,tb_cou,d&0xFFF) ;
			tb_cou++ ;
		}

		//LOG(TERR,"Ch %d, %d ADCs",ch,tb_cou) ;
		return 1 ;
	}

	

	return 0 ;
}

int fcs_data_c::accum(int ch, int tb, u_short sadc)
{
	adc[tb] = sadc ;


	if(ped_run) {
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
