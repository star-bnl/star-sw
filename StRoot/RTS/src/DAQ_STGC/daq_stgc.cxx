#include <sys/types.h>
#include <errno.h>
#include <assert.h>

#include <rtsLog.h>
#include <rtsSystems.h>


#include <SFS/sfs_index.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>




//#include <DAQ_TPC/daq_tpc.h>	// solely for the "legacy" use!
//#include <TPC/trans_table.hh>	// same...
//#include <TPC/rowlen.h>

#include <DAQ1000/ddl_struct.h>	// for the misc DDL hardware constructs

#include "daq_stgc.h"


#include <DAQ_TPX/tpxCore.h>

//#include "tpxGain.h"
//#include "tpxPed.h"
//#include "tpxFCF.h"
//#include "tpxFCF_2D.h"
//#include "tpxStat.h"




class daq_det_stgc_factory : public daq_det_factory
{
public:
        daq_det_stgc_factory() {
                daq_det_factory::det_factories[STGC_ID] = this ;
        }

        daq_det *create() {
                return new daq_stgc ;
        }
} ;

static daq_det_stgc_factory stgc_factory ;



const char *daq_stgc::help_string = "\
\n\
****** STGC Help ******* \n\
\n\
Sector is [1..24]\n\
Rdo    is [1..6]\n\
\n\
Supported Banks: \n\
	raw	(sector,rdo); returns (char *) of start of DDL data\n\
	log\n\
" ;


void daq_stgc::help() const 
{ 
	printf("%s\n%s\n",GetCVS(),help_string) ; 
} ;

daq_stgc::daq_stgc(daqReader *rts_caller) 
{
	// override mother...

	rts_id = STGC_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "stgc" ;
	caller = rts_caller ;

	if(caller) caller->insert(this, rts_id) ;

	raw = new daq_dta ;	// in file, compressed
	altro = new daq_dta ;

	LOG(DBG,"%s: constructor: caller %p",name, caller) ;
	return ;
}

daq_stgc::~daq_stgc() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	// daq data 
	delete raw ;
	delete altro ;

	LOG(DBG,"%s: DEstructor done",name) ;
	return ;
}






daq_dta *daq_stgc::get(const char *in_bank, int sec, int row, int pad, void *p1, void *p2) 
{
	const char *bank ;

	if(in_bank==0) {	// just wants to know if I'm here so return some read-only non-NULL memory
		bank = "altro" ;	// default		
	}
	else {
		bank = in_bank ;
	}

	Make() ;

	LOG(NOTE,"%s: looking for bank %s",name,bank) ;

	if(!present) return 0 ;	// this det is not in this event...


	if(strcasecmp(bank,"raw")==0) {
		return handle_raw(sec,row) ;		// actually sec, rdo; r
	}
	else if(strcasecmp(bank,"altro")==0) {
		return handle_altro(sec,row) ;	// actually sec, rdo:
	}
	else {
		LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	}


	return 0 ;
}

daq_dta *daq_stgc::handle_altro(int sec, int rdo)
{

	int min_sec, max_sec ;
	int min_rdo, max_rdo ;


	// sanity
	if(sec <= 0) {
		min_sec = 1 ;
		max_sec = MAX_SEC ;
	}
	else if((sec<1) || (sec>MAX_SEC)) return 0 ;
	else {
		min_sec = sec ;
		max_sec = sec ;
	}

	if(rdo <= 0) {
		min_rdo = 1 ;
		max_rdo = 6 ;
	}
	else if((rdo<0) || (rdo>6)) return 0 ;
	else {
		min_rdo = max_rdo = rdo ;
	}

	// get a size estimate
	int rdos = 0 ;
	
	for(int s=min_sec;s<=max_sec;s++) {
	for(int r=min_rdo;r<=max_rdo;r++) {
		rdos++ ;
	}
	}


	//IMPORTANT:
	tpx_is_stgc = 1 ;


	// guess the byte size...
	int guess_bytes = rdos * 1152 * (sizeof(daq_store) + 10*sizeof(daq_adc_tb)) ;

	altro->create(guess_bytes,(char *)"adc",rts_id,DAQ_DTA_STRUCT(daq_adc_tb)) ;


	for(int s=min_sec;s<=max_sec;s++) {
	for(int r=min_rdo;r<=max_rdo;r++) {
		daq_dta *rdo_dta ;


		char *rdo_ptr ;
		struct tpx_rdo_event rdo ;
		struct tpx_altro_struct a ;
		int rdo_words ;

		LOG(NOTE,"Calling handle_raw for %d:%d",s,r) ;		
		rdo_dta = handle_raw(s, r) ;	// 	bring the raw data in, RDO-by_RDO!


		if(rdo_dta == 0) {
			LOG(WARN,"rdo_dta NULL?") ;
			continue ;	// sorry, not found...
		}

		int ret = rdo_dta->iterate() ;	// move from the header...
		if(ret==0) {	// no content
			continue ;
		}

		LOG(DBG,"Called handle_raw for %d:%d, iterate %d, returned %d objs",s,r,ret,rdo_dta->ncontent) ;				
		if(rdo_dta->ncontent == 0) continue ;	// nothing found...

		rdo_ptr = (char *)rdo_dta->Byte ;
		rdo_words = rdo_dta->ncontent / 4 ;


		int token = tpx_get_start(rdo_ptr, rdo_words, &rdo, 0) ;

		if(token <= 0) {
			LOG(ERR,"horrible error, token is %d?",token) ;
			continue ;
		}

		if(rdo.rdo != r) {
			LOG(ERR,"RDO mismatch: in data %d, expect %d",rdo.rdo,r) ;
		}


		//LOG(TERR,"S%d:%d = RDO %d, token %d, words %d %d",s,r,rdo.rdo,token,rdo_words,rdo.data_end-rdo.data_start) ;

		u_int *data_end = rdo.data_end ;

		a.rdo = rdo.rdo -1 ;
		a.t = token ;
		a.what = TPX_ALTRO_DO_ADC ;
		a.log_err = 0 ;
		a.sector = s ;

		do {
			data_end = tpx_scan_to_next(data_end, rdo.data_start, &a) ;		


			//LOG(TERR,"... A%d:%d count %d",a.id,a.ch,a.count) ;

			if(a.count == 0) continue ;	// no data for this guy...

			// unallowed rows, pads...
			//if((a.row>45) || (a.pad==0) || (a.pad>182)) {
			//	LOG(ERR,"STGC: S%02d:RDO%d: row %d, pad %d",a.sector,rdo.rdo,a.row,a.pad) ;
			//}

			daq_adc_tb *at = (daq_adc_tb *) altro->request(a.count) ;
	
			//LOG(DBG,"%d: %d:%d %d",altro->obj_cou,a.row,a.pad,a.count) ;

			for(u_int i=0 ; i < a.count ; i++) {
				at[i].adc = a.adc[i] ;
				at[i].tb = a.tb[i] ;

			}

			altro->finalize(a.count, s, a.id, a.ch) ;

		} while(data_end && (data_end > rdo.data_start)) ;	


	}
	}

	tpx_is_stgc = 0 ;

	altro->rewind() ;	// wind data pointers to the beginning so that they can be used
	
	return altro ;

}


	

daq_dta *daq_stgc::handle_raw(int sec, int rdo)
{
	char str[128] ;
	int tot_bytes ;
	int min_sec, max_sec, min_rdo, max_rdo ;
	struct {
		int sec ;
		int rb ;
		u_int bytes ;
	} obj[MAX_SEC*6] ;

	// sanity
	if(sec <= 0) {		// ALL sectors
		min_sec = 1 ;
		max_sec = MAX_SEC ;
	}
	else if((sec<1) || (sec>MAX_SEC)) return 0 ;
	else {
		min_sec = max_sec = sec ;
	}

	if(rdo <= 0) {		// ALL RDOs in this sector
		min_rdo = 1 ;
		max_rdo = 6 ;
	}
	else if((rdo<1) || (rdo>6)) return 0 ;
	else {
		min_rdo = max_rdo = rdo ;
	}

	assert(caller) ;


	// calc total bytes
	tot_bytes = 0 ;
	int o_cou = 0 ;

	for(int s=min_sec;s<=max_sec;s++) {
	for(int r=min_rdo;r<=max_rdo;r++) {

		sprintf(str,"%s/sec%02d/rb%02d/adc",sfs_name, s, r) ;
	
		LOG(NOTE,"%s: trying sfs on \"%s\"",name,str) ;

		char *full_name = caller->get_sfs_name(str) ;
		if(full_name == 0) continue ;

		int size = caller->sfs->fileSize(full_name) ;	// this is bytes

		LOG(DBG,"%s: sector %d, rdo %d : raw size %d",name,s,r,size) ;

		if(size <= 0) {
			if(size < 0) {
				LOG(DBG,"%s: %s: not found in this event",name,str) ;
			}
			continue ;
		}
		else {
			obj[o_cou].rb = r ;
			obj[o_cou].sec = s ;
			obj[o_cou].bytes = size ;

			o_cou++ ;

			tot_bytes += size ;

			LOG(DBG,"%s: %s: reading in \"%s\": bytes %d",name,str,"raw", size) ;
		}
	}
	}

	raw->create(tot_bytes,(char *)"raw",rts_id,DAQ_DTA_STRUCT(u_char)) ;

	// bring in the bacon from the SFS file....
	for(int i=0;i<o_cou;i++) {
		
		sprintf(str,"%s/sec%02d/rb%02d/adc",sfs_name,obj[i].sec, obj[i].rb) ;
		char *full_name = caller->get_sfs_name(str) ;
		if(!full_name) continue ;

		LOG(NOTE,"%s: request %d bytes",name,obj[i].bytes) ;
		
		char *mem = (char *) raw->request(obj[i].bytes) ;

		int ret = caller->sfs->read(full_name, mem, obj[i].bytes) ;

		if(ret != (int)obj[i].bytes) {
			LOG(ERR,"%s: %s: read failed, expect %d, got %d [%s]",name,str,
				obj[i].bytes,ret,strerror(errno)) ;
		}
		else {
			LOG(NOTE,"%s: %s read %d bytes",name,str,ret) ;
		}
		
		raw->finalize(obj[i].bytes, obj[i].sec, obj[i].rb, 0) ;
	}

	
	LOG(DBG,"Returning from raw_handler") ;
	raw->rewind() ;
	return raw ;

}

// knows how to get the token out of an event while trying also find a l0 command
int daq_stgc::get_token(char *addr, int words)
{
	daq_trg_word trgs[128] ;

	get_l2(addr, words, trgs, 1) ;

	

	if(trgs[0].t==0) {
		LOG(ERR,"Token 0 not allowed but I will try to use the other triggers...") ;
		trgs[0].t = 4097 ;
	}


	return trgs[0].t ;

}

// dumps the known accept/abort trigger decisions from
// the FIFO part of the event.
// returns the count
int daq_stgc::get_l2(char *addr, int words, struct daq_trg_word *trgs, int do_log)
{
	struct tpx_rdo_event rdo ;
	int cou = 0 ;
	u_int collision = 0 ;
	int err = 0 ;

	int ret = tpx_get_start(addr, words, &rdo, do_log) ;
	if(ret < 0) {
		LOG(ERR,"get_l2: broken data!") ;
		return 0 ;
	}

	LOG(DBG,"rdo %d, rdo token %d, trg cou %d",rdo.rdo,rdo.token,rdo.trg_cou) ;

	// grab only the prompt contribution...
	for(u_int i=0;i<rdo.trg_cou;i++) {
		u_int dta = rdo.trg[i].data ;
		u_int marker = rdo.trg[i].csr >> 24 ;
		u_int rhic = rdo.trg[i].rhic_counter ;


//#define WANT_LOGGING
#ifdef WANT_LOGGING
		if(rdo.rdo==1 && rdo.sector==1) {
			int delta = rhic - rdo.trg[0].rhic_counter ;
			LOG(TERR,"RDO %d: trg %d/%d: dta 0x%08X, CSR 0x%08X, RHIC %u, delta %u",rdo.rdo,
			    i,rdo.trg_cou,
			    dta,rdo.trg[i].csr,rhic,
			    delta) ;

		}
#endif

		if((marker==0) || (marker==0xEE)) {	// marks the prompt configuration

			trgs[cou].t = dta & 0xFFF ;
			trgs[cou].daq = (dta >> 12) & 0xF ;
			trgs[cou].trg = (dta >> 16) & 0xF ;
			trgs[cou].rhic_delta = 0 ;
			trgs[cou].rhic = rhic ;
			trgs[cou].reserved[0] = 0xF0000000 | (0x0FFFFFFF & dta) ;	// 0xF for "fired"

			switch(trgs[cou].trg) {
			case 4 :	// physics
			case 8 :	// interleaved laser
			case 9 :	// usual laser
			case 10 :	// pulser
				break ;
			default :
				LOG(ERR,"RDO %d: T %d: prompt: bad trg: 0x%08X",rdo.rdo,trgs[cou].t,dta) ;
				err = 1 ;
				continue ;
			}

			if(trgs[cou].t==0) {
				LOG(ERR,"RDO %d: token 0 (prompt) -- ignoring: 0x%08X",rdo.rdo,dta) ;
				err = 1 ;
				continue ;
			}

			// check for busy overrun
			if((dta & 0x3000000) != 0x2000000) {
				LOG(ERR,"RDO %d: T %d: prompt: BUSY overrun: 0x%08X",rdo.rdo,trgs[cou].t,dta) ;
				err = 1 ;
				continue ;
			}

			if(cou) {
				LOG(ERR,"RDO %d: duplicate prompt trigger",rdo.rdo) ;
				err = 1  ;
				continue ;
			}

			cou++ ;

			collision = rhic ;	// mark the collission time...
		}

	}
	
	if(cou==0) {	// no prompt trigger contrib; use the one from the RDO header.
		// either for a logging event (4096) or the one from the trigger-only (4097)
		
		if(rdo.token != 4096) {
			rdo.token = 4097 ;
			LOG(NOTE,"No triggers in event, making it %d",rdo.token) ;
		}

		trgs[cou].t = rdo.token ;
		trgs[cou].daq = 0 ;
		trgs[cou].trg = 4 ;	// dummy
		trgs[cou].rhic_delta = 0 ;
		trgs[cou].rhic = 0 ;
		trgs[cou].reserved[0] = 0 ;

		cou++ ;

	}
	else if (cou > 1) {
		LOG(ERR,"RDO %d: token %d? -- too many prompt contributions!",rdo.rdo,trgs[0].t) ;
		err = 1 ;
	}

	// at this point at least one contribution exists (i.e. cou>=1):
	// real L0 t
	// 4096 for log events
	// 4097 for events without an L0

	if(trgs[0].t == 0) {
		LOG(ERR,"Token 0 in RDO %d: making it 4097",rdo.rdo) ;
		err = 1 ;
		trgs[0].t = 4097 ;
	}

	for(u_int i=0;i<rdo.trg_cou;i++) {
		u_int dta = rdo.trg[i].data ;
		u_int marker = rdo.trg[i].csr >> 24 ;
		u_int rhic = rdo.trg[i].rhic_counter ;

		if(marker==0xFF) {	// FIFO
			int daq10k = 0 ;

			trgs[cou].t = dta & 0xFFF ;
			trgs[cou].daq = (dta >> 12) & 0xF ;
			trgs[cou].trg = (dta >> 16) & 0xF ;
			trgs[cou].rhic = rhic ;
			trgs[cou].reserved[0] = 0x20000000 | (0x0FFFFFFF & dta) ;

			switch(trgs[cou].trg) {
			case 13 :	// L2 ABORT
			case 14 :	// L1 ACCEPT
			case 15 :	// L2 ACCEPT
				break ;

			case 2 :	// RESET

				if((trgs[cou].t == 0x345) && (trgs[cou].daq == 2)) { // reset at run start from L0
					continue ;	// skip it
				}
				if((trgs[cou].t == 1001) && (trgs[cou].daq == 3)) { // reset at run start from TCD in local
					continue ;	// skip it
				}

				// WARNING: no "break" here!!!


			default :	// readout command; BUT not necessarily for DAQ10k!

				// check for overrun UNLESS the actual command

				if((dta & 0x03A00000) == 0x00800000) {
					//LOG(WARN,"DAQ10k trigger; no data") ;
					daq10k = 1 ;
				}
				else {
					if((dta & 0x3000000) != 0x2000000) {
						if(trgs[0].trg == 9) { // laser!	
							LOG(NOTE,"RDO %d: T %d: FIFO: BUSY overrun: 0x%08X",rdo.rdo,trgs[cou].t,dta) ;
						}
						else {
							LOG(ERR,"RDO %d: T %d: FIFO: BUSY overrun: 0x%08X",rdo.rdo,trgs[cou].t,dta) ;
							err = 1 ;
						}
					}
				}

				switch(trgs[cou].trg) {
				case 4 :	// physics
				case 8 :	// interspersed laser
				case 9 :	// laser
				case 10 :	// pulser
					break ;
				default:
					if(trgs[0].trg == 9) { //laser!	
						LOG(NOTE,"RDO %d: T %d: FIFO: bad trg_cmd: 0x%08X",rdo.rdo,trgs[cou].t,dta) ;
					}
					else {
						LOG(ERR,"RDO %d: T %d: FIFO: bad trg_cmd: 0x%08X",rdo.rdo,trgs[cou].t,dta) ;
						err = 1 ;
					}
					break ;
				}

				trgs[cou].reserved[0] = 0xA0000000 | (0x0FFFFFFF & dta) ;	// maybe DAQ10k?

				// new in FY12, DAQ10k -- we leave all of them for receiver.C!
				
				if(daq10k) break ;	// use it
				else continue ;		// skip it...

			}

			// we took out all the FIFO l0 contribs here!

			if(rhic >= collision) {
				trgs[cou].rhic_delta = rhic - collision ;
			}
			else {
				trgs[cou].rhic_delta = -(collision - rhic) ;
			}

			if(trgs[cou].rhic_delta == 1) {
				LOG(NOTE,"RDO %d: T %d: FIFO: delta == 1: 0x%08X",rdo.rdo,trgs[cou].t,dta) ;
				if(trgs[0].trg == 9) {		// a laser...
					continue ;
				}
			}

			if((trgs[cou].t == 0)) {
				LOG(ERR,"RDO %d: token 0 in L2 contribution 0x%08X -- skipping",rdo.rdo,dta) ;
				err = 1 ;
				continue ;
			}

			cou++ ;
		}

	}

	if(err) {	// dump out everyhign
		LOG(ERR," RDO %d: words %d",rdo.rdo,words) ;
		for(u_int i=0;i<rdo.trg_cou;i++) {
			LOG(ERR,"  RDO %d: T %4d: %d/%d: data 0x%08X, CSR 0x%08X, RHIC %u",rdo.rdo, rdo.token, i, rdo.trg_cou, rdo.trg[i].data, rdo.trg[i].csr, rdo.trg[i].rhic_counter) ;
		}
	}



	return cou ;
}
