#include <sys/types.h>
#include <errno.h>
#include <assert.h>

#include <rtsLog.h>
#include <rtsSystems.h>



#include <SFS/sfs_index.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>


#include "daq_bsmd.h"

const char *daq_bsmd::help_string = "BSMD\n\
adc	returns bsmd_t;\n\
raw	returns raw data\n" ;

class daq_det_bsmd_factory : public daq_det_factory
{
public:
	daq_det_bsmd_factory() {
		daq_det_factory::det_factories[BSMD_ID] = this ;
	}

	daq_det *create() {
		return new daq_bsmd ;
	}
} ;

static daq_det_bsmd_factory bsmd_factory ;


daq_bsmd::daq_bsmd(daqReader *rts_caller) 
{
	rts_id = BSMD_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "bsmd" ;
	caller = rts_caller ;
	if(caller) caller->insert(this, rts_id) ;


	adc = new daq_dta ;
	adc_non_zs = new daq_dta ;
	ped = new daq_dta ;
	rms = new daq_dta ;
	
	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_bsmd::~daq_bsmd() 
{
	LOG(DBG,"%s: DEstructor",name) ;


	delete adc ;
	delete ped ;
	delete rms ;
	delete adc_non_zs ;

	return ;
}



daq_dta *daq_bsmd::get(const char *bank, int sec, int row, int pad, void *p1, void *p2) 
{
	Make() ;
	if(!present) return 0 ;

	LOG(DBG,"got something") ;

	if(strcasecmp(bank,"adc")==0) {
		return handle_adc(row) ;
	}
	else if(strcasecmp(bank,"adc_non_zs")==0) {
		return handle_adc_non_zs(row) ;
	}
	else if(strcasecmp(bank,"ped")==0) {
		return handle_ped_rms(row,1) ;
	}
	else if(strcasecmp(bank,"rms")==0) {
		return handle_ped_rms(row,0) ;
	}


	LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	return 0 ;
}

// this is the zero-suppressed reader!
daq_dta *daq_bsmd::handle_adc(int rdo)
{
	struct bsmd_desc bsmd_d ;
	int start_r, stop_r ;
	int bytes ;

	assert(caller) ;	// sanity...

	LOG(DBG,"%s: present 0x%X",name,present) ;

	if(!present) return 0 ;

	if(rdo <= 0) {
		start_r = 1 ;
		stop_r = BSMD_FIBERS ;
	}
	else {
		start_r = stop_r = rdo ;
	}

	bytes = 0 ;

	if(present & DET_PRESENT_DATAP) {	// in datap!
		char *emcp = (char *)legacyDetp(rts_id, caller->mem) ;
		//LOG(NOTE,"EMCP %p?",emcp) ;
		if(bsmd_reader(emcp, &bsmd_d)==0) return 0 ;
	}
	else return 0 ;	// SFS does not exist yet!

	for(int r=start_r;r<=stop_r;r++) {	
		bytes += bsmd_d.bytes[r-1][1] ;
	}

	LOG(DBG,"rdo %d: bytes %d",rdo,bytes) ;

	if(bytes==0) return 0 ;

	adc->create(bytes,"adc",rts_id,DAQ_DTA_STRUCT(bsmd_t)) ;

	for(int r=start_r;r<=stop_r;r++) {
		int count ;
		int version, fiber ;

		if(bsmd_d.bytes[r-1][1] == 0) continue ;

		bsmd_t *bsmd  = (bsmd_t *) adc->request(1) ;

		memset(bsmd,0,sizeof(bsmd_t)) ;	

		u_short *data = (u_short *)((u_int)bsmd_d.dta[r-1][1]) ;	// move to data start

		if(bsmd_d.endian[r-1][1]) {	// big!
			version = b2h16(data[0]) ;
			count = b2h16(data[1]) ;
			bsmd->cap = b2h16(data[2]) ;
			fiber = b2h16(data[3]) ;

			LOG(NOTE,"%s: fiber %d[%d]: count %d, cap %d, version 0x%04X",name,r,fiber,count,bsmd->cap,version) ;
		
			data += 4 ;
		
			for(int c=0;c<count;c++) {
				int ch = b2h16(*data++) ;
				int adc = b2h16(*data++) ;

				bsmd->adc[ch] = adc ;
			}
		}
		else {
			version = l2h16(data[0]) ;
			count = l2h16(data[1]) ;
			bsmd->cap = l2h16(data[2]) ;
			fiber = l2h16(data[3]) ;

			LOG(NOTE,"%s: fiber %d[%d]: count %d, cap %d, version 0x%04X",name,r,fiber,count,bsmd->cap,version) ;

			data += 4 ;
		
			for(int c=0;c<count;c++) {
				int ch = l2h16(*data++) ;
				int adc = l2h16(*data++) ;

				bsmd->adc[ch] = adc ;
			}
		}



		adc->finalize(1,0,r,bsmd->cap) ;
	}
		
	adc->rewind() ;

	return adc ;
	
}

daq_dta *daq_bsmd::handle_ped_rms(int rdo, int is_ped)
{
	int start_r, stop_r ;
	int bytes ;
	struct bsmd_desc bsmd_d ;
	daq_dta *dta_use ;

	if(rdo <= 0) {
		start_r = 1 ;
		stop_r = BSMD_FIBERS ;
	}
	else {
		start_r = stop_r = rdo ;
	}

	bytes = 0 ;

	if(present & DET_PRESENT_DATAP) {	// in datap!
		char *emcp = (char *)legacyDetp(rts_id, caller->mem) ;
		//LOG(NOTE,"EMCP %p?",emcp) ;
		if(bsmd_reader(emcp, &bsmd_d)==0) return 0 ;
	}
	else return 0 ;	// SFS does not exist yet!

	for(int r=start_r;r<=stop_r;r++) {	
		bytes += bsmd_d.bytes[r-1][2] ;
	}

	LOG(DBG,"rdo %d: bytes %d",rdo,bytes) ;

	if(bytes==0) return 0 ;


	if(is_ped) {
		dta_use = ped ;
		dta_use->create(1,"bsmd_ped",rts_id,DAQ_DTA_STRUCT(bsmd_t)) ;
	}
	else {
		dta_use = rms ;
		dta_use->create(1,"bsmd_rms",rts_id,DAQ_DTA_STRUCT(bsmd_t)) ;
	}




	LOG(DBG,"doing rdos: %d-%d",start_r,stop_r) ;

	for(int r=start_r;r<=stop_r;r++) {
			
		if(bsmd_d.bytes[r-1][2] == 0) continue ;

		u_short *data = (u_short *)((u_int)bsmd_d.dta[r-1][2]) ;	// move to data start

		LOG(DBG,"BSMD PEDR: rdo %d: 0x%04X 0x%04X 0x%04X 0x%04X",r,data[0],data[1],data[2],data[3]) ;

		
		data += 4 ;	// skip 4 shorts...

		
		for(int c=0;c<128;c++) {
			bsmd_t *bsmd  = (bsmd_t *) dta_use->request(1) ;
			bsmd->cap = c ;
			for(int ch=0;ch<4800;ch++) {
				int adc ;
				if(bsmd_d.endian[r-1][2]) {	
					adc = b2h16(*data++) ;
				}
				else {
					adc = l2h16(*data++) ;
				}

				if(is_ped) adc &= 0x3FF ;	// ped is lower 10 bits
				else {
					adc >>= 10 ;		// rms is upper 6
				}

				bsmd->adc[ch] = adc ;
			}
			dta_use->finalize(1,0,r,c) ;
		} 


	}
		
	dta_use->rewind() ;

	return dta_use ;

}


daq_dta *daq_bsmd::handle_adc_non_zs(int rdo)
{
	struct bsmd_desc bsmd_d ;
	int start_r, stop_r ;
	int bytes ;

	assert(caller) ;	// sanity...

	if(!present) return 0 ;

	if(rdo <= 0) {
		start_r = 1 ;
		stop_r = BSMD_FIBERS ;
	}
	else {
		start_r = stop_r = rdo ;
	}

	bytes = 0 ;

	if(present & DET_PRESENT_DATAP) {	// in datap!
		char *emcp = (char *)legacyDetp(rts_id, caller->mem) ;

		if(bsmd_reader(emcp, &bsmd_d)==0) return 0 ;
	}
	else return 0 ;	// SFS does not exist yet!

	//LOG(NOTE,"BSMD: rdo %d: start %d, stop %d",rdo,start_r, stop_r) ;

	for(int r=start_r;r<=stop_r;r++) {	
		//LOG(NOTE,"BSMD: adc_non_zs: fiber %d, bytes %d",r,bsmd_d.bytes[r-1][0]) ;
		bytes += bsmd_d.bytes[r-1][0] ;
	}

	if(bytes==0) return 0 ;

	adc_non_zs->create(bytes,"adc_nzs",rts_id,DAQ_DTA_STRUCT(bsmd_t)) ;

	for(int r=start_r;r<=stop_r;r++) {
		
		if(bsmd_d.bytes[r-1][0] == 0) continue ;

		bsmd_t *bsmd  = (bsmd_t *) adc_non_zs->request(1) ;

//		memset(bsmd,0,sizeof(bsmd_t)) ;	

		// cap is 64 bytes after the start
		bsmd->cap = *(char *)((u_int)bsmd_d.dta[r-1][0] + 4 + 4*16) ;

		LOG(DBG,"Found cap %d",bsmd->cap) ;

		u_short *data = (u_short *)((u_int)bsmd_d.dta[r-1][0] + 4 + 256) ;	// move to data start
		
		for(int c=0;c<BSMD_DATSIZE;c++) {
			bsmd->adc[c] = l2h16(*data++) ;
		} 

		adc_non_zs->finalize(1,0,r,bsmd->cap) ;
	}
		
	adc_non_zs->rewind() ;

	return adc_non_zs ;
	
}




int daq_bsmd::get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt)
{
	u_short *us = (u_short *)buff ;

	// L0 part
	trg[0].t = l2h16(us[2])*256 + l2h16(us[3]) ;
	trg[0].daq = 0 ;
	trg[0].trg = 4 ;	// BSMD does not give the correct L0, only L2 so we invent 4
	trg[0].rhic = l2h16(us[4]) ;
	
	// L2 part
	trg[1].t = trg[0].t ;	// copy over token
	trg[1].trg = us[0] ;	// this is where the trg cmd ought to be
	trg[1].daq = us[1] ;
	trg[1].rhic = trg[0].rhic + 1 ;

	
	return 2 ;
}
