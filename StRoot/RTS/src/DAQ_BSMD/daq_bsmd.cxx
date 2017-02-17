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

static const struct bsmd_old_to_new_map_t {
	char new_sec ;
	char new_rdo ;
} bsmd_old_to_new_map[13] = {
	{ 0, 0 },	// we start from 1 so this a dummy!
	{ 1, 1 },
	{ 1, 2 },
	{ 1, 3 },
	{ 1, 4 },
	{ 1, 5 },
	{ 1, 6 },
	{ 2, 1 },
	{ 2, 2 },
	{ 2, 3 },
	{ 2, 4 },
	{ 2, 5 },
	{ 2, 6 }

} ;



daq_bsmd::daq_bsmd(daqReader *rts_caller) 
{
	rts_id = BSMD_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "bsmd" ;
	caller = rts_caller ;
	t_data = 0 ;

	if(caller) caller->insert(this, rts_id) ;


	adc = new daq_dta ;
	adc_non_zs = new daq_dta ;
	ped = new daq_dta ;
	rms = new daq_dta ;
	raw = new daq_dta ;

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
	delete raw ;

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
	else if(strcasecmp(bank,"raw")==0) {
		return handle_raw(row) ;
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
	memset(&bsmd_d,0,sizeof(bsmd_d)) ;

	if(present & DET_PRESENT_DATAP) {	// in datap!
		char *emcp = (char *)legacyDetp(rts_id, caller->mem) ;
		//LOG(NOTE,"EMCP %p?",emcp) ;
		if(bsmd_reader(emcp, &bsmd_d)==0) return 0 ;

		for(int r=start_r;r<=stop_r;r++) {	
			bytes += bsmd_d.bytes[r-1][1] ;
		}

	}
	else {
		int s_new, r_new ;
		char str[256] ;
		char *full_name ;
		
		for(int r=start_r;r<=stop_r;r++) {
			int l_bytes ;

			s_new = bsmd_old_to_new_map[r].new_sec ;
			r_new = bsmd_old_to_new_map[r].new_rdo ;

			sprintf(str,"%s/sec%02d/rb%02d/adc",sfs_name,s_new,r_new) ;

			full_name = caller->get_sfs_name(str) ;

			if(!full_name) continue ;

			l_bytes = caller->sfs->fileSize(full_name) ;
			if(!l_bytes) continue ;

			bsmd_d.bytes[r-1][1] = l_bytes ;

			bytes += l_bytes ;
		}

	}



	LOG(DBG,"rdo %d: bytes %d",rdo,bytes) ;

	if(bytes==0) return 0 ;	// odd, nothing found...

	adc->create(bytes,"adc",rts_id,DAQ_DTA_STRUCT(bsmd_t)) ;

	u_short *data_alloc ;
	int malloced_bytes ;

	if(present & DET_PRESENT_DATAP) {
		malloced_bytes = 0 ;
		data_alloc = 0 ;
	}
	else {
		// ZS _could_ be larger than raw by factors of 2!
		malloced_bytes = (2400+10+3+100)*4   *2 ;
		data_alloc = (u_short *)malloc(malloced_bytes) ;
		assert(data_alloc) ;
	}


	for(int r=start_r;r<=stop_r;r++) {
		int count ;
		int version, fiber ;
		u_short *data ;

		if(bsmd_d.bytes[r-1][1] == 0) continue ;

		if(present & DET_PRESENT_DATAP) {

			data = (u_short *)(bsmd_d.dta[r-1][1]) ;	// move to data start
		}
		else {
			int s_new, r_new ;
			char str[256] ;
			char *full_name ;

			s_new = bsmd_old_to_new_map[r].new_sec ;
			r_new = bsmd_old_to_new_map[r].new_rdo ;

			sprintf(str,"%s/sec%02d/rb%02d/adc",sfs_name,s_new,r_new) ;

			full_name = caller->get_sfs_name(str) ;

			if(!full_name) continue ;

			bytes = caller->sfs->fileSize(full_name) ;

			if(bytes > malloced_bytes) {
				LOG(ERR,"Too big %s is %d",str,bytes) ;
				continue ;
			}

			data = data_alloc ;
			caller->sfs->read(str, (char *)data, bytes) ;

			bsmd_d.endian[r-1][1] = 0 ;	// little endian
		}

		bsmd_t *bsmd  = (bsmd_t *) adc->request(1) ;

		memset(bsmd,0,sizeof(bsmd_t)) ;	


		if(bsmd_d.endian[r-1][1]) {	// big!
			version = b2h16(data[0]) ;
			count = b2h16(data[1]) ;
			bsmd->cap = b2h16(data[2]) ;
			fiber = b2h16(data[3]) ;

			bsmd->cap &= 0x7F ;
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

			bsmd->cap &= 0x7F ;
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

	if(malloced_bytes) free(data_alloc) ;

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

	// for SFS based
	int want_sec[3] = { 0, 0, 0 } ;
	u_short *pedrms[3] = { 0, 0, 0 } ;

	for(int r=start_r;r<=stop_r;r++) {	

		int s_new, r_new ;
		s_new = bsmd_old_to_new_map[r].new_sec ;
		r_new = bsmd_old_to_new_map[r].new_rdo ;

		want_sec[s_new] = 1 ;
	}


	bytes = 0 ;

	if(present & DET_PRESENT_DATAP) {	// in datap!
		char *emcp = (char *)legacyDetp(rts_id, caller->mem) ;
		//LOG(NOTE,"EMCP %p?",emcp) ;
		if(bsmd_reader(emcp, &bsmd_d)==0) return 0 ;


		for(int r=start_r;r<=stop_r;r++) {	
			bytes += bsmd_d.bytes[r-1][2] ;
		}

		
	}
	else {
		for(int s=1;s<=2;s++) {
			char str[256] ;
			char *full_name ;
			int l_bytes ;

			if(want_sec[s] == 0) continue ;

			sprintf(str,"%s/sec%02d/pedrms",sfs_name,s) ;

			full_name = caller->get_sfs_name(str) ;

			if(!full_name) continue ;

			l_bytes = caller->sfs->fileSize(full_name) ;

			bytes += l_bytes ;


			pedrms[s] = (u_short *) malloc(l_bytes) ;

			caller->sfs->read(str, (char *)(pedrms[s]), l_bytes) ;

		}
#if 0
		// try to fix FY13 bug!
		if(bytes==0) {	// not found
			char str[256] ;
			char *full_name ;


			sprintf(str,"%s/pedrms",sfs_name) ;

			full_name = caller->get_sfs_name(str) ;

			LOG(TERR,"FY13 bug -- Got %s for RDO%d, ped %d?!",str,rdo,is_ped) ;
		}
#endif
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
		u_short *data ;

		if(present & DET_PRESENT_DATAP) {
			if(bsmd_d.bytes[r-1][2] == 0) continue ;

			data = (u_short *)(bsmd_d.dta[r-1][2]) ;	// move to data start
		}
		else {	// SFS
			int s_new, r_new ;

			s_new = bsmd_old_to_new_map[r].new_sec ;
			r_new = bsmd_old_to_new_map[r].new_rdo ;
			if(pedrms[s_new]==0) continue ;

			data = pedrms[s_new] + ((4+128*4800)*(r_new-1)) ;
			
			bsmd_d.endian[r-1][2] = 0 ;// little endian
			
		}

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

	for(int s=1;s<=2;s++) {
		if(pedrms[s]) free(pedrms[s]) ;
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


		for(int r=start_r;r<=stop_r;r++) {	
			//LOG(NOTE,"BSMD: adc_non_zs: fiber %d, bytes %d",r,bsmd_d.bytes[r-1][0]) ;
			bytes += bsmd_d.bytes[r-1][0] ;
		}

		if(bytes==0) return 0 ;

		adc_non_zs->create(bytes,"adc_nzs",rts_id,DAQ_DTA_STRUCT(bsmd_t)) ;

		for(int r=start_r;r<=stop_r;r++) {
		
			if(bsmd_d.bytes[r-1][0] == 0) continue ;

			bsmd_t *bsmd  = (bsmd_t *) adc_non_zs->request(1) ;

//			memset(bsmd,0,sizeof(bsmd_t)) ;	

			// cap is 64 bytes after the start
			bsmd->cap = *(char *)((char *)bsmd_d.dta[r-1][0] + 4 + 4*16) ;
			
			LOG(DBG,"Found cap %d",bsmd->cap) ;

			u_short *data = (u_short *)((char *)bsmd_d.dta[r-1][0] + 4 + 256) ;	// move to data start
		
			for(int c=0;c<BSMD_DATSIZE;c++) {
				bsmd->adc[c] = l2h16(*data++) ;
			} 

			adc_non_zs->finalize(1,0,r,bsmd->cap) ;
		}


	}
	else {

		for(int r=start_r;r<=stop_r;r++) {
			bytes += sizeof(bsmd_t) ;	// approx
		}

	
		adc_non_zs->create(bytes,"adc_nzs",rts_id,DAQ_DTA_STRUCT(bsmd_t)) ;

		for(int r=start_r;r<=stop_r;r++) {
			daq_dta *dd = handle_raw(r) ;
			
			if(dd==0) continue ;

			if(dd->iterate() == 0) continue ;


			u_int *d = (u_int *)dd->Void ;
			u_int rdo_words = dd->ncontent ;

			if(rdo_words == 0) continue ;

			bsmd_t *bsmd  = (bsmd_t *) adc_non_zs->request(1) ;

//			memset(bsmd,0,sizeof(bsmd_t)) ;	

			// cap is 64 bytes after the start
			bsmd->cap = d[8] & 0x7F ;
			
			
			LOG(DBG,"Found cap %d",bsmd->cap) ;

			u_short *data = (u_short *)(d+10) ;	// move to data start which is 10 words after
		
			for(int c=0;c<BSMD_DATSIZE;c++) {
				bsmd->adc[c] = l2h16(*data++) ;
			} 

			adc_non_zs->finalize(1,0,r,bsmd->cap) ;
		}





	}

	//LOG(NOTE,"BSMD: rdo %d: start %d, stop %d",rdo,start_r, stop_r) ;

		
	adc_non_zs->rewind() ;

	return adc_non_zs ;
	
}


daq_dta *daq_bsmd::handle_raw(int rdo)
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


		for(int r=start_r;r<=stop_r;r++) {	
			//LOG(NOTE,"BSMD: adc_non_zs: fiber %d, bytes %d",r,bsmd_d.bytes[r-1][0]) ;
			bytes += bsmd_d.bytes[r-1][0] ;
		}

		if(bytes==0) return 0 ;

		raw->create(bytes,"raw",rts_id,DAQ_DTA_STRUCT(char)) ;

		for(int r=start_r;r<=stop_r;r++) {
			bytes = bsmd_d.bytes[r-1][0] ;

			if(bytes == 0) continue ;

			char *st  = (char *) raw->request(bytes) ;

			memcpy(st,(char *)bsmd_d.dta[r-1][0],bytes) ;

			raw->finalize(bytes,0,r,0) ;
		}
	}
	else if(present & DET_PRESENT_SFS) {
		int s_new, r_new ;
		char str[256] ;
		char *full_name ;


		for(int r=start_r;r<=stop_r;r++) {

			s_new = bsmd_old_to_new_map[r].new_sec ;
			r_new = bsmd_old_to_new_map[r].new_rdo ;

			sprintf(str,"%s/sec%02d/rb%02d/raw",sfs_name,s_new,r_new) ;

			full_name = caller->get_sfs_name(str) ;

			if(!full_name) continue ;

			bytes += caller->sfs->fileSize(full_name) ;

		}

		
		LOG(NOTE,"BSMD raw: total of %d bytes",bytes) ;
		if(bytes == 0) return 0 ;

		raw->create(bytes,"bsmd_raw",rts_id,DAQ_DTA_STRUCT(char)) ;


		for(int r=start_r;r<=stop_r;r++) {

			s_new = bsmd_old_to_new_map[r].new_sec ;
			r_new = bsmd_old_to_new_map[r].new_rdo ;

			sprintf(str,"%s/sec%02d/rb%02d/raw",sfs_name,s_new,r_new) ;

			full_name = caller->get_sfs_name(str) ;

			if(!full_name) continue ;

			bytes = caller->sfs->fileSize(full_name) ;

			char *st = (char *) raw->request(bytes) ;
			int ret = caller->sfs->read(str, st, bytes) ;
			if(ret != bytes) {
				LOG(ERR,"ret is %d != %d",ret,bytes) ;
			}

			raw->finalize(bytes,s_new,r) ;	// add the sector but keep the old "RDO"!
		}



	}
	else return 0 ;



		
	raw->rewind() ;

	return raw ;
	
}




int daq_bsmd::get_l2(char *buff, int words, struct daq_trg_word *trg, int rdo)
{
	const int BSMD_BYTES_MIN = ((2400+10+3)*4) ;	// this is the minimum
	const int BSMD_BYTES_MAX = ((2400+10+3+30)*4) ;
//	const u_int BSMD_VERSION = 0x8035 ;		// Nov 09
	const u_int BSMD_SIGNATURE = 0x42534D44 ;	// "BSMD"
	const u_int BSMD_HDR_ID = 5 ;	// by some definiton somewhere...

	int t_cou = 0 ;
	int bad = 0 ;
	u_int *d32 = (u_int *)buff ;
	int id_check_failed = 0 ;

	//HACKINTOSH!
	words = 2413 ;

	// FIRST we check the length
	int buff_bytes = 4 * words ;

	if((buff_bytes < BSMD_BYTES_MIN) || (buff_bytes > BSMD_BYTES_MAX)) {
		LOG(ERR,"RDO %d: expect %d bytes, received %d",rdo,BSMD_BYTES_MIN,buff_bytes) ;
		bad |= 1 ;
	}


	// grab crc from the last word
	int last_ix = words - 1 ;
	// misc signatures and errors from the header
	if(d32[1] != BSMD_SIGNATURE) {	// "BSMD"
		LOG(ERR,"RDO %d: bad header sig 0x%08X, expect 0x%08X",rdo,d32[1], BSMD_SIGNATURE) ;
		bad |= 1 ;
	}

	if(d32[9] != 0xDEADFACE) {	// deadface
		LOG(ERR,"RDO %d: bad deadface 0x%08X",rdo,d32[9]) ;
		bad |= 1 ;
	}


/* wait for it to stabilize
	if((d32[2] >> 16) != BSMD_VERSION) {
		LOG(ERR,"RDO %d: bad version 0x%04X, expect 0x%04X",rdo,d32[2] >> 16, BSMD_VERSION) ;
		bad |= 2 ;	// soft error
	
	}
*/

	if((d32[3] & 0xFFFF0000)) {	// error_flags
		LOG(ERR,"RDO %d: error flags 0x%04X",rdo,d32[3]>>16) ;
		bad |= 2 ;
	}

	if(((d32[3]>>8) & 0xFF) != BSMD_HDR_ID) {	// det_id
		LOG(ERR,"RDO %d: bad det_id 0x%02X",rdo,(d32[3]>>8)&0xFF) ;
		bad |= 1 ;
	}

	int rdo_in_dta = d32[3] & 0xFF ;	// fiber ID via jumpers...
	if(rdo_id[rdo] != 0xFF) {		// skip the check!
		if(rdo_id[rdo] != rdo_in_dta) {
			id_check_failed++ ;
		}
	}

	// compare to what?
	LOG(DBG,"RDO %d: in data %d",rdo,rdo_id) ;


	int format_code = (d32[2] >> 8) & 0xFF ;
	if(format_code == 0x02) {	// null event
		LOG(NOTE,"RDO %d: null event",rdo) ;

		trg[0].t = 4097 ;
		trg[0].daq = 0 ;
		trg[0].trg = 0 ;
		trg[0].rhic = d32[4] ;

		return 1 ;

	}

#if 1
#define	 G_CONST  0x04C11DB7 

	u_int crc_in_data = d32[last_ix] ;
	register u_int crc = 0xFFFFFFFF ;
	if(crc_in_data) {	
		for(int i=0;i<last_ix;i++) {
			u_int datum ;

			datum = d32[i] ;
			register u_int data_j ;
			register u_int crc_31 ;

			for(register int j=31;j>=0;j--) {
				data_j = (datum >> j) & 1 ;
				crc_31 = (crc & 0x80000000) ? 1 : 0 ;

				if(crc_31 == data_j) {
					crc = (crc<<1) ^ G_CONST ;
				}
				else {
					crc = (crc<<1) ;
				}
			}
		}

		if(crc != crc_in_data) {
			LOG(ERR,"RDO %d: CRC in data 0x%08X, CRC calculated 0x%08X",rdo,crc_in_data,crc) ;
			bad |= 1 ;
		}
	}	

	LOG(DBG,"RDO %d: CRC in data 0x%08X, CRC calculated 0x%08X",rdo,crc_in_data,crc) ;
#endif


	// L0 part
	t_cou = 0 ;
	trg[0].t = d32[0] & 0xFFF ;
	trg[0].daq = d32[2] & 0xF ;
	trg[0].trg = (d32[2] >> 4) & 0xF ;
	trg[0].rhic = d32[4] ;
	
	t_cou++ ;


	LOG(NOTE,"RDO %d: token %d, trg %d, daq %d: rhic %u",rdo, 
		trg[0].t, trg[0].trg, trg[0].daq, trg[0].rhic) ;


	// check token and trg_cmd sanity...
	if(trg[0].t == 0) {
		LOG(ERR,"RDO %d: token 0?",rdo) ;
		trg[0].t = 4097 ;	// turn it to sanity!
		bad |= 2 ;
	}


#if 1	// skip for the temporary 0x8129 V

	// get mesg_length
	int mesg_length = d32[last_ix-1] & 0xFFF ;	// 12 bits only
	if(mesg_length > 30) {
		LOG(ERR,"RDO %d: bad trigger length %d",rdo,mesg_length) ;
		// kill it! this will make the main length bad too
		mesg_length = 0 ;
		bad |= 2 ;
	}

	for(int i=0;i<mesg_length;i++) {
		u_int trg_data = d32[last_ix - 2 - i] ;

		
		
		trg[t_cou].t = (trg_data >> 8) & 0xFFF ;
		trg[t_cou].trg = (trg_data >> 4) & 0xF ;
		trg[t_cou].daq = trg_data & 0xF ;
		trg[t_cou].rhic = (trg_data >> 20) & 0x7FF ;


		// check the triggers here!
		if(trg_data & 0x80000000) {
			LOG(ERR,"RDO %d: FIFO limit 0x%08X",rdo,trg_data) ;
			bad |= 2 ;
		}


		// need some sane limit here on t_cou
		if(t_cou >= 120) {
			LOG(ERR,"RDO %d: too many triggers %d",rdo,t_cou) ;
			bad |= 2 ;
			break ;
		}

		t_cou++ ;
	}


//	int trailer_event = d32[last_ix - 2 - mesg_length] & 0xFFFF ;


//	if(trailer_event != d32[0]) {
//		LOG(ERR,"RDO %d: bad trailer event 0x%08X != header 0x%08X",rdo,trailer_event,d32[0]) ;
//		bad |= 2 ;
//	}
#endif


	// check trigger command sanity...
	switch(trg[0].trg) {
	case 2 :
		LOG(WARN,"RDO %d: T %04d: odd trg_cmd %d (daq_cmd %d) -- will ignore this event even if it had errors...",
			rdo, trg[0].t, trg[0].trg, trg[0].daq) ;
		trg[0].t = 4097 ;	// sanitize...
		bad = 0 ;		// and force-clear any error....
		break ;
	case 4 :
	case 8 :
	case 11 :
	case 12 :
		break ;
	default :
		LOG(ERR,"RDO %d: T %04d: bad trg_cmd %d (daq_cmd %d)",rdo, trg[0].t, trg[0].trg, trg[0].daq) ;
		// sanitize
		trg[0].t = 4097 ;
		bad |= 2 ;
		break ;
	}

	
	if(bad) {	
		LOG(WARN,"RDO %d: words %d: bad %d:",rdo,words,bad) ;
		// dump the whole header
		for(int i=0;i<10;i++) {
			LOG(WARN,"\tRDO %d: header %4d: 0x%08X",rdo,i,d32[i]) ;
		}
		// dump last 4 words of the trailer as well
		for(int i=(words-4);i<words;i++) {
			LOG(WARN,"\tRDO %d: trailer %4d: 0x%08X",rdo,i,d32[i]) ;
		}
		
	}
	else if(trg[0].trg==11) {	// special test pattern!
		int bad_cou = 0;
		int shutup = 0 ;
		for(int i=10;i<2410;i++) {
			u_int should ;
			u_int b31, b21, b1, b0 ;

			b31 = (t_data >> 31) & 1 ;
			b21 = (t_data >> 21) & 1 ;
			b1 = (t_data >> 1) & 1 ;
			b0 = (t_data >> 0) & 1 ;

			should = !(b31 ^ b21 ^ b1 ^b0) ;
			should = (t_data << 1) | should ;

			if(d32[i] != t_data) {
				if(!shutup) LOG(WARN,"word %4d: should 0x%08X, is 0x%08X",i,t_data,d32[i]) ;
				bad_cou++ ;
			}

			if(bad_cou > 2) shutup = 1  ;

			should = !(b31 ^ b21 ^ b1 ^b0) ;
			should = (t_data << 1) | should ;

			t_data = should ;

		}

		if(bad_cou) LOG(ERR,"RDO %d: bad locations %d",rdo,bad_cou) ;
	}
	
	if(bad & 1) {	// critical errors
		return 0 ;	// no trigger!
	}
	else {

		if(id_check_failed) {
			rdo_warns[rdo]++ ;
			if(rdo_warns[rdo] < 5) {
				if(rdo_id[rdo]==0x5 || rdo_id[rdo]==0xD) {
					LOG(ERR,"RDO %d: rdo check failed: expect 0x%02X, found 0x%02X",
					    rdo,rdo_id[rdo],rdo_in_dta) ;
				}
				else {
					LOG(CAUTION,"RDO %d: rdo check failed: expect 0x%02X, found 0x%02X",
					    rdo,rdo_id[rdo],rdo_in_dta) ;
				}
			}			
		}

/*
		rdo_warns[rdo]++ ;
		if(rdo_warns[rdo]<2) {
			LOG(TERR,"RDO %d: rdo check: expect 0x%02X, found 0x%02X",
			    rdo,rdo_id[rdo],rdo_in_dta) ;
		}
*/

		return t_cou ;
	}
}
