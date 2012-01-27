#include <sys/types.h>
#include <errno.h>
#include <assert.h>
#include <stdlib.h>

#include <rtsLog.h>
#include <rtsSystems.h>



#include <SFS/sfs_index.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>


#include "daq_fgt.h"


const char *daq_fgt::help_string = "FGT\n\
adc	returns fgt_t;\n\
raw	returns raw data\n" ;

class daq_det_fgt_factory : public daq_det_factory
{
public:
	daq_det_fgt_factory() {
		daq_det_factory::det_factories[FGT_ID] = this ;
	}

	daq_det *create() {
		return new daq_fgt ;
	}
} ;

static daq_det_fgt_factory fgt_factory ;


daq_fgt::daq_fgt(daqReader *rts_caller) 
{
	rts_id = FGT_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "fgt" ;
	caller = rts_caller ;
	
	if(caller) caller->insert(this, rts_id) ;

	raw = new daq_dta ;
	adc = new daq_dta ;
	phys = new daq_dta ;
	ped = new daq_dta ;

#if 0
	// zap maps to unknown first
	memset(adc_to_phys,0xff,sizeof(adc_to_phys)) ;
	memset(phys_to_adc,0xff,sizeof(phys_to_adc)) ;

	// create/load maps...
#endif

	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_fgt::~daq_fgt() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete raw ;
	delete adc ;
	delete phys ;
	delete ped ;

	return ;
}



daq_dta *daq_fgt::get(const char *bank, int sec, int rdo, int pad, void *p1, void *p2) 
{	
	Make() ;

	if(present == 0) return 0 ;


	if(strcasecmp(bank,"raw")==0) {
		return handle_raw(rdo) ;
	}
	else if(strcasecmp(bank,"adc")==0) {
		return handle_adc(rdo,0) ;
	}
	else if(strcasecmp(bank,"phys")==0) {
		return handle_phys(sec,rdo,pad) ;
	}
	else if(strcasecmp(bank,"pedrms")==0) {
		return handle_ped(rdo) ;
	}


	LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	return 0 ;
}



daq_dta *daq_fgt::handle_raw(int rdo)
{
	char *st ;
	int r_start, r_stop ;
	int bytes ;

	assert(caller) ;	// sanity...

	if(!present) {
		return 0 ;
	}
	else {
		LOG(DBG,"%s: present %d",name,present) ;
	}

	char str[256] ;
	char *full_name ;


	if(rdo<=0) {
		r_start = 1 ;
		r_stop = 2 ;	
	}
	else {
		r_start = r_stop = rdo ;
	}


	raw->create(8*1024,"fgt_raw",rts_id,DAQ_DTA_STRUCT(char)) ;

	for(int r=r_start;r<=r_stop;r++) {
		sprintf(str,"%s/sec%02d/rb%02d/raw",sfs_name, 1, r) ;
		full_name = caller->get_sfs_name(str) ;
		
		if(!full_name) continue ;
		bytes = caller->sfs->fileSize(full_name) ;	// this is bytes


		st = (char *) raw->request(bytes) ;
		
		int ret = caller->sfs->read(str, st, bytes) ;
		if(ret != bytes) {
			LOG(ERR,"ret is %d") ;
		}

	
		raw->finalize(bytes,0,r,0) ;	// sector 0;
	}

	raw->rewind() ;

	return raw ;
	
}

	

daq_dta *daq_fgt::handle_adc(int rdo, char *rdobuff)
{
	int r_start, r_stop ;


	adc->create(1000,"fgt_adc",rts_id,DAQ_DTA_STRUCT(fgt_adc_t)) ;

	LOG(NOTE,"FGT: doing ADC") ;

	if((rdo <= 0) || (rdo > FGT_RDO_COU)){
		r_start = 1 ;
		r_stop = FGT_RDO_COU ;
	}
	else {
		r_start = r_stop = rdo ;
	}



	for(int r=r_start;r<=r_stop;r++) {
		u_int *d ;

		if(rdobuff == 0) {
			daq_dta *dd = handle_raw(r) ;
			if(dd == 0) continue ;

			if(dd->iterate() == 0) continue ;

			d = (u_int *) dd->Void ;
			int words = dd->ncontent/4 ;

			if(words <= 0) continue ;

		}
		else {
			d = (u_int *) rdobuff ;
		}



		int format_code = (d[2] >> 8) & 0xFF ;
		// 0: normal code
		// 1: test
		// 2: null

		int arm_mask = (d[3] >> 8) & 0x3F ;
		LOG(NOTE,"ARC Header: format %d, ARM mask 0x%02x",format_code,arm_mask);

		u_int *dta = d + 6 ;	// start at the 6th word

		for(int arm=0;arm<6;arm++) {
			if(arm_mask & (1<<arm)) ;
			else continue ;

			LOG(NOTE,"RDO %d, ARM %d, format_code %d",r,arm,format_code) ;

			int arm_id = *dta & 0x7 ;
			int arm_seq = (*dta >> 20) & 0xfff;
			int arm_err = (*dta >>16) & 0xf;
			dta++ ;

			if(arm_id != arm) {
				LOG(ERR,"RDO %d: Bad ARM ID: expect %d, have %d",r,arm,arm_id) ;
				LOG(ERR,"0x%08x 0x%08x 0x%08x",*(dta-2),*(dta-1),*dta);
				continue ;
			}

			if(arm_err != 0) {
				LOG(ERR,"RDO %d ARM %d: Error code 0x%x",r,arm,arm_err) ;
				continue ;
			}

			u_int apv_mask = *dta & 0x00FFFFFF ;
			dta++ ;

			// would get monitor register values from this word (not implemented yet, but need to skip over of course)
			dta++ ;


			for(int apv=0;apv<24;apv++) {
				if(apv_mask & (1<<apv)) ;
				else continue ;



				LOG(NOTE,"  APV %d",apv) ;

				int apv_id = *dta & 0x1F ;
				int length = (*dta >> 5) & 0x3FF ;   // it's probable that we never use >0x1ff, so there is a hidden 'reserved' bit here
				int fmt = (*dta >> 16) & 0xf ;       // promoted to 4 bits on 1/26/2012 (take over reserved=0 bit)
				int seq = (*dta >> 20) & 0xfff;

				dta++ ;

				LOG(NOTE,"  ID %d, length %d, fmt %d",apv_id,length,fmt) ;

				int capid = *dta & 0xFF ;
				int nhits = (*dta >> 8) & 0x7F ;
				int is_error = (*dta >> 15) & 1 ;
				int refadc = (*dta >> 16) & 0xFFF ;
				int ntim = (*dta >> 28) & 0x7 ;
				int is_0 = *dta & 0x80000000 ;

				dta++ ;

				nhits = ( ntim>0 ? nhits+1 : 0 );    // nhits (range 0-128) is encoded like this in 7-bit nhits field and ntim field

				LOG(NOTE,"  capid %d, nhits %d, is_error 0x%X, refadc %d, ntim %d, is_0 0x%X",
				    capid, nhits, is_error, refadc, ntim, is_0) ;


				// sanity checks
				if(apv != apv_id) {
					LOG(ERR,"Bad APV ID: expect %d, read %d",apv,apv_id) ;
					continue ;
				}

				if(seq != arm_seq) {
				  // should be ERR, not yet; when promoting to err we need to change fmt code from 1 to something new, since there
				  // is old data (before ARM firmware r67), with fmt code 1, that needs to be supported and which has wrong sequence numbers
				  LOG(WARN,"RDO %d ARM %d APV %d: Sequence number mismatch, expect %d have %d",r,arm,apv,arm_seq,seq);
				  //continue;
				}

				if((ntim <= 0) || (ntim > 7)) {
					LOG(ERR,"Ntim %d ?!",ntim) ;
					continue ;
				}
				if(fmt != 1) {
					LOG(ERR,"FMT %d != 1",fmt) ;
					continue ;
				}


				// set of Gerard's hadrcoded hacks...
				// [Gerard]: Modified "my" hardcoded hacks so it would work regardless of which APV's (except #23) are used, despite that the length code is not properly filled in.
				// This is necessary to support present cosmic ray test setup at STAR, original plan was no good (I forgot we would need different APV channels to support the other half of quadrant readout).
				// This here remains still quite a hack, I must fix the length code but that is slightly nontrivial for today
				// This will fail perhaps very occasionally if the data word is just exactly right to make it fail... pretty unlikely I hope!
				// It also fails if ever APV #23 is recognized / non-truncated. That shouldn't happen, of course (in FGT! not IST!)
				if (length==2)      // i.e. nothing but 2 header words in this record
				  continue;         //here paving the way for the length hack to be removed, this should remain working, for non-ZS data at least
				if (apv==23) {
				  dta += 3;
				  continue;
				}
				u_long expect0 = *(dta-2);
				expect0 = (expect0 & 0xffffffe0)|((expect0+1) & 0x0000001f); // increment just the APV id
				u_long expect1 = *(dta-1);
				// look for the signature of a skipped APV -- the expected header for next APV found at dta+3 !!
				// of course, it could be a fluke, then we will mess up, accept that risk
				if((*(dta+3)==expect0) && (*(dta+4)==expect1)) {
					dta += 3 ;
					continue ;
				}
				// END of the hacks to deal with skipped APV's
	
	
				fgt_adc_t *fgt_d = (fgt_adc_t *) adc->request(FGT_TB_COU*FGT_CH_COU) ;
				int cou = 0 ;

				// extract data here...


				u_short *d16 = (u_short *) dta ;
				u_short wfm[2720] ;       // actually use 1000 for 7 timebins, less for less, but better cover worst case length from header
				int i = 0 ;
				
				// it is important to note, this is specifically for format 1, and the length will always be of form 2+3*n
				for(int j=0;j<((length-2)/3)*2;j++) {
					u_short wtmp ;

					wfm[4*j]	= 0x0fff & d16[i] ;

					wtmp		= 0x000f & (d16[i++] >> 12) ;

					wfm[4*j + 1]	= (0x0ff0 & (d16[i]<<4)) | wtmp ;

					wtmp		= 0x00ff & (d16[i++] >> 8) ;

					wfm[4*j+2]	= (0x0f00 & (d16[i]<<8)) | wtmp ;
					wfm[4*j+3]	= 0x0fff & (d16[i++]>>4) ;
				}

				dta += (length - 2) ;	// skip "length" words - 2 for the header

				/*
				u_int *o_dta = dta ;
				dta = (u_int *)(d16 + i) ;

				LOG(TERR,"Diff %d",dta-o_dta) ;
				*/

				if ( 27+127+(ntim-1)*140 >= ((length-2)/3)*8 ) {
				  LOG(ERR,"trouble in APV block content, it's shorter than required to unpack %d timebins",ntim);
				  continue;
				}
				for(int ch=0;ch<128;ch++) {
					int rch = 32*(ch%4) + 8*(ch/4) - 31*(ch/16) ;     // see APV user guide (channel mux sequence)
					for(int tb=0;tb<ntim;tb++) {

						int adc = wfm[27+ch+tb*140] ;
						fgt_d[cou].ch = rch ;
						fgt_d[cou].tb = tb ;
						fgt_d[cou].adc = adc ;
						cou++ ;
					}
				}

				// note the reversal: sector->arm, row->rdo,
				adc->finalize(cou, arm, r, apv) ;

			}

		}
	}

	adc->rewind() ;

	return adc ;
			
}

daq_dta *daq_fgt::handle_ped(int rdo)
{

	int r_start, r_stop ;


	LOG(NOTE,"FGT PED is not yet supported") ;
	return 0 ;

	ped->create(1000,"fgt_pedrms",rts_id,DAQ_DTA_STRUCT(fgt_pedrms_t)) ;



	if((rdo <= 0) || (rdo > FGT_RDO_COU)){
		r_start = 1 ;
		r_stop = FGT_RDO_COU ;
	}
	else {
		r_start = r_stop = rdo ;
	}


	// HACK! EMULATION!!!
	for(int r=r_start;r<=r_stop;r++) {
		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {

			fgt_pedrms_t *d = (fgt_pedrms_t *) ped->request(FGT_TB_COU*FGT_CH_COU) ;

			int cou = 0 ;

			for(int tb=0;tb<FGT_TB_COU;tb++) {
			for(int ch=0;ch<FGT_CH_COU;ch++) {
			
				d[cou].ch = ch ;
				d[cou].tb = tb ;
				d[cou].ped = drand48() * 1024.0 ;
				d[cou].rms = 1.0/((double)(ch+1)) ;
				cou++ ;

			}
			}

			ped->finalize(cou,r,arm,apv) ;

		}
		}
	}

	ped->rewind() ;

	return ped ;
			


}


daq_dta *daq_fgt::handle_phys(int disk, int quadrant, int strip_type)
{

	return 0 ;

	LOG(WARN,"FGT PHYS not yet supported....") ;
	return 0 ;
}


// used to grab trigger info from the event header
int daq_fgt::get_l2(char *buff, int words, struct daq_trg_word *trg, int rdo)
{
	const int FGT_BYTES_MIN = ((6)*4) ;	// this is the minimum
	const int FGT_BYTES_MAX = (32768*4) ;
	const u_int FGT_VERSION = 0x0034 ;		 
	const u_int FGT_SIGNATURE = 0x46475420 ;	// "FGT"

	int t_cou = 0 ;
	int bad = 0 ;
	u_int *d32 = (u_int *)buff ;
	int id_check_failed = 0 ;

	// FIRST we check the length
	int buff_bytes = 4 * words ;

	if((buff_bytes < FGT_BYTES_MIN) || (buff_bytes > FGT_BYTES_MAX)) {
		LOG(ERR,"RDO %d: expect %d bytes, received %d",rdo,FGT_BYTES_MIN,buff_bytes) ;
		bad |= 1 ;
	}


	// grab crc from the last word

	// misc signatures and errors from the header
	if(d32[1] != FGT_SIGNATURE) {	// "FGT"
		LOG(ERR,"RDO %d: bad header sig 0x%08X, expect 0x%08X",rdo,d32[1], FGT_SIGNATURE) ;
		bad |= 1 ;
	}

	if(d32[5] != 0xFEEDBEEF) {	// deadface
		LOG(ERR,"RDO %d: bad deadface 0x%08X",rdo,d32[9]) ;
		bad |= 1 ;
	}


	/* wait for it to stabilize */
	if((d32[2] >> 16) != FGT_VERSION) {
		LOG(ERR,"RDO %d: bad version 0x%04X, expect 0x%04X",rdo,d32[2] >> 16, FGT_VERSION) ;
		bad |= 2 ;	// soft error
	
	}


	if((d32[3] & 0xFFFF0000)) {	// error_flags
		LOG(ERR,"RDO %d: error flags 0x%04X",rdo,d32[3]>>16) ;
		bad |= 2 ;
	}


//	int arm_mask = (d32[3]>>8) & 0x3F ;


	int rdo_in_dta = d32[3] & 0xFF ;	// fiber ID via jumpers...
	if(rdo_id[rdo] != 0xFF) {
		if(rdo_id[rdo] != rdo_in_dta) {
			id_check_failed++ ;
		}
	}


	// compare to what?
	LOG(DBG,"RDO %d: in data %d",rdo,rdo_in_dta) ;


	int format_code = (d32[2] >> 8) & 0xFF ;
	if(format_code == 0x02) {	// null event
		LOG(WARN,"RDO %d: format code 0x%X? (null event)",rdo,format_code) ;

//		trg[0].t = 4097 ;
//		trg[0].daq = 0 ;
//		trg[0].trg = 0 ;
//		trg[0].rhic = d32[4] ;

//		return 1 ;

	}

#if 0
#define	 G_CONST  0x04C11DB7 
	int last_ix = words - 1 ;
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
	trg[t_cou].t = d32[0] & 0xFFF ;
	trg[t_cou].daq = d32[2] & 0xF ;
	trg[t_cou].trg = (d32[2] >> 4) & 0xF ;
	trg[t_cou].rhic = d32[4] ;
	t_cou++ ;


	LOG(NOTE,"RDO %d: words %d: token %d, trg %d, daq %d: rhic %u: rdo_in_data %d, format_code 0x%X",rdo,words,
		trg[0].t, trg[0].trg, trg[0].daq, trg[0].rhic,
		rdo_in_dta,format_code) ;


	// check token and trg_cmd sanity...
	if(trg[0].t == 0) {
		LOG(ERR,"RDO %d: token 0?",rdo) ;
		trg[0].t = 4097 ;	// turn it to sanity!
		bad |= 2 ;
	}

	switch(trg[0].trg) {
	case 4 :
		break ;
	default :
		LOG(ERR,"RDO %d: bad trg_cmd %d",rdo, trg[0].trg) ;
		// sanitize
		trg[0].t = 4097 ;
		bad |= 2 ;
		break ;
	}

#if 0	// skip for the temporary 0x8129 V

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


	
	if(bad) {	
		LOG(WARN,"RDO %d: words %d: bad %d:",rdo,words,bad) ;
		// dump the whole header
		for(int i=0;i<10;i++) {
			LOG(WARN,"\tRDO %d: %4d: 0x%08X",rdo,i,d32[i]) ;
		}
		// dump last 4 words of the trailer as well
		for(int i=(words-4);i<words;i++) {
			LOG(WARN,"\tRDO %d: %4d: 0x%08X",rdo,i,d32[i]) ;
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
				LOG(CAUTION,"RDO %d: rdo check failed: expect 0x%02X, found 0x%02X",
				    rdo,rdo_id[rdo],rdo_in_dta) ;
			}			
		}


		rdo_warns[rdo]++ ;
		if(rdo_warns[rdo]<2) {
			LOG(NOTE,"RDO %d: rdo check: expect 0x%02X, found 0x%02X",
			    rdo,rdo_id[rdo],rdo_in_dta) ;
		}


		return t_cou ;
	}
}


