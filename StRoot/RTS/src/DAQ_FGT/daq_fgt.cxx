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

// for FGT proper
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


// for IST
class daq_det_ist_factory : public daq_det_factory
{
public:
	daq_det_ist_factory() {
		daq_det_factory::det_factories[IST_ID] = this ;
	}

	daq_det *create() {
		daq_det *dd = new daq_fgt ;

		((daq_fgt *)dd)->set_flavor(IST_ID) ;

		return dd ;
	}
} ;

static daq_det_ist_factory ist_factory ;

// for FST
class daq_det_fst_factory : public daq_det_factory
{
public:
	daq_det_fst_factory() {
		daq_det_factory::det_factories[FST_ID] = this ;
	}

	daq_det *create() {
		daq_det *dd = new daq_fgt ;

		((daq_fgt *)dd)->set_flavor(FST_ID) ;

		return dd ;
	}
} ;

static daq_det_fst_factory fst_factory ;

// for GMT
class daq_det_gmt_factory : public daq_det_factory
{
public:
	daq_det_gmt_factory() {
		daq_det_factory::det_factories[GMT_ID] = this ;
	}

	daq_det *create() {
		daq_det *dd = new daq_fgt ;

		((daq_fgt *)dd)->set_flavor(GMT_ID) ;

		return dd ;
	}
} ;

static daq_det_gmt_factory gmt_factory ;



void daq_fgt::set_flavor(int id)
{
	switch(id) {
	case IST_ID :
		rts_id = id ;
		name = rts2name(rts_id) ;
		sfs_name = "ist" ;
		break ;
	case FST_ID :
		rts_id = id ;
		name = rts2name(rts_id) ;
		sfs_name = "fst" ;
		break ;
	case GMT_ID :
		rts_id = id ;
		name = rts2name(rts_id) ;
		sfs_name = "gmt" ;
		break ;
	default :
		rts_id = FGT_ID ;	
		name = rts2name(rts_id) ;
		sfs_name = "fgt" ;
		break ;
	}

	LOG(DBG,"set_flavor %d [%s]",id,name) ;

	return ;
} ;

daq_fgt::daq_fgt(daqReader *rts_caller) 
{
	rts_id = FGT_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "fgt" ;
	caller = rts_caller ;
	
	if(caller) caller->insert(this, rts_id) ;

	raw = new daq_dta ;
	adc = new daq_dta ;
	zs = new daq_dta ;
	ped = new daq_dta ;


	adc->meta = (void *) &apv_meta ;	// meta exists for adc data only!
	zs->meta = (void *) &apv_meta ;

	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_fgt::~daq_fgt() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete raw ;
	delete adc ;
	delete zs ;
	delete ped ;

	return ;
}



daq_dta *daq_fgt::get(const char *bank, int sec, int rdo, int pad, void *p1, void *p2) 
{	

	Make() ;

	if(present == 0) return 0 ;


	zs->meta = 0 ;
	adc->meta = 0 ;

	if(strcasecmp(bank,"raw")==0) {
		return handle_raw(sec,rdo) ;
	}
	else if(strcasecmp(bank,"adc")==0) {
		return handle_adc(sec,rdo,0) ;
	}
	else if(strcasecmp(bank,"pedrms")==0) {
		return handle_ped(sec,rdo) ;
	}
	else if(strcasecmp(bank,"zs")==0) {
		return handle_zs(sec,rdo) ;
	}


	LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	return 0 ;
}



daq_dta *daq_fgt::handle_raw(int sec, int rdo)
{
	char *st ;
	int r_start, r_stop ;
	int bytes ;
	int s = 1 ;

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
		r_stop = 6 ;	
	}
	else {
		r_start = r_stop = rdo ;
	}

	raw->create(8*1024,"fgt_raw",rts_id,DAQ_DTA_STRUCT(char)) ;


	for(int r=r_start;r<=r_stop;r++) {
		sprintf(str,"%s/sec%02d/rb%02d/raw",sfs_name, s, r) ;
		full_name = caller->get_sfs_name(str) ;
		
		if(!full_name) continue ;
		bytes = caller->sfs->fileSize(full_name) ;	// this is bytes


		st = (char *) raw->request(bytes) ;
		
		int ret = caller->sfs->read(str, st, bytes) ;
		if(ret != bytes) {
			LOG(ERR,"ret is %d") ;
		}

	
		raw->finalize(bytes,s,r,0) ;	// sector 0;
	}

	raw->rewind() ;

	return raw ;
	
}

	
daq_dta *daq_fgt::handle_zs(int sec, int rdo, char *rdobuff, int inbytes)
{
	int r_start, r_stop ;
	int s = 1 ;	// for now...
	
	zs->create(1000,"fgt_zs",rts_id,DAQ_DTA_STRUCT(fgt_adc_t)) ;

	LOG(NOTE,"FGT: doing ZS") ;
	memset(&apv_meta,0,sizeof(apv_meta)) ;

	if((rdo <= 0) || (rdo > FGT_RDO_COU)){
		r_start = 1 ;
		r_stop = FGT_RDO_COU ;
	}
	else {
		r_start = r_stop = rdo ;
	}

	int found_some = 0 ;

	for(int r=r_start;r<=r_stop;r++) {
		u_short *d ;
		int bytes ;

		if(rdobuff == 0) {
			char str[128] ;
			char *full_name ;

			sprintf(str,"%s/sec%02d/rb%02d/zs",sfs_name, s, r) ;
			full_name = caller->get_sfs_name(str) ;
		
			if(!full_name) continue ;
			bytes = caller->sfs->fileSize(full_name) ;	// this is bytes

			d = (u_short *) malloc(bytes) ;
			
			int ret = caller->sfs->read(str, (char *)d, bytes) ;
			if(ret != bytes) {
				LOG(ERR,"ret is %d") ;
			}

		}
		else {
			d = (u_short *) rdobuff ;
			bytes = inbytes ;
		}

		if(d == 0) continue ;

		
		int *d32 = (int *) d ;

		if(d32[0]  != (int)0xFEEDBEEF) {
			LOG(ERR,"Bad signature 0x%04X",d32[0]) ;
			continue ;
		}


		switch(d32[1]) {
		case META_ZS_VERSION :
		case META_PED_ZS_VERSION :
			break ;
		default :
			LOG(ERR,"Unknown version 0x%04X",d32[1]) ;
			continue ;
		}

		found_some = 1 ;

		bytes = d32[2] ;

		int meta_bytes = d32[3] ;



		// grab the count of dumped channels from the trailer
		int dumped_chs = d[bytes/2-1] ;
		int got_chs = 0 ;


		// do meta...
		apv_meta_zs_t *meta_zs = (apv_meta_zs_t *) (d32+4) ;


		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
			if(meta_zs->status[arm][apv]) {
				apv_meta.arc[r].arm[arm].present = 1 ;
				apv_meta.arc[r].present = 1 ;

				apv_meta.arc[r].arm[arm].apv[apv].present = 1 ;
				apv_meta.arc[r].arm[arm].apv[apv].ntim = meta_zs->tb_cou ;

				if(meta_zs->status[arm][apv] != 1) {
					apv_meta.arc[r].arm[arm].apv[apv].error = 1 ;
				}
			}

		}}


		LOG(NOTE,"Expect %d dumped CHs in %d bytes, meta_bytes %d, tb_cou %d",dumped_chs,bytes,meta_bytes,meta_zs->tb_cou) ;

//		for(int i=0;i<100;i++) {
//			LOG(TERR,"%d: 0x%04X",i,d[i]) ;
//		}

		int ix = 2*4 + meta_bytes/2 ;

		bytes -= 4*4 ;	// 3 header d-words...
		bytes -= meta_bytes ;	// apv_meta_zs_t ;
		bytes -= 1*2 ;	// 1 trailer s_word

//		LOG(TERR,"ix %d, bytes left %d",ix,bytes) ;

		int arc = 0 ;
		int arm = 0 ;
		int apv = 0 ;

		int cou = 0 ;

		fgt_adc_t *fgt_d = 0 ;

		while(bytes>0) {


			while((d[ix] & 0xFF00) == 0xAB00) {
				//LOG(TERR,"%d %d %d %d",cou,arm,arc,apv) ;

				if(cou) {
					//LOG(TERR,"finalize: %d %d %d %d",cou,arm,arc,apv) ;
					zs->finalize(cou,arm,arc,apv) ;
				}
				cou = 0 ;

				arc = d[ix] & 0xFF ;
				ix++ ;
			
				arm = d[ix] >> 8 ;
				apv = d[ix] & 0xFF ;
				ix++ ;

				bytes -= 4 ;

			}
		

			if(bytes <= 0) continue ;

			//LOG(TERR,"r: %d %d %d (%d)",arc,arm,apv,cou) ;

			if(cou==0) {
				//LOG(TERR,"request: %d %d %d %d",cou,arm,arc,apv) ;
				fgt_d = (fgt_adc_t *) zs->request(FGT_CH_COU*FGT_TB_COU) ;
			}

			int ch = d[ix] & 0xFF ;
			int tb_cou = d[ix] >> 8 ;
			ix++ ;

			got_chs++ ;

			
			bytes -= 2*(1+tb_cou) ;
			for(int i=0;i<tb_cou;i++) {
				fgt_d[cou].ch = ch ;
				fgt_d[cou].tb = i ;
				fgt_d[cou].adc = d[ix] ;
				cou++ ;

				//printf("ZS: %d %d %d %d %d = %d\n",arc,arm,apv,ch,i,d[ix]) ;


				ix++ ;
			}
		}

		if(cou) {
			zs->finalize(cou,arm,arc,apv) ;

		}

		if(got_chs != dumped_chs) {
			LOG(ERR,"Mismatch: got %d, expect %d",got_chs,dumped_chs) ;
		}

		if(rdobuff == 0) free(d) ;

	}

	zs->rewind() ;
	
	if(found_some) {
		zs->meta = (void *) &apv_meta ;
	}
	else {
		return 0 ;
	}

	return zs ;
}


daq_dta *daq_fgt::handle_adc(int sec, int rdo, char *rdobuff)
{
	int r_start, r_stop ;
	int s = 1 ;	// for now...
	
	adc->create(1000,"fgt_adc",rts_id,DAQ_DTA_STRUCT(fgt_adc_t)) ;

	memset(&apv_meta,0,sizeof(apv_meta)) ;

	LOG(NOTE,"FGT: doing ADC") ;

	if((rdo <= 0) || (rdo > FGT_RDO_COU)){
		r_start = 1 ;
		r_stop = FGT_RDO_COU ;
	}
	else {
		r_start = r_stop = rdo ;
	}


	int found_some = 0 ;

	for(int r=r_start;r<=r_stop;r++) {
		u_int *d ;

		if(rdobuff == 0) {
			daq_dta *dd = handle_raw(s,r) ;
			if(dd == 0) continue ;

			if(dd->iterate() == 0) continue ;

			d = (u_int *) dd->Void ;
			int words = dd->ncontent/4 ;

			if(words <= 0) continue ;

		}
		else {
			d = (u_int *) rdobuff ;
		}


		found_some = 1 ;

		int format_code = (d[2] >> 8) & 0xFF ;
		// 0: normal code
		// 1: test
		// 2: null

		int arm_mask = (d[3] >> 8) & 0x3F ;
		LOG(NOTE,"[evt %d]: RDO %d: ARC Header: format %d, ARM mask 0x%02x",get_global_event_num(),r,format_code,arm_mask);

		apv_meta.arc[r].present = 1 ;
		apv_meta.arc[r].format_code = format_code ;
		apv_meta.arc[r].arm_mask = arm_mask ;


		u_int *dta = d + 6 ;	// start at the 6th word

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
			if(arm_mask & (1<<arm)) ;
			else continue ;

			LOG(NOTE,"[evt %d]: RDO %d: Handling ARM %d",get_global_event_num(),r,arm) ;

			// digest the ARM header, word 0
			int arm_id = *dta & 0x7 ;
			int arm_seq = (*dta >> 20) & 0xfff;
			int arm_err = (*dta >>16) & 0xf;
			dta++ ;
			// word 1
			u_int apv_mask = *dta & 0x00FFFFFF ;
			dta++ ;
			// word 2, would get monitor register values from this word (not implemented yet, still need to skip over of course)
			dta++ ;
			
			apv_meta.arc[r].arm[arm].present = 1 ;
			apv_meta.arc[r].arm[arm].arm_id = arm_id ;
			apv_meta.arc[r].arm[arm].arm_seq = arm_seq ;
			apv_meta.arc[r].arm[arm].arm_err = arm_err ;
			apv_meta.arc[r].arm[arm].apv_mask = apv_mask ;
			
			LOG(NOTE,"[evt %d]: ARM_ID %d SEQ %d ERR %1x APV_MASK %06x",get_global_event_num(),
			    arm_id,arm_seq,arm_err,apv_mask);

			if(arm_id != arm) {
				apv_meta.arc[r].arm[arm].error = 1 ;
				LOG(ERR,"[evt %d]: RDO %d ARM %d: Bad ARM ID is %d",get_global_event_num(),r,arm,arm_id) ;
				goto unrecoverable_error ;
			}

			if(arm_err != 0) {
				apv_meta.arc[r].arm[arm].error = 1 ;
				LOG(ERR,"[evt %d]: RDO %d ARM %d: Error code 0x%x",get_global_event_num(),r,arm,arm_err) ;
				continue ;  // I think we can leave this as a 'recoverable' error for now ??
			}


			for(int apv=0;apv<FGT_APV_COU;apv++) {
				if(apv_mask & (1<<apv)) ;
				else continue ;

				LOG(NOTE,"[evt %d]: Handling APV %d",get_global_event_num(),apv) ;

				int apv_id = *dta & 0x1F ;
				int fmt = (*dta >> 16) & 0xf ;       // promoted to 4 bits on 1/26/2012 (take over reserved=0 bit)
				int length;
				if ((fmt==1)||(fmt==2))              // old formats used in run 12 FGT, retired 11/2012
				  length = (*dta >> 5) & 0x3FF ;
				else
				  length = (*dta >> 5) & 0x7ff ;

				int seq = (*dta >> 20) & 0xfff;

				dta++ ;

				LOG(NOTE,"  ID %d, length %d, fmt %d",apv_id,length,fmt) ;

				int capid = *dta & 0xFF ;
				int nhits = (*dta >> 8) & 0x7F ;
				int is_error = (*dta >> 15) & 1 ;

				int refadc,ntim,is_0;
				if ((fmt==1)||(fmt==2)) {             // old formats used in run 12 FGT, retired 11/2012
				  refadc = (*dta >> 16) & 0xFFF ;
				  ntim = (*dta >> 28) & 0x7 ;
				  is_0 = *dta & 0x80000000 ;
				}
				else {
				  refadc = (*dta >> 16) & 0x7FF ;
				  ntim = (*dta >> 27) & 0x1f ;
				  is_0 = 0 ;
				}


				dta++ ;

				nhits = ( ntim>0 ? nhits+1 : 0 );    // nhits (range 0-128) is encoded like this in 7-bit nhits field and ntim field

				LOG(NOTE,"  capid %d, nhits %d, is_error 0x%X, refadc %d, ntim %d, is_0 0x%X",
				    capid, nhits, is_error, refadc, ntim, is_0) ;

				apv_meta.arc[r].arm[arm].apv[apv].present = 1 ;

				apv_meta.arc[r].arm[arm].apv[apv].apv_id = apv_id ;
				apv_meta.arc[r].arm[arm].apv[apv].fmt = fmt ;
				apv_meta.arc[r].arm[arm].apv[apv].length = length ;
				apv_meta.arc[r].arm[arm].apv[apv].seq = seq ;
				apv_meta.arc[r].arm[arm].apv[apv].capid = capid ;
				apv_meta.arc[r].arm[arm].apv[apv].nhits = nhits ;
				apv_meta.arc[r].arm[arm].apv[apv].is_error = is_error ;
				apv_meta.arc[r].arm[arm].apv[apv].refadc = refadc ;
				apv_meta.arc[r].arm[arm].apv[apv].ntim = ntim ;
				
				// sanity checks
				if(apv != apv_id) {
					apv_meta.arc[r].arm[arm].apv[apv].error = 1 ;
					LOG(ERR,"[evt %d]: RDO %d ARM %d APV %d: Bad APV ID, got %d",get_global_event_num(),r,arm,apv,apv_id) ;
					goto unrecoverable_error ;
				}

				if(seq != arm_seq) {
					// In old test stand data (before ARM firmware r67) we have wrong sequence numbers
#if 0
					LOG(WARN,"[evt %d]: RDO %d ARM %d APV %d: Sequence number mismatch, expect %d have %d",get_global_event_num(),r,arm,apv,arm_seq,seq);
#else
					apv_meta.arc[r].arm[arm].apv[apv].error = 1 ;
					LOG(ERR,"[evt %d]: RDO %d ARM %d APV %d: Sequence number mismatch, expect %d have %d",get_global_event_num(),r,arm,apv,arm_seq,seq);
					goto unrecoverable_error ;
#endif
				}

				if((ntim < 0) || (ntim > 31)) {  // 0 is a valid value (used to encode NHITS=0)
					apv_meta.arc[r].arm[arm].apv[apv].error = 1 ;
					LOG(ERR,"Ntim %d ?!",ntim) ;
					continue ;
				}
				if((fmt != 1)&&(fmt != 2)&&(fmt != 3)) {
					apv_meta.arc[r].arm[arm].apv[apv].error = 1 ;
					LOG(ERR,"Invalid FMT %d (evt %d)",fmt,get_global_event_num()) ;
					continue ;
				}


				// set of Gerard's hadrcoded hacks...
				if (fmt==1) {  // applies ONLY in fmt 1, which is retired 7/23/2012 [was used for all run 12 operations]
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
				}
				// END of the hacks to deal with skipped APV's

				fgt_adc_t *fgt_d = (fgt_adc_t *) adc->request(FGT_TB_COU*FGT_CH_COU) ;
				int cou = 0 ;

				// extract data here...

				u_short *d16 = (u_short *) dta ;

				u_short wfm[((2047-2)/3)*8] ;       // worst case wfm length (from header max length value 2047)
				int i = 0 ;
				
				// it is important to note, this is specifically for the non-ZS formats, and the length will always be of form 2+3*n
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

				if ((ntim>0) && ( 27+127+(ntim-1)*140 >= ((length-2)/3)*8 )) {
				apv_meta.arc[r].arm[arm].apv[apv].error = 1 ;
				  LOG(ERR,"[evt %d]: RDO %d ARM %d APV %d: Trouble in APV block content, it's shorter than required to unpack %d timebins",
				      get_global_event_num(),r,arm,apv,ntim);
				  continue;  // this is a recoverable error
				}
				for(int ch=0;ch<128;ch++) {
					int rch = 32*(ch%4) + 8*(ch/4) - 31*(ch/16) ;     // see APV user guide (channel mux sequence)
					for(int tb=0;tb<ntim;tb++) {      // note that ntim=0 in case of skipped APV in format 1 or 2 (non-ZS)

						int adc = wfm[27+ch+tb*140] ;
						fgt_d[cou].ch = rch ;
						fgt_d[cou].tb = tb ;
						fgt_d[cou].adc = adc ;
						cou++ ;
					}
				}

				// note the reversal: sector->arm, row->rdo,
				if (nhits>0)
				  adc->finalize(cou, arm, r, apv) ;

			}

		}
		continue;
unrecoverable_error:
		apv_meta.arc[r].error = 1 ;
		LOG(WARN,"[evt %d]: RDO %d: Cannot reliably recover pointer to next item, dropping the rest of this event on this rdo",
		    get_global_event_num(),r);
	}

	adc->rewind() ;

	if(found_some) {
		adc->meta = (void *) &apv_meta ;
	}
	else {
		return 0 ;
	}



	return adc ;
			
}

daq_dta *daq_fgt::handle_ped(int sec, int rdo)
{

	ped->create(1000,"fgt_pedrms",rts_id,DAQ_DTA_STRUCT(fgt_pedrms_t)) ;


	char str[128] ;
	char *full_name ;
	int bytes ;
	u_short *d ;

	sprintf(str,"%s/sec%02d/pedrms",sfs_name, 1) ;
	full_name = caller->get_sfs_name(str) ;
		
	if(full_name) {
		LOG(TERR,"full_name %s",full_name) ;
	}

	if(!full_name) return 0  ;
	bytes = caller->sfs->fileSize(full_name) ;	// this is bytes

	LOG(TERR,"bytes %d",bytes) ;

	d = (u_short *) malloc(bytes) ;
			
	int ret = caller->sfs->read(str, (char *)d, bytes) ;
	if(ret != bytes) {
		LOG(ERR,"ret is %d") ;
	}


	if(d[0] != 0xBEEF) {
		LOG(ERR,"Bad pedestal version") ;
	}

	if(d[1] != 1 ) {
		LOG(ERR,"Bad pedestal version") ;
	}

//	int arm_cou = d[2] ;
//	int apv_cou = d[3] ;
	int ch_cou = d[4] ;
	int tb_cou = d[5] ;

	int ix = 6 ;
	int max_ix = (bytes/2) ;

	fgt_pedrms_t *f_ped = 0 ;

	while(ix < max_ix) {
		int arc = d[ix++] ;
		int apvs = d[ix++] ;

		while(apvs) {
			int arm = d[ix++] ;
			int apv = d[ix++] ;

			f_ped = (fgt_pedrms_t *) ped->request(ch_cou * tb_cou) ;

			int cou = 0 ;
			for(int ch=0;ch<ch_cou;ch++) {
				for(int t=0;t<tb_cou;t++) {
					u_short ped = d[ix++] ;
					u_short rms = d[ix++] ;

					f_ped[cou].ch = ch ;
					f_ped[cou].tb = t ;
					f_ped[cou].ped = ((float)ped) / 16.0 ;
					f_ped[cou].rms = ((float)rms) / 16.0 ;
					cou++ ;
				}
			}

			ped->finalize(cou,arm,arc,apv) ;
			apvs-- ;
		}

	}

	free(d) ;

	ped->rewind() ;


	return ped ;
			


}




// used to grab trigger info from the event header
int daq_fgt::get_l2(char *buff, int words, struct daq_trg_word *trg, int rdo)
{
	const int FGT_BYTES_MIN = ((6)*4) ;	// this is the minimum
	const int FGT_BYTES_MAX = (512*1024) ;
//	const u_int FGT_VERSION = 0x0034 ;		 
	const u_int FGT_SIGNATURE = 0x46475420 ;	// "FGT"

	int t_cou = 0 ;
	int bad = 0 ;
	u_int *d32 = (u_int *)buff ;
	int id_check_failed = 0 ;
	int last_ix = words - 1 ;

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
		LOG(ERR,"RDO %d: bad FEEDBEEF 0x%08X",rdo,d32[5]) ;
		bad |= 1 ;
	}


	/* wait for it to stabilize 
	if((d32[2] >> 16) != FGT_VERSION) {
		LOG(ERR,"RDO %d: bad version 0x%04X, expect 0x%04X",rdo,d32[2] >> 16, FGT_VERSION) ;
		bad |= 2 ;	// soft error
	
	}
	*/

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

	
	LOG(DBG,"RDO %d: expect %d, in data %d",rdo,rdo_id[rdo],rdo_in_dta) ;


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
			if(rdo_warns[rdo] < 15) {
				LOG(CAUTION,"RDO %d: rdo check failed: expect 0x%02X, found 0x%02X",
				    rdo,rdo_id[rdo],rdo_in_dta) ;
			}			
		}

		return t_cou ;
	}
}


