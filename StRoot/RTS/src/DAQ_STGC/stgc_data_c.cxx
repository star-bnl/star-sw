#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <stdlib.h>


#include <rtsLog.h>

#include "daq_stgc.h"
#include "stgc_data_c.h"

stgc_data_c::feb_t stgc_data_c::feb[STGC_SECTOR_COU][4][6] ;
stgc_data_c::errs_t stgc_data_c::errs[4] ;

u_int stgc_data_c::run_type ;
u_int stgc_data_c::run_number ;

static u_int gray2dec(u_int gray)
{
	u_int dec ;

	dec = gray ;

	for(int i=0;i<11;i++) {
		gray >>= 1 ;
		dec ^= gray ;
	}

	return dec ;

}

stgc_data_c::stgc_data_c()
{
	id = 0 ;

	realtime = 0 ;
	run_type = 3 ;
	run_number = 123 ;

	version = 0 ;

	bad_error = 0 ;
	want_saved = 0 ;

	sector1 = 1 ;
	rdo1 = 1 ;

	xing_min = -65000 ;
	xing_max = 65000 ;

	event_any = event_data = 0 ;

	return ;
}

int stgc_data_c::run_start()
{
	bad_error = 0 ;
	event_any = 0 ;
	event_data = 0 ;

	return 0 ;
} ;

const char *stgc_data_c::type_c(u_short type)
{

	switch(type) {
	case 0x5244:
		return "RESPONSE" ;
	case 0x414B :
		return "ECHO" ;
	case 0x5445 :
		return "TIMER" ;
	case 0x4544 :
		return "EVENT" ;
	case 0x1F00 :
		return "PROM" ;
	default :
		return "UNKNOWN" ;
	}
}

int stgc_data_c::hdr_check(u_short *d, int shorts)
{
	u_short st[6] ;
	const char *c_type ;


	d16_start = d ;	// points to start-comma

	d += 8 ;	// skip TEF header
	shorts -= 8 ;

	trg_cou = 0 ;
	evt_type = 0 ;
	token = 4096 ;
	trg_cmd = daq_cmd = 0 ;
	echo = 0 ;
	adc_cou = 0 ;
	version = 0 ;
	datum_ix = 0 ;
	fee_status = 0 ;

	if(d[1] != 0xCCCC) {
		bad_error |= 1 ;
		LOG(ERR,"%d: evt %d: corrupt header 0x%04X, shorts %d",rdo1,event_any,d[1],shorts) ;
		return bad_error ;
	}

	version = d[2] ;
//	if(version != 0x8) {
//		LOG(ERR,"%d: VERSION 0x%04X",rdo1,d[2]) ;
//	}

	evt_type = d[8] ;
	c_type = type_c(evt_type) ;

	trg_cmd = d[3]&0xF ;
	daq_cmd = d[4]&0xF ;

	int t_hi = (d[4]>>4)&0xF ;
	int t_mid = (d[4]>>8)&0xF ;
	int t_lo = (d[4]>>12)&0xF ;

	token = (t_hi<<8)|(t_mid<<4)|t_lo ;

//	if(realtime && (token==0)) {
//		LOG(ERR,"%d: evt %d: token0: 0x%X 0x%X 0x%X 0x%X 0x%X",rdo1,event_any,
//			d[0],d[1],d[2],d[3],d[4]) ;
//	}
	
	trg_counter = d[5]>>6 ;

	// FEE status
	st[5] = d[9]>>8 ;
	st[4] = d[9]&0xFF ;
	st[3] = d[10]>>8 ;
	st[2] = d[10]&0xFF ;
	st[1] = d[11]>>8 ;
	st[0] = d[11]&0xFF ;

	mhz_trg_marker = 0 ;
	mhz_start_evt_marker = (((unsigned long)d[5]&0x3F)<<32)|((unsigned long)d[6]<<16)|(unsigned long)d[7] ;

	echo = d[12] ;

	response = 0 ;

	d16_last = d + shorts - 2 ;	// should be at the last 0xFEED


	for(int i=0;i<shorts;i++) {
//		LOG(TERR,"+++ %d/%d = 0x%04X",i,shorts,d16_last[-i]) ;
		if(d16_last[-i] != 0xFEED) {
			d16_last = d + shorts - 2 - i ;
			break ;
		}

	}

	// d16_last[0] should be 0xFEEC
	if(d16_last[0] != 0xFEEC) {
		if(realtime) LOG(ERR,"%d: evt %d: last is 0x%04X",rdo1,event_any,d16_last[0]) ;
	}

//	LOG(TERR,"d_last is 0x%04X",*d16_last) ;	// at the datum just before the first 0xFEED

	mhz_stop_evt_marker = (d16_last[-2]<<16)|d16_last[-1] ;

//	if(realtime>90) {
//		printf("before 0x%04X, last 0x%04X: stop 0x%08X (%u)\n",d16_last[-2],d16_last[-1],mhz_stop_evt_marker,
//		       mhz_stop_evt_marker) ;
//	}

	d16_data = d + 13 ;	// first datum is at d[13]

 //	adc_cou = d16_last - d16_data + 1 ;	// effective ADC length
	adc_cou = d16_last - d16_data  ;	// effective ADC length
	adc_cou -= 2 ;	// remove the stop_mhz 2 shorts...

	// MOVE this only for EVENTS	
	if((evt_type==0x4544 && (adc_cou%4)) || shorts>40000) {
		if(realtime) LOG(ERR,"%d: evt %d: %s: adc_cou is %d (%d), shorts %d!?",rdo1,event_any,c_type,adc_cou,adc_cou/4,shorts) ;
		if(realtime>100) {
			for(int i=0;i<16;i++) {
				LOG(TERR,"%d/%d = 0x%04X",i,shorts,d[i]) ;
				LOG(TERR,"last %d = 0x%04X",i,d16_last[-i]) ;
			}
		}	
	}

	adc_cou /= 4 ;	// number of ADCs....

	// echo and timer have len 0
	// response has adc_cou 4 
	// physics has 
//	LOG(TERR,"%d: type 0x%04X: d_last is 0x%04X; effective length %d",rdo1,evt_type,*d16_last,adc_cou) ;	// at the datum just before the first 0xFEED
//	for(int i=0;i<32;i++) {
//		LOG(TERR,"... %d/%d = 0x%04X",i,shorts,d[i]) ;
//	}

	

	switch(evt_type) {
	case 0x5244 :	//response
		{
		int sh = 3 ;
		for(int i=0;i<4;i++) {
			response |= (unsigned long)d[13+i]<<(sh*16) ;
			sh-- ;
		}
		sh = 0 ;	// reuse
		u_int feb = (response>>56)&0xF ;
		if((feb<1)||(feb>6)) sh |= 1 ;
		if(((response>>48)&0xF0F0)!=0x1000) {
			sh |= 2 ;
		}

		if(echo==0 || echo==0x00C0) {	// VMM config
//			if((response & 0xFFFFFFFFFFFFFl) != 0x1150000000000l) sh |= 4;
		}
		else {
			if((response & 0xFFFFFFFFFFFFFl) != 0x2C00000000000l) sh |= 8;
		}

		if(sh) {
			LOG(ERR,"S%d:%d: RESPONSE of cmd 0x%04X = 0x%lX: sh 0x%X",sector1,rdo1,echo,response,sh) ;
		}
		else {
			if(realtime>1) LOG(TERR,"S%d:%d: RESPONSE of cmd 0x%04X",sector1,rdo1,echo) ;
		}
		}

		token = 4096 ;
		trg_cmd = daq_cmd = 0 ;
		break ;
	case 0x414B :	//echo 
		if(realtime>1) LOG(TERR,"S%d:%d: ECHO of cmd 0x%04X",sector1,rdo1,echo) ;

		token = 4096 ;
		trg_cmd = daq_cmd = 0 ;
		
		break ;
	case 0x4544 :	// event
		LOG(NOTE,"S%d:%d: %d: VERSION 0x%04X: T %d, trg %d, daq %d; shorts %d, ADCs %d; start_mhz %ul, delta %d",sector1,rdo1,id,version,token,trg_cmd,daq_cmd,
			shorts,adc_cou,
		    	mhz_start_evt_marker,mhz_stop_evt_marker-mhz_start_evt_marker) ;
		LOG(NOTE,"%d: evts %d: T %d: trg_counter %d",rdo1,event_any,token,trg_counter) ;

		break ;
	case 0x5445 :	// timer
		if(realtime>1) LOG(TERR,"S%d:%d: %d: T %d, trg %d, daq %d; shorts %d (timer evt)",sector1,rdo1,id,token,trg_cmd,daq_cmd,shorts) ;

		token = 4096 ;
		trg_cmd = daq_cmd = 0 ;

		break ;	
	case 0x1F00 :	// PROM response
		token = 4096 ;
		trg_cmd = daq_cmd = 0 ;
		break ;
	default :
		LOG(ERR,"S%d:%d: evt %d: T %d, trg %d, daq %d, UNKNOWN type 0x%04X; shorts %d",sector1,rdo1,event_any,token,trg_cmd,daq_cmd,evt_type,shorts) ;
		bad_error |= 2 ;

		token = 4096 ;
		trg_cmd = daq_cmd = 0 ;

	}


	if(evt_type != 0x5244) {	// not a RESPONSE
		if((d[3] & (1<<14))==0) {
			LOG(ERR,"%d: %s: RESPONSE FIFO",rdo1,c_type) ;
		}
	}
	
	status = d[3] ;
	
	if((d[3] & (1<<13))==0) {
		LOG(ERR,"%d: %s: GTP not ready",rdo1,c_type) ;
	}
	if((d[3] & (1<<12))==0) {
		switch(evt_type) {
		case 0x414B :	// echo
		//case 0x5244 :	// response
			if(echo != 0xF010) LOG(ERR,"%d: %s of echo 0x%04X: RHICx5 PLL not locked",rdo1,c_type,echo) ;
			break ;
		}
	}
	if((d[3] & (1<<8))) {
		if(realtime && errs[rdo1-1].fifo==0) {
			LOG(ERR,"%d: %s: EVENT FIFO FULL latched",rdo1,c_type) ;
		}
		errs[rdo1-1].fifo = 1 ;
	}
	if((d[3] & (1<<4))==0) {
		LOG(ERR,"%d: %s: FEB Config FIFO",rdo1,c_type) ;
	}


	for(int i=0;i<6;i++) {
		fee_status |= (((unsigned long)st[i]&0xFF)<<(i*8)) ;

		//LOG(TERR,"%d: evt %d: FEB %d: 0x%X",rdo1,event_any,i,st[i]) ;

		switch(st[i]) {
		case 3 :	//present & OK
		case 0 :	//not present
			break ;
		default :
			if(realtime>10) LOG(ERR,"%d: evt %d: %s: FEB %d status: 0x%X",rdo1,event_any,c_type,i,st[i]) ;
			break ;
		}

		//LOG(TERR,"%d: FEE %d: 0x%X (0x%X)",rdo1,i,st[i],fee_status) ;
	}


	return bad_error ;
}

// unused...
int stgc_data_c::start_0001(u_short *d, int shorts)
{
	return 0 ;

}

int stgc_data_c::event_0001()
{
	int evt_err = 0 ;
	u_int feb_id = 255 ;

//	LOG(TERR,"ADC cou %d, feb %d",adc_cou,feb_id) ;

//	if(bad_error) return 0 ;
	if(adc_cou<=0) return 0 ;

	u_short d[4] ;

	for(int i=0;i<4;i++) {
		d[i] = *d16_data++ ;
	}

	u_int dd = ((u_int)d[0]<<16)|(u_int)d[1] ;

	feb_id = dd>>29 ;

	datum_ix++ ;



	if(feb_id==7) {	// trigger	
		u_short t_hi, t_mid, t_lo ;
		u_short t ;
		u_short mhz_hi ;
		u_char t_cmd, d_cmd ;
		u_char err = 0 ;
		unsigned long mhz_trg ;

		t_cmd = d[1] & 0xF ;
		d_cmd = (d[1]>>4) & 0xF ;
		t_hi = (d[1]>>8)& 0xF ;
 		t_mid = (d[1]>>12)&0xF ;
		t_lo = (d[0]>>0) & 0xF ;

		t = (t_hi<<8)|(t_mid<<4)|t_lo ;

		mhz_hi = (d[0]>>4)&0x1FF ;
		mhz_trg = ((unsigned long)mhz_hi<<32)|((unsigned long)d[2]<<16)|(unsigned long)d[3] ;	// or RHIC clock

//		if((mhz_trg_marker+1) < mhz_start_evt_marker) err = 1 ;
//		if(mhz_trg_marker > (mhz_start_evt_marker+1)) err |= 1 ;

//		if((t != token)||(t_cmd != trg_cmd)||(d_cmd != daq_cmd)) err |= 2 ;
			
		if(trg_cou) err |= 4 ;	// more than 1 trigger


		if(t==0) err |= 8 ;
		if(t_cmd==0) err |= 8 ;
		
		
		if(err) {	// typically corrupt data -- should STOP!
			if(trg_cou==1 && datum_ix==2 && realtime<=1) {
				err = 0 ;
			}
			else {
				bad_error |= 0x10 ;
			}



			if(realtime && err) {
				LOG(ERR,"%d: evt %d: err 0x%X: 0x%04X 0x%04X 0x%04X 0x%04X at adc_cou %d/%d",rdo1,event_any,err,d[0],d[1],d[2],d[3],datum_ix,adc_cou) ;
				LOG(ERR,"%d: evt %d: data: trg_cou %d: T %d, trg %d, daq %d; trg_mhz %lu",rdo1,event_any,trg_cou,t,
					t_cmd,d_cmd,mhz_trg_marker) ;
				LOG(ERR,"%d: evt %d: hdr : trg_counter %d: T %d, trg %d, daq %d; evt_mhz %lu",rdo1,event_any,trg_counter,token,
					trg_cmd,daq_cmd,mhz_start_evt_marker) ;
			}
			
		}
		else {
			//LOG(INFO,"%d: trg_cou %d: T %d, trg %d, daq %d; mhz_counter %u",rdo1,trg_counter,t,t_cmd,d_cmd,mhz_trg_marker) ;
		}

		if(trg_cou==0) {	// only use the first!
			token = t ;	
			trg_cmd = t_cmd ;
			daq_cmd = d_cmd ;

			mhz_trg_marker = mhz_trg ;
		}

		// to indicate no data!
		vmm.feb_vmm = 0 ;
		vmm.ch = 0 ;
		vmm.adc = 0 ;
		vmm.bcid = 0 ;
		vmm.tb = 0 ;
		vmm.bcid_delta = 0 ;

		adc_cou-- ;
		trg_cou++ ;

//		if(err) return 0 ;	// HALT on error
		return 1 ;
	}

	//datum_ix==1 for Trigger
	// ==2 for the first occurence of ADC data which is often corrupt -- skip it!
	if(datum_ix==2) {
		vmm.feb_vmm = 0 ;
		vmm.ch = 0 ;
		vmm.adc = 0 ;
		vmm.bcid = 0 ;
		vmm.tb = 0 ;
		vmm.bcid_delta = 0 ;

		adc_cou-- ;
		return 1 ;
	}

	
	if(feb_id>5) {
		evt_err |= 0x20 ;	// hard error; corrupt data
	}


	u_int mhz_adc_marker = dd & 0x1FFFFFFF ;	// 29 bits of trigger

	u_int vmm_id = (d[2]>>13)&0x7 ;
	int crc_ok = (d[2]>>12)&1 ;
	int channel = (d[2]>>6)&0x3F ;

	int pdo = (d[2]&0x3F)<<4 ;
	pdo |= (d[3]>>12)&0xF ;

	int bcid = d[3]&0xFFF ;
	bcid = gray2dec(bcid) ;

	if(crc_ok==0) evt_err |= 0x80 ;	// soft error
	
	if(vmm_id<4) evt_err |= 0x40 ;	// hard error; corrupt data


	if(evt_err) {
		if(evt_err==0x20 && datum_ix==2 && realtime<=1) {
			evt_err = 0 ;
		}


		char c_err[128] ;
		c_err[0] =  0 ;

		if(evt_err&0x20) strcat(c_err,"FEBerr ") ;
		if(evt_err&0x40) strcat(c_err,"VMMerr ") ;
		if(evt_err&0x80) strcat(c_err,"CRCerr ") ;

		
		vmm.feb_vmm = 0 ;
		vmm.ch = 0 ;
		vmm.adc = 0 ;
		vmm.bcid = 0 ;
		vmm.tb = 0 ;
		vmm.bcid_delta = 0 ;

		bad_error |= evt_err ;

		if(realtime>1 && evt_err) {
			LOG(ERR,"S%d:%d: evt %d: FEB %d, VMM %d, ch %d: evt_err [%s]0x%X at adc_cou %d/%d, trg_cou %d",sector1,rdo1,event_any,feb_id,
			vmm_id,channel,c_err,evt_err,datum_ix,adc_cou,trg_cou) ;	

			if(realtime > 2) {
				LOG(ERR,"  0x%04X 0x%04X 0x%04X 0x%04X",d[0],d[1],d[2],d[3]) ;
			}
		}

		adc_cou-- ;
		return 0 ;	// stop at the first occurence
//		return 1 ;
	}
	else {
//		vmm.feb_vmm = ((feb_id-1)<<2)|(vmm_id-4) ;
		vmm.feb_vmm = ((feb_id)<<2)|(vmm_id-4) ;
		vmm.ch = channel ;
		vmm.adc = pdo ;
		vmm.bcid = bcid ;

		int delta = bcid - (mhz_trg_marker%4096) ;

		if(delta<0) delta += 4096 ;

		vmm.bcid_delta = delta ;
	}


//	LOG(TERR,"feb_id %d, vmm_id %d, 0x%X",feb_id,vmm_id,vmm.feb_vmm) ;

	long tb = (long)mhz_adc_marker - (long)(mhz_trg_marker&0x1FFFFFFF) ;

	// since vmm.tb is only 16 bits
	if(tb<-0x8000) vmm.tb = 0x8000 ;
	else if(tb>0x7FFF) vmm.tb = 0x7FFF ;
	else vmm.tb = tb ;

//	LOG(ERR,"Hack %d",tb) ;

	if(vmm.tb<xing_min || vmm.tb>xing_max) {
		vmm.feb_vmm = 0 ;
		vmm.ch = 0 ;
		vmm.adc = 0 ;
		vmm.bcid = 0 ;

		// but leave tb as-is
	}


	adc_cou-- ;
	return 1 ;
}

// unpacks and sanity-checks 1 RDO event
int stgc_data_c::start(u_short *d, int shorts)
{
	bad_error = 0 ;
	want_saved = 0 ;

	event_any++ ;

	if(hdr_check(d,shorts)) return -1 ;

	return 0 ;	// stop checking the version

	switch(version) {
	case 0x0001 :
	case 0x0002 :
	case 0x0003 :
	case 0x0004 :
	case 0x0005 :
	case 0x0006 :
	case 0x0007 :
	case 0x0008 :
	case 0x0009 :
	case 0x000A :
		return start_0001(d,shorts) ;
	default :
		break ;
	}

	// OLD CODE BELOW
	LOG(ERR,"OLD CODE") ;
	return -1 ;

	u_short *d16_last = d + shorts - 2 ;	// should be at the last 0xFEED
	
	for(int i=0;i<shorts;i++) {
//		LOG(TERR,"last %d: 0x%04X",i,d16_last[-i]) ;

		if(d16_last[-i] != 0xFEED) {
			d16_last = d + shorts - 2 - i ;
			break ;
		}
		

	}

	d += 8 ;	// at event type

	event_type = d[0] ;

//	LOG(TERR,"last final 0x%04X",d16_last[0]) ;

	//d16_last[0] now points at the command e.g. 0x4544 ;
	int len = d16_last - d ;

	LOG(NOTE,"%d: type 0x%04X: token %d, trg_cmd %d, daq_cmd %d - len %d",rdo1,d[0],token,trg_cmd,daq_cmd,len) ;

	d++ ;	// advance to start-of-data, just after event type

//	for(int i=0;i<len;i++) {
//		LOG(TERR,"%d/%d = 0x%04X",i,len,d[i]) ;
//	}

	d16_data = d ;

	event_any++ ;

	switch(event_type) {
	case 0x4544:		// physics
		event_data++ ;
		LOG(NOTE,"%d: data %d: chs %d: T %d, trg %d, daq %d",rdo1,event_data,len/4,token,trg_cmd,daq_cmd) ;
		if(len%4) {
			LOG(ERR,"%d: data len odd %d",rdo1,len) ;
		}
		break ;
	case 0x5445 :		// timer
		LOG(DBG,"%d: timer",rdo1) ;

		if((d16_data[0]&0xFF00)==0x0300) feb[sector1-1][rdo1-1][5].present = 1 ;
		if((d16_data[0]&0xFF)==0x03) feb[sector1-1][rdo1-1][4].present = 1 ;
		if((d16_data[1]&0xFF00)==0x0300) feb[sector1-1][rdo1-1][3].present = 1 ;
		if((d16_data[1]&0xFF)==0x03) feb[sector1-1][rdo1-1][2].present = 1 ;
		if((d16_data[2]&0xFF00)==0x0300) feb[sector1-1][rdo1-1][1].present = 1 ;
		if((d16_data[2]&0xFF)==0x03) feb[sector1-1][rdo1-1][0].present = 1 ; 
		break ;
	case 0x414B :		// ROD response/echo
		LOG(DBG,"%d: echo",rdo1) ;
		break ;
	case 0x5244 :		// FEB response to VMM config
		LOG(DBG,"%d: FEB response",rdo1) ;
		break ;
	default :
		LOG(ERR,"%d: unknown event type 0x%04X",rdo1,event_type) ;
		break ;
	}


	ch_count = len/4 ;

	LOG(NOTE,"ch_count %d",ch_count) ;

	if(len<=0) return 0 ;	// nothing there...

	
	return 1 ;
}


int stgc_data_c::event()
{
	u_short d[4] ;
	int rod_id ;
	int feb_id ;
	int vmm_id ;
	int threshold ;
	int channel ;
	int pdo ;
	int bcid ;
	int trigger_id ;
	int err = 0 ;

	if(version!=0) return event_0001() ;

	
	switch(version) {
	case 0x0001 :
	case 0x0002 :
	case 0x0003 :
	case 0x0004 :
	case 0x0005 :
	case 0x0006 :
	case 0x0007 :
	case 0x0008 :
	case 0x0009 :
	case 0x000A :
		return event_0001() ;
	default:
		break ;
	}

	if(ch_count <= 0) return 0 ;
	if(event_type != 0x4544) return 0 ;

	for(int i=0;i<4;i++) {
		d[i] = *d16_data++ ;		
	}

	// check
	if((d[0]&0xF000) != 0x1000) err |= 1 ;

	rod_id = (d[0]>>4)&0xFF ;
	feb_id = d[0] & 0xF ;

	// always 1
	if(rod_id != 1) err |= 2 ;

	// feb_id is 1..6
	if(feb_id==0 || feb_id>6) err |= 2 ;
		
	trigger_id = d[1] ;

	vmm_id = (d[2] >> 13) & 0x7 ;

	threshold = (d[2]>>12) & 1 ;	// 1 bit

	channel = (d[2]>>6) & 0x3F ;	// 6 bits

	pdo = (d[2] & 0x3F)<<4 ;	// 10 bits: upper 6
	pdo |= (d[3]>>12)&0xF ;		// lower 4

	bcid = d[3]&0xFFF ;		// 12 bits
	bcid = gray2dec(bcid) ;

		
	if(vmm_id<4) err |= 4 ;


	if(err) {
		vmm.feb_vmm = 0 ;
		vmm.ch = 0 ;
		vmm.adc = 0 ;
		vmm.bcid = 0 ;

		LOG(ERR,"event %d: pkt %3d: rod_id %d, feb_id %d, vmm_id %d, ch %d, trigger %d, threshold %d, pdo %d, bcid %d: error %d",event_data,
		       ch_count,rod_id,feb_id,vmm_id,channel,trigger_id,
		       threshold,pdo,bcid,err) ;

	}
	else {

		vmm.feb_vmm = ((feb_id-1)<<2)|(vmm_id-4) ;
		vmm.ch = channel ;
		vmm.adc = pdo ;
		vmm.bcid = bcid ;

	}


	ch_count-- ;
	return 1 ;
}


int stgc_data_c::event_end(int flag)
{
	return 0 ;
}

