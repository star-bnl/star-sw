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

	xing_min = -10 ;
	xing_max = 20 ;

	return ;
}

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
	default :
		return "UNKNOWN" ;
	}
}

int stgc_data_c::hdr_check(u_short *d, int shorts)
{
	u_short st[6] ;
	const char *c_type ;


//	for(int i=0;i<32;i++) {
//		LOG(TERR,"... %d/%d = 0x%04X",i,shorts,d[i]) ;
//	}


	d16_start = d ;	// points to start-comma

	d += 8 ;	// skip TEF header
	shorts -= 8 ;

	u_int evt_err = 0 ;

	if(d[1] != 0xCCCC) {
		evt_err |= 1 ;
	}

	version = d[2] ;
	if(version != 0x6) {
		LOG(ERR,"VERSION 0x%04X",d[2]) ;
	}

	evt_type = d[8] ;
	c_type = type_c(evt_type) ;

	trg_cmd = d[3]&0xF ;
	daq_cmd = d[4]&0xF ;

	int t_hi = (d[4]>>4)&0xF ;
	int t_mid = (d[4]>>8)&0xF ;
	int t_lo = (d[4]>>12)&0xF ;

	token = (t_hi<<8)|(t_mid<<4)|t_lo ;


	st[5] = d[9]>>8 ;
	st[4] = d[9]&0xF ;
	st[3] = d[10]>>8 ;
	st[2] = d[10]&0xF ;
	st[1] = d[11]>>8 ;
	st[0] = d[11]&0xF ;


	mhz_start_evt_marker = (d[6]<<16)|d[7] ;

	u_short last_cmd = d[12] ;


	unsigned long response = 0 ;

	d16_last = d + shorts - 2 ;	// should be at the last 0xFEED


	for(int i=0;i<shorts;i++) {
//		LOG(TERR,"+++ %d/%d = 0x%04X",i,shorts,d16_last[-i]) ;
		if(d16_last[-i] != 0xFEED) {
			d16_last = d + shorts - 2 - i ;
			break ;
		}

	}

//	LOG(TERR,"d_last is 0x%04X",*d16_last) ;	// at the datum just before the first 0xFEED

	mhz_stop_evt_marker = (d16_last[-1]<<16)|d16_last[0] ;


	d16_data = d + 13 ;	// first datum is at d[13]

 	adc_cou = d16_last - d16_data + 1 ;	// effective ADC length
	adc_cou -= 2 ;	// remove the stop_mhz 2 shorts...
	
	if(realtime) {
	if(adc_cou%4 || shorts>40000) {
		LOG(ERR,"%d: len is %d!?",rdo1,adc_cou) ;
		for(int i=0;i<32;i++) {
			LOG(TERR,"%d/%d = 0x%04X",i,shorts,d[i]) ;
			LOG(TERR,"last %d = 0x%04X",i,d16_last[-i]) ;
		}
	
	}
	}

	adc_cou /= 4 ;	// number of ADCs....

	// echo and timer have len 0
	// response has adc_cou 4 
	// physics has 
//	LOG(TERR,"d_last is 0x%04X; effective length %d",*d16_last,adc_cou) ;	// at the datum just before the first 0xFEED

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

		if(last_cmd==0) {	// VMM config
			if((response & 0xFFFFFFFFFFFFFl) != 0x1150000000000l) sh |= 4;
		}
		else {
			if((response & 0xFFFFFFFFFFFFFl) != 0x2C00000000000l) sh |= 8;
		}

		if(sh) {
			LOG(ERR,"S%d:%d: RESPONSE of cmd 0x%04X; sh 0x%X",sector1,rdo1,last_cmd,sh) ;
		}
		else {
			LOG(INFO,"S%d:%d: RESPONSE of cmd 0x%04X",sector1,rdo1,last_cmd) ;
		}
		}

		break ;
	case 0x414B :	//echo 
		LOG(INFO,"S%d:%d: ECHO of cmd 0x%04X",sector1,rdo1,last_cmd) ;
		break ;
	case 0x4544 :	// event
//		if(realtime) LOG(INFO,"S%d:%d: %d: T %d, trg %d, daq %d; shorts %d, ADCs %d; start_mhz %u, delta %u",sector1,rdo1,id,token,trg_cmd,daq_cmd,shorts,adc_cou,
//		    mhz_start_evt_marker,mhz_stop_evt_marker-mhz_start_evt_marker) ;
		LOG(NOTE,"S%d:%d: %d: VERSION 0x%04X: T %d, trg %d, daq %d; shorts %d, ADCs %d; start_mhz %u, delta %u",sector1,rdo1,id,version,token,trg_cmd,daq_cmd,shorts,adc_cou,
		    mhz_start_evt_marker,mhz_stop_evt_marker-mhz_start_evt_marker) ;

		break ;
	case 0x5445 :	// timer
		if(realtime) LOG(TERR,"S%d:%d: %d: T %d, trg %d, daq %d; shorts %d",sector1,rdo1,id,token,trg_cmd,daq_cmd,shorts) ;
		break ;	
	default :
		LOG(ERR,"S%d:%d: %d: T %d, trg %d, daq %d, UNKNOWN type 0x%04X; shorts %d",sector1,rdo1,id,token,trg_cmd,daq_cmd,evt_type,shorts) ;
		evt_err |= 2 ;
	}


	if(evt_type != 0x5244) {	// not a RESPONSE
		if((d[3] & (1<<14))==0) {
			LOG(ERR,"%d: %s: RESPONSE FIFO",rdo1,c_type) ;
		}
	}

	if((d[3] & (1<<13))==0) {
		LOG(ERR,"%d: %s: GTP not ready",rdo1,c_type) ;
	}
	if((d[3] & (1<<12))==0) {
		LOG(ERR,"%d: %s: RHICx5 PLL not locked",rdo1,c_type) ;
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


	for(int i=0;i>6;i++) {
		switch(st[i]) {
		case 3 :	//present & OK
		case 0 :	//not present
			break ;
		default :
			if(realtime) LOG(ERR,"%d: %s: FEB %d status: 0x%X",rdo1,c_type,i,st[i]) ;
			break ;
		}
	}

	if(evt_err) bad_error |= 1 ;

	return 0 ;
}

// unpacks and sanity-checks 1 RDO event
int stgc_data_c::start_0001(u_short *d, int shorts)
{
	trg_cou = 0 ;

	// move to start of actual data...
	return 0 ;

}

int stgc_data_c::event_0001()
{
	int evt_err = 0 ;

	if(adc_cou<=0) return 0 ;

	u_short d[4] ;

	for(int i=0;i<4;i++) {
		d[i] = *d16_data++ ;
	}

	u_int dd = (d[0]<<16)|d[1] ;

	int feb_id = dd>>29 ;

	if(feb_id==7) {	// trigger	
		u_short t_hi, t_mid, t_lo ;
		u_short t ;
		u_short trg_counter ;
		u_char t_cmd, d_cmd ;
		u_char err = 0 ;

		t_cmd = d[1] & 0xF ;
		d_cmd = (d[1]>>4) & 0xF ;
		t_hi = (d[1]>>8)& 0xF ;
		t_mid = (d[1]>>12)&0xF ;
		t_lo = (d[0]>>0) & 0xF ;

		t = (t_hi<<8)|(t_mid<<4)|t_lo ;

		trg_counter = (d[0]>>4)&0x1FF ;
		mhz_trg_marker = (d[2]<<16)|d[3] ;	// or RHIC clock

//		if((mhz_trg_marker+1) < mhz_start_evt_marker) err = 1 ;
//		if(mhz_trg_marker > (mhz_start_evt_marker+1)) err = 1 ;

		if((t != token)||(t_cmd != trg_cmd)||(d_cmd != daq_cmd)) err = 1 ;
			
		if(trg_cou) err = 1 ;	// more than 1 trigger

		if(err) {
			if(realtime) {
				LOG(ERR,"%d: trg_counter %d: T %d, trg %d, daq %d; mhz_counter %u",rdo1,trg_counter,t,t_cmd,d_cmd,mhz_trg_marker) ;
				LOG(ERR,"%d: trg_cou %d: T %d, trg %d, daq %d; mhz_counter %u",rdo1,trg_cou,token,trg_cmd,daq_cmd,mhz_trg_marker) ;
			}
		}
		else {
			//LOG(INFO,"%d: trg_cou %d: T %d, trg %d, daq %d; mhz_counter %u",rdo1,trg_counter,t,t_cmd,d_cmd,mhz_trg_marker) ;
		}

		// to indicate no data!
		vmm.feb_vmm = 0 ;
		vmm.ch = 0 ;
		vmm.adc = 0 ;
		vmm.bcid = 0 ;
		vmm.tb = 0 ;

		adc_cou-- ;
		trg_cou++ ;
		return 1 ;
	}

	
	if(feb_id>5) {
		evt_err |= 2 ;
	}


	u_int mhz_adc_marker = dd & 0x1FFFFFFF ;	// 29 bits of trigger

	int vmm_id = (d[2]>>13)&0x7 ;
	int crc_ok = (d[2]>>12)&1 ;
	int channel = (d[2]>>6)&0x3F ;

	int pdo = (d[2]&0x3F)<<4 ;
	pdo |= (d[3]>>12)&0xF ;

	int bcid = d[3]&0xFFF ;
	bcid = gray2dec(bcid) ;

	if(crc_ok==0) evt_err |= 8 ;

	if(vmm_id<4) evt_err |= 4 ;


	if(evt_err) {
		vmm.feb_vmm = 0 ;
		vmm.ch = 0 ;
		vmm.adc = 0 ;
		vmm.bcid = 0 ;
		vmm.tb = 0 ;

		bad_error |= evt_err ;



		if(realtime) LOG(ERR,"S%d:%d evt_err 0x%X at adc_cou %d",sector1,rdo1,evt_err,adc_cou) ;	

		adc_cou-- ;
		return 0 ;	// stop at the first occurence
	}

	vmm.feb_vmm = ((feb_id-1)<<2)|(vmm_id-4) ;
	vmm.ch = channel ;
	vmm.adc = pdo ;
	vmm.bcid = bcid ;

	int tb = (int)mhz_adc_marker - (int)(mhz_trg_marker&0x1FFFFFFF) ;

	if(tb<-32000) vmm.tb = 0x8000 ;
	else if(tb>32000) vmm.tb = 0x7FFF ;
	else vmm.tb = tb ;


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
#if 0
	int t_hi, t_mid, t_lo ;

	d16_start = d ;

	
	d += 8 ;	// skip TEF header
	shorts -= 8 ;

//	for(int i=0;i<shorts;i++) {
//		LOG(TERR,"%d/%d = 0x%04X",i,shorts,d[i]) ;
//	}

	trg_cmd = d[3]&0xF ;
	daq_cmd = d[4]&0xF ;

	t_hi = (d[4]>>4)&0xF ;
	t_mid = (d[4]>>8)&0xF ;
	t_lo = (d[4]>>12)&0xF ;

	token = (t_hi<<8)|(t_mid<<4)|t_lo ;


#endif
	bad_error = 0 ;
	want_saved = 0 ;

	hdr_check(d,shorts) ;

	switch(version) {
	case 0x0001 :
	case 0x0002 :
	case 0x0003 :
	case 0x0004 :
	case 0x0005 :
	case 0x0006 :
		return start_0001(d,shorts) ;
	default :
		break ;
	}


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

	switch(version) {
	case 0x0001 :
	case 0x0002 :
	case 0x0003 :
	case 0x0004 :
	case 0x0005 :
	case 0x0006 :
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

