#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <stdlib.h>


#include <rtsLog.h>

#include "daq_stgc.h"
#include "stgc_data_c.h"

stgc_data_c::feb_t stgc_data_c::feb[STGC_SECTOR_COU][4][6] ;

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

	return ;
}


int stgc_data_c::start(u_short *d, int shorts)
{
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


int stgc_data_c::event_end()
{
	return 0 ;
}

