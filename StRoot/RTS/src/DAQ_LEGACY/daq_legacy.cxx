#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>

#include <rtsLog.h>
#include <rtsSystems.h>
#include <rts.h>

#include <daqFormats.h>


#include "daq_legacy.h"

int legacyCheckBank(char *in, char *expect) 
{ 
	char buff[12] ;

	memset(buff,0,sizeof(buff)) ;
	memcpy(buff,in,8) ;

	if(memcmp(buff,expect,8)) {
		LOG(ERR,"Read \"%s\", expect \"%s\"",buff,expect) ;
	}

	return 0 ;
}
	
// returns word aligned ptr
int *legacyDetp(int rts_id, char *m) 
{
	struct DATAP *datap = (struct DATAP *)m ;
	struct DATAPX *datapx ;
	
	int len, off ;
	int id ;

	int swapdatap = 0 ;
	int swapdatapx = 0 ;

	if(datap == 0) return 0 ;


	LOG(DBG,"Checking for %s",rts2name(rts_id)) ;

	// verify bank
	if(legacyCheckBank(datap->bh.bank_type, CHAR_DATAP) < 0) {
		return 0 ;
	}

	// set order
	if(datap->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapdatap = 1;

	for(int i=0;i<10;i++) {
		if(datap->det[i].len) {
			LOG(DBG,"Found DATAP ix %d: [%s]",i,rts2name(i)) ;
		}
	}

	// ugly special cases
	switch(rts_id) {
	case BSMD_ID :
		id = BTOW_ID ;
		break ;
	case ESMD_ID :
		id = ETOW_ID ;
		break ;
	default :
		id = rts_id ;
		break ;
	}

	if(id < 10) {
		len = qswap32(swapdatap, datap->det[id].len) ;
		off = qswap32(swapdatap, datap->det[id].off) ;

		LOG(DBG, "Checking for datap: len=%d off=%d",len,off);

		if((len == 0) || (off == 0)) {
			return 0 ;
		}

		// navigate to DETP
		return ((int *)datap + off) ;
	}
	else {

		len = qswap32(swapdatap, datap->det[EXT_ID].len) ;
		off = qswap32(swapdatap, datap->det[EXT_ID].off) ;

		LOG(DBG, "Checking for datapx: len=%d off=%d",len,off);

		if((len == 0) || (off == 0)) {
			return 0 ;
		}

		// navigate to datapx
		datapx = (struct DATAPX *)((int *)datap + off) ;

		// verify bank
		if(legacyCheckBank(datapx->bh.bank_type, CHAR_DATAPX) < 0) {
			return 0 ;
		}

		if(datapx->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapdatapx = 1;

		for(int i=0;i<22;i++) {
			if(datapx->det[i].len) {
				LOG(DBG,"Found DATAPX ix %d: ID %d [%s]",i,i+10,rts2name(i+10)) ;
			}
		}


		len = qswap32(swapdatapx, datapx->det[id-10].len) ;
		off = qswap32(swapdatapx, datapx->det[id-10].off) ;


		if((len == 0) || (off == 0)) {
		  	return 0 ;
		}
	
		// navigate to EMCP
		return ((int *)datapx + off) ;

	}

}

