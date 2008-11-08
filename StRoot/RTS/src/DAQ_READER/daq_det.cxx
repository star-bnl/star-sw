#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <arpa/inet.h>	// for htonl

#include <rtsLog.h>
#include <rts.h>
#include <rtsSystems.h>
#include <daqFormats.h>

#include <SFS/sfs_index.h>

#include "daqReader.h"
#include "daq_det.h"
#include "daq_dta.h"


int daq_det::endianess = 0 ; // the executing machine is little endian (0)



daq_det::daq_det(daqReader *rts_caller) 
{	
	name = sfs_name = "(generic)" ;
	rts_id = -1 ;			// this should be correctly overriden in the member
	caller = rts_caller ;	
	present = 0 ;

	SetMode(0) ;
	m_Debug = 0 ;

	def_sector = def_rdo = -1 ;	// assume ALL sectors and RDOs..

	in_buffer = 0 ;
	out_buffer = 0 ;
	in_bytes = 0 ;
	out_bytes = 0 ;

	if(htonl(0x12345678) != 0x12345678) {
		endianess = 0 ;	// little ;
	}
	else {
		endianess = 1 ;	// big
	}


	LOG(DBG,"daq_det: %s [%d], caller %p: endianess %c",name,rts_id,caller,endianess?'B':'L') ;

	return ;
}


daq_det::~daq_det() 
{
	LOG(DBG,"~daq_det: %s [%d]",name,rts_id) ;

	if(caller) caller->de_insert(rts_id) ;

	return ;
}

void daq_det::managed_by(class daqReader *c)
{
	caller = c ;

	assert(caller) ;

	caller->insert(this, rts_id) ;
}

int daq_det::Make() 
{
	present = 0 ;
	LOG(NOTE,"daq_det: Make: %s [%d]: evt_num %d",name,rts_id,evt_num) ;

	evt_num++ ;

	if(presence()) {
		present |= 2 ;	// in sfs
	}
	else if(legacyDetp(rts_id, caller->mem)) {
		present |= 1 ;
	}


	LOG(NOTE,"%s: has DATAP: %s; has SFS: %s",name,(present&1)?"YES":"NO",(present&2)?"YES":"NO") ;

	return present ;
}

int daq_det::Init() 
{
	present = 0 ;

	LOG(DBG,"Init: %s [%d]",name,rts_id) ;

	return 0 ;
}

int daq_det::InitRun(int run_number) 
{
	run_num = run_number ;
	evt_num = 0 ;
	present = 0 ;

	LOG(DBG,"InitRun: %s [%d]: run %09u",name,rts_id,run_number) ;

	return 0 ;
}

int daq_det::FinishRun(int old_run_number) 
{

	LOG(DBG,"FinishRun: %s [%d]: run %09u",name,rts_id,old_run_number) ;

	return 0 ;
}

void daq_det::help() const 
{	
	printf("***************** %s: %s ******************\n",name,GetCVS()) ;

}


int daq_det::presence() 
{ 

	int pres = 0 ;

	if(caller==0) {	// in case we are running online, there is no "caller" so assume presence!
		LOG(WARN,"no caller? %s",name) ;
		pres = 1 ;
		goto ret_here;
	}

	if(caller->sfs == 0) goto ret_here ;

	if(caller->sfs->opendirent((char *)sfs_name)) {
		pres = 1 ;
	}
	else {
		pres = 0 ;
	}

	ret_here: ;

	LOG(NOTE,"sfs presence(%s): %d",sfs_name,pres) ;

	return pres ;
	
} ;



int daq_det::get_token(char *addr, int words)
{
	daq_trg_word trgs[128] ;

	int ret = get_l2(addr, words, trgs, 1) ;
	if(ret == 0) {
		LOG(ERR,"No triggers?") ;
		return -1000 ;
	}
	

	if(trgs[0].t==0) {
		LOG(ERR,"Token 0 not allowed but I will try to use the other triggers...") ;
		trgs[0].t = 4097 ;
	}


	return trgs[0].t ;

}


int daq_det::get_l2(char *buff, int buff_bytes, daq_trg_word *trg, int prompt) 
{

	LOG(ERR,"%s: get_l2() not written!",name) ;
	return 0 ;
}


daq_dta  *daq_det::get(const char *bank,int c1, int c2, int c3, void *p1, void *p2) 
{
	LOG(ERR,"%s [%d]: get() not written!",name,rts_id) ;

	return 0 ;
}
	

daq_dta  *daq_det::put(const char *bank,int c1, int c2, int c3, void *p1, void *p2) 
{
	LOG(ERR,"%s: put() not written!",name) ;

	return 0 ;
}
	


// helpers
int checkBank(char *in, char *expect) 
{ 
	char buff[12] ;

	memcpy(buff,in,8) ;
	buff[9] = 0 ;

	if(memcmp(buff,expect,strlen(expect))) {
		LOG(ERR,"Read \"%s\", expect \"%s\"",buff,expect) ;
		return -1 ;
	}
	else {
		LOG(DBG,"Found \"%s\", as expected...",expect) ;
	}

	return 0 ;
}
	
/*
	Returns pointer to the DETP banks using
	legacy DATAP/DATAPX banks...
*/
int *legacyDetp(int rts_id, char *m) 
{
	struct DATAP *datap = (struct DATAP *)m ;	// assume we are pointed to DATAP
	struct DATAPX *datapx ;
	int *ret_p ;
	
	int len, off ;
	int id ;


	if(datap == 0) {
		LOG(DBG,"No DATAP needed for %s [%d]",rts2name(rts_id),rts_id) ;
		return 0 ;
	}

	int swapdatap = 0 ;
	int swapdatapx = 0 ;


	LOG(DBG,"Checking for %s",rts2name(rts_id)) ;

	// verify bank
	if(checkBank(datap->bh.bank_type, CHAR_DATAP) < 0) {
		return 0 ;
	}

	LOG(DBG,"Here...") ;
	// set order
	if(datap->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapdatap = 1;

	for(int i=0;i<10;i++) {
		if(datap->det[i].len) {
			LOG(DBG,"Found DATAP ix %d: [%s]",i,rts2name(i)) ;
		}
	}

	// ugly special cases which need override...
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

	ret_p = 0 ;	// assume not found...

	// navigate to DETP
	if(id < 10) {	// DATAP
		len = qswap32(swapdatap, datap->det[id].len) ;
		off = qswap32(swapdatap, datap->det[id].off) ;

		LOG(DBG, "Checking for datap: len=%d off=%d",len,off);

		if((len == 0) || (off == 0)) {
			return 0 ;
		}

		// navigate to DETP
		LOG(DBG,"%s [%d] found in this event",rts2name(rts_id),rts_id) ;
		ret_p = ((int *)datap + off) ;
	}	
	else {	// DATAPX

		len = qswap32(swapdatap, datap->det[EXT_ID].len) ;
		off = qswap32(swapdatap, datap->det[EXT_ID].off) ;

		LOG(DBG, "Checking for datapx: len=%d off=%d",len,off);

		if((len == 0) || (off == 0)) {
			return 0 ;
		}

		// navigate to datapx
		datapx = (struct DATAPX *)((int *)datap + off) ;

		// verify bank
		if(checkBank(datapx->bh.bank_type, CHAR_DATAPX) < 0) {
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
	
		// navigate to DETP
		LOG(DBG,"%s [%d] found in this event",rts2name(rts_id),rts_id) ;
		ret_p = ((int *)datapx + off) ;

	}


	if(ret_p == 0) return 0 ;

	// special case for EMCs: we need to discern the SMDs....
	EMCP *emcp = (EMCP *) ret_p ;

	switch(rts_id) {
	case BTOW_ID :
	case ETOW_ID :
		if(emcp->sec[0].len) return ret_p ;
		else return 0 ;
	case BSMD_ID :
	case ESMD_ID :
		if(emcp->sec[1].len) return ret_p ;
		else return 0 ;
	}

	return ret_p ;		
}


