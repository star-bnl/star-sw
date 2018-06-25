#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <arpa/inet.h>	// for htonl
#include <dlfcn.h>	// shared lib support
#include <errno.h>

#include <rtsLog.h>
#include <rts.h>
#include <rtsSystems.h>
#include <daqFormats.h>

#include <SFS/sfs_index.h>

#include "daqReader.h"
#include "daq_det.h"
#include "daq_dta.h"


//static
daq_det_factory *daq_det_factory::pseudo_factories[48] ;
daq_det_factory *daq_det_factory::det_factories[48] ;


//static
daq_det *daq_det_factory::make_det(int wh)
{
	daq_det_factory *use_factory = 0  ;
	char libname[64] ; 

	libname[0] = 0 ;	// cautious...

	if(wh < 0) {	// super-special rare pseudo det cases; deal by hand...
		switch(wh) {
		case -BTOW_ID :
			sprintf(libname,"libdaq_emc.so") ;
			break ;
		case -L3_ID :
			sprintf(libname,"libdaqhlt.so") ;
			break ;
		case -SVT_ID :	//itpc_pseud
			sprintf(libname,"libitpc.so") ;
			break ;
//		case -L4_ID:
//		    sprintf(libname,"libdaql4.so");
//		    break;
		}
	}
	else {
		sprintf(libname,"libdaq_%s.so",rts2name(wh)) ;
	}

	// dets are in uppercase, turn all to lowercase...
	for(u_int i=0;i<strlen(libname);i++) {
		libname[i] = tolower(libname[i]) ;
	}

	LOG(NOTE,"factory for det %d, lib %s",wh,libname) ;

	if(wh < 0) {		// is this a pseudo det?
		wh = -wh ;	// reverse sign!
		use_factory = pseudo_factories[wh] ;
	}
	else {	// normal det
		use_factory = det_factories[wh] ;
	}


	if(use_factory == 0) {	// not inserted? need shared lib load...
#if 0	// this was never completed...
#ifdef __ROOT__
		gSomething->Loadsomething(libname) ;
#else
		// shared lib load not done yet, let it fail...
		#if 0
		errno = 0 ;
		void *handle = dlopen("../DAQ_SC/libdaq_sc.so", RTLD_LAZY | RTLD_GLOBAL) ;
		if(handle == 0) {
			LOG(ERR,"dlopen failed for %s [%s]",libname,dlerror()) ;
		}
		else {
			LOG(NOTE,"dlopen OK for det %d, lib %s",wh,libname) ;
		}
		#endif
#endif
#endif
	}

	if(use_factory == 0) {	// still nothing???
		LOG(ERR,"Can't load or find detector %d,libname %s",wh,libname) ;
	}

	assert(use_factory) ;	// what else...

	LOG(NOTE,"factory for %d: calling create",wh) ;

	return use_factory->create() ;
}


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

#ifndef PP_MVME
	if(caller) {
		LOG(DBG,"Before de_insert(%d)",rts_id) ;
		caller->de_insert(rts_id) ;
		LOG(DBG,"After de_insert(%d)",rts_id) ;
	}
#endif
	return ;
}


unsigned int daq_det::get_global_event_num()
{
	if(caller) return caller->seq ;
	
	return 0 ;	// unknown...
};

void daq_det::managed_by(class daqReader *c)
{
	caller = c ;

	assert(caller) ;
#ifndef PP_MVME
	caller->insert(this, rts_id) ;
#endif
}

int daq_det::Make() 
{
	present = 0 ;	// assume not...
	

	if(presence()) {	// this is SFS presence...
		present |= DET_PRESENT_SFS ;	// in sfs: 1
		LOG(NOTE,"%s(SFS %s): present via SFS",name,sfs_name) ;
	}
	else if(legacyDetp(rts_id, caller->mem)) {
		present |= DET_PRESENT_DATAP ;	// in datap: 2
		LOG(NOTE,"%s(SFS %s): present via DATAP",name,sfs_name) ;
	}

	if(present) {
		evt_num++ ;	// nah, this is not really event number but let it stay...
	}
	else {
		LOG(DBG, "%s: not found",name) ;
	}

	return present ;
}

int daq_det::Init() 
{
	present = 0 ;
	evt_num = 0 ;
	run_num = 0 ;

	LOG(DBG,"Init: %s[%d], sfs_name %s",name,rts_id,sfs_name) ;

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

/*
	This checks SFS presence!
*/
int daq_det::presence() 
{ 

	int pres = 0 ;

	if(caller==0) {	// in case we are running online, there is no "caller" so assume presence!
		LOG(NOTE,"no caller? %s",name) ;
		pres = 1 ;
		goto ret_here;
	}

#ifdef PP_MVME
	pres = 1 ;
	goto ret_here ;
#else
	if(caller->sfs == 0) goto ret_here ;	// no sfs?

	if(caller->sfs->opendirent((char *)sfs_name)) {	// gotcha!
		pres = 1 ;
	}
	else {
		pres = 0 ;
	}
#endif	
	ret_here: ;

	LOG(DBG,"sfs presence(%s): %d",sfs_name,pres) ;

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

int daq_det::bad_sanity()
{
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

	/* SPECIAL HACK for i.e. 2000 DATA
	In 2000 the DATAP bank was "DATAP" and not the usual "DATAP   "
	

	*/
	if(strcmp(buff,"DATAP")==0) {
		memcpy(buff,CHAR_DATAP,8) ;
	}

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
		LOG(DBG,"No DATAP: I would need it for %s [%d]",rts2name(rts_id),rts_id) ;
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
		LOG(DBG,"[BE]TOW: %p: %d, %d",emcp,emcp->sec[0].len, emcp->sec[1].len) ;
		if(emcp->sec[0].len) ; // do nothing...
		else ret_p = 0 ;	// however, it is still possible that they are in trigger's bank
		break ;
	case BSMD_ID :
	case ESMD_ID :
		LOG(DBG,"[BE]SMD: %p: %d, %d",emcp,emcp->sec[0].len, emcp->sec[1].len) ;
		if(emcp->sec[1].len) ; // do nothing ...
		else ret_p = 0 ;
		break ;
	}

	if(ret_p) LOG(DBG,"%s [%d] found in this event",rts2name(rts_id),rts_id) ;

	return ret_p ;		
}

