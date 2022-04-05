#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>


#include <rtsLog.h>

#include "daq_dta.h"

daq_dta::daq_dta() 
{
	bytes_alloced = 0 ;
	store = 0 ;
	store_cur = 0 ;
	do_swap = 0 ;	// obviosly
	nitems = 0 ;

	mode = 0 ;
	meta = 0 ;
} ;


daq_dta::~daq_dta() {
	release() ;
}

int daq_dta::iterate() 
{
	if(nitems==0) return 0 ;	// done!


	sec = (store_cur->sec) ;
	row = (store_cur->row) ;
	pad = (store_cur->pad) ;
	ncontent = (store_cur->nitems) ;


	store_cur++ ;		// skip this standard header...
	Byte = (unsigned char *) store_cur  ;	// ...point to start of data!

	// and advance for next
	store_cur = (daq_store *)((char *)store_cur + ncontent * hdr->obj_bytes) ;
	nitems-- ;

	return 1 ;	// return as "have data"
}


void daq_dta::release()
{
	if(store && bytes_alloced) {
		LOG(DBG,"freeing store") ;
		free(store) ;
		store = 0 ;
		bytes_alloced = 0 ;
	}

}

/*
	objects is a first guess as to what we'll need, it is best that it is good!
*/
daq_store *daq_dta::create(u_int bytes, const char *name, int rts_id, const char *o_name, u_int obj_size) 
{
	u_int requested ;

	// size of the store's header
	requested = sizeof(daq_store) + sizeof(daq_store_hdr) ;	// first "object" is the header
	
	// if I asked for less bytes than 1 object, I will overrule
	if(bytes < obj_size) bytes = obj_size ;
	// otherwise I will abide	
	
	requested += sizeof(daq_store) + bytes ;


	if(bytes_alloced < requested) {
		u_int b_all_cache = bytes_alloced ;

		release() ;	// free current store...

		// allocate new one
		bytes_alloced = requested ;
		store = (daq_store *) valloc(bytes_alloced) ;
		assert(store) ;
		LOG(DBG,"Allocated %d bytes for %d bytes required (%d was available)",bytes_alloced,bytes,b_all_cache) ;
	}
	else {
		LOG(DBG,"Reusing %d bytes for %d bytes required",bytes_alloced,bytes) ;
	}
		

	// put header
	store->sec = rts_id ;

	store->nitems = 1 ;	// header
	nitems = store->nitems ;	// copy used for the iterator()!

	hdr = (daq_store_hdr *) (store + 1) ;

	hdr->hdr_bytes = sizeof(daq_store_hdr) ;
	hdr->hdr_version = DAQ_DTA_C_VERSION ;
	hdr->endianess = DAQ_DTA_ENDIANESS ;
	hdr->obj_bytes = obj_size ;
	hdr->bytes_used = sizeof(daq_store) + hdr->hdr_bytes ;
	strncpy(hdr->obj_name,o_name,sizeof(hdr->obj_name)-1) ;
	sprintf(hdr->describe,"%s[%d]:%s[%d bytes]:%s",name,rts_id,o_name,obj_size,__DATE__) ;

	mode = 0 ;

	LOG(DBG,"CREATE:%s: nitems %d, bytes %u/%u",hdr->describe,store->nitems,hdr->bytes_used,bytes_alloced) ;

	store_cur = (daq_store *)((char *)store + hdr->bytes_used) ;	// set to next item...

	return store ;	// not meant to be used except for a free!
}

int daq_dta::is_empty()
{
	if(store == 0) return 1 ;
	
	if(store->nitems <= 1) return 1 ;

	return 0 ;
}

/*
	Keeps the header, object types etc but just zaps them
*/
void daq_dta::clear()
{
	assert(store) ;

	store->nitems = 1 ; //the header
	nitems = store->nitems ;

	hdr = (daq_store_hdr *) (store + 1) ;
	hdr->bytes_used = sizeof(daq_store) + hdr->hdr_bytes ;


	store_cur = (daq_store *)((char *)store + hdr->bytes_used) ;


}


void *daq_dta::request(u_int obj_cou)
{
	daq_store *tmp_store ;

	LOG(DBG,"Requesting %d objects",obj_cou) ;

	if(obj_cou <= 0) obj_cou = 16 ;	// ad hoc rule...

	tmp_store = get(obj_cou) ;

	LOG(DBG,"get returns %p",tmp_store) ;

	return (void *)(tmp_store + 1) ;

}

void daq_dta::finalize(u_int obj_cou, int sec, int row, int pad)
{
//	if(obj_cou==0) {
//		daq_store_hdr *hdr = (daq_store_hdr *)(store + 1 ) ;
//
//		LOG(NOTE,"%s: finalize with obj cou 0?",hdr->describe) ;
//		return ;	// didn't find anything so just do nothing...
//	}


//	LOG(DBG,"Finilizing %d objects for sec %d, row %d, pad %d",obj_cou,sec,row,pad) ;

	store_cur->sec = sec ;
	store_cur->row = row ;
	store_cur->pad = pad ;
	store_cur->nitems = obj_cou ;
	

	LOG(DBG,"Finilizing %d objects for sec %d, row %d, pad %d [%d]",obj_cou,sec,row,pad,store_cur->pad) ;

	int bytes = sizeof(daq_store) + store_cur->nitems * hdr->obj_bytes ;
	commit(bytes) ;


	return ;
}

	
//daq_store *daq_dta::get(u_int *ret_avail)
daq_store *daq_dta::get(u_int obj_cou)
{
	int do_realloc = 0 ;
	u_int avail ;
	u_int need ;

	// how much to we need
	if(obj_cou==0) obj_cou = 16 ;	// is not specified, as for 16...

	assert(hdr) ;

	need = sizeof(daq_store) + (obj_cou * hdr->obj_bytes) ;

	// attempt heuristics for storage reallocation
	avail = bytes_alloced - hdr->bytes_used ;

	if(avail < need) do_realloc = 1 ;

		
	LOG(DBG,"get(): avail bytes %d, need %d, objects %d requested of %d bytes each",avail, need, obj_cou,hdr->obj_bytes) ;

	if(do_realloc) {
		char *new_store ;

		// round to 16 kB
		const int chunk = 16*1024 ;
		int cou16 = need/chunk ;
		cou16++ ;
		need = cou16 * chunk ;

		u_int old_alloced = bytes_alloced ;

		bytes_alloced = hdr->bytes_used + need ;

		LOG(DBG,"Reallocing from %d to %d",old_alloced,bytes_alloced) ;

		// remember pointers!
		u_int hdr_off = (char *)hdr-(char *)store ;
		u_int Byte_off = (char *)Byte - (char *)store ;
		u_int store_cur_off = (char *)store_cur - (char *)store ;

#define USE_REALLOC
#ifdef USE_REALLOC
		new_store = (char *) realloc(store, bytes_alloced) ;
		if(new_store == 0) {
			LOG(WARN,"realloc failed!") ;
			assert(new_store) ;

		}
#else
			
		LOG(DBG,"Before valloc %d",bytes_alloced) ;
		new_store = (char *)valloc(bytes_alloced) ;
		assert(new_store) ;
		LOG(DBG,"Before memcopy of %d",hdr->bytes_used) ;
		memcpy(new_store,store,hdr->bytes_used) ;

		free(store) ;
#endif



		// apply ptrs
		hdr = (daq_store_hdr *) ((char *)new_store + hdr_off) ;
		Byte = (u_char *)new_store + Byte_off ;
		store_cur = (daq_store *) ((char *)new_store + store_cur_off) ;

		store = (daq_store *) new_store ;

		LOG(DBG,"Realloc done") ;

	}
	avail = bytes_alloced - hdr->bytes_used ;

	store_cur->sec = 0xFF ;
	store_cur->nitems = 0 ;


	return store_cur ;
}

void daq_dta::commit(u_int bytes) 
{
	char *mem = (char *)store_cur ;
	if(bytes == 0) {
		bytes = sizeof(daq_store) + store_cur->nitems * hdr->obj_bytes ;
	}

	mem += bytes ;

	store->nitems++ ;
	nitems = store->nitems ;
	hdr->bytes_used = mem - (char *)store ;

	store_cur = (daq_store *) mem ;
		
	LOG(DBG,"commit: %dth nitem, bytes %d, bytes_used %d/%d",store->nitems,bytes,hdr->bytes_used,bytes_alloced) ;

	if(hdr->bytes_used > bytes_alloced) {
		LOG(CRIT,"Storage overrun: %d > %d",hdr->bytes_used, bytes_alloced) ;
	}


	return ;
}

/*
	At the end of fillling, before we return the daq_det to the
	user, we must rewind so the user can start reading from the start!
*/
void daq_dta::rewind() 
{
	if(store == 0) return ;	// not yet created!

	store_cur = store ;
	nitems = store->nitems ;
	
	if(nitems <= 0) {
		LOG(NOTE,"No items %d???",nitems) ;
		return ;
		assert(nitems>0) ;
	}

	// adjust for the next so that iterate has something to do
	nitems-- ;
	store_cur = (daq_store *)((char *)hdr + hdr->hdr_bytes) ;
	assert(store_cur) ;


	LOG(DBG,"READY:%s: nitems %d, bytes %u/%u",hdr->describe,store->nitems,hdr->bytes_used,bytes_alloced) ;

	return  ;
}

