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
		free(store) ;
		store = 0 ;
		bytes_alloced = 0 ;
	}

}

/*
	bytes is a first guess as to what we'll need, it is best that it is good!
*/
daq_store *daq_dta::create(u_int bytes, char *name, int rts_id, const char *o_name, u_int obj_size) 
{
	u_int requested = bytes ;

	// bytes is the raw storage needed but we'll add about 1/5th to it so we don't
	// have to reallocate too often...
	bytes += sizeof(daq_store) + sizeof(daq_store_hdr) + sizeof(daq_store) ;
	bytes += bytes/5 ;

	if(bytes_alloced < bytes) {
		release() ;

		bytes_alloced = ((bytes/(16*1024))+1)*16*1024 ;	// 16 kB chunks
		store = (daq_store *) valloc(bytes_alloced) ;
		assert(store) ;
		LOG(DBG,"Allocated %d bytes for %d bytes required",bytes_alloced,requested) ;
	}
	else {
		LOG(DBG,"Reusing %d bytes for %d bytes required",bytes_alloced,requested) ;
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

	tmp_store = get(obj_cou) ;


	return (void *)(tmp_store + 1) ;

}

void daq_dta::finalize(u_int obj_cou, int sec, int row, int pad)
{
	store_cur->sec = sec ;
	store_cur->row = row ;
	store_cur->pad = pad ;
	store_cur->nitems = obj_cou ;


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

	if(obj_cou==0) {
		need = bytes_alloced/5 ;
	}
	else {
		need = 256 + sizeof(daq_store) + obj_cou * hdr->obj_bytes ;
		if(need < bytes_alloced/5) need = bytes_alloced/5 ;
	}

	// attempt heuristics for storage reallocation
	avail = bytes_alloced - hdr->bytes_used ;

	if(avail < need) do_realloc = 1 ;

		
	LOG(DBG,"get(): avail bytes %d",avail) ;

	if(do_realloc) {
		LOG(NOTE,"Reallocing from %d to %d",bytes_alloced,bytes_alloced+need) ;


		bytes_alloced += need ;

		LOG(DBG,"Before valloc %d",bytes_alloced) ;
		char *new_store = (char *)valloc(bytes_alloced) ;
		assert(new_store) ;

		LOG(DBG,"Before memcopy of %d",hdr->bytes_used) ;
		memcpy(new_store,store,hdr->bytes_used) ;

		// adjust pointers!
		u_int off = (char *)hdr-(char *)store ;
		hdr = (daq_store_hdr *) ((char *)new_store + off) ;

		// and current Byte
		off = (char *)Byte - (char *)store ;
		Byte = (u_char *)new_store + off ;

		off = (char *)store_cur - (char *)store ;
		store_cur = (daq_store *) ((char *)new_store + off) ;

		LOG(DBG,"Before free %p",store) ;	
		free(store) ;
		LOG(DBG,"After free?") ;

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
	store_cur = store ;
	nitems = store->nitems ;
	
	if(nitems <= 0) {
		assert(nitems>0) ;
	}

	// adjust for the next so that iterate has something to do
	nitems-- ;
	store_cur = (daq_store *)((char *)hdr + hdr->hdr_bytes) ;
	assert(store_cur) ;


	LOG(DBG,"READY:%s: nitems %d, bytes %u/%u",hdr->describe,store->nitems,hdr->bytes_used,bytes_alloced) ;

	return  ;
}

