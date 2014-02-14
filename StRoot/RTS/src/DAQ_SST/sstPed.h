#ifndef _SST_PED_HH_
#define _SST_PED_HH_


#include <sys/types.h>

#include "daq_sst.h"



class sstPed {
public:
	sstPed() ;
	~sstPed() ;


	int sector ; 
	int valid ;	// when calced or loaded

	int rb_mask ;

	void init(int active_rbs) ;					// mallocs (if nece) and clears ped_store

	void accum(char *evbuff, int bytes, int rdo1) ;

	void calc() ;					// calculates mean/rms into ped_store
	int to_evb(char *buff) ;			// to EVB format from ped_store


	int to_cache(char *fname = 0, u_int run = 0) ;			// to cached file from ped_store


	// allocated per RDO
	daq_sst_ped_t *ped_store ;


	daq_sst *sst_rdr ;	// need it for something...

	int sizeof_ped ;

} ;

#endif
