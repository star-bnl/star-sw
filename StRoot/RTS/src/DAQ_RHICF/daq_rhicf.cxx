#include <assert.h>
#include <sys/types.h>
#include <errno.h>

#include <rtsLog.h>
#include <rtsSystems.h>
#include <daqFormats.h>

#include <SFS/sfs_index.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>


#include "daq_rhicf.h"

extern int rhicf_reader(char *mem, struct rhicf_t *rhicf, u_int driver) ;




const char *daq_rhicf::help_string = "\
\n\
RHICF Help: \n\
Supported Banks: \n\
	raw	returns=ptr of start of DDL data; c1=sector[1..1]; c2=rdo[1..4]; \n\
\n\
\n\
" ;

class daq_det_rhicf_factory : public daq_det_factory
{
public:
        daq_det_rhicf_factory() {
                daq_det_factory::det_factories[RHICF_ID] = this ;
        }

        daq_det *create() {
                return new daq_rhicf ;
        }
} ;

static daq_det_rhicf_factory rhicf_factory ;



daq_rhicf::daq_rhicf(daqReader *rts_caller) 
{
	rts_id = RHICF_ID ;
	name = rts2name(rts_id) ;
	sfs_name = "rhicf" ;
	caller = rts_caller ;
	if(caller) caller->insert(this, rts_id) ;

	raw = new daq_dta ;


	
	LOG(DBG,"%s: constructor: caller %p",name,rts_caller) ;
	return ;
}

daq_rhicf::~daq_rhicf() 
{
	LOG(DBG,"%s: DEstructor",name) ;

	delete raw ;


	return ;
}



daq_dta *daq_rhicf::get(const char *bank, int sec, int row, int pad, void *p1, void *p2) 
{
	Make() ;

	if(present==0) return 0 ;

	LOG(DBG,"%s: looking for bank %s",name,bank) ;

	if(strcmp(bank,"*")==0) bank = "raw" ;
		


	if(strcasecmp(bank,"raw")==0) {
		if((present & DET_PRESENT_SFS)==0) return 0 ;		// no DDL
		return handle_raw(sec,row) ;		// actually sec, rdo; r1 is the number of bytes
	}
	else {
		LOG(ERR,"%s: unknown bank type \"%s\"",name,bank) ;
	}

	return 0 ;
}

daq_dta *daq_rhicf::handle_raw(int sec, int rdo)
{
	char str[128] ;

	// bring in the bacon from the SFS file....
	assert(caller) ;

	
	sprintf(str,"%s/sec01/rb01/raw",sfs_name) ;
	char *full_name = caller->get_sfs_name(str) ;
	
	LOG(DBG,"%s: trying sfs on \"%s\"",name,str) ;
	if(full_name == 0) return 0 ;

	int size = caller->sfs->fileSize(full_name) ;	// this is bytes

	LOG(DBG,"Got size %d",size) ;
	if(size <= 0) {
		LOG(DBG,"%s: %s: not found in this event",name,str) ;
		return 0 ;
	}

	char *ptr = (char *) malloc(size) ;
	LOG(DBG,"Malloc at %p",ptr) ;

	caller->sfs->read(full_name, ptr, size) ;

	LOG(DBG,"sfs read succeeded") ;

	//I need to skip the 40 byte bankHeader!
	{
	daq_trg_word trg ;
	int ret = get_l2(ptr,size*4,&trg,1) ;
	LOG(NOTE,"get_l2 returns %d: %d %d %d",ret,trg.trg,trg.daq,trg.t) ;
	}
	size -= 40 ;	//bank header

	raw->create(size,"rhicf_raw",rts_id,DAQ_DTA_STRUCT(u_char)) ;

	char *st = (char *) raw->request(size) ;

	memcpy(st,ptr+40,size) ;
	free(ptr) ;


        raw->finalize(size,1,1,0) ;

	raw->rewind() ;

	return raw ;

}

// knows how to get the token out of an event...
int daq_rhicf::get_token(char *addr, int words)
{
	LOG(ERR,"get_token") ;

	int cou ;
	struct daq_trg_word trg[128] ;

	cou = get_l2(addr,words,trg,1) ;

	if(cou==0) return -1000 ;	// special marker...
	if(trg[0].t==0) return -ENOSYS ;

	return trg[0].t ;
}



/*
	Trigger info is in the bank header crc field as

	0-3	trg_cmd
	4-7	daq_cmd
	8-19	token
	20	star_trigger flag
	21	star_trgx flag
	22	star_trg4 flag
	23	star_busy flag
*/

// knows how to get a/the L2 command out of the event...
int daq_rhicf::get_l2(char *addr, int words, struct daq_trg_word *trg, int rdo)
{
	int t_cou = 0 ;
	int in_words = words ;
	int err = 0 ;
	static int token ;

	//LOG(TERR,"get_l2") ;
	struct bankHeader *bh ;

	bh = (bankHeader *)addr ;

	LOG(NOTE,"Token %d, CRC word 0x%08X, format number 0x%08X",bh->token,bh->crc,bh->format_number) ;

	trg[0].t = bh->crc & 0xFFF ;
	trg[0].trg = (bh->crc>>16)&0xF ;
	trg[0].daq = (bh->crc>>12)&0xF ;

	t_cou++ ;

	if(err) {
		LOG(ERR,"[%d] Bad Event: T %4d: words %d",
		    rdo,trg[0].t,in_words) ;

	}

	if(err & 1) {	// critical -- blow the whole event
		return -1 ;
	}

	return t_cou ;
}

