#include <sys/types.h>

#include <rtsLog.h>
#include <rtsSystems.h>
#include <daqFormats.h>
#include <rts.h>

#include <DAQ_READER/daq_det.h>

const struct hdrs {
	char *emcp ;
	char *secp ;
	char *rbp ;
	char *adc ;
} hdrs[2] = {
	{ CHAR_EMCP, CHAR_EMCSECP, CHAR_EMCRBP, CHAR_EMCADCR },
	{ CHAR_EECP, CHAR_EECSECP, CHAR_EECRBP, CHAR_EECADCR }
} ;

/*
	Returns the pointer to the raw data start.
	This will include the aux 4 bytes of VME RB stuff.

*/
char *emc_single_reader(char *e, int *bytes, int rts_id)
{
	struct EMCP *emcp = (struct EMCP *)e ;
	u_int off, len ;
	int hdr_ix, sec_ix ;

	*bytes = 0 ;

	if(!emcp) return 0 ;

	switch(rts_id) {
	case BTOW_ID :
		hdr_ix = 0 ;
		sec_ix = 0 ;
		break ;
	case ETOW_ID :
		hdr_ix = 1 ;
		sec_ix = 0 ;
		break ;
	case ESMD_ID :
		hdr_ix = 1 ;
		sec_ix = 1 ;
		break ;
	default :
		return 0 ;
	}

	if(checkBank(emcp->bh.bank_type, hdrs[hdr_ix].emcp)<0) return 0 ;

	off = b2h32(emcp->sec[sec_ix].off) ;
	len = b2h32(emcp->sec[sec_ix].len) ;

	if((len==0) || (off==0)) return 0 ;

	struct EMCSECP *emcsecp = (struct EMCSECP *)((u_int *)emcp + off) ;

	if(checkBank(emcsecp->bh.bank_type, hdrs[hdr_ix].secp)<0) return 0 ;

	// only 1 fiber
	len = b2h32(emcsecp->fiber[0].len) ;
	off = b2h32(emcsecp->fiber[0].off) ;

	if((len==0) || (off==0)) return 0 ;

	struct EMCRBP *emcrbp = (struct EMCRBP *)((u_int *)emcsecp + off) ;

	if(checkBank(emcrbp->bh.bank_type, hdrs[hdr_ix].rbp)< 0) return 0 ;

	// raw data is in bank 0
	len = b2h32(emcrbp->banks[0].len) ;
	off = b2h32(emcrbp->banks[0].off) ;

	if((len==0) || (off==0)) return 0 ;

	struct DUMMYDATA *emcadc = (DUMMYDATA *) ((u_int *)emcrbp + off) ;

	if(checkBank(emcadc->bh.bank_type, hdrs[hdr_ix].adc)<0) return 0 ;

	
	*bytes = l2h32(emcadc->bh.length)*4 - 40 ;		// length is in words but includes the bankHeader of 40 bytes
	return ((char *)emcadc + 40) ;			// skip the 40 bytes bankHeader
}


	
