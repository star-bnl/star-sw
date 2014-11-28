#include <sys/types.h>

#include <rtsLog.h>
#include <rtsSystems.h>
#include <daqFormats.h>
#include <rts.h>

#include <DAQ_READER/daq_det.h>

#include "daq_bsmd.h"

const char *hdrs[3] = {
	CHAR_EMCADCR,
	CHAR_EMCADCD,
	CHAR_EMCPEDR
} ;

char *bsmd_reader(char *e, struct bsmd_desc *bsmd_d)
{
	struct EMCP *emcp = (struct EMCP *)e ;
	u_int off, len ;
	int found_some ;

	found_some =  0 ;	// assume we haven't found anything
	memset(bsmd_d,0,sizeof(struct bsmd_desc)) ;

	LOG(DBG,"BSMD: %p",emcp) ;

	if(!emcp) return 0 ;


	if(checkBank(emcp->bh.bank_type, CHAR_EMCP)<0) return 0 ;

	off = b2h32(emcp->sec[1].off) ;	// BSMD is at 1
	len = b2h32(emcp->sec[1].len) ;
	LOG(DBG,"BSMD: sector 1: %d %d",off,len) ;

	if((len==0) || (off==0)) return 0 ;

	struct EMCSECP *emcsecp = (struct EMCSECP *)((u_int *)emcp + off) ;

	if(checkBank(emcsecp->bh.bank_type, CHAR_EMCSECP)<0) return 0 ;

	int fibers = (b2h32(emcsecp->bh.length)-10) / 2 ;

	LOG(DBG,"BSMD: %d fibers",fibers) ;

	for(int f=0;f<fibers;f++) {	// 12 fibers!	

		len = b2h32(emcsecp->fiber[f].len) ;
		off = b2h32(emcsecp->fiber[f].off) ;

		if((len==0) || (off==0)) continue ;

		struct EMCRBP *emcrbp = (struct EMCRBP *)((u_int *)emcsecp + off) ;

		if(checkBank(emcrbp->bh.bank_type, CHAR_EMCRBP)< 0) return 0 ;

		int banks = (b2h32(emcrbp->bh.length) - 10)/2 ;

		LOG(DBG,"BSMD: fiber %d: %d banks",f,banks) ;

		for(int b=0;b<banks;b++) {

			len = b2h32(emcrbp->banks[b].len) ;
			off = b2h32(emcrbp->banks[b].off) ;

			if((len==0) || (off==0)) continue ;

			struct DUMMYDATA *emcadc = (DUMMYDATA *) ((u_int *)emcrbp + off) ;

			if(checkBank(emcadc->bh.bank_type, (char *)hdrs[b])<0) return 0 ;

			// mark the fact that we found at least one good bank
			found_some = 1 ;

			if(b!=0) {	// big endian for pedrms and ZS data!
				bsmd_d->bytes[f][b] = b2h32(emcadc->bh.length)*4 - 40 ;	// length is in words but includes the bankHeader
				bsmd_d->endian[f][b] = 1 ;	// big!
			}
			else {
				bsmd_d->bytes[f][b] = l2h32(emcadc->bh.length)*4 - 40 ;	// length is in words but includes the bankHeader
				bsmd_d->endian[f][b] = 0 ;	// little!
			}

			bsmd_d->dta[f][b] = ((char *)emcadc + 40) ;			// skip the 40 bytes bankHeader

			LOG(DBG,"BSMD: fiber %d: bank %d: bytes %d",f,b,bsmd_d->bytes[f][b]) ;
		}
	}

	LOG(DBG,"bsmd_reader: found some %d",found_some) ;

	if(found_some) return (char *)bsmd_d ;
	else return 0 ;
}


	
