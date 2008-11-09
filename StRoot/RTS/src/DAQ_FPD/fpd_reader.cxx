#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>

#include <rtsLog.h>
#include <rts.h>
#include <rtsSystems.h>
#include <daqFormats.h>

#include "daq_fpd.h"

int fpd_reader(char *m, struct fpd_t *fpd, u_int driver)
{
	struct DATAP *datap = (struct DATAP *)m ;
	struct FPDP *fpdp ;
	struct FPDADCD *adcd ;
	struct FPDTDCD *tdcd ;
	struct FPDREGD *regd ;
	struct FPDPEDR *pedr ;
	struct FPDSCL *scl ;
	struct BBCDAT *bbcdat ;
	struct BBCPED *bbcped ;
	struct BBCSCL *bbcscl ;

	int len, off ;
	int i, banks, type ;
	int ch ;

	fpd->mode = 1 ;
	fpd->max_channels = 1 ;
	fpd->channels = 0 ;

	if(datap == NULL) return 0 ;

	len = ntohl(datap->det[FPD_ID].len) * 4 ;
	if(len == 0) return 0 ;

	off = ntohl(datap->det[FPD_ID].off)  ;
	if(off == 0) return 0 ;


	fpdp = (struct FPDP *)((u_int *)m + off) ;

	if(checkBank(fpdp->bh.bank_type,"FPDP") < 0) {
		return -1 ;
	}

	memset(&fpd,0,sizeof(fpd)) ;
	fpd->mode = 1 ;
	fpd->max_channels = 256 ;
	fpd->channels = 0 ;


	banks = (l2h32(fpdp->bh.length) - 10)/2 ;	// number of pointers present

	for(type=0;type<banks;type++) {
		if(fpdp->type[type].len == 0) {
			LOG(DBG,"FPD bank %d not present (len==0)",type,0,0,0,0) ;
			continue  ;
		}
		else {
			LOG(DBG,"FPD bank %d found: len %d qw, off %d qw (%d %d)",type,
			    l2h32(fpdp->type[type].len),l2h32(fpdp->type[type].off),fpdp->type[type].len,fpdp->type[type].off) ;
		}

		switch(type) {
		case FPDP_ADCD :

			adcd = (struct FPDADCD *)((u_int *)fpdp + l2h32(fpdp->type[type].off)) ;
//			adcd = (struct FPDADCD *)((u_int *)fpdp + (fpdp->type[type].off)) ;
			if(checkBank(adcd->bh.bank_type,CHAR_FPDADCD) < 0) {
				return -1 ;
			}

			// could be variable size
			ch = (l2h32(adcd->bh.length) - 10)*2 ;	// size in shorts!
			fpd->channels = ch ;

			if(ch > (int)(sizeof(fpd->adc)/sizeof(fpd->adc[0]))) {
				LOG(ERR,"Counter overflow in FPDADCD! ch == %d",ch,0,0,0,0) ;
				return -1 ;
			}

			for(i=0;i<ch;i++) {
				fpd->adc[i] = l2h16(adcd->data[i]) ;
			}

			break ;
		case FPDP_TDCD :

			tdcd = (struct FPDTDCD *)((u_int *)fpdp + l2h32(fpdp->type[type].off)) ;
			if(checkBank(tdcd->bh.bank_type,CHAR_FPDTDCD) < 0) {
				return -1 ;
			}

			// fixed size 8
			for(i=0;i<8;i++) {
				fpd->tdc[i] = l2h16(tdcd->data[i]) ;
			}

			break ;

		case FPDP_REGD :
			regd = (struct FPDREGD *)((u_int *)fpdp + l2h32(fpdp->type[type].off)) ;
			if(checkBank(regd->bh.bank_type,CHAR_FPDREGD) < 0) {
				return -1 ;
			}

			// fixed size 3
			for(i=0;i<3;i++) {
				fpd->reg[i] = l2h16(regd->data[i]) ;
			}

			break ;

		case FPDP_PEDR :
			pedr = (struct FPDPEDR *)((u_int *)fpdp + l2h32(fpdp->type[type].off)) ;
			if(checkBank(pedr->bh.bank_type,CHAR_FPDPEDR) < 0) {
				return -1 ;
			}

			// fixed sizes...
			for(i=0;i<256;i++) {
				fpd->ped[i] = l2h16(pedr->ped[i]) ;
			}
			for(i=0;i<256;i++) {
				fpd->rms[i] = l2h16(pedr->rms[i]) ;
			}

			break ;
		case FPDP_SCL :
			scl = (struct FPDSCL *)((u_int *)fpdp + l2h32(fpdp->type[type].off)) ;
			if(checkBank(scl->bh.bank_type,CHAR_FPDSCL) < 0) {
				return -1 ;
			}
			
			break ;
		case FPDP_BBCDAT :
			bbcdat = (struct BBCDAT *)((u_int *)fpdp + l2h32(fpdp->type[type].off)) ;
			if(checkBank(bbcdat->bh.bank_type,CHAR_BBCDAT) < 0) {
				return -1 ;
			}

			// fixed sizes...
			for(i=0;i<32;i++) {
				fpd->bbc.pulse[i] = l2h16(bbcdat->pulse[i]) ;
			}
			for(i=0;i<32;i++) {
				fpd->bbc.time[i] = l2h16(bbcdat->time[i]) ;
			}
			for(i=0;i<2;i++) {
				fpd->bbc.proof[i] = l2h16(bbcdat->proof[i]) ;
			}
			for(i=0;i<6;i++) {
				fpd->bbc.spare[i] = l2h16(bbcdat->spare[i]) ;
			}


			break ;
		case FPDP_BBCPED :
			bbcped = (struct BBCPED *)((u_int *)fpdp + l2h32(fpdp->type[type].off)) ;
			if(checkBank(bbcped->bh.bank_type,CHAR_BBCPED) < 0) {
				return -1 ;
			}

			for(i=0;i<32;i++) {
				fpd->bbc.ped[i] = l2h16(bbcped->ped[i]) ;
			}
			for(i=0;i<32;i++) {
				fpd->bbc.rms[i] = l2h16(bbcped->rms[i]) ;
			}
			for(i=0;i<64;i++) {
				fpd->bbc.peaks[i] = l2h16(bbcped->peaks[i]) ;
			}

			break ;
		case FPDP_BBCSCL :
			bbcscl = (struct BBCSCL *)((u_int *)fpdp + l2h32(fpdp->type[type].off)) ;
			if(checkBank(bbcscl->bh.bank_type,CHAR_BBCSCL) < 0) {
				return -1 ;
			}

			for(i=0;i<32;i++) {
				fpd->bbc.scl[i] = l2h16(bbcscl->scl[i]) ;
			}

			break ;


		default :
			LOG(ERR,"Unknown bank id %d in FPDP?",type,0,0,0,0) ;
			break ;
		}

	}

	return len ;

}

