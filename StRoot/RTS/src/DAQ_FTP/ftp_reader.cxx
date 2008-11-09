#include <string.h>
#include <arpa/inet.h>
#include <stdio.h>

#include <daqFormats.h>
#include <rtsSystems.h>
#include <rtsLog.h>
#include <rts.h>

#include "daq_ftp.h"

static int unpackRaw(int sec, int what, struct TPCPADK *padk, struct TPCCPPR_l *cppr, char *mem, ftp_t *ftp) ;

int ftp_reader(char *m, struct ftp_t *ftp, u_int driver)
{
	int sec ;
	int rb, mz ;
	int len ;
	u_int off ;

	struct TPCP *ftpp ;
	struct TPCSECP *secp ;
	struct TPCRBP *rbp ;
	struct TPCMZP *mzp ;
	struct TPCSEQD *seqd ;
	struct TPCADCD *adcd ;
	struct TPCPADK *padk ;
	struct TPCADCR_l *adcr ;
	struct TPCCPPR_l *cppr ;
	struct TPCPEDR *pedr ;
	struct TPCRMSR *rmsr ;
	struct DATAP *datap ;


	// clear total channels 
	ftp->channels = 0 ;

	// clear mode before
	ftp->mode = 0 ;

	// set the max channel num - constant
	ftp->max_channels = 2*10*960*256 ;

	if(m == NULL) return 0 ;
	datap = (struct DATAP *)m ;

	len = ntohl(datap->det[FTP_ID].len) ;
	if(len == 0) return 0 ;
	len *= 4 ;	// make it bytes...

	off = ntohl(datap->det[FTP_ID].off) ;
	if(off == 0) return 0 ;

	ftpp = (struct TPCP *)((u_int *)m + off) ;
	if(checkBank((char *)ftpp,"FTPP") < 0) return -1 ;	// wrong bank!

	// clear the data part
	memset((char *)ftp->counts,0,sizeof(ftp->counts)) ;


	for(sec=0;sec<2;sec++) {
		int last ;


		if(ftpp->sb[sec].len == 0) continue ;

		secp = (struct TPCSECP *) ((char *)ftpp + b2h32(ftpp->sb[sec].off)*4) ;
		if(checkBank((char *)secp,"FTPSECP") < 0) return -1 ;

		last = 10 ;

		for(rb=0;rb<last;rb++) {


			if(secp->rb[rb].len == 0) continue ;

			rbp = (struct TPCRBP *) ((char *)secp + b2h32(secp->rb[rb].off)*4) ;
			if(checkBank((char *)rbp,"FTPRBP") < 0) {
				break ;
				return -1 ;
			}

			// at this point (RBP) data is different endianess...

			for(mz=0;mz<3;mz++) {
				if(rbp->mz[mz].len == 0) continue ;

				mzp = (struct TPCMZP *)((char *)rbp + l2h32(rbp->mz[mz].off)*4) ;
				if(checkBank((char *)mzp,"FTPMZP") < 0) {
					break ;
					return -1 ;
				}

				u_int mz_pix = 0 ;

				// what do we have here...
				if((mzp->banks[TPC_ADCD].len != 0) && (ftp->mode==0)) {	// zero-suppressed


					seqd = (struct TPCSEQD *)((char *)mzp + l2h32(mzp->banks[TPC_SEQD].off)*4) ;
					adcd = (struct TPCADCD *)((char *)mzp + l2h32(mzp->banks[TPC_ADCD].off)*4) ;

					if(checkBank((char *)seqd,"FTPSEQD") < 0) return -1 ;
					if(checkBank((char *)adcd,"FTPADCD") < 0) return -1 ;

					int len = l2h32(seqd->bh.length) - 10 ;
					len *= 2 ;

					int adccou = 0 ;
					int jj ;



					for(jj=0;jj<len;jj++) {
						int start, last, length, stop ;
						u_short ss, f8 ;
						static int rr, pp ;
						int tbin ;
						int frow, fpad ;	// FTPC row, pad

						ss = l2h16(seqd->seq[jj]) ;
						f8 = (ss & 0x8000) ? 1 : 0 ;


						frow = (rr-1)/6 ;
						fpad = ((rr-1)%6)*160 + (pp-1) ;


						if(f8) {	// new pad flags

							pp = (ss & 0x7FFF) % 256 ;	// daqpad
							rr = (ss & 0x7FFF) / 256 ;	// daqrow



							last = 0 ;

							if(pp==0xFF) {	// dummy
								LOG(DBG,"Dummy row %d...",jj,0,0,0,0) ;
								continue ;
							}


						}
						else {
							int counter ;
							last = (ss & 0x0020) ? 1 : 0 ;
							start = (ss & 0x7FC0) >> 6 ;

							length = ss & 0x1F ;

							stop = start + length ;



							for(tbin=start;tbin<stop;tbin++) {
								u_char val ;

								val = adcd->adc[adccou++] ;
								mz_pix++ ;
								if(tbin > 255) continue ;

								//LOG(DBG,"row %d, pad %d, tbin %d, val %d",frow,fpad,tbin,val,0) ;
								counter = ftp->counts[sec][frow][fpad] ;

								ftp->adc[sec][frow][fpad][counter] = val ;
								ftp->timebin[sec][frow][fpad][counter] = tbin ;
								if(ftp->counts[sec][frow][fpad] != 255) {
									(ftp->counts[sec][frow][fpad])++ ;
								}
								ftp->channels++ ;
							}
						}

						if(last) {
							pp++ ;
						}
					}

					LOG(DBG,"FTP: Setor %d, RB %d, MZ %d: ticks %u, pix %u",sec+1,rb+1,mz+1,l2h32(mzp->bh.w9),mz_pix) ;

					//printf("Found %d channels\n",ftp->channels-chSave) ;

					continue ;	// don;t look at more banks!
				}

				padk = NULL ;
				if(mzp->banks[TPC_PADK].len != 0) {	
					padk = (struct TPCPADK *)((char *)mzp + l2h32(mzp->banks[TPC_PADK].off)*4) ;

					if(checkBank((char *)padk,"FTPPADK") < 0) return -1 ;

				}

				cppr = NULL ;
				if(mzp->banks[TPC_CPPR].len != 0) {

					cppr = (struct TPCCPPR_l *)((char *)mzp + l2h32(mzp->banks[TPC_CPPR].off)*4) ;
					if(checkBank((char *)cppr,"FTPCPPR") < 0) return -1 ;
				}

				if((mzp->banks[TPC_ADCR].len != 0) && (ftp->mode==0)) {	// raw ...
					adcr = (struct TPCADCR_l *)((char *)mzp + l2h32(mzp->banks[TPC_ADCR].off)*4) ;

					if(checkBank((char *)adcr,"FTPADCR") < 0) return -1 ;

					if(unpackRaw(sec, 0, padk, cppr, (char *)adcr, ftp) < 0) {
						LOG(ERR,"Problems in RAW data in sector %d, RB %d, MZ %d - skipping...",
						    sec+1,rb+1,mz+1,0,0) ;
					}

					LOG(DBG,"FTP Raw data bank in sector %d, RB %d, MZ %d!",
					    sec+1,rb+1,mz+1,0,0) ;
					continue ;
				}

				if(mzp->banks[TPC_PEDR].len != 0) {	// pedestal data!
					pedr = (struct TPCPEDR *)((char *)mzp + l2h32(mzp->banks[TPC_PEDR].off)*4) ;

					if(checkBank((char *)pedr,"FTPPEDR") < 0) return -1 ;

					unpackRaw(sec, 1, padk, cppr, (char *)pedr, ftp) ;
					ftp->mode = 1 ;	// pedestal data!
			
				}

				if(mzp->banks[TPC_RMSR].len != 0) {	// RMS too 
					rmsr = (struct TPCRMSR *)((char *)mzp + l2h32(mzp->banks[TPC_RMSR].off)*4) ;

					if(checkBank((char *)rmsr,"FTPRMSR") < 0) return -1 ;

					unpackRaw(sec, 2, padk, cppr, (char *)rmsr, ftp) ;
					ftp->mode = 1 ;	// pedestal data!

				}
				
				
			}
				
		}
			
	}

	

	return len ;
}


/*
	what	== 0	ADCR
	what	== 1	PEDR
	what	== 2	RMSR
*/
static int unpackRaw(int sec, int what, struct TPCPADK *padk, struct TPCCPPR_l *cppr, char *mem, ftp_t *ftp)
{
	int i, j, t ;
	u_char row, pad ;
	u_short *cppseq ;
	u_char *adcseq ;
	u_char *adcdata ;
	u_short *cppdata ;
	int frow, fpad ;
	int timebins, cpps ;

	timebins = 512 ;
	cpps = 31 ;



	if(padk == NULL) {
		LOG(WARN,"No PADK? - skipping...",0,0,0,0,0) ;
		return -1 ;
	}
	if(mem == NULL) {
		LOG(WARN,"No DATA? - skipping...",0,0,0,0,0) ;
		return -1 ;
	}

	adcdata = NULL ;
	cppdata = NULL ;

	switch(what) {
	case 0 :	// ADCR 
		adcdata = (u_char *) mem + sizeof(struct TPCADCR_l);

		if(cppr == NULL) {
			LOG(WARN,"No CPPR? - skipping...",0,0,0,0,0) ;
			return -1 ;
		}

		cppdata = (u_short *)((char *)cppr + sizeof(struct TPCCPPR_l)) ;
		break ;
	case 1 :	// PEDR
		adcdata = ((struct TPCPEDR *)mem)->ped ;
		break ;
	case 2 :	// RMSR
		adcdata = ((struct TPCRMSR *)mem)->rms ;
		break ;
	}

	
	for(i=0;i<320;i++) {
		row = padk->rp[i].row ;
		pad = padk->rp[i].pad ;

		if((row==0xFF) || (pad==0xFF)) continue ;


		// we'll count from 0
		row-- ;
		pad-- ;

		// get to the real FTPC row:pad
		frow = row/6 ;
		fpad = (row%6)*160 + pad ;

		adcseq = (u_char *) adcdata + timebins*i ;

		switch(what) {
		case 0 :
			cppseq = (u_short *)((char *) cppdata + 2*2*32*i) ;	
			for(j=0;j<cpps;j++) {
				u_short start, stop ;
				u_char val ;
				int counter ;



				start = l2h16(*cppseq++)  ;
				stop = l2h16(*cppseq++)  ;


				if(start & 0xFE00) break ;
				if(start >= 256) break ;	// no point...

				// redundant for the FTP but anyway...
				if(start == 511) stop = 511 ;

				if((stop < start) || (stop >= timebins)) {
					LOG(WARN,"Bad data stop<start %d<%d - skipping",stop,start,0,0,0) ;
					return -1 ;
				}

				// stop here for SVT
				if(stop >= 256) stop = 255 ;

				for(t=start;t<=stop;t++) {
					
					val = *(adcseq + t) ;

					if(val == 0) {	// skip...
						return -1 ;	// hardware error in the MZ!
					}

					counter = ftp->counts[sec][frow][fpad] ;

					//printf("%d %d\n",t,val) ;
					ftp->adc[sec][frow][fpad][counter] = val ;
					ftp->timebin[sec][frow][fpad][counter] = t ;
					if(ftp->counts[sec][frow][fpad] != 255) {
						ftp->counts[sec][frow][fpad]++ ;
					}
					ftp->channels++ ;
				}

			}
			break ;
		case 1 :	// PEDR is in ftp->adc!
			for(j=0;j<256;j++) {
				u_char val ;
				val = *adcseq++ ;

				ftp->adc[sec][frow][fpad][j] = val ;
				ftp->channels++ ;	// count only once!

			}
			break ;
		case 2 :	// RMSR is in ftp->timebin!
			for(j=0;j<256;j++) {
				u_char val ;
				val = *adcseq++ ;

				ftp->timebin[sec][frow][fpad][j] = val ;
			}
			break ;
		}
			

	}
	

	return 0 ;

}
