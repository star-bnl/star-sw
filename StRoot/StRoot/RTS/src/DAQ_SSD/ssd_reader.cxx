#include <string.h>
#include <arpa/inet.h>

#include <daqFormats.h>
#include <rtsSystems.h>
#include <rtsLog.h>
#include <rts.h>


#include "daq_ssd.h"

static int unpackRaw(int rb, int mz, int what, char *mem, ssd_t *ssd) ;


/* UNUSED 
static struct ssdMap {
	u_char start_ladder ;
	u_char side ;
} ssdMap[4][2] = {
	{ {0,0}, {5,0} },
	{ {10,0}, {15,0} },
	{ {0,1}, {5,1} },
	{ {10,1}, {15,1} }
} ;
*/

int ssd_reader(char *m, struct ssd_t *ssd, u_int driver)
{
	int sec ;
	int rb, mz ;
	int len ;
	u_int off ;
	static int init ;

	struct TPCP *ssdp ;
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
	struct DATAPX *datapx ;



	// clear total channels 
	ssd->channels = 0 ;

	// set the max channel num - constant
	ssd->max_channels = 20*16*2*768 ;


	if(m == NULL) return 0 ;

	datap = (struct DATAP *)m ;

	int swapdatap = 0;
	if(datap->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapdatap = 1;
	
	len = qswap32(swapdatap, datap->det[EXT_ID].len) ;
	if(len == 0) return 0 ;	// not even a xtended det
	off = qswap32(swapdatap, datap->det[EXT_ID].off) ;
	if(off == 0) return 0 ;	// not even a xtended det

	datapx = (struct DATAPX *)((u_int *)m + off) ;

	int swapdatapx = 0;
	if(datapx->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapdatapx = 1;

	// verify bank
	if(checkBank(datapx->bh.bank_type, CHAR_DATAPX) < 0) {
		return 0 ;
	}


	len = qswap32(swapdatapx, datapx->det[SSD_ID-10].len) * 4 ;
	if(len == 0) return 0 ;


	off = qswap32(swapdatapx, datapx->det[SSD_ID-10].off) ;
	if(off == 0) return 0 ;

	ssdp = (struct TPCP *)((u_int *)datapx + off) ;
	if(checkBank((char *)ssdp,"SSDP") < 0) return 0 ;	// wrong bank!


	if(!init) {	// this is the first call
		// fool the system into clearing stuff
		ssd->mode = 1 ;

		init = 1 ;
	}


	// clear the raw contrib
	memset(ssd->raw,0,sizeof(ssd->raw)) ;

	// clear the data part
	memset((char *)ssd->counts,0,sizeof(ssd->counts)) ;

	// clear mode before
	ssd->mode = 0 ;

	u_int occup[4][3] ;
	memset(occup,0,sizeof(occup)) ;

	for(sec=0;sec<1;sec++) {	// just one sector! I kept the for loop for compatibility
		int last ;


		if(ssdp->sb[sec].len == 0) continue ;

		secp = (struct TPCSECP *) ((u_int *)ssdp + b2h32(ssdp->sb[sec].off)) ;
		if(checkBank((char *)secp,"SSDSECP") < 0) return 0 ;

		last = 4 ;	// 4 RBs max!

		for(rb=0;rb<last;rb++) {


			if(secp->rb[rb].len == 0) continue ;

			rbp = (struct TPCRBP *) ((u_int *)secp + b2h32(secp->rb[rb].off)) ;
			if(checkBank((char *)rbp,"SSDRBP") < 0) {
				continue ;
			}

			// at this point (RBP) data is different endianess...

			for(mz=0;mz<2;mz++) {	// 3rd mezzanine will be always empty...
				if(rbp->mz[mz].len == 0) continue ;

				mzp = (struct TPCMZP *)((u_int *)rbp + l2h32(rbp->mz[mz].off)) ;
				if(checkBank((char *)mzp,"SSDMZP") < 0) {
					continue ;
				}

				// what do we have here...
				if((mzp->banks[TPC_ADCD].len != 0) && (ssd->mode==0)) {	// zero-suppressed


					seqd = (struct TPCSEQD *)((u_int *)mzp + l2h32(mzp->banks[TPC_SEQD].off)) ;
					adcd = (struct TPCADCD *)((u_int *)mzp + l2h32(mzp->banks[TPC_ADCD].off)) ;

					if(checkBank((char *)seqd,"SSDSEQD") < 0) continue ;
					if(checkBank((char *)adcd,"SSDADCD") < 0) continue ;

					int len = l2h32(seqd->bh.length) - 10 ;
					len *= 2 ;

					int adccou = 0 ;
					int jj ;

					int rr, pp ;

					rr = pp = 1 ;

					for(jj=0;jj<len;jj++) {
						int start, last, length, stop ;
						u_short ss, f8 ;
						int tbin ;

						ss = l2h16(seqd->seq[jj]) ;
						f8 = ss & 0x8000 ;


						if(f8) {	// new pad flags
							pp = (ss & 0x7FFF) % 256 ;	// pad
							rr = (ss & 0x7FFF) / 256 ;	// row

							//LOG(DBG,"f8: last %d, start %d, stop %d, pp %d, rr %d",last,start,stop,pp,rr) ;
							if(pp == 0xff) break ;
						}
						else {
							last = ss & 0x0020 ;
							length = ss & 0x1F ;
							start = (ss & 0x7FC0) >> 6 ;
							stop = start + length ;

							//LOG(DBG,"last %d, start %d, stop %d, pp %d, rr %d",last,start,stop,pp,rr) ;

							for(tbin=start;tbin<stop;tbin++) {
								u_char val ;
								int counter ;

								val = adcd->adc[adccou++] ;



								//printf("SSD %d:%d strip %d: adc %d\n",rr,pp,tbin,val) ;

								if(val==0) continue ;

								counter = ssd->counts[rr-1][pp-1] ;

								ssd->adc[rr-1][pp-1][counter] = val ;
								ssd->strip[rr-1][pp-1][counter] = tbin ;
								ssd->counts[rr-1][pp-1] += 1 ;
								ssd->channels++ ;

								occup[rb][mz]++ ;
							}

							if(last) pp++ ;	// increment the pad for next time
						}

					}

				LOG(NOTE,"SSD: RB %d, MZ %d: counts %d",rb,mz,occup[rb][mz]) ;

					continue ;	// don;t look at more banks!


				}

				padk = NULL ;
				if(mzp->banks[TPC_PADK].len != 0) {	
					padk = (struct TPCPADK *)((u_int *)mzp + l2h32(mzp->banks[TPC_PADK].off)) ;

					if(checkBank((char *)padk,"SSDPADK") < 0) return 0 ;

				}

				cppr = NULL ;
				if(mzp->banks[TPC_CPPR].len != 0) {

					cppr = (struct TPCCPPR_l *)((u_int *)mzp + l2h32(mzp->banks[TPC_CPPR].off)) ;
					if(checkBank((char *)cppr,"SSDCPPR") < 0) return 0 ;
				}

				if((mzp->banks[TPC_ADCR].len != 0) && (ssd->mode==0)) {	// raw ...
					adcr = (struct TPCADCR_l *)((u_int *)mzp + l2h32(mzp->banks[TPC_ADCR].off)) ;

					if(checkBank((char *)adcr,"SSDADCR") < 0) return 0 ;

					if(unpackRaw(rb, mz, 0, (char *)adcr, ssd) < 0) {
						LOG(ERR,"Problems in RAW data in sector %d, RB %d, MZ %d - skipping...",
						    sec+1,rb+1,mz+1,0,0) ;
					}

					LOG(DBG,"SSD Raw data bank in sector %d, RB %d, MZ %d!",
					    sec+1,rb+1,mz+1,0,0) ;
					continue ;
				}

				if(mzp->banks[TPC_PEDR].len != 0) {	// pedestal data!
					pedr = (struct TPCPEDR *)((u_int *)mzp + l2h32(mzp->banks[TPC_PEDR].off)) ;

					if(checkBank((char *)pedr,"SSDPEDR") < 0) return 0 ;

					unpackRaw(rb, mz, 1,(char *)pedr,ssd) ;
					ssd->mode = 1 ;	// pedestal data!
			
				}

				if(mzp->banks[TPC_RMSR].len != 0) {	// RMS too 
					rmsr = (struct TPCRMSR *)((u_int *)mzp + l2h32(mzp->banks[TPC_RMSR].off)) ;

					if(checkBank((char *)rmsr,"SSDRMSR") < 0) return 0 ;

					unpackRaw(rb, mz, 2, (char *)rmsr, ssd) ;
					ssd->mode = 1 ;	// pedestal data!

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

static int unpackRaw(int rb, int mz, int what, char *mem, ssd_t *ssd)
{

	u_char *adcdata ;
	int as, ch, strip, cou ;
	u_char *dta ;
	int row ;

	if(mem == NULL) {
		LOG(WARN,"No DATA? - skipping...",0,0,0,0,0) ;
		return 0 ;
	}

	if(mz == 2) {
		LOG(WARN,"MZ3 should not exist in SSD's RB %d - skipping...",rb+1,0,0,0,0) ;
	}

	adcdata = NULL ;

	switch(what) {
	case 0 :	// ADCR 
		adcdata = (u_char *) mem + sizeof(struct TPCADCR_l);
		ssd->raw[rb][mz] = adcdata ;
		LOG(NOTE,"SSD rb %d, mz %d: raw...",rb,mz) ;
		break ;
	case 1 :	// PEDR
		adcdata = ((struct TPCPEDR *)mem)->ped ;
		ssd->raw[rb][mz] = adcdata ;
		for(as=0;as<5;as++) {
			row = rb*10+mz*5+as ;	// calc the pseudo-row

			//printf("Doing RB %d, MZ %c: AS %c\n",rb+1,'A'+mz,'A'+as);

			for(ch=0;ch<64;ch++) {
				dta = adcdata + (as*64*512) + (ch*512) ;
				memcpy(ssd->adc[row][ch],dta,192) ;
			}
		}

		ssd->channels += 5*64*192 ;
		return 0 ;
	case 2 :	// RMSR
		adcdata = ((struct TPCRMSR *)mem)->rms ;
		for(as=0;as<5;as++) {
			row = rb*10+mz*5+as ;
			for(ch=0;ch<64;ch++) {
				dta = adcdata + (as*64*512) + (ch*512) ;
				memcpy(ssd->strip[row][ch],dta,192) ;
			}
		}
		return 0 ;

	}



	// I'll go over the first 5 ASIC blocks

	for(as=0;as<5;as++) {
		row = rb*10+mz*5+as ;	// pseudo-row

		for(ch=0;ch<64;ch++) {

			dta = adcdata + (as*64*512) + ch*512 ;

			cou = 0 ;
			for(strip=0;strip<192;strip++) {


				if(*dta) {
					ssd->channels++ ;
					ssd->strip[row][ch][cou] = strip ;
					ssd->adc[row][ch][cou] = *dta ;
					cou++ ;
				}

				dta++ ;
			}

			ssd->counts[row][ch] += cou ;
		}


	}
	

	return 0 ;

}
