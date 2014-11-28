#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>

#include <daqFormats.h>
#include <rtsSystems.h>
#include <rtsLog.h>
#include <rts.h>

#include <DAQ_READER/daq_det.h>


#include <SVT/key_map.h>



//#include <evpReader.hh>
//#include <evpSupport.h>
//#include <svtReader.h>

#include "daq_svt.h"


//int DAQsvtReader(char *m);
static int unpackRaw(int sec, int what, struct SVTANODK *padk, struct TPCCPPR_l *cppr, char *mem, svt_t *svt) ;



 

// m is DATAP
int svt_reader(char *m, struct svt_t *svt, u_int driver)
{
	int sec, i ;
	int rb, mz ;
	int len ;
	u_int off ;
	static int first = 1 ;

	struct DATAP *datap ;
	struct TPCP *svtp  ;
	struct TPCSECP *secp ;
	struct TPCRBP *rbp ;
	struct TPCMZP *mzp ;
	struct TPCSEQD *seqd ;
	struct TPCADCD *adcd ;
	struct SVTANODK *padk ;
	struct TPCADCR_l *adcr ;
	struct TPCCPPR_l *cppr ;
	struct TPCPEDR *pedr ;
	struct TPCRMSR *rmsr ;

	// clear total channels 
	svt->channels = 0 ;

	// clear mode before
	svt->mode = 0 ;


	if(m==NULL) return 0 ;

	datap = (struct DATAP *) m ;

	len = ntohl(datap->det[SVT_ID].len) ;
	if(len == 0) return 0 ;
	len *= 4 ;

	off = ntohl(datap->det[SVT_ID].off) ;
	if(off == 0) return 0 ;

	svtp = (struct TPCP *)((u_int *)m+off) ;
	if(checkBank((char *)svtp,"SVTP") < 0) return -1 ;	// wrong bank!


//	if(first) {	// this can't work anymore since the svt is allocated and not static!
	{
		int j, k ;

		svt->max_channels = 24*3*6*240*128 ;
		// create the remap file which maps from [rb(1..24)][mz(1..3)][hy(1..6)] to
		// barrel, ladder, wafer, hybrid
		for(i=0;i<24;i++) {
			for(j=0;j<3;j++) {
				for(k=0;k<6;k++) {
					svt->B[i][j][k] = (svt_pad_key[i][j][k] & 0xFF000000) >> 24 ;
					svt->L[i][j][k] = (svt_pad_key[i][j][k] & 0x00FF0000) >> 16 ;
					svt->W[i][j][k] = (svt_pad_key[i][j][k] & 0x00000F00) >> 8 ;
					svt->H[i][j][k] = (svt_pad_key[i][j][k] & 0x0000F000) >> 12 ;
				}
			}
		}

		first = 0 ;
	}


	// clear the data part
	memset((char *)svt->counts,0,sizeof(svt->counts)) ;

	for(sec=0;sec<4;sec++) {
		int last ;

		if((b2h32(svtp->bh.format_number)==1) && (sec%2)) continue ;	// 2 sectors packed per offset - no even sectors

		if(svtp->sb[sec].len == 0) continue ;

		LOG(DBG,"SVT sector %d: len %d, off %d",sec+1,svtp->sb[sec].len,svtp->sb[sec].off) ;

		secp = (struct TPCSECP *) ((char *)svtp + b2h32(svtp->sb[sec].off)*4) ;
		if(checkBank((char *)secp,"SVTSECP") < 0) return -1 ;

		// if "year 1" format we have 12 RB packed in one DAQ "sector"
		if(b2h32(svtp->bh.format_number)==1) {
			last = 12 ;
		}
		else {
			last = 6 ;
		}

		for(rb=0;rb<last;rb++) {
			int rsec ;	// the real, SVT sector...
			int rrb ;	// the real, SVT RB	[0..23]

			if(rb >= 6) {
				rsec = sec+1 ;
				rrb = rsec*6 + (rb - 6) ;
			}
			else {
				rsec = sec ;
				rrb = rsec*6 + rb ;
			}

			if(secp->rb[rb].len == 0) continue ;

			rbp = (struct TPCRBP *) ((char *)secp + b2h32(secp->rb[rb].off)*4) ;
			if(checkBank((char *)rbp,"SVTRBP") < 0) return -1 ;

			// at this point (RBP) data is different endianess...

			for(mz=0;mz<3;mz++) {
				if(rbp->mz[mz].len == 0) continue ;

				mzp = (struct TPCMZP *)((char *)rbp + l2h32(rbp->mz[mz].off)*4) ;
				if(checkBank((char *)mzp,"SVTMZP") < 0) return -1 ;


				// what do we have here...
				if((mzp->banks[TPC_ADCD].len != 0) && (svt->mode==0)) {	// zero-suppressed

			     


					seqd = (struct TPCSEQD *)((char *)mzp + l2h32(mzp->banks[TPC_SEQD].off)*4) ;
					adcd = (struct TPCADCD *)((char *)mzp + l2h32(mzp->banks[TPC_ADCD].off)*4) ;

					//printf("(%d %d) -- adcd 0x%x,  seqd 0x%x\n",rb,mz,adcd,seqd);


					//printf("adcx offset = 0x%x, len=%d\n",
					//					       l2h32(mzp->banks[TPC_ADCX].off)*4,
					//     l2h32(mzp->banks[TPC_ADCX].len)*4);


//					TPCADCD *fb = adcd;
  // char buff[10];
//   memset(buff, 0, sizeof(buff));
//   memcpy(buff,fb->bh.bank_type,8);
//   printf("buff = %s\n",buff);
//   printf("len=%d bank_id=%d format=%d 0x%x token=%d w9=%d\n",
// 	 fb->bh.length,
// 	 fb->bh.bank_id,
// 	 fb->bh.format_ver,
// 	 fb->bh.byte_order,
// 	 fb->bh.token,
// 	 fb->bh.w9);

					if(checkBank((char *)seqd,"SVTSEQD") < 0) return -1 ;
					if(checkBank((char *)adcd,"SVTADCD") < 0) return -1 ;

					int len = l2h32(seqd->bh.length) - 10 ;
					len *= 2 ;

					int adccou = 0 ;
					int jj ;



					for(jj=0;jj<len;jj++) {
						int start, last, length, stop ;
						u_short ss, f8 ;
						u_int rr, pp, hy ;
						int tbin ;
						int k ;

						ss = l2h16(seqd->seq[jj]) ;
						f8 = (ss & 0x8000) ? 1 : 0 ;


						if(f8) {	// new pad flags
							pp = (ss & 0x7FFF) % 256 ;	// anode
							hy = (ss & 0x7FFF) / 256 ;	// hybrid

							last = 0 ;

							rr = 0 ;
							// get the ASIC from the hybrid
							for(k=0;k<6;k++) {
								if((svt_pad_key[rrb][mz][k] & 0x7F) == hy) {
									rr = k + 1 ;	// make it from 1
									break ;
								}
							}



							if(rr == 0) {
								if(hy == 0) { // hm, seem like a normal end...
									LOG(DBG,"Hybrid 0: 0x%04X, seq %d of %d",
									    ss,jj,len,0,0) ;
									break ;
								}
								LOG(ERR,"Can't find hybrid 0x%02X in the svt_key for RB %d, MZ %d (seq %d of %d)!",
								    hy,rrb+1,mz+1,jj,len) ;
								break ;
							}

							LOG(DBG,"Hy %d %d %d 0x%02X %d",rrb+1,mz+1,rr,hy,pp) ;

							if(pp == 0xFF) {
								LOG(WARN,"Anode is 0xFF!",0,0,0,0,0) ;
								continue ;
							}

						}
						else {
							last = (ss & 0x0020) ? 1 : 0 ;
							length = ss & 0x1F ;
							start = (ss & 0x7FC0) >> 6 ;
							stop = start + length ;

							if(pp == 1) {
								//LOG(DBG,"Anode 1: RB %d, MZ %d, HY %d: start %d, stop %d",
								//    rrb+1,mz+1,rr,start,stop-1) ;
							}

							for(tbin=start;tbin<stop;tbin++) {
								u_char val ;
								val = adcd->adc[adccou++] ;
			
								int counter = svt->counts[rrb][mz][rr-1][pp-1] ;
								svt->adc[rrb][mz][rr-1][pp-1][counter] = val ;
								svt->timebin[rrb][mz][rr-1][pp-1][counter] = tbin ;
								svt->counts[rrb][mz][rr-1][pp-1] += 1 ;
								svt->channels++ ;
							}
						}

						if(last) {
							pp++ ;
						}
					}


					continue ;	// don;t look at more banks!
				}

				padk = NULL ;
				if(mzp->banks[SVT_ANODK].len != 0) {	// raw ...
					padk = (struct SVTANODK *)((char *)mzp + l2h32(mzp->banks[SVT_ANODK].off)*4) ;

					if(checkBank((char *)padk,"SVTANODK") < 0) return -1 ;
				}


				cppr = NULL ;
				if(mzp->banks[TPC_CPPR].len != 0) {

					cppr = (struct TPCCPPR_l *)((char *)mzp + l2h32(mzp->banks[TPC_CPPR].off)*4) ;
					if(checkBank((char *)cppr,"SVTCPPR") < 0) return -1 ;
				}


				if((mzp->banks[TPC_ADCR].len != 0) && (svt->mode==0)) {	// raw ...
					adcr = (struct TPCADCR_l *)((char *)mzp + l2h32(mzp->banks[TPC_ADCR].off)*4) ;

					if(checkBank((char *)adcr,"SVTADCR") < 0) return -1 ;



					if(unpackRaw(rrb*3+mz, 0, padk, cppr, (char *)adcr, svt) < 0) {
						LOG(ERR,"Problems in RAW data in sector %d, RB %d, MZ %d - skipping...",
						    rsec+1,rrb+1,mz+1,0,0) ;
					}

					LOG(DBG,"SVT Raw data bank in sector %d, RB %d, MZ %d [sec %d, rb %d]!",
					    rsec+1,rrb+1,mz+1,sec,rb) ;
					continue ;
				}

				if(mzp->banks[TPC_PEDR].len != 0) {	// pedestal data!
					pedr = (struct TPCPEDR *)((char *)mzp + l2h32(mzp->banks[TPC_PEDR].off)*4) ;

					if(checkBank((char *)pedr,"SVTPEDR") < 0) return -1 ;

					unpackRaw(rrb*3+mz, 1, padk, cppr, (char *)pedr, svt) ;
					svt->mode = 1 ;	// pedestal data!
			
				}

				if(mzp->banks[TPC_RMSR].len != 0) {	// RMS too 
					rmsr = (struct TPCRMSR *)((char *)mzp + l2h32(mzp->banks[TPC_RMSR].off)*4) ;

					if(checkBank((char *)rmsr,"SVTRMSR") < 0) return -1 ;

					unpackRaw(rrb*3+mz, 2, padk, cppr, (char *)rmsr, svt) ;
					svt->mode = 1 ;	// pedestal data!

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
static int unpackRaw(int sec, int what, struct SVTANODK *padk, struct TPCCPPR_l *cppr, char *mem, svt_t *svt)
{
	int i, j, t ;
	u_char pad ;
	u_short *cppseq ;
	u_char *adcseq ;
	u_char *adcdata ;
	u_short *cppdata ;
	int rb, mz ;
	static u_char svtindex[128] ;
	int timebins, cpps ;

	timebins = 128 ;
	cpps = 8 ;

	if(padk == NULL) {
		LOG(WARN,"No ANODK? - skipping...",0,0,0,0,0) ;
		return -1 ;
	}
	if(mem == NULL) {
		LOG(WARN,"No DATA? - skipping...",0,0,0,0,0) ;
		return -1 ;
	}

	adcdata = NULL ;
	cppdata = NULL ;

	// check SVT version - pre 2001 is excluded!
	int ver = l2h32((((struct TPCRMSR *)mem)->bh.format_number)) ;

	if(ver != 2) {
		LOG(WARN,"Can't work with pre-2001 data - sorry (ver %d)",ver,0,0,0,0) ;
		return -1 ;
	}


	svt->pre = l2h32(padk->pre) ;
	svt->post = l2h32(padk->post) ;
	svt->pedoffset = l2h32(padk->pedOff) ;

	LOG(DBG,"SVT: pre %d, post %d, pedOff %d",svt->pre,svt->post,svt->pedoffset) ;

//	for(i=0;i<6;i++) {
//		fprintf(stderr,"sector %d, hybrid %d : 0x%04X\n",sec,i,padk->hybrids[i]) ;
//	}


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

	// unpack "sec" which is svtrb*3+mz
	mz = sec % 3 ;
	rb = sec / 3 ;


	for(i=0;i<6;i++) {

		for(pad=0;pad<240;pad++) {


		// arghh... one has to be careful with the SVT due to the new packing
		u_int anode_offset = (pad % 0x40) * 128 * 4 + (pad/64) ;

		adcseq = (u_char *) adcdata + i*0x8000 + anode_offset ;

		switch(what) {
		case 0 :
			cppseq = (u_short *)((char *) cppdata + 2*2*8*(i*256+pad)) ;	

			// let's make life easier
			memset(svtindex,0,sizeof(svtindex)) ;

			for(j=0;j<cpps;j++) {
				u_short start, stop ;
				int pre, post ;


				start = l2h16(*cppseq++)  ;
				stop = l2h16(*cppseq++)  ;



				if(start & 0xFE00) break ;

				if(j==7) stop &= 0x7F ;	// ASIC bug - SVT only...
				if(start == 127) stop = 127 ;	// ASIC bug

				if((stop < start) || (stop >= timebins)) {
					LOG(WARN,"Bad data stop<start %d<%d, cpp %d - skipping",stop,start,j,0,0) ;
					return -1 ;
				}

				pre = (int)start - svt->pre ;
				if(pre < 0) start = 0 ;
				else start = pre ;

				post = stop + svt->post ;
				if(post > 127) stop = 127 ;
				else stop = post ;

				for(t=start;t<=stop;t++) {
					svtindex[t] = 1 ;
				}
			}

			for(t=0;t<128;t++) {
				u_char val ;
				int counter ;

				if(svtindex[t]) {	// use it

					val = *(adcseq + t*4) ;	// weird SVT scheme

					if(val == 0) {	// skip...
						// don't on the last timebin - ASIC bug
						//if(t!=127) return -1 ;	// hardware error in the MZ!
					}

					counter = svt->counts[rb][mz][i][pad] ;
					svt->adc[rb][mz][i][pad][counter] = val ;
					svt->timebin[rb][mz][i][pad][counter] = t ;
					svt->counts[rb][mz][i][pad]++ ;

					svt->channels++ ;
				}
			}

			break ;
		case 1 :	// PEDR is in svt->adc!
			for(j=0;j<timebins;j++) {
				svt->adc[rb][mz][i][pad][j] = *(adcseq + j*4) ;
				svt->channels++ ;	// count only once!
			}
			break ;
		case 2 :	// RMSR is in svt->timebin!
			for(j=0;j<timebins;j++) {
				svt->timebin[rb][mz][i][pad][j] = *(adcseq + j*4) ;
			}
			break ;
		}
		}

	}
	

	return 0 ;

}
