#include <string.h>
#include <stdio.h>

#include <daqFormats.h>
#include <rtsSystems.h>
#include <rtsLog.h>
#include <fcfClass.hh>

#include <evpReader.hh>
#include <evpSupport.h>
#include <tpcReader.h>
#define ROWS 45
#define PADS_PER_ROW 182

#include <TPC/offsets.h>
//#include <TPC/rowlen.h>

#include <TPC/trans_table.hh>
#include <RTS_READER/rts_reader.h>	// for RR
#include <RTS_READER/daq_dta.h>
#include <RTS_READER/daq_det.h>
#include <DAQ_TPX/daq_tpx.h>		// for RR


struct tpc_t tpc;


static int unpackRaw(int sec, int what, struct TPCPADK *padk, struct TPCCPPR_l *cppr, char *mem) ;
static int unpackCld(int sec, struct TPCMZCLD_local *mzcld) ;
static int dumpGainr(int sec, int rb, struct TPCGAINR *g) ;

static u_int max_ticks ;
static u_int max_clust ;
static u_int max_row ;

int DAQtpcReader(char *m,int sector);
#ifdef TPXREADER
int DAQtpxReader(char *m);
#endif
int tpcReader(char *m, int sector)
{
  static int first=1;
  static int oldsector;
  static int oldrun;
  static int oldbytes;
  
  tpc.channels_sector = 0;

  if(first) {   
    first = 0;
    oldsector = 0;
    oldrun=0;
    oldbytes=0;
  }

  if(m == NULL) return EVP_DATA_ERR ;	// error
  
  evpReader *rrr = (evpReader *)m;

  if((oldsector == sector) &&
     (oldrun == (int)rrr->event_number)) {
    return oldbytes;
  };


  
  // clear mode before
  tpc.mode = 0 ;
  
  // set max channels always
  tpc.max_channels_sector = 512*5692 ;
  tpc.max_channels_all = tpc.max_channels_sector * 24 ;

  // assume no clusters
  tpc.has_clusters = 0 ;

  // TPX hooks!
  if(sector==15) {	// actually 16!
	int some_data = 0 ;
	daq_dta *dta ;
	int tpx_sec = sector + 1 ;

	tpc.has_clusters = 0 ;
	memset(tpc.rdo_present,0,sizeof(tpc.rdo_present)) ;	// hm, this is tricky.. I don't have an "RDO"
	
	dta = rrr->rts_rr->det("tpx")->get("adc",tpx_sec) ;
	if(dta) {
		LOG(NOTE,"TPX: sector %d, ADCs found.",tpx_sec) ;


		memset(tpc.counts,0,sizeof(tpc.counts)) ;


		while(dta->iterate()) {
			int r = dta->row ;	// I count from 1
			if(r==0) continue ;	// TPC does not support unphy. rows so we skip em
			r-- ;			// TPC wants from 0
			int p = dta->pad - 1 ;	// ibid.

			for(u_int i=0;i<dta->ncontent;i++) {
				int tb = dta->adc[i].tb ;
				int adc = dta->adc[i].adc ;
				int cou = tpc.counts[r][p] ;	// shorthand

				// TPC has only 8 bits so we must pass thtough a 10->8 table, arghhhh...
				adc = log10to8_table[adc] ;

				tpc.adc[r][p][cou] = adc ;
				tpc.timebin[r][p][cou] = tb ;
				cou++ ;

				tpc.counts[r][p] = cou ;	// put back...
				tpc.channels_sector++;
			}
		}

		some_data++ ;	// I don't know the bytecount but I'll return something...
	}

	dta = rrr->rts_rr->det("tpx")->get("cld",tpx_sec) ;
	if(dta) {
		LOG(NOTE,"TPX: sector %d, CLDs found",tpx_sec) ;

		tpc.has_clusters = 1 ;
		memset(tpc.cl_counts,0,sizeof(tpc.cl_counts)) ;


		while(dta->iterate()) {
			int r = dta->row ;	// I count from 1
			if(r==0) continue ;	// TPC does not support unphy. rows so we skip em
			r-- ;			// TPC wants from 0

			for(u_int i=0;i<dta->ncontent;i++) {
				int cou = tpc.cl_counts[r] ;	// shorthand

				tpc_cl *c = &tpc.cl[r][cou] ;

				c->p = dta->cld[i].pad ;
				c->t = dta->cld[i].tb ;
				c->charge = dta->cld[i].charge ;
				c->flags = dta->cld[i].flags ;

				c->p1 = dta->cld[i].p1 ;
				c->p2 = dta->cld[i].p2 ;
				c->t1 = dta->cld[i].t1 ;
				c->t2 = dta->cld[i].t2 ;

				cou++ ;

				tpc.cl_counts[r] = cou ;	// put back...

			}
		}

		some_data++ ;
	}

	if(some_data) return some_data ;
	else return EVP_NO_DATA ;     // Just because no tpx, doesn't mean tpc not present!
	//else return EVP_NO_DET;

  }


  if(rrr->mem) {
    int retval = DAQtpcReader(rrr->mem, sector);
    oldbytes = (retval > 0) ? retval : 0;
    oldrun = rrr->event_number;
    oldsector = sector;
    
    return oldbytes;
  }
  else 
    return EVP_NO_DET;



#ifdef OLD_2007
  if(rrr->mem) {
    int retval = DAQtpcReader(rrr->mem,sector);

    oldbytes = (retval > 0) ? retval : 0;

#ifdef TPXREADER
    LOG(DBG, "Check TPX reader...sector=%d",sector+1);
    int ret = tpxReader(m, sector, STYLE_TPC);
    if(ret > 0) {
      LOG(NOTE, "TPX Reader contributed to TPC data");
      oldbytes += ret;
    }
    else {
      LOG(NOTE, "No TPX reader contribution to sector %d",sector+1);
    }
    
#endif
    if(oldbytes <= 0) oldbytes = retval;
    
  }
  else if(rrr->fsr) {
#ifdef TPXREADER
    int zzz = tpxReader(m, sector, STYLE_TPC);
    oldbytes = zzz;
    
    LOG(DBG, "tpx reader returned %d %d %d",sector,oldbytes,zzz);
#endif
  }
  else {
    oldbytes = EVP_NO_DET;
  }

  oldrun = rrr->event_number;
  oldsector = sector;
  
  return oldbytes;
#endif
}

int DAQtpcReader(char *m, int sector)
{
	u_int rb, mz ;
	u_int len  ;
	u_int off ;
	u_int t ;
	u_int first, last ;
	u_int hsec ;
	int ret ;
	u_int tot_bytes ;
	static u_int evt ;

	int retval = 0;

	int swapdatap=0;
	int swaptpcp=0;
	// clear tot_bytes
	tot_bytes = 0 ;	

	struct DATAP *datap ;
	struct TPCP *tpcp  ;
	struct TPCSECP *secp ;
	struct TPCRBP *rbp ;
	struct TPCMZP *mzp ;
	struct TPCSEQD *seqd ;
	struct TPCADCD *adcd ;
	struct TPCADCX *adcx ;
	struct TPCPADK *padk ;
	struct TPCADCR_l *adcr ;
	struct TPCCPPR_l *cppr ;
	struct TPCPEDR *pedr ;
	struct TPCRMSR *rmsr ;
	struct TPCGAINR *gainr ;
	struct TPCBADR *badr ;
	struct TPCMZCLD_local *mzcld ;
	struct TPCSECLP *seclp ;
	struct TPCRBCLP *rbclp ;


//	LOG(DBG,"Sizes %d %d %d %d %d",sizeof(tpc.counts),sizeof(tpc.timebin),
//		sizeof(tpc.adc),sizeof(tpc.cl_counts),sizeof(tpc.cl)) ;




	memset(tpc.rdo_present,0,sizeof(tpc.rdo_present)) ;	// nix RDO presence bit...
	// clear the data part - always
	memset((char *)tpc.counts,0,sizeof(tpc.counts)) ;


	// clear the cluster part - always
	memset(tpc.cl_counts,0,sizeof(tpc.cl_counts)) ;
	memset(tpc.cl_p,0,sizeof(tpc.cl_p)) ;


	max_ticks = max_clust = 0 ;	// for DAQ100 timing...

	if(m == NULL) return EVP_DATA_ERR ;	// error

	datap = (struct DATAP *)m ;

	if(datap->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapdatap = 1;

	len = qswap32(swapdatap, datap->det[TPC_ID].len) ;
	if(len==0) {
	  retval = EVP_NO_DET ;
	  return retval;
	}

	len *= 4 ;	// make it bytes

	off = qswap32(swapdatap, datap->det[TPC_ID].off) ;
	if(off==0) {
	  retval = EVP_NO_DET ;
	  return retval;
	}

	LOG(DBG,"TPCP len %d, off %d",len,off) ;

	tpcp = (struct TPCP *)((u_int *)m + off) ;
	if(checkBank((char *)tpcp,"TPCP") < 0) return EVP_DATA_ERR ;	// wrong bank!
	if(tpcp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swaptpcp = 1;

	t = qswap32(swaptpcp, tpcp->bh.token) ;



	LOG(DBG,"Token %d...",t,0,0,0,0) ;


	// current (i.e. 2002) setup has 2 real sectors packed in
	// one hyper-sector 
	// if "year 1" format we have 12 RB packed in one DAQ "sector"
	if(qswap32(swaptpcp, tpcp->bh.format_number)==1) {
		if((sector%2)==0) {
			first = 0 ;
			last = 6 ;
			hsec = sector ;
		}
		else {
			first = 6 ;
			last = 12 ;
			hsec = sector -1 ;
		}
	}
	else {
		first = 0 ;
		last = 6 ;
		hsec = sector ;
	}


	LOG(DBG,"Wanted %d is in %d, first RB %d, fmt %d",sector,hsec,first,tpcp->bh.format_number,0) ;

	LOG(DBG,"Sector %d: len %d, offset %d, len+offset %d",
	    hsec,tpcp->sb[hsec].len,tpcp->sb[hsec].off,tpcp->sb[hsec].len+tpcp->sb[hsec].off,0) ;
	

	if(tpcp->sb[hsec].len == 0) {
	  retval =  EVP_NO_DATA ;	// no sector present 
	  return retval;
	}

	// TPC can't rely on the tpcp lenghts due to 2sector/crate
	// packing
	tot_bytes = qswap32(swaptpcp, tpcp->sb[hsec].len) * 4 ;

	// hack! we'll assume that the data is equally shared between the
	// 2 sectors in this crate
	tot_bytes /= 2 ;


	secp = (struct TPCSECP *) ((char *)tpcp + qswap32(swaptpcp, tpcp->sb[hsec].off)*4) ;
	if(checkBank((char *)secp,"TPCSECP") < 0) return EVP_DATA_ERR ;


	if(t != b2h32(secp->bh.token)) {
		LOG(CAUTION,"Different token %d in sector %d - should be %d",
		    b2h32(secp->bh.token),hsec,t,0,0) ;
		return EVP_DATA_ERR ;
	}



	// if after April 2002 we may have SECLP following in the reserved WORD9
	LOG(DBG,"Sector %d: format number %d",hsec,b2h32(secp->bh.format_number),0,0,0) ;

	seclp = NULL ;
	if(b2h32(secp->bh.format_number)==2) {
		if(secp->bh.w9) {
			seclp = (struct TPCSECLP *)((u_int *)secp + b2h32(secp->bh.w9)) ;
			if(checkBank((char *)seclp,CHAR_TPCSECLP) < 0) {
				seclp = NULL ;
			}
		}
	}

	// Do the cluster data here...
	if(seclp) {	// Cluster data present!

		for(rb=first;rb<last;rb++) {
			int rrb ;	// the real, TPC RB [0..5]

			if(rb >= 6) {
				rrb = rb - 6 ;
			}
			else {
				rrb = rb ;
			}

			if(seclp->rb[rb].len == 0) {
				LOG(DBG,"RB %d has no CLD data (in SECLP)...",rb,0,0,0,0) ;
				continue ;
			}

			rbclp = (struct TPCRBCLP *) ((char *)seclp + b2h32(seclp->rb[rb].off)*4) ;
			if(checkBank((char *)rbclp,CHAR_TPCRBCLP) < 0) {
				break ;
			}

			// at this point (RBCLP) data is little endian...

			if(t != l2h32(rbclp->bh.token)) {
				LOG(CAUTION,"Different token %d in rb %d - should be %d",
				    l2h32(rbclp->bh.token),rb,t,0,0) ;
				continue ;
			}

			for(mz=0;mz<3;mz++) {
				if(rbclp->mz[mz].len == 0) {
					LOG(DBG,"MZ %d has no CLD data (in RBCLP)...",mz,0,0,0,0) ;
					continue ;
				}

				mzcld = (struct TPCMZCLD_local *)((char *)rbclp + l2h32(rbclp->mz[mz].off)*4) ;
				if(checkBank((char *)mzcld,CHAR_TPCMZCLD) < 0) {
					LOG(ERR,"Bad TPCMZCLD data bank in sector %d, RB %d, MZ %d (t %d)!",
					    sector+1,rrb+1,mz+1,t,0) ;

					break ;

				}

				if(t != l2h32(mzcld->bh.token)) {
					LOG(CAUTION,"Different token %d in mz %d - should be %d",
					    l2h32(mzcld->bh.token),mz,t,0,0) ;
					continue ;
				}

				LOG(DBG,"TPCMZCLD MZ %d, token %d",mz,l2h32(mzcld->bh.token),0,0,0) ;

				ret = unpackCld(sector, mzcld) ;
				if(ret > 0) {	// at least one hit found
					tpc.has_clusters = 1 ;
				}

				if(DAQ_RAW_FORMAT_WORD9 != l2h32(mzcld->bh.w9)) {
					//printf("%d %d %d %d %u %d\n",evt,sector,rb,l2h32(mzcld->bh.bank_id),l2h32(mzcld->bh.w9),ret) ;
					//LOG(DBG,"Sec %d, RB %d, MZ %d: w9 %u, hits %u",sector,rb,l2h32(mzcld->bh.bank_id),l2h32(mzcld->bh.w9),ret) ;
					LOG(DBG,"FCF timing: sec %d, mz %d, hits %d, ms %f",sector+1,rrb*3+mz,ret,((double)l2h32(mzcld->bh.w9)*8.0)/33000.0) ;
				}

			}

		}
	}


	// the raw data parts

	for(rb=first;rb<last;rb++) {
		int rrb ;	// the real, TPC RB [0..5]

		if(rb >= 6) {
			rrb = rb - 6 ;
		}
		else {
			rrb = rb ;
		}

		if(secp->rb[rb].len == 0) {
			LOG(DBG,"Sector %d: RB %d not present in raw data...",sector,rb,0,0,0) ;
			continue ;
		}

		rbp = (struct TPCRBP *) ((char *)secp + b2h32(secp->rb[rb].off)*4) ;
		if(checkBank((char *)rbp,"TPCRBP") < 0) {
			continue ;
		}


		// mark the RB/RDO as present
		tpc.rdo_present[rrb] =  1;

		// at this point (RBP) data is different endianess...

		if(t != l2h32(rbp->bh.token)) {
			LOG(CAUTION,"Different token %d in rb %d - should be %d",
			    l2h32(rbp->bh.token),rb,t,0,0) ;
			continue ;
		}

		for(mz=0;mz<3;mz++) {
			LOG(DBG,"RB %d, MZ %d: len %d, off %d",rb,mz,l2h32(rbp->mz[mz].len),l2h32(rbp->mz[mz].off)) ;
		}

		for(mz=0;mz<3;mz++) {
			if(rbp->mz[mz].len == 0) continue ;



			mzp = (struct TPCMZP *)((char *)rbp + l2h32(rbp->mz[mz].off)*4) ;
			if(checkBank((char *)mzp,"TPCMZP") < 0) {
				LOG(ERR,"Bad TPCMZP data bank in sector %d, RB %d, MZ %d (t %d)!",
				    sector+1,rrb+1,mz+1,t,0) ;

					continue ;

			}

			if(t != l2h32(mzp->bh.token)) {
				LOG(CAUTION,"Different token %d in mz %d - should be %d",
				    l2h32(mzp->bh.token),mz,t,0,0) ;
				continue ;
			}

			LOG(DBG,"TPCMZP token %d",l2h32(mzp->bh.token),0,0,0,0) ;

			adcr = NULL ;
			cppr = NULL ;
			padk = NULL ;
			rmsr = NULL ;
			pedr = NULL ;
			adcd = NULL ;
			seqd = NULL ;


			if(mzp->banks[TPC_MZCLD].len != 0) {
				LOG(DBG,"MZCLD len 0x%08X, off 0x%08X",l2h32(mzp->banks[TPC_MZCLD].len),l2h32(mzp->banks[TPC_MZCLD].off),0,0,0) ;

				mzcld = (struct TPCMZCLD_local *)((char *)mzp + l2h32(mzp->banks[TPC_MZCLD].off)*4) ;
				if(checkBank((char *)mzcld,"TPCMZCLD") < 0)  ;

				// do NOTHING!

			}

			if(mzp->banks[TPC_SEQD].len != 0) {	// RMS too 
				seqd = (struct TPCSEQD *)((char *)mzp + l2h32(mzp->banks[TPC_SEQD].off)*4) ;

				if(checkBank((char *)seqd,CHAR_TPCSEQD) < 0)  ;


			}

			if(mzp->banks[TPC_ADCX].len != 0) {	// RMS too 
				adcx = (struct TPCADCX *)((char *)mzp + l2h32(mzp->banks[TPC_ADCX].off)*4) ;

				if(checkBank((char *)adcx,CHAR_TPCADCX) < 0)  ;
			}



			// what do we have here...
			if((mzp->banks[TPC_ADCD].len != 0) && (tpc.mode==0)) {	// zero-suppressed
				int rr, pp ;


				if(mzp->banks[TPC_SEQD].len == 0) { // huh?
					LOG(WARN,"SEQD has 0 length and ADCD doesn't???",0,0,0,0,0) ;
					continue ;
				}

				seqd = (struct TPCSEQD *)((char *)mzp + l2h32(mzp->banks[TPC_SEQD].off)*4) ;
				adcd = (struct TPCADCD *)((char *)mzp + l2h32(mzp->banks[TPC_ADCD].off)*4) ;

				if(checkBank((char *)seqd,"TPCSEQD") < 0) {
					LOG(ERR,"Bad TPCSEQD data bank in sector %d, RB %d, MZ %d!",
					    sector+1,rrb+1,mz+1,0,0) ;

					break ;
				}

				if(checkBank((char *)adcd,"TPCADCD") < 0) return EVP_DATA_ERR ;

				int len = l2h32(seqd->bh.length) - 10 ;
				len *= 2 ;

				int adccou = 0 ;
				int jj ;


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

						if(pp == 0xff) break ;
					}
					else {
						last = ss & 0x0020 ;
						length = ss & 0x1F ;
						start = (ss & 0x7FC0) >> 6 ;
						stop = start + length ;
							
						for(tbin=start;tbin<stop;tbin++) {
							u_char val ;
							int counter ;

							val = adcd->adc[adccou++] ;
																
							if((rr > 45) || (pp > 182)) {
							  LOG(CRIT, "rr = %d pp=%d",rr,pp);
							}
							counter = tpc.counts[rr-1][pp-1] ;

							if(counter>512) {
							  LOG(CRIT, "%d %d %d counter = %d",ss,rr,pp,counter);
							}


							tpc.adc[rr-1][pp-1][counter] = val ;
							tpc.timebin[rr-1][pp-1][counter] = tbin ;
							tpc.counts[rr-1][pp-1] += 1 ;
							tpc.channels_sector++ ;
						}

						if(last) pp++ ;	// increment the pad for next time
					}


				}

				//printf("Found %d channels\n",tpc.channels-chSave) ;

				continue ;	// don;t look at more banks!
			}


			padk = NULL ;
			if((mzp->banks[TPC_PADK].len != 0)) {	// raw ...
				padk = (struct TPCPADK *)((char *)mzp + l2h32(mzp->banks[TPC_PADK].off)*4) ;
				if(checkBank((char *)padk,"TPCPADK") < 0) return EVP_DATA_ERR ;

			}

			cppr = NULL ;
			if(mzp->banks[TPC_CPPR].len != 0) {

				cppr = (struct TPCCPPR_l *)((char *)mzp + l2h32(mzp->banks[TPC_CPPR].off)*4) ;
				if(checkBank((char *)cppr,"TPCCPPR") < 0) return EVP_DATA_ERR ;
			}



			if((mzp->banks[TPC_ADCR].len != 0) && (tpc.mode==0)) {	// raw ...
				adcr = (struct TPCADCR_l *)((char *)mzp + l2h32(mzp->banks[TPC_ADCR].off)*4) ;

				if(checkBank((char *)adcr,"TPCADCR") < 0) return EVP_DATA_ERR ;

				if(unpackRaw(sector, 0, padk, cppr, (char *)adcr) < 0) {
					LOG(ERR,"Problems in RAW data in sector %d, RB %d, MZ %d - skipping...",
					    sector+1,rrb+1,mz+1,0,0) ;
				}

				LOG(DBG,"TPC Raw data bank in sector %d, RB %d, MZ %d!",
				    sector+1,rrb+1,mz+1,0,0) ;
				continue ;
			}

			if(mzp->banks[TPC_PEDR].len != 0) {	// pedestal data!
				pedr = (struct TPCPEDR *)((char *)mzp + l2h32(mzp->banks[TPC_PEDR].off)*4) ;

				if(checkBank((char *)pedr,"TPCPEDR") < 0) return EVP_DATA_ERR ;

				unpackRaw(sector, 1, padk, cppr, (char *)pedr) ;
				tpc.mode = 1 ;	// pedestal data!
		
			}

			if(mzp->banks[TPC_RMSR].len != 0) {	// RMS too 
				rmsr = (struct TPCRMSR *)((char *)mzp + l2h32(mzp->banks[TPC_RMSR].off)*4) ;

				if(checkBank((char *)rmsr,"TPCRMSR") < 0) return EVP_DATA_ERR ;

				unpackRaw(sector, 2, padk, cppr, (char *)rmsr) ;
				tpc.mode = 1 ;	// pedestal data!

			}

			if(mzp->banks[TPC_GAINR].len != 0) {	// RMS too 
				gainr = (struct TPCGAINR *)((char *)mzp + l2h32(mzp->banks[TPC_GAINR].off)*4) ;

				if(checkBank((char *)gainr,CHAR_TPCGAINR) < 0)  ;
					dumpGainr(sector, rrb, gainr) ;

				}

			if(mzp->banks[TPC_BADR].len != 0) {	// RMS too 
				badr = (struct TPCBADR *)((char *)mzp + l2h32(mzp->banks[TPC_BADR].off)*4) ;

				if(checkBank((char *)badr,CHAR_TPCBADR) < 0)  ;


			}


				
		}
				
	}

#ifdef SOME_OTHER_DAY
	if(tpc.has_clusters) {
		int i, j ;
		for(i=0;i<45;i++) {
			printf("Row %2d: ",i+1) ;
			for(j=0;j<3;j++) {
				if(tpc.cl_p[i][j]) {
					printf(" %d[%2d] ",j,l2h32(*tpc.cl_p[i][j])) ;
				}
			}
			printf("\n") ;
		}
	}
#endif

	LOG(DBG,"FCF: sector %2d: worst timing %d (hits %d); last row %d",sector+1,max_ticks,max_clust,max_row) ;

	if(tpc.mode == 1) {   // hack to prevent pedestal events from having invalid data...
	  for(int r=0;r<45;r++) {
	    for(int p=0;p<182;p++) {
	      if(tpc.counts[r][p] == 0) {
		memset(tpc.adc[r][p], 0, sizeof(tpc.adc[r][p]));
		memset(tpc.timebin[r][p], 0, sizeof(tpc.timebin[r][p]));
	      }
	    }
	  }
	}

	evt++ ;
	return tot_bytes ;
}


/*
	what	== 0	ADCR
	what	== 1	PEDR
	what	== 2	RMSR
*/
static int unpackRaw(int sec, int what, struct TPCPADK *padk, struct TPCCPPR_l *cppr, char *mem)
{
	int i, j, t ;
	u_char row, pad ;
	u_short *cppseq ;
	u_char *adcseq ;
	u_char *adcdata ;
	u_short *cppdata ;

	int timebins, cpps ;

	timebins = 512 ;
	cpps = 31 ;

	adcdata = NULL ;
	cppdata = NULL ;
	

	if(padk == NULL) {
		LOG(WARN,"No PADK? - skipping...",0,0,0,0,0) ;
		return -1 ;
	}
	if(mem == NULL) {
		LOG(WARN,"No DATA? - skipping...",0,0,0,0,0) ;
		return -1 ;
	}


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
	default :
		LOG(ERR,"Unknown case variable %d",what,0,0,0,0) ;
		return -1 ;
	}



	for(i=0;i<384;i++) {
		row = padk->rp[i].row ;
		pad = padk->rp[i].pad ;

		if((row==0xFF) || (pad==0xFF)) continue ;	// unphysical pads...

		// we'll count from 0
		row-- ;
		pad-- ;

		adcseq = (u_char *) adcdata + timebins*i ;


		switch(what) {
		case 0 :
			cppseq = (u_short *)((char *) cppdata + 2*2*32*i) ;	

			for(j=0;j<cpps;j++) {
				u_short start, stop ;
				u_char val ;
				int counter ;



				//if(j==0) {
				//	start = 0 ;
				//	stop = 511 ;
				//}
				//else break ;

				start = l2h16(*cppseq++)  ;
				stop = l2h16(*cppseq++)  ;




				if(start & 0xFE00) break ;
				if(start == 511) stop = 511 ;

				//LOG(DBG,"Doing row %d, pad %d, 0x%04X, 0x%04X",row,pad,start,stop,0) ;


				if((stop < start) || (stop >= timebins)) {
					LOG(WARN,"Bad data stop<start %d<%d - skipping",stop,start,0,0,0) ;
					return -1 ;
				}


				for(t=start;t<=stop;t++) {
					
					val = *(adcseq + t) ;

					if(val == 0) {	// skip...
						// ASIC bug - occasional bad data in timebin 511
						if(t!=511) {
							LOG(WARN,"ADC==0, timebin %d, seq %d, lrow %d, row %d, pad %d",t,j,i,row+1,pad+1) ;
							break ;
							//return -1 ;	// hardware error in the MZ!
						}
					}


					counter = tpc.counts[row][pad] ;
					tpc.adc[row][pad][counter] = val ;
					tpc.timebin[row][pad][counter] = t ;
					tpc.counts[row][pad]++ ;
					tpc.channels_sector++ ;

					//LOG(DBG,"Doing row %d, pad %d, tb %d, ADC %d",row,pad,t,val,0) ;
				}

			}
			break ;
		case 1 :	// PEDR is in tpc.adc!
			for(j=0;j<timebins;j++) {
				tpc.adc[row][pad][j] = *adcseq++ ;
				tpc.channels_sector++ ;	// count only once!
				tpc.counts[row][pad]++; // 
			}
			break ;
		case 2 :	// RMSR is in tpc.timebin!
			for(j=0;j<timebins;j++) {
				tpc.timebin[row][pad][j] = *adcseq++ ;
				tpc.counts[row][pad]++;
			}
			break ;
		}
			

	}
	

	return 0 ;

}


static int unpackCld(int sec, struct TPCMZCLD_local *mzcld)
{
	u_int i, j ;
	u_int nrows, nclust, row ;
	u_int *rdata ;
	u_int tot_hits ;

	tot_hits = 0 ;

	int bytes = l2h32(mzcld->bh.length)*4 ;
	if(bytes == sizeof(struct bankHeader)) {
		LOG(DBG,"No data in TPCMZCLD...",0,0,0,0,0) ;
		return 0 ;
	}

	nrows = l2h32(mzcld->rows) ;

	if(nrows == 0) {
		LOG(NOTE,"No MZCLD rows?...",0,0,0,0,0) ;
		return tot_hits ;
	}

	u_int ticks = l2h32(mzcld->bh.w9) ;


	rdata = (u_int *) mzcld->padrowFiller ;

	LOG(DBG,"unpackCld: 0x%08X 0x%08X %d: 0x%08X 0x%08X",mzcld,rdata,rdata-(u_int*)mzcld,*rdata,*(rdata+1),0) ;

	row = 0 ;

	for(i=0;i<nrows;i++) {

		//LOG(WARN,"Sector %d: 0x%08X 0x%08X 0x%08X 0x%08X",sec,*(rdata+0),*(rdata+1),*(rdata+2),*(rdata+3)) ;

		row = l2h32(*rdata++) ;		// row starts from "1"!
		nclust = l2h32(*rdata++) ;



		int instance ;
		for(instance=0;instance<3;instance++) {
			if(tpc.cl_p[row-1][instance]) continue ;
			tpc.cl_p[row-1][instance] = rdata -2 ;	// go back to the "row" pointer!
			break ;
		}



		if(instance >= 3) {
			LOG(ERR,"Too many row %d contributions!",row) ;
		}

		LOG(DBG,"Row %d (%d of %d), %d clusters (tot bytes %d)...",row,i,nrows,nclust,bytes) ;



		for(j=0;j<nclust;j++) {
			u_short pad, tm, flags, charge, fl ;
			u_int t1, t2, p1, p2 ;
			u_int pt, cf ;

			pt = l2h32(*rdata++) ;
			cf = l2h32(*rdata++) ;


			tm = (pt >> 16) & 0x7FFF ;
			pad = pt & 0x3FFF ;
			charge = cf >> 16 ;
			fl = cf & 0xFFFF ;

			flags = 0 ;
			if(pt & 0x8000) flags |= FCF_DOUBLE_PAD ;
			if(pt & 0x4000) flags |= FCF_DEAD_EDGE ;
			if(pt & (0x8000 << 16)) flags |= FCF_ONEPAD ;
			if(fl & 0x8000) flags |= FCF_ROW_EDGE ;
			if(fl & 0x4000) flags |= FCF_BROKEN_EDGE ;

			t2 = (fl & 0x00F0) >> 4 ;
			t1 = fl & 0x000F ;

			t2 = (tm >> 6) + t2 ;
			t1 = (tm >> 6) - t1 ;

			p2 = (fl & 0x3800) >> 11 ;
			p1 = (fl & 0x0700) >> 8 ;

			p2 = (pad >> 6) + p2 ;
			p1 = (pad >> 6) - p1 ;


			double dpad, dtimebin ;

			dpad = (double)(pad)/64.0 + 0.5 ;	// make pads count from 1.5
			dtimebin = (double)(tm)/64.0 + 0.5 ;	// make timebins count from 0.5

			LOG(DBG,"   pad %d, timebin %d, charge %d, flags 0x%04X, t1 %d, t2 %d, p1 %d, p2 %d",(int)dpad,(int)dtimebin,charge,flags,t1,t2,p1,p2) ;

			u_int ix = tpc.cl_counts[row-1] ;	
			if(ix < TPC_READER_MAX_CLUSTERS) {
				tpc.cl[row-1][ix].p = (float) dpad ;
				tpc.cl[row-1][ix].t = (float) dtimebin ;
				tpc.cl[row-1][ix].charge = charge ;
				tpc.cl[row-1][ix].flags = flags ;
				tpc.cl[row-1][ix].t1 = t1 ;
				tpc.cl[row-1][ix].t2 = t2 ;
				tpc.cl[row-1][ix].p1 = p1 ;
				tpc.cl[row-1][ix].p2 = p2 ;

			


				tpc.cl_counts[row-1]++ ;
				tot_hits++ ;
			}
			else {
				LOG(WARN,"Too many clusters (%d) in sector %d, row %d",ix,sec+1,row,0,0) ;
				return -1 ;
			}

			//printf("%2d %2d %7.4f %7.4f %2d %d\n",
			//       sec, row, pad, timebin, flags, charge) ;

		}

	}

	if(ticks > max_ticks) {
		max_ticks = ticks ;
		max_clust = tot_hits ;
		max_row = row ;
	}

	LOG(DBG,"FCF: sec %d, mz %d (last row %d): %d clust in %u ticks",sec+1,l2h32(mzcld->bh.bank_id),row,tot_hits,ticks) ;

	return tot_hits ;
}

static int dumpGainr(int sec, int rrb, struct TPCGAINR *g)
{
	static int inited ;
	static struct blah {
		u_short row, pad ;
	} seq[6][3][6*256] ;

	int i ;
	int rmz = l2h32(g->bh.bank_id) ;


// 	printf("**** Sector %d: RB %d, MZ %d: events: %d, meanGain %d\n",
// 	    sec,rrb,rmz, l2h32(g->events),l2h32(g->meanGain)) ;

	rmz -= 1 ;	// start from 0 - God knows why anymore...

	if(!inited) {
	u_int vram, row, pad ;

	inited = 1 ;


	int rb, mz ;

	for(rb=1;rb<=6;rb++) {
	for(mz=0;mz<3;mz++) {
	for(i=0;i<MZ_MAX_PADS;i++) {
		int as = i/64 ;

		if(as > 5) break ;


		if(as <= 2) {
			vram = as * 0x8000 ;
		}
		else {
			vram = (as+1)*0x8000 ;
		}

		vram += (i%64)*0x200 ;

		// and shift to get rid of 0s
		vram >>= 9 ;

		// and tack on RB MZ
		vram |= (rb<<12) | (mz<<9);



		for(row=1;row<=45;row++) {
			for(pad=1;pad<=182;pad++) {
			        if((adc_offset[row][pad] & 0xFFFF) == (int)vram) {
					seq[rb-1][mz][i].row = row ;
					seq[rb-1][mz][i].pad = pad ;
					break ;
				}
			}
		}

	}
	}
	}

	}

	for(i=0;i<MZ_MAX_PADS;i++) {
		if(seq[rrb][rmz][i].row == 0) continue ;
//		printf("Sec %d, row %d, pad %d: gain %d\n",sec+1,seq[rrb][rmz][i].row,seq[rrb][rmz][i].pad,g->gain[i].rel_gain) ;
	}


	return 0 ;
}
