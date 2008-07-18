#define PP printf(
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <string.h>
#include <arpa/inet.h>

#include "daqFormats.h"
#include "rtsSystems.h"


#include "evpSupport.h"
#include "ssdReader.h"
using namespace OLDEVP;
namespace OLDEVP {
struct ssd_t ssd ;
}

static int unpackRaw(int rb, int mz, int what, char *mem) ;
//________________________________________________________________________________
ssd_t::ssd_t()
{
  memset(this,0,sizeof(ssd_t));
  fenceA=1946;
  fenceZ=1946;
	// set the max channel num - constant
  max_channels = 20*16*2*768 ;
}
//________________________________________________________________________________
void ssd_t::reset()
{
	// clear total channels 
  ssd.channels = 0 ;
}
//________________________________________________________________________________
int ssd_t::check()
{
 assert(fenceA==1946);
 assert(fenceZ==1946);
 return 0;
}


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
int OLDEVP::ssdReader(char *m)
{
	int sec ;
	int rb, mz ;
	int len ;
	u_int off ;
	static int init=0 ;

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


   ssd.check();
	// clear total channels 
	ssd.reset() ;


	if(m == NULL) return EVP_DATA_ERR ;

	datap = (struct DATAP *)m ;


	len = l2h32(datap->det[EXT_ID].len) ; // Changed by Herb from b2h to l2h Nov 17 2003.
	if(len == 0) return EVP_NO_DET ;	// not even a xtended det
	assert(len>0&&len<99999);	// If this fails, you'll have to fix the endian (l2h to b2h) problem.  Or just
					// move from Solaris to Linux, or vice-versa.
	off = l2h32(datap->det[EXT_ID].off) ; // Changed by Herb from b2h to l2h Nov 17 2003.
	if(off == 0) return EVP_NO_DET ;	// not even a xtended det

	datapx = (struct DATAPX *)((u_int *)m + off) ;

	// verify bank
	if(checkBank(datapx->bh.bank_type, CHAR_DATAPX) < 0) assert(0);


	len = l2h32(datapx->det[SSD_ID-10].len) * 4 ; // Changed by Herb from b2h to l2h Nov 17 2003.
	if(len == 0) return EVP_NO_DET ;


        /**/ if(datapx->bh.byte_order==0x01020304) off = b2h32(datapx->det[SSD_ID-10].off) ; 
        else if(datapx->bh.byte_order==0x04030201) off = l2h32(datapx->det[SSD_ID-10].off) ; 
        else assert(0); // Some type of data corruption, very serious.
	if(off == 0) return EVP_NO_DET ;
        // printf("BBB marker = 0x%08x.  EVP/ssdReader.cxx ssdReader off = 0x%08x.\n",datapx->bh.byte_order,off);

	ssdp = (struct TPCP *)((u_int *)datapx + off) ;
	if(checkBank((char *)ssdp,"SSDP") < 0) assert(0);


	if(!init) {	// this is the first call
		// fool the system into clearing stuff
		ssd.mode = 1 ;

		init = 1 ;
	}

	// clear the raw contrib
	memset(ssd.raw,0,sizeof(ssd.raw)) ;

	// clear the data part
	memset((char *)ssd.counts,0,sizeof(ssd.counts)) ;

	// clear mode before
	ssd.mode = 0 ;


	for(sec=0;sec<1;sec++) {	// just one sector! I kept the for loop for compatibility
		int last, herb ;


		if(ssdp->sb[sec].len == 0) continue ;

		{ int herb=b2h32(ssdp->sb[sec].off); assert(herb>0&&herb<99999); }
		secp = (struct TPCSECP *) ((u_int *)ssdp + b2h32(ssdp->sb[sec].off)) ; 

		if(checkBank((char *)secp,"SSDSECP") < 0) assert(0);

		last = 4 ;	// 4 RBs max!

		for(rb=0;rb<last;rb++) {


			if(secp->rb[rb].len == 0) continue ;

                        herb=b2h32(secp->rb[rb].off); assert(herb>0&&herb<999999); 
			rbp = (struct TPCRBP *) ((u_int *)secp + herb ) ;
			if(checkBank((char *)rbp,"SSDRBP") < 0) assert(0);

			// at this point (RBP) data is different endianess...

			for(mz=0;mz<2;mz++) {	// 3rd mezzanine will be always empty...
				if(rbp->mz[mz].len == 0) continue ;

				{ int herb=l2h32(rbp->mz[mz].off); assert(herb>0&&herb<99999); }
				mzp = (struct TPCMZP *)((u_int *)rbp + l2h32(rbp->mz[mz].off)) ;
				if(checkBank((char *)mzp,"SSDMZP") < 0) assert(0);

				// what do we have here...
				if((mzp->banks[TPC_ADCD].len != 0) && (ssd.mode==0)) {	// zero-suppressed


					{ int herb=l2h32(mzp->banks[TPC_SEQD].off); assert(herb>0&&herb<99999); }
					seqd = (struct TPCSEQD *)((u_int *)mzp + l2h32(mzp->banks[TPC_SEQD].off)) ; 
					{ int herb=l2h32(mzp->banks[TPC_ADCD].off); assert(herb>0&&herb<99999); }
					adcd = (struct TPCADCD *)((u_int *)mzp + l2h32(mzp->banks[TPC_ADCD].off)) ; 

					if(checkBank((char *)seqd,"SSDSEQD") < 0) assert(0);
					if(checkBank((char *)adcd,"SSDADCD") < 0) assert(0);

					int len = l2h32(seqd->bh.length) - 10 ; 
					assert(len<99999);
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

								if(val==0) continue ;

								counter = ssd.counts[rr-1][pp-1] ;

								ssd.adc[rr-1][pp-1][counter] = val ;
								ssd.strip[rr-1][pp-1][counter] = tbin ;
								ssd.counts[rr-1][pp-1] += 1 ;
								ssd.channels++ ;
							}

							if(last) pp++ ;	// increment the pad for next time
						}

					}

					continue ;	// don;t look at more banks!


				}

				padk = NULL ;
				if(mzp->banks[TPC_PADK].len != 0) {	
					{
                                          int herb= l2h32(mzp->banks[TPC_PADK].off); // Changed from b to l for 
                                                                                     // ped data, Herb Mar 3 2004
                                          assert(herb>0&&herb<99999);
                                        }
					padk = (struct TPCPADK *)((u_int *)mzp + l2h32(mzp->banks[TPC_PADK].off)) ; 

					if(checkBank((char *)padk,"SSDPADK") < 0) assert(0);

				}

				cppr = NULL ;
				if(mzp->banks[TPC_CPPR].len != 0) {

					cppr = (struct TPCCPPR_l *)((u_int *)mzp + l2h32(mzp->banks[TPC_CPPR].off)) ; 
					if(checkBank((char *)cppr,"SSDCPPR") < 0) assert(0);
				}

				if((mzp->banks[TPC_ADCR].len != 0) && (ssd.mode==0)) {	// raw ...
					adcr = (struct TPCADCR_l *)((u_int *)mzp + l2h32(mzp->banks[TPC_ADCR].off)) ; 

					if(checkBank((char *)adcr,"SSDADCR") < 0) assert(0);

					if(unpackRaw(rb, mz, 0, (char *)adcr) < 0) {
						// LOG(ERR,"Problems in RAW data in sector %d, RB %d, MZ %d - skipping...",
						//    sec+1,rb+1,mz+1,0,0) ;
					}
					
					// LOG(DBG,"SSD Raw data bank in sector %d, RB %d, MZ %d!",
					//    sec+1,rb+1,mz+1,0,0) ;
					continue ;
				}
				
				if(mzp->banks[TPC_PEDR].len != 0) {	// pedestal data!
					pedr = (struct TPCPEDR *)((u_int *)mzp + l2h32(mzp->banks[TPC_PEDR].off)) ; 

					if(checkBank((char *)pedr,"SSDPEDR") < 0) assert(0);
					
					unpackRaw(rb, mz, 1,(char *)pedr) ;
					 ssd.mode = 1 ;
				}	// pedestal data!
				
				
				if(mzp->banks[TPC_RMSR].len != 0) {	// RMS too 
				  rmsr = (struct TPCRMSR *)((u_int *)mzp + l2h32(mzp->banks[TPC_RMSR].off)) ; 
				  
				  if(checkBank((char *)rmsr,"SSDRMSR") < 0) assert(0);
				  unpackRaw(rb, mz, 2, (char *)rmsr) ;
				  ssd.mode = 1 ;
				}	// pedestal data!
				
			}
			
		}
		
	}
	
	
        ssd.check();
	return len ;
}


/*
	what	== 0	ADCR
	what	== 1	PEDR
	what	== 2	RMSR
*/

static int unpackRaw(int rb, int mz, int what, char *mem)
{

	u_char *adcdata ;
	int as, ch, strip, cou ;
	u_char *dta ;
	int row ;

	if(mem == NULL) {
		// LOG(WARN,"No DATA? - skipping...",0,0,0,0,0) ;
		return -1 ;
	}

	if(mz == 2) {
		// LOG(WARN,"MZ3 should not exist in SSD's RB %d - skipping...",rb+1,0,0,0,0) ;
	}

	adcdata = NULL ;

	switch(what) {
	case 0 :	// ADCR 
		adcdata = (u_char *) mem + sizeof(struct TPCADCR_l);
		ssd.raw[rb][mz] = adcdata ;

		break ;
	case 1 :	// PEDR
		adcdata = ((struct TPCPEDR *)mem)->ped ;
		ssd.raw[rb][mz] = adcdata ;
		for(as=0;as<5;as++) {
			row = rb*10+mz*5+as ;	// calc the pseudo-row
			for(ch=0;ch<64;ch++) {
				dta = adcdata + (as*64*512) + (ch*512) ;
				memcpy(ssd.adc[row][ch],dta,192) ;
			}
		}
		ssd.channels += 5*64*192 ;
		return 0;
	case 2 :	// RMSR
		adcdata = ((struct TPCRMSR *)mem)->rms ;
		for(as=0;as<5;as++) {
			row = rb*10+mz*5+as ;
			for(ch=0;ch<64;ch++) {
				dta = adcdata + (as*64*512) + (ch*512) ;
				memcpy(ssd.strip[row][ch],dta,192) ;
			}
		}
		return 0 ;
	default :printf("WARN,unknown case, do nothing \n"); 
	  break;
	  //LOG(WARN,"unknown case, do nothing \n");
	  
	}


	/*
	// I'll go over the first 5 ASIC blocks
	
	for(as=0;as<5;as++) {
	  row = rb*10+mz*5+as ;	// pseudo-row
	  
	  for(ch=0;ch<64;ch++) {
	    
	    dta = adcdata + (as*64*512) + ch*512 ;
	    
	    cou = 0 ;
	    for(strip=0;strip<192;strip++) {
	      
	      if(*dta) {
		ssd.channels++ ;
		ssd.strip[row][ch][cou] = strip ;
		ssd.adc[row][ch][cou] = *dta ;
		cou++ ;
	      }
	      
	      dta++ ;
	    }
	  }
	  
	  ssd.counts[row][ch] = cou ;
	}
	

	return 0 ;
	*/
	return 0;
}
