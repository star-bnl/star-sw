#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <arpa/inet.h>

//VP#include "rtsLog.h"
#include "rtsSystems.h"
#include "daqFormats.h"

//VP#include "evpReader.hh"
#include "evpSupport.h"
#include "emcReader.h"

#include "TString.h"		// Form()
#include "StMessMgr.h"		// LOG_INFO, LOG_WARN, LOG_ERROR, LOG_DEBUG, etc.
#include <algorithm>

using namespace OLDEVP;
namespace OLDEVP {
struct emc_t emc ;
}
#define SWAP32(bk,x) ((bk->bh.byte_order==0x4030201)?(bk->x):swap32(bk->x))

/*********************** BYTESWAPPING STUFF ***********************/
/* online/RTS/include/rts.h */
#ifdef __linux__
/* linux has its own (fast) swaps */
#include <byteswap.h>

#define swap16(x) bswap_16(x)
#define swap32(x) bswap_32(x)

#else	/* non-linux stuff */

extern inline unsigned short swap16(unsigned short x)
{
	return ((((x) >> 8) & 0xff) | (((x) & 0xff) << 8)) ;
}

extern inline unsigned int swap32(unsigned int x) 
{
     return ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) |               \
      (((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24)) ;
}

#endif /* BYTESWAP */

#define qswap16(test,x) ((test)?swap16(x):(x))
#define qswap32(test,x) ((test)?swap32(x):(x))

static inline unsigned int bswap(unsigned int x)
{
  return swap32(x);
}

//________________________________________________________________________________
emc_t::emc_t()
{
  memset(this,0,sizeof(emc_t));
  fenceA=fenceB=fenceC=fenceD=fenceE=fenceF=fenceG=fenceH=1946;
  fenceZ=1946;
  btow_max_ch = 4800  ;
  bsmd_max_ch = EMC_FIBER_NUM*4800 ;	// extended with BPRE

//	bpre_max_ch = 4800 ;	// unknown...

  etow_max_ch = ETOW_MAXFEE*ETOW_DATSIZE ;	
  esmd_max_ch = ESMD_MAXFEE*ESMD_DATSIZE ;	// unknown...
}
//________________________________________________________________________________
void emc_t::reset()
{
  btow_ch = 0 ;
  bsmd_ch = 0 ;
//	bpre_ch = 0 ;

  etow_ch = 0 ;
  esmd_ch = 0 ;
  btow_in = 0 ;
  bsmd_in = 0 ;
//	bpre_in = 0 ;
  etow_in = 0 ;
  esmd_in = 0 ;
  btow_raw = 0 ;
  etow_raw = 0 ;
}
//________________________________________________________________________________
int emc_t::check()
{
 assert(fenceA==1946);
 assert(fenceB==1946);
 assert(fenceC==1946);
 assert(fenceD==1946);
 assert(fenceE==1946);
 assert(fenceF==1946);
 assert(fenceG==1946);
 assert(fenceH==1946);
 assert(fenceZ==1946);
 return 0;
}
//________________________________________________________________________________
static char *id2char(int id)
{
	switch(id) {
	case BTOW_ID :
		return "BARREL" ;
	case ETOW_ID :
		return "ENDCAP" ;
	default :
		return "unknown" ;
	}
}

static char *inst2char(int inst)
{
	switch(inst) {
	case 1 :
		return "TOWER" ;
	case 2 :
		return "SMD" ;
	default :
		return "UNKNOWN" ;
	}

}

namespace OLDEVP {
char *getEmcTrgData(DATAP *datap,int index);
int readBTOW(u_short *_data, int token);
int readETOW(u_short *_data, int token);
int DAQemcReader(char *m);
}
int OLDEVP::emcReader(char *m)
{
  if (m == NULL) return EVP_DATA_ERR; // error
  return DAQemcReader(m);
}

int OLDEVP::DAQemcReader(char *m) 
{
	struct DATAP *datap = (struct DATAP *)m ;
	struct DATAPX *datapx ;
	struct EMCP *emcp;
	struct EMCSECP *emcsecp ;
	struct EMCRBP *emcrbp ; 
	struct DUMMYDATA *emcadcr, *emcadcd ;
	char *p, *secp, *rbp, *adcr, *adcd ;
	u_int local_token, token ;

	int len, off ;
	int i, j, k ;
	int cou, cou2 ;
	int instance = 0 ;
	int type, id ;

	int bytes ;

	int swapdatap = 0;
	int swapdatapx = 0;
	int swapemcp = 0;

	if(datap->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapdatap = 1;

	token = qswap32(swapdatap, datap->bh.token);

	emc.btow_max_ch = 4800  ;
	emc.bsmd_max_ch = 12*4800 ;	// extended with BPRE

//	emc.bpre_max_ch = 4800 ;	// unknown...

	emc.etow_max_ch = ETOW_MAXFEE*ETOW_DATSIZE ;	
	emc.esmd_max_ch = ESMD_MAXFEE*ESMD_DATSIZE ;	// unknown...


	emc.btow_ch = 0 ;
	emc.bsmd_ch = 0 ;
//	emc.bpre_ch = 0 ;

	emc.etow_ch = 0 ;
	emc.esmd_ch = 0 ;


	emc.btow_in = emc.bsmd_in = 0 ;
//	emc.bpre_in = 0 ;
	emc.etow_in = emc.esmd_in = 0 ;


	emc.btow_raw = 0 ;
	emc.etow_raw = 0 ;

	if(datap == NULL) return 0 ;


	bytes = 0 ;

	// Now read the emc banks...
	//char *trg_btow_data = getEmcTrgData(datap, y8BTOW_INDEX);
	char *trg_btow_data = 0; // BTOW data read elsewhere
	if(trg_btow_data) {
	  LOG_INFO << "Getting BTOW data from trigger banks..." << endm;
	  readBTOW((u_short *)(trg_btow_data), token);
	  bytes += (64 + 2 + 30 * 164) * 2;
	}


	char *trg_etow_data = getEmcTrgData(datap, y8ETOW_INDEX);
	if(trg_etow_data) {
	  LOG_INFO << "Getting ETOW data from trigger banks..." << endm;
	  readETOW((u_short *)(trg_etow_data), token);
	  bytes += (64 + 2 + 6 * 164) * 2;
	}


	// let's first do the Barrel Tower
	for(type=0;type<2;type++) {	// 0 - Barrel, 1 - Endcap
		if(type==0) {
		  continue;	// BTOW data read elsewhere
			id = BTOW_ID ;
			p = "EMCP" ;
			secp = "EMCSECP" ;
			rbp = "EMCRBP" ;
			adcr= "EMCADCR" ;
			adcd = "EMCADCD" ;

			len = qswap32(swapdatap, datap->det[id].len) * 4 ;
			off = qswap32(swapdatap, datap->det[id].off) ;
			if((len == 0) || (off == 0)) {
			  continue;
			}
		}
		else {
			id = ETOW_ID ;
			p = "EECP" ;
			secp = "EECSECP" ;
			rbp = "EECRBP" ;
			adcr= "EECADCR" ;
			adcd = "EECADCD" ;

			// EEC is in DATAPX...
			len = qswap32(swapdatap, datap->det[EXT_ID].len) ;
			off = qswap32(swapdatap, datap->det[EXT_ID].off) ;
			if((len == 0) || (off == 0)) {
			  continue;
			}

			datapx = (struct DATAPX *)(m + off*4) ;

			// verify bank
			if(checkBank(datapx->bh.bank_type, CHAR_DATAPX) < 0) {
				continue ;
			}

			if(datapx->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapdatapx = 1;

			len = qswap32(swapdatapx, datapx->det[id-10].len) * 4 ;
			off = qswap32(swapdatapx, datapx->det[id-10].off) ;
			if((len == 0) || (off == 0)) {
			  continue;
			}

			// override "m"
			m = (char *)datapx ;
		}

	       
		
		LOG_DEBUG << Form("EMC %s: bytes %d, off %d",id2char(id),len,off) << endm;
		
		bytes += len  ;	// save
		
		emcp = (struct EMCP *)((u_int *)m + off) ;
		
		if(checkBank(emcp->bh.bank_type,p) < 0) {
		  return -1 ;
		}
		
		if(emcp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapemcp = 1;
		
		token = qswap32(swapemcp, emcp->bh.token) ;


		// let's see how many contributions (subdetectors) does this event have

		for(i=0;i<3;i++) {	// go through subdets
		  
			len = qswap32(swapemcp, emcp->sec[i].len) ;
			if(len == 0) continue ;

			instance = i + 1 ;	// EMC subinstances star from 1...

			off = qswap32(swapemcp, emcp->sec[i].off) ;

			emcsecp = (struct EMCSECP *)((u_int *)emcp + off) ;

			if(checkBank(emcsecp->bh.bank_type,secp) < 0) {
			  continue ;
			}
			 
			cou = (b2h32(emcsecp->bh.length) - 10) / 2 ;	// contributions!
			 
			 
			LOG_DEBUG << Form("EMC %s: instance %s: %d fibers possible",id2char(id),inst2char(instance),cou) << endm;

			for(j=0;j<cou;j++) {
				len = b2h32(emcsecp->fiber[j].len) ;

				if(len == 0) continue ;

				off = b2h32(emcsecp->fiber[j].off) ;

				emcrbp = (struct EMCRBP *)((u_int *)emcsecp + off) ;

				LOG_DEBUG << Form("EMC %s: instance %s: fiber %d: len %d, off %d",id2char(id),inst2char(instance),j+1,len,off) << endm;

				if(checkBank(emcrbp->bh.bank_type,rbp) < 0) {
					continue ;
				}


				cou2 = (b2h32(emcrbp->bh.length) - 10) /2 ;

				LOG_DEBUG << Form("EMC %s: instance %s: fiber %d: %d banks used",id2char(id),inst2char(instance),j+1,cou2) << endm;

				emcadcr = emcadcd = NULL ;

				for(k=0;k<cou2;k++) {
					len = b2h32(emcrbp->banks[k].len) ;

					if(len == 0) continue ;

					off = b2h32(emcrbp->banks[k].off) ;

					emcadcr = NULL ;

					switch(k) {
					case 0 :	// Raw, ADCR
						emcadcr = (struct DUMMYDATA *)((u_int *)emcrbp + off) ;
						if(checkBank(emcadcr->bh.bank_type,adcr) < 0) {
							continue ;
						}

						break ;
					case 1 :	// zero-suppressed...
						emcadcd = (struct DUMMYDATA *)((u_int *)emcrbp + off) ;
						if(checkBank(emcadcr->bh.bank_type,adcd) < 0) {
							continue ;
						}

						break ;
					default :
					  LOG_ERROR << Form("Unknown subbank %d in EMCRBP!",k) << endm;
						continue ;
					}

					// I currently only know about RAW data
					if(emcadcr == NULL) {
					  LOG_WARN << Form("EMC %d: instance %d, format %d is not implemented yet!",
							   id2char(id),inst2char(instance), k) << endm;
						continue ;
					}


					if((type==0) && (i == EMC_B_TOW)) {	// barrel tower
					  if(trg_btow_data) {
					    LOG_ERROR << "Reading BTOW data from DAQ banks but already read it from trigger banks" << endm;
					  }
					  readBTOW((u_short *)((char *)emcadcr + 40), token);
					}
					else if((type==0) && (i == EMC_B_SMD)) {	// barrel SMD
						
						u_short *data ;
						int l ;

						emc.bsmd_in = 1;
						// get to the data: 40 bytes bank header, 4 bytes dummy,
						// 256 bytes fiber header...
						data = (u_short *) ((char *) emcadcr + 40 + 4 + 256) ; 
					

						emc.bsmd_cap[j] = *(u_char *)((char *)emcadcr + 40 + 4 + 4*16) ;
						for(l=0;l<4800;l++) {
							emc.bsmd[j][l] = l2h16(*data++) ;
							if(emc.bsmd[j][l] > 0) emc.bsmd_ch++ ;
							LOG_DEBUG << Form("BSMD %d: %d",l,emc.bsmd[j][l]) << endm;
						}

					}
					else if((type == 1) && (i == EMC_B_TOW)) {		// endcap tower

					  if(trg_etow_data) {
					    LOG_ERROR << "Reading ETOW data in the DAQ banks, but already read it from the trigger banks." << endm;
					  }

					  readETOW((u_short *)((char*)emcadcr + 40), token);
					}
					else if((type==1) && (i == EMC_B_SMD)) {	// endcap SMD
						
						u_short *data ;
						u_int tlo, thi ;
						int l, m ;

						emc.esmd_in = 1;

						// get to the data: 40 bytes bank header, 4 bytes dummy,
						// 128 bytes fiber header...
						// ...but first grab the token from the header...
						data = (u_short *) ((char*) emcadcr + 40 + 4 + 4) ;
						thi = l2h16(*data) ;
						data = (u_short *) ((char*) emcadcr + 40 + 4 + 6) ;
						tlo = l2h16(*data) ;

						local_token = thi * 256 + tlo ;

						if(token != local_token) {
						  LOG_ERROR << Form("ESMD: Token in bank %d different from token in data %d",token,local_token) << endm;
						}

						data = (u_short *) ((char*) emcadcr + 40 + 4 + 128) ; 
					
						emc.esmd_raw = data ;

						// get the header size...
						if(l2h32(emcadcr->bh.length) < 3000) {	// FY04 data
							emc.esmd_max_fee = 30 ;
						}
						else {
							emc.esmd_max_fee = 48 ;
						}

						emc.esmd_max_ch = emc.esmd_max_fee*ETOW_DATSIZE ;	


						// get the preamble
						for(m=0;m<ESMD_PRESIZE;m++) {
							for(l=0;l<emc.esmd_max_fee;l++) {
								emc.esmd_pre[l][m] = l2h16(*data++) ;
							}
						}

						for(m=0;m<ESMD_DATSIZE;m++) {
							for(l=0;l<emc.esmd_max_fee;l++) {
								emc.esmd[l][m] = l2h16(*data++) ;
								if(emc.esmd[l][m] > 0) emc.esmd_ch++ ;
							}
						}
					}
				}
			}
		}
	}
					
						
	return bytes ;

}

// Starts from after the EMCADCR bankHeader...
//
//    ie...   data = (char*)emcadcr + 40
//                 = (u_int)trg_btow_data
//
int OLDEVP::readBTOW(u_short *_data, int token)
{
  u_short *data ;
  int l, m ;
  int thi, tlo, local_token;

  emc.btow_in = 1;


  data = (u_short *)((char*)_data + 4 + 4);
  thi = l2h16(*data);
  data = (u_short *)((char*)_data + 4 + 6);
  tlo = l2h16(*data);
  data = (u_short *)((char*)_data + 4 + 128);

  local_token = thi * 256 + tlo ;

  if(token != local_token) {
    LOG_ERROR << Form("BTOW: Event token different from token in BTOW data %d vs %d (%d,%d)",token,local_token,thi,tlo) << endm;
  }		


  // 4 bytes dummy, 128 bytes fiber header...
  data = (u_short *)((char*)_data + 4 + 128);
  emc.btow_raw = data ;
						
  // get the preamble
  for(m=0;m<BTOW_PRESIZE;m++) {
    for(l=0;l<BTOW_MAXFEE;l++) {
      emc.btow_pre[l][m] = l2h16(*data++) ;;
    }
  }


  for(m=0;m<BTOW_DATSIZE;m++) {
    for(l=0;l<BTOW_MAXFEE;l++) {
      emc.btow_new[l][m] = l2h16(*data++) ;
    }
  }

  // roll back
  data = emc.btow_raw ;
  data += 120 ;	// skip the preamble of 4X30 shorts

  for(l=0;l<4800;l++) {
    emc.btow[l] = l2h16(*data++) ;
    if(emc.btow[l] > 0) emc.btow_ch++ ;
  }

  return 0;
}



// Starts from after the bankHeader...
//
//    ie...   data = (char*)emcadcr + 40
//                 = (u_int)trg_btow_data + 136
//
int OLDEVP::readETOW(u_short *_data, int token) {
  u_short *data ;
  u_int tlo, thi ;
  int local_token;

  int l,m ;

  emc.etow_in = 1;
  // get to the data: 40 bytes bank header, 4 bytes dummy,
  // 128 bytes fiber header...
  // ...but first grab the token from the header...

  
  data = (u_short *)((char*)_data + 4 + 4);
  thi = l2h16(*data);
  data = (u_short *)((char*)_data + 4 + 6);
  tlo = l2h16(*data);
  data = (u_short *)((char*)_data + 4 + 128);

  local_token = thi * 256 + tlo ;

  if(token != local_token) {
    LOG_ERROR << Form("ETOW: Event token different from token in ETOW data %d vs %d (%d,%d)",token,local_token,thi,tlo) << endm;
  }					
					
  emc.etow_raw = data ;
  
  // get the preamble
  for(m=0;m<ETOW_PRESIZE;m++) {
    for(l=0;l<ETOW_MAXFEE;l++) {
      emc.etow_pre[l][m] = l2h16(*data++) ;;
    }
  }
  

  for(m=0;m<ETOW_DATSIZE;m++) {
    for(l=0;l<ETOW_MAXFEE;l++) {
      emc.etow[l][m] = l2h16(*data++) ;
      if(emc.etow[l][m] > 0) emc.etow_ch++ ;
    }
  }

  // hack!
  //emc.etow[0][0] = local_token ;

  return 0;
}

char* OLDEVP::getEmcTrgData(DATAP* datap, int index)
{
  LOG_INFO << "Starting EMC reader..." << endm;

  // DATA pointer bank
  LOG_INFO << Form("DATAP=08x%x, byte order=0x%08x", datap, datap->bh.byte_order) << endm;
  if (!(datap->det[TRG_ID].off && datap->det[TRG_ID].len)) return 0;
  bool swapdatap = datap->bh.byte_order != DAQ_RAW_FORMAT_ORDER;
  int off = qswap32(swapdatap, datap->det[TRG_ID].off);

  // TRG pointer bank
  TRGP* trgp = (TRGP*)((int*)datap + off) ;
  LOG_INFO << Form("TRGP=0x%08x, byte order=0x%08x", trgp, trgp->bh.byte_order) << endm;
  if (checkBank(trgp->bh.bank_type, "TRGP") < 0) return 0; // Wrong bank!

  if (!trgp->bh.token) {
    LOG_INFO << "Token 0 - skipping..." << endm;
    return 0;
  }

  if (!(trgp->trgData.off && trgp->trgData.len)) return 0;

  bool swaptrgp = trgp->bh.byte_order != DAQ_RAW_FORMAT_ORDER;
  off = qswap32(swaptrgp, trgp->trgData.off);

  // TRG data bank
  TRGD* trgd = (TRGD*)((int*)trgp + off);
  LOG_INFO << Form("TRGD=0x%08x, byte order=0x%08x", trgd, trgd->bh.byte_order) << endm;

  // Check miscellanious things
  if (checkBank(trgd->bh.bank_type, "TRGD") < 0) return 0; // Wrong bank!
	
  bool swaptrgd = trgd->bh.byte_order != DAQ_RAW_FORMAT_ORDER;
  int len = qswap32(swaptrgd, trgd->bh.length);

  if (len == 10) return 0;	// No trigger data, just bank header

  // Swap TrgTowerTrnfer2008
  if (swaptrgd) {
    const size_t SIZE = sizeof(TrgTowerTrnfer2008) / 4;
    unsigned int* p = (unsigned int*)&trgd->tow;
    std::transform(p, p + SIZE, p, bswap);
  }

  TrgTowerTrnfer2008* trgtowertrnfer = &trgd->tow;
  unsigned int byteCount_Version = qswap32(swaptrgd, trgtowertrnfer->byteCount_Version);
  unsigned int byteCount = byteCount_Version >> 8 & 0xffffff;
  unsigned char version  = byteCount_Version & 0xff;

  switch (version) {
  case y8TRANSFER_VERSION: {
    int offset = qswap32(swaptrgd, trgtowertrnfer->OffsetBlock[index].offset);
    int length = qswap32(swaptrgd, trgtowertrnfer->OffsetBlock[index].length);

    if (!(offset && length)) return 0;

    LOG_INFO << Form("TrgTowerTrnfer2008: byte count=%d, version=0x%02x, index=%d, offset=%d, length=%d",
		     byteCount, version, index, offset, length) << endm;

    switch (index) {
    case y8BTOW_INDEX:
      if (length != (64 + 2 + 30 * 164) * 2) {
	LOG_ERROR << Form("Have BTOW in event, but data length incorrect: length=%d, not %d. Ignoring data...",
			  length, (64 + 2 + 30 * 164) * 2) << endm;
	return 0;
      }

      LOG_INFO << Form("Have BTOW data in trigger banks: offset=%d, length=%d", offset, length) << endm;
      break;

    case y8ETOW_INDEX:
      if (length != (64 + 2 + 6 * 164) * 2) {
	LOG_ERROR << Form("Have ETOW in event, but data length incorrect: length=%d, not %d. Ignoring data...",
			  length, (64 + 2 + 6 * 164) * 2) << endm;
	return 0;
      }

      LOG_INFO << Form("Have ETOW data in trigger banks: offset=%d, length=%d", offset, length) << endm;
      break;

    default:
      LOG_ERROR << Form("Unknown transfer data index=%d", index) << endm;
      return 0;
    }

    return (char*)trgtowertrnfer + offset;
    }

 default :
   LOG_INFO << Form("Trigger transfer version 0x%02x. No EMC data in trigger banks.", version) << endm;
   return 0;
  }

  return 0;
}

