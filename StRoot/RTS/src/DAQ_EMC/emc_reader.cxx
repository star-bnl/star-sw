#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>
#include <assert.h>

#include <rtsLog.h>
#include <rtsSystems.h>
#include <rts.h>

#define TRG_VERSION 0x32               
#include <daqFormats.h>
#include <trgStructures.h>

#include "daq_emc.h"








//static char *getEmcTrgData(char *input, int idx, int *bytes) ;

static int readBTOW(u_short *_data, int token);
static int readETOW(u_short *_data, int token);

static emc_t *emc_p ;

/*
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

*/



static struct emc_describe_t {
	int id ;

	char *p ;
	char *secp ;
	char *rbp ;
	char *adcr ;
	char *adcd ;	
	int trg_index ;
	int type ;	// 0 Barrel, 1 Endcap
	int sub ;	// 0 tower, 1 SMD
} emc_describe[4] = {
	{ BTOW_ID, CHAR_EMCP, CHAR_EMCSECP, CHAR_EMCRBP, CHAR_EMCADCR, CHAR_EMCADCD, BTOW_INDEX, 0, 0 },
	{ BSMD_ID, CHAR_EMCP, CHAR_EMCSECP, CHAR_EMCRBP, CHAR_EMCADCR, CHAR_EMCADCD, -1 , 0, 1},
	{ ETOW_ID, CHAR_EECP, CHAR_EECSECP, CHAR_EECRBP, CHAR_EECADCR, CHAR_EECADCD, ETOW_INDEX, 1, 0 },
	{ ESMD_ID, CHAR_EECP, CHAR_EECSECP, CHAR_EECRBP, CHAR_EECADCR, CHAR_EECADCD, -1, 1, 1 },
} ;


int emc_reader(char *m, struct emc_t *emc, u_int driver, int rts_id, char *ptrs[12], int f_bytes[12]) 
{
#if 1
	return 0 ;
#else
  	struct DATAP *datap = (struct DATAP *)m ;
  	struct DATAPX *datapx ;
  	struct EMCP *emcp;
  	struct EMCSECP *emcsecp ;
  	struct EMCRBP *emcrbp ; 
  	struct DUMMYDATA *dummy ;
  	int len, off ;
  	int err ;

  	int swapdatap = 0;
  	int swapdatapx = 0;
  	int swapemcp = 0;

	// setup
	if(datap == 0) return 0 ;
	if(f_bytes) memset(f_bytes,0,sizeof(f_bytes)) ;
	
	emc_p = emc ;
	u_char emc_wanted[4] ;

	if(emc_p == 0) {	// just the concrete rts_id!
		for(int i=0;i<4;i++) {
			if(emc_describe[i].id == rts_id) {	
				emc_wanted[i] = 1  ;
			}
			else {
				emc_wanted[i] = 0 ;
			}
		}
	}
	else {	// ALL!
		memset(emc,0,sizeof(emc_t)) ;

		emc_p->btow_max_ch = BTOW_MAXFEE*BTOW_DATSIZE  ;
		emc_p->bsmd_max_ch = 12*4800 ;	// extended with BPRE
		emc_p->etow_max_ch = ETOW_MAXFEE*ETOW_DATSIZE ;	
		emc_p->esmd_max_ch = ESMD_MAXFEE*ESMD_DATSIZE ;	// unknown...
	
		memset(emc_wanted,0x1,sizeof(emc_wanted)) ;
	}

	err = 0 ;
	int found = -1 ;
	
	for(int i=0;i<4;i++) {	// go through all EMC typez

	if(emc_wanted[i] == 0) continue ;

	char *dta_ptr = 0 ;
	int bytes = 0 ;

	// let's check for data within the trigger bank first...
	dta_ptr = getEmcTrgData((char *)datap, emc_describe[i].trg_index, &bytes);		
		

	if(dta_ptr) {
		f_bytes[0] = bytes ;
		ptrs[0] = dta_ptr ;
		found = 1 ;
		goto found_specific_emc ;
	}


	// OK, nothing in the Trigger bank, let's continue ;;;

	if(emc_describe[i].type==0) {
		int id = BTOW_ID ;

		len = qswap32(swapdatap, datap->det[id].len) * 4 ;
		off = qswap32(swapdatap, datap->det[id].off) ;
		if((len == 0) || (off == 0)) {
		  continue;
		}
	}
	else {
		int id = ETOW_ID ;

		// EEC is in DATAPX...
		len = qswap32(swapdatap, datap->det[EXT_ID].len) ;
		off = qswap32(swapdatap, datap->det[EXT_ID].off) ;

		LOG(DBG, "Checking for datapx: len=%d off=%d",len,off);

		if((len == 0) || (off == 0)) {
		  continue;
		}

		datapx = (struct DATAPX *)(m + off*4) ;

		// verify bank
		if(checkBank(datapx->bh.bank_type, CHAR_DATAPX) < 0) {
			err = 1 ;
			continue ;
		}

		if(datapx->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapdatapx = 1;

		len = qswap32(swapdatapx, datapx->det[id-10].len) * 4 ;
		off = qswap32(swapdatapx, datapx->det[id-10].off) ;

		LOG(DBG, "Checking for EEC: len=%d off=%d",len,off);

		if((len == 0) || (off == 0)) {
		  continue;
		}

		// override "m"
		m = (char *)datapx ;
	}

	       
	emcp = (struct EMCP *)((u_int *)m + off) ;
		
	if(checkBank(emcp->bh.bank_type,emc_describe[i].p) < 0) {
	  err = 1 ;
	  continue ;
	}
		
	if(emcp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapemcp = 1;
		
	int token = qswap32(swapemcp, emcp->bh.token) ;

	char *name = rts2name(emc_describe[i].id) ;	
	int sub = emc_describe[i].sub ;


	// let's see how many contributions (subdetectors) does this event have

	len = qswap32(swapemcp, emcp->sec[sub].len) ;
	off = qswap32(swapemcp, emcp->sec[sub].off) ;

	if(!len || !off) continue ;	// not found...
	
	emcsecp = (struct EMCSECP *)((u_int *)emcp + off) ;

	if(checkBank(emcsecp->bh.bank_type,emc_describe[i].secp) <= 0) {
	  err = 1 ;
	  continue ;
	}
			 
	int fibers = (b2h32(emcsecp->bh.length) - 10) / 2 ;	// contributions aka fibers
				 
	if(fibers > 12) {
		LOG(ERR,"%s: too many fibers %d > %d",name,fibers,12) ;
		err = 1 ;
		continue ;
	}

	LOG(DBG,"EMC %s: %d fibers possible",name,fibers) ;

	for(int j=0;j<fibers;j++) {	// and now we count the fibers
		len = b2h32(emcsecp->fiber[j].len) ;
		off = b2h32(emcsecp->fiber[j].off) ;

		if(!len || !off) continue ;

		emcrbp = (struct EMCRBP *)((u_int *)emcsecp + off) ;

		if(checkBank(emcrbp->bh.bank_type,emc_describe[i].rbp) <= 0) {
			err = 1 ;
			continue ;
		}

		found = i ;

		int banks = (b2h32(emcrbp->bh.length) - 10) /2 ;

		if(banks > 3) {
			LOG(ERR,"%s: fiber %d: too many banks %d > %d",name,j,banks,3) ;
			err = 1 ;
			continue ;
		}

		LOG(DBG,"EMC %s: fiber %d: %d banks used",name,j,banks) ;

		/* extract stuff from the banks:
			ESMD, BTOW, ETOW: ADCR
			BSMD: ADCR, ADCD, PED_RMS ;
		*/
		
		char *b_ptrs[3] ;
		int b_bytes[3] ;
	
		memset(b_ptrs,0,sizeof(b_ptrs)) ;
		memset(b_bytes,0,sizeof(b_bytes)) ;

		for(int k=0;k<banks;k++) {
			len = b2h32(emcrbp->banks[k].len) ;
			off = b2h32(emcrbp->banks[k].off) ;

			if(!len || !off) continue ;

			bytes = len *4 ;
			dta_ptr = 40 + (char *)emcrbp + off*4 ;	// points to raw data, skipping 40 bytes bankHeader!
			
			dummy = (DUMMYDATA *) ((u_int *)emcrbp + off) ;

			b_bytes[k] = bytes ;
			b_ptrs[k] = dta_ptr ;

			
			switch(k) {
			case 0 :	// ADCR, raw non-ZS data
				if(checkBank(dummy->bh.bank_type, emc_describe[i].adcr)<0) {
					err = 1 ;
					continue ;
				}
				
				break ;
			case 1 :	// ADCD, ZS data -- BSMD only!
				if(checkBank(dummy->bh.bank_type, "EMCADCD")<0) {
					err = 1 ;
					continue ;
				}

				break ;
			case 2 :	// PEDRMSR -- BSMD only!
				if(checkBank(dummy->bh.bank_type, "EMCPEDR")<0) {
					err = 1 ;
					continue ;
				}

				break ;
			default :
				LOG(ERR,"%s: fiber %d: unknown bank at %d",name,j,k) ;
				err = 1 ;
				continue ;
			}

		}	// banks


		if(asdasd & (1<<2)) {	// gimme ZS data!
			f_bytes[j] = b_bytes[1] ;
			ptrs[j] = b_ptrs[1] ;
		}
		else if(b_bytes[2]) {
			f_bytes[j] = b_bytes[2] ;
			ptrs[j] = b_ptrs[1] ;
		}
		else {	
			f_bytes[j] = b_bytes[0] ;
			ptrs[j] = b_ptrs[0] ;
		}

	}	// fibers


	found_specific_emc:

	if(found <0) continue ;	// keep going...


	if(emc==0) {
		if(err) return -1 ;	
		
		return 1 ;
	}

	// here I need to fill in the old emc_t struct....
	if(err) continue ;	
	



	}	// dets

	if(err) return -1 ;
	if(found<0) return 0 ;

	
#if 0

						emcadcr = NULL ;


					emcadcd = NULL ;

					switch(k) {
					case 0 :	// Raw, ADCR
						emcadcr = (struct DUMMYDATA *)((u_int *)emcrbp + off) ;
						if(checkBank(emcadcr->bh.bank_type,adcr) < 0) {
							continue ;
						}

						break ;
					case 1 :	// zero-suppressed...
						emcadcd = (struct DUMMYDATA *)((u_int *)emcrbp + off) ;
						if(checkBank(emcadcd->bh.bank_type,adcd) < 0) {
							continue ;
						}

						break ;
					default :
						LOG(ERR,"Unknown subbank %d in EMCRBP!",k,0,0,0,0) ;
						continue ;
					}

					/* this is not true anymore since BSMD can have ZS data
					// I currently only know about RAW data
					
					if(emcadcr == NULL) {
						LOG(WARN,"EMC %d: instance %d, format %d is not implemented yet!",
						    id2char(id),inst2char(instance), k,0,0) ;
						continue ;
					}
					*/

					if((type==0) && (i == EMC_B_TOW)) {	// barrel tower
					  if(trg_btow_data) {
					    LOG(ERR, "Reading BTOW data from DAQ banks but already read it from trigger banks");
					  }


					  if((rts_id == BTOW_ID) && (emc==0)) {
						  ptrs[0] = (char *) emcadcr + 40 ;
						  f_bytes[0] = l2h32(emcadcr->bh.length)*4 - 40 ;
					  	  return 1 ;
					  }

					  readBTOW((u_short *)((u_int)emcadcr + 40), token);

					}
					else if((type==0) && (i == EMC_B_SMD)) {	// barrel SMD
						u_short *data ;
						int l ;

						emc_p->bsmd_in = 1;
					
						// logic...
						if(emcadcr) { //raw data present


							// get to the data: 40 bytes bank header, 4 bytes dummy,
							// 256 bytes fiber header...
							data = (u_short *) ((u_int) emcadcr + 40 + 4 + 256) ; 
					

							emc_p->bsmd_cap[j] = *(u_char *)((u_int)emcadcr + 40 + 4 + 4*16) ;
							if(adcd_found) {	// ALSO the ZS present!!!
								emc_p->bsmd_raw_in = 1 ;
								for(l=0;l<4800;l++) {
									emc_p->bsmd_raw[j][l] = l2h16(*data++) ;
								}
							}
							else {
								for(l=0;l<4800;l++) {
									emc_p->bsmd[j][l] = l2h16(*data++) ;
									if(emc_p->bsmd[j][l] > 0) emc_p->bsmd_ch++ ;
									
								}
							}

							LOG(DBG,"BSMD raw data present. Sent to the bsmd_raw bank? -- %s",(emc_p->bsmd_raw_in?"Yes":"No")) ;
						}

						if(emcadcd) {

							int datums = b2h32(emcadcd->bh.length)-10-1 ;	// 10 header, 1 fiber:count combo

							LOG(DBG,"BSMD ZS data present, %d hits",datums) ;

							data = (u_short *) ((uint) emcadcd + 40) ;	// skip header
							LOG(DBG,"local fiber %d, channels %d [== %d]",b2h16(data[0]),b2h16(data[1]),datums) ;
							data += 2 ;

							for(l=0;l<datums;l++) {
								int ch = b2h16(*data++) ;
								int adc = b2h16(*data++) ;
								
								emc_p->bsmd[j][ch] = adc ;
								
								//LOG(DBG,"ch %4d = %4d",ch,adc) ;								

								

							}

							emc_p->bsmd_ch += datums ;
						}

					}
					else if((type==1) && (i == EMC_B_TOW)) {		// endcap tower

					  if(trg_etow_data) {
					    LOG(ERR, "Reading ETOW data in the DAQ banks, but already read it from the trigger banks.");
					  }



					  if((rts_id == ETOW_ID) && (emc == 0)) {
					  	ptrs[0] = (char *) emcadcr + 40 ;
					  	f_bytes[0] = l2h32(emcadcr->bh.length)*4 - 40 ;
					  	return 1 ;					  
					  }

					  readETOW((u_short *)((u_int)emcadcr + 40), token);

					}
					else if((type==1) && (i == EMC_B_SMD)) {	// endcap SMD
						
						u_short *data ;
						u_int tlo, thi ;
						int l, m ;

						emc_p->esmd_in = 1;

						// get to the data: 40 bytes bank header, 4 bytes dummy,
						// 128 bytes fiber header...
						// ...but first grab the token from the header...
						data = (u_short *) ((u_int) emcadcr + 40 + 4 + 4) ;
						thi = l2h16(*data) ;
						data = (u_short *) ((u_int) emcadcr + 40 + 4 + 6) ;
						tlo = l2h16(*data) ;

						local_token = thi * 256 + tlo ;

						if(token != local_token) {
							LOG(ERR,"ESMD: Token in bank %d different from token in data %d",token,local_token,0,0,0) ;
						}

						  ptrs[0] = (char *) emcadcr + 40 ;
						  f_bytes[0] = l2h32(emcadcr->bh.length)*4 - 40 ;
						  if((rts_id == ESMD_ID) && (emc == 0)) return 1 ;


						data = (u_short *) ((u_int) emcadcr + 40 + 4 + 128) ; 
					
						emc_p->esmd_raw = data ;

						// get the header size...
						if(l2h32(emcadcr->bh.length) < 3000) {	// FY04 data
							emc_p->esmd_max_fee = 30 ;
						}
						else {
							emc_p->esmd_max_fee = 48 ;
						}

						emc_p->esmd_max_ch = emc_p->esmd_max_fee*ETOW_DATSIZE ;	


						// get the preamble
						for(m=0;m<ESMD_PRESIZE;m++) {
							for(l=0;l<emc_p->esmd_max_fee;l++) {
								emc_p->esmd_pre[l][m] = l2h16(*data++) ;
							}
						}

						for(m=0;m<ESMD_DATSIZE;m++) {
							for(l=0;l<emc_p->esmd_max_fee;l++) {
								emc_p->esmd[l][m] = l2h16(*data++) ;
								if(emc_p->esmd[l][m] > 0) emc_p->esmd_ch++ ;
							}
						}
					}
				}
			}
		}
	}
					
						
	return bytes ;
#endif
#endif

}

// Starts from after the EMCADCR bankHeader...
//
//    ie...   data = (u_`int)emcadcr + 40
//                 = (u_int)trg_btow_data
//
int readBTOW(u_short *_data, int token)
{
  u_short *data ;
  int l, m ;
  int thi, tlo, local_token;

  emc_p->btow_in = 1;

  data = (u_short *)((char *)_data + 4 + 4);
  thi = l2h16(*data);
  data = (u_short *)((char *)_data + 4 + 6);
  tlo = l2h16(*data);
  data = (u_short *)((char *)_data + 4 + 128);

  local_token = thi * 256 + tlo ;

  if(token != local_token) {
    LOG(ERR,"BTOW: Event token different from token in BTOW data %d vs %d (%d,%d)",token,local_token,thi,tlo,0) ;
  }		

  // 4 bytes dummy, 128 bytes fiber header...
  data = (u_short *)((char *)_data + 4 + 128);
  emc_p->btow_raw = data ;
						
  // get the preamble
  for(m=0;m<BTOW_PRESIZE;m++) {
    for(l=0;l<BTOW_MAXFEE;l++) {
      emc_p->btow_pre[l][m] = l2h16(*data++) ;;
    }
  }

  for(m=0;m<BTOW_DATSIZE;m++) {
    for(l=0;l<BTOW_MAXFEE;l++) {
      emc_p->btow_new[l][m] = l2h16(*data++) ;
    }
  }

  // roll back
  data = emc_p->btow_raw ;
  data += 120 ;	// skip the preamble of 4X30 shorts

  for(l=0;l<4800;l++) {
    emc_p->btow[l] = l2h16(*data++) ;
    if(emc_p->btow[l] > 0) emc_p->btow_ch++ ;
  }

  return 0;
}



// Starts from after the bankHeader...
//
//    ie...   data = (u_int)emcadcr + 40
//                 = (u_int)trg_btow_data + 136
//
int readETOW(u_short *_data, int token) {
  u_short *data ;
  u_int tlo, thi ;
  int local_token;

  int l,m ;

  emc_p->etow_in = 1;
  // get to the data: 40 bytes bank header, 4 bytes dummy,
  // 128 bytes fiber header...
  // ...but first grab the token from the header...

  
  data = (u_short *)((char *)_data + 4 + 4);
  thi = l2h16(*data);
  data = (u_short *)((char *)_data + 4 + 6);
  tlo = l2h16(*data);
  data = (u_short *)((char *)_data + 4 + 128);

  local_token = thi * 256 + tlo ;

  if(token != local_token) {
    LOG(ERR,"ETOW: Event token different from token in ETOW data %d vs %d (%d,%d)",token,local_token,thi,tlo,0) ;
  }					
					
  emc_p->etow_raw = data ;
  
  // get the preamble
  for(m=0;m<ETOW_PRESIZE;m++) {
    for(l=0;l<ETOW_MAXFEE;l++) {
      emc_p->etow_pre[l][m] = l2h16(*data++) ;;
    }
  }
  

  for(m=0;m<ETOW_DATSIZE;m++) {
    for(l=0;l<ETOW_MAXFEE;l++) {
      emc_p->etow[l][m] = l2h16(*data++) ;
      if(emc_p->etow[l][m] > 0) emc_p->etow_ch++ ;
    }
  }

  // hack!
  //emc_p->etow[0][0] = local_token ;

  return 0;
}

char *getEmcTrgData(char *input, int idx, int *bytes)
{
  DATAP *datap = (DATAP *) input ;
  struct TRGP *trgp  ;
  struct TRGD *trgd ;
  UINT32 *ptr;

  int len, off ;
  
  *bytes = 0 ;
  if(idx < 0) return 0 ;
  if(datap == 0) return 0 ;

  int swapdatap=0;
  int swaptrgp=0;
  int swaptrgd=0;


  LOG(DBG, "Starting getEmcTrgData 0x%x",datap);
  
  if(datap->bh.byte_order != DAQ_RAW_FORMAT_ORDER) 
    swapdatap = 1;

  len = qswap32(swapdatap, datap->det[TRG_ID].len);
  if(len == 0) return NULL;
  len *= 4 ;

  off = qswap32(swapdatap, datap->det[TRG_ID].off);
  if(off == 0) return NULL;


  ptr = (UINT32 *)datap;
  trgp = (struct TRGP *)(ptr + off) ;
  if(checkBank(trgp->bh.bank_type,"TRGP") < 0) {	// wrong bank!
    return NULL;				
  }

  if(trgp->bh.byte_order != DAQ_RAW_FORMAT_ORDER)
    swaptrgp = 1;

  if(trgp->bh.token == 0) {
    LOG(DBG,"Token 0 - skipping...",0,0,0,0,0) ;
    return NULL;
  }

  if(trgp->trgData.len == 0) return NULL;
  if(trgp->trgData.off == 0) return NULL;

  off = qswap32(swaptrgp, trgp->trgData.off);
  ptr = (u_int *)trgp;
  trgd = (struct TRGD *)(ptr + off) ;

  if(trgd->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swaptrgd = 1;
	
  // check misc. things
  if(checkBank(trgd->bh.bank_type,"TRGD") < 0) {
    return NULL;
  }
	
  LOG(DBG,"TRGD len %d",
      qswap32(swaptrgd, trgd->bh.length),0,0,0) ;

  if(qswap32(swaptrgd, trgd->bh.length) == 10) return NULL;	// no trigger data - just bh


  u_char trg_version = trgd->desc.TrgDataFmtVer ;
  char *cptr;
  TrgTowerTrnfer *trgtowertrnfer;

  switch(trg_version) {
  case 0x10 :

    LOG(NOTE, "TRG transverse version 0x%02x, EMC data supported", trg_version);
  
    
    cptr = (char *)trgd;
    cptr += sizeof(bankHeader);
    
    trgtowertrnfer = (TrgTowerTrnfer *)cptr;
    
    off = qswap32(swaptrgd, trgtowertrnfer->OffsetBlock[idx].offset);
    len = qswap32(swaptrgd, trgtowertrnfer->OffsetBlock[idx].length);

    // Tonko: "len" is _already_ in bytes!!! Arghhhhh...

    LOG(NOTE, "IDX = %d  offset = %d length = %d",
	idx, off, len);

    if(off && len) {
      if(idx == BTOW_INDEX) {

	if(len != (64 + 2 + 30*164)*2) {
	  LOG(ERR, "Have BTOW in event, but data length incorrect: len=%d not %d, ignoring data...",
	      len, (64 + 2 + 30 * 164) * 2);
	  return NULL;
	}

	LOG(NOTE, "Have BTOW data in trigger banks: off=%d, len=%d", off, len);
      }
      else if (idx == ETOW_INDEX) {
	
	if(len != (64 + 2 + 6*164)*2) {
	  LOG(ERR, "Have ETOW in event, but data length incorrect: len=%d not %d.  Ignoring data...",len,
	      (64+2+6*164)*2);
	  return NULL;
	}

	LOG(NOTE, "Have ETOW data in trigger banks: off=%d, len=%d", off, len);
      }

      cptr += off;	// move to data
      *bytes = len  ;	// length: was already in bytes! Contrary to the usual practices....
      return cptr;
    }
    break;
    
  default :
    LOG(NOTE, "Trigger transfer version 0x%x... no EMC data in trigger banks.", trg_version);
  }


  return NULL;
}
