#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>

#include <rtsLog.h>
#include <rtsSystems.h>

#define TRG_VERSION 0x32               
#include <daqFormats.h>

#include <evpReader.hh>
#include <evpSupport.h>
#include <emcReader.h>

struct emc_t emc ;
char *getEmcTrgData(DATAP *datap,int index);
int readBTOW(u_short *_data, int token);
int readETOW(u_short *_data, int token);

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

int DAQemcReader(char *m);

int emcReader(char *m)
{
  static int oldrun=-1;
  static int oldbytes=-1;
  
  if(m == NULL) return EVP_DATA_ERR ;	// error
  
  evpReader *rrr = (evpReader *)m;
  if(!rrr->mem) {
    LOG(DBG, "No datap:   mem=0x%x sfs=0x%x",rrr->mem,rrr->sfs);
    return EVP_NO_DET;
  }

  if(oldrun == (int)rrr->event_number) {
    return oldbytes;
  };

  oldrun = rrr->event_number;
  oldbytes = DAQemcReader(rrr->mem);

  return oldbytes;
}

int DAQemcReader(char *m) 
{
	struct DATAP *datap = (struct DATAP *)m ;
	struct DATAPX *datapx ;
	struct EMCP *emcp;
	struct EMCSECP *emcsecp ;
	struct EMCRBP *emcrbp ; 
	struct DUMMYDATA *emcadcr, *emcadcd ;
	char *p, *secp, *rbp, *adcr, *adcd ;
	u_int local_token, token ;
	int adcd_found ;
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
	adcd_found = 0 ;

	emc.etow_max_ch = ETOW_MAXFEE*ETOW_DATSIZE ;	
	emc.esmd_max_ch = ESMD_MAXFEE*ESMD_DATSIZE ;	// unknown...


	emc.btow_ch = 0 ;
	emc.bsmd_ch = 0 ;

	emc.etow_ch = 0 ;
	emc.esmd_ch = 0 ;


	emc.btow_in = emc.bsmd_in = emc.bsmd_raw_in = 0 ;
	emc.etow_in = emc.esmd_in = 0 ;

	

	emc.btow_raw = 0 ;
	emc.etow_raw = 0 ;

	if(datap == NULL) return 0 ;


	bytes = 0 ;

	// Now read the emc banks...
	char *trg_btow_data = getEmcTrgData(datap, BTOW_INDEX);

	if(trg_btow_data) {
	  LOG(NOTE, "Getting BTOW data from trigger banks...");
	  readBTOW((u_short *)((u_int)trg_btow_data), token);
	  LOG(DBG, "Got btow data from trigger banks...");
	  bytes += (64 + 2 + 30 * 164) * 2;
	}
	
	char *trg_etow_data = getEmcTrgData(datap, ETOW_INDEX);
	if(trg_etow_data) {
	  LOG(NOTE, "Getting ETOW data from trigger banks...");
	  readETOW((u_short *)((u_int)trg_etow_data), token);
	  bytes += (64 + 2 + 6 * 164) * 2;
	}


	// let's first do the Barrel Tower
	for(type=0;type<2;type++) {	// 0 - Barrel, 1 - Endcap
		if(type==0) {
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

			LOG(DBG, "Checking for datapx: len=%d off=%d",len,off);

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

			LOG(DBG, "Checking for EEC: len=%d off=%d",len,off);

			if((len == 0) || (off == 0)) {
			  continue;
			}

			// override "m"
			m = (char *)datapx ;
		}

	       
		
		LOG(DBG,"EMC %s: bytes %d, off %d",id2char(id),len,off,0,0) ;
		
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
			 
			 
			LOG(DBG,"EMC %s: instance %s: %d fibers possible",id2char(id),inst2char(instance),cou,0,0) ;

			for(j=0;j<cou;j++) {
				len = b2h32(emcsecp->fiber[j].len) ;

				if(len == 0) continue ;

				off = b2h32(emcsecp->fiber[j].off) ;

				emcrbp = (struct EMCRBP *)((u_int *)emcsecp + off) ;

				LOG(DBG,"EMC %s: instance %s: fiber %d: len %d, off %d",id2char(id),inst2char(instance),j+1,len,off) ;

				if(checkBank(emcrbp->bh.bank_type,rbp) < 0) {
					continue ;
				}


				cou2 = (b2h32(emcrbp->bh.length) - 10) /2 ;

				LOG(DBG,"EMC %s: instance %s: fiber %d: %d banks used",id2char(id),inst2char(instance),j+1,cou2,0) ;

				if(cou2 == 2) adcd_found = 1 ;	// must be!
				emcadcr = emcadcd = NULL ;
	
				for(k=0;k<cou2;k++) {	// banks 0 & 1 might exist...
					len = b2h32(emcrbp->banks[k].len) ;

					if(len == 0) continue ;

					off = b2h32(emcrbp->banks[k].off) ;

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
					  readBTOW((u_short *)((u_int)emcadcr + 40), token);
					}
					else if((type==0) && (i == EMC_B_SMD)) {	// barrel SMD
						u_short *data ;
						int l ;

						emc.bsmd_in = 1;
					
						// logic...
						if(emcadcr) { //raw data present


							// get to the data: 40 bytes bank header, 4 bytes dummy,
							// 256 bytes fiber header...
							data = (u_short *) ((u_int) emcadcr + 40 + 4 + 256) ; 
					

							emc.bsmd_cap[j] = *(u_char *)((u_int)emcadcr + 40 + 4 + 4*16) ;
							if(adcd_found) {	// ALSO the ZS present!!!
								emc.bsmd_raw_in = 1 ;
								for(l=0;l<4800;l++) {
									emc.bsmd_raw[j][l] = l2h16(*data++) ;
								}
							}
							else {
								for(l=0;l<4800;l++) {
									emc.bsmd[j][l] = l2h16(*data++) ;
									if(emc.bsmd[j][l] > 0) emc.bsmd_ch++ ;
									
								}
							}

							LOG(DBG,"BSMD raw data present. Sent to the bsmd_raw bank? -- %s",(emc.bsmd_raw_in?"Yes":"No")) ;
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
								
								emc.bsmd[j][ch] = adc ;
								
								//LOG(DBG,"ch %4d = %4d",ch,adc) ;								

								

							}

							emc.bsmd_ch += datums ;
						}

					}
					else if((type==1) && (i == EMC_B_TOW)) {		// endcap tower

					  if(trg_etow_data) {
					    LOG(ERR, "Reading ETOW data in the DAQ banks, but already read it from the trigger banks.");
					  }

					  readETOW((u_short *)((u_int)emcadcr + 40), token);
					}
					else if((type==1) && (i == EMC_B_SMD)) {	// endcap SMD
						
						u_short *data ;
						u_int tlo, thi ;
						int l, m ;

						emc.esmd_in = 1;

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

						data = (u_short *) ((u_int) emcadcr + 40 + 4 + 128) ; 
					
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
//    ie...   data = (u_`int)emcadcr + 40
//                 = (u_int)trg_btow_data
//
int readBTOW(u_short *_data, int token)
{
  u_short *data ;
  int l, m ;
  int thi, tlo, local_token;

  emc.btow_in = 1;

  data = (u_short *)((u_int)_data + 4 + 4);
  thi = l2h16(*data);
  data = (u_short *)((u_int)_data + 4 + 6);
  tlo = l2h16(*data);
  data = (u_short *)((u_int)_data + 4 + 128);

  local_token = thi * 256 + tlo ;

  if(token != local_token) {
    LOG(ERR,"BTOW: Event token different from token in BTOW data %d vs %d (%d,%d)",token,local_token,thi,tlo,0) ;
  }		

  // 4 bytes dummy, 128 bytes fiber header...
  data = (u_short *)((u_int)_data + 4 + 128);
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
//    ie...   data = (u_int)emcadcr + 40
//                 = (u_int)trg_btow_data + 136
//
int readETOW(u_short *_data, int token) {
  u_short *data ;
  u_int tlo, thi ;
  int local_token;

  int l,m ;

  emc.etow_in = 1;
  // get to the data: 40 bytes bank header, 4 bytes dummy,
  // 128 bytes fiber header...
  // ...but first grab the token from the header...

  
  data = (u_short *)((u_int)_data + 4 + 4);
  thi = l2h16(*data);
  data = (u_short *)((u_int)_data + 4 + 6);
  tlo = l2h16(*data);
  data = (u_short *)((u_int)_data + 4 + 128);

  local_token = thi * 256 + tlo ;

  if(token != local_token) {
    LOG(ERR,"ETOW: Event token different from token in ETOW data %d vs %d (%d,%d)",token,local_token,thi,tlo,0) ;
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

char *getEmcTrgData(DATAP *datap, int idx)
{
  struct TRGP *trgp  ;
  struct TRGD *trgd ;
  UINT32 *ptr;

  int len, off ;

  int swapdatap=0;
  int swaptrgp=0;
  int swaptrgd=0;


  LOG(DBG, "Starting emc reader 0x%x",datap);

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

      cptr += off;
      return cptr;
    }
    break;
    
  default :
    LOG(NOTE, "Trigger transfer version 0x%x... no EMC data in trigger banks.", trg_version);
  }


  return NULL;
}
