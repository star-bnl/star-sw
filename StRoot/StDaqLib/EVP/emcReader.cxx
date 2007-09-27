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

struct emc_t emc ;
#define qswap32(test,x) ((test)?swap32(x):(x))
//________________________________________________________________________________
emc_t::emc_t()
{
  memset(this,0,sizeof(emc_t));
  fenceA=1946;
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


int emcReader(char *m)
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

        emc.check();
        emc.reset();
	if(datap->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapdatap = 1;

	if(datap == NULL) return 0 ;

	bytes = 0 ;

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
			if(len == 0) continue ;

			off = qswap32(swapdatap, datap->det[id].off) ;
			if(off == 0) continue ;

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
			if(len == 0) continue ;	// not even a xtended det

			off = qswap32(swapdatap, datap->det[EXT_ID].off) ;
			if(off == 0) continue ;

			datapx = (struct DATAPX *)(m + off*4) ;

			// verify bank
			if(checkBank(datapx->bh.bank_type, CHAR_DATAPX) < 0) {
				continue ;
			}

			if(datapx->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapdatapx = 1;

			len = qswap32(swapdatapx, datapx->det[id-10].len) * 4 ;
			if(len == 0) continue ;

			off = qswap32(swapdatapx, datapx->det[id-10].off) ;
			if(off == 0) continue ;


			// override "m"
			m = (char *)datapx ;
		}

	

		//LOG(DBG,"EMC %s: bytes %d, off %d",id2char(id),len,off,0,0) ;

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


			//LOG(DBG,"EMC %s: instance %s: %d fibers possible",id2char(id),inst2char(instance),cou,0,0) ;

			for(j=0;j<cou;j++) {
				len = b2h32(emcsecp->fiber[j].len) ;

				if(len == 0) continue ;

				off = b2h32(emcsecp->fiber[j].off) ;

				emcrbp = (struct EMCRBP *)((u_int *)emcsecp + off) ;

				//LOG(DBG,"EMC %s: instance %s: fiber %d: len %d, off %d",id2char(id),inst2char(instance),j+1,len,off) ;

				if(checkBank(emcrbp->bh.bank_type,rbp) < 0) {
					continue ;
				}


				cou2 = (b2h32(emcrbp->bh.length) - 10) /2 ;

				//LOG(DBG,"EMC %s: instance %s: fiber %d: %d banks used",id2char(id),inst2char(instance),j+1,cou2,0) ;

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
						//LOG(ERR,"Unknown subbank %d in EMCRBP!",k,0,0,0,0) ;
						continue ;
					}

					// I currently only know about RAW data
					if(emcadcr == NULL) {
						//LOG(WARN,"EMC %d: instance %d, format %d is not implemented yet!",
						//    id2char(id),inst2char(instance), k,0,0) ;
						continue ;
					}


					if((type==0) && (i == EMC_B_TOW)) {	// barrel tower
						u_short *data ;
						int l, m ;

						emc.btow_in = 1;
						// get to the data: 40 bytes bank header, 4 bytes dummy,
						// 128 bytes fiber header...
						data = (u_short *) ((u_int) emcadcr + 40 + 4 + 128) ; 
					
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
					}
					else if((type==0) && (i == EMC_B_SMD)) {	// barrel SMD
						
						u_short *data ;
						int l ;

						emc.bsmd_in = 1;
						// get to the data: 40 bytes bank header, 4 bytes dummy,
						// 256 bytes fiber header...
						data = (u_short *) ((u_int) emcadcr + 40 + 4 + 256) ; 
					

						emc.bsmd_cap[j] = *(u_char *)((u_int)emcadcr + 40 + 4 + 4*16) ;
						for(l=0;l<4800;l++) {
							emc.bsmd[j][l] = l2h16(*data++) ;
							if(emc.bsmd[j][l] > 0) emc.bsmd_ch++ ;
							//LOG(DBG,"BSMD %d: %d",l,emc.bsmd[j][l]) ;
						}

					}
					else if((type=1) && (i == EMC_B_TOW)) {		// endcap tower
						u_short *data ;
						u_int tlo, thi ;

						int l,m ;

						emc.etow_in = 1;
						// get to the data: 40 bytes bank header, 4 bytes dummy,
						// 128 bytes fiber header...
						// ...but first grab the token from the header...
						data = (u_short *) ((u_int) emcadcr + 40 + 4 + 4) ;
						thi = l2h16(*data) ;
						data = (u_short *) ((u_int) emcadcr + 40 + 4 + 6) ;
						tlo = l2h16(*data) ;

						local_token = thi * 256 + tlo ;

						if(token != local_token) {
							//LOG(ERR,"ETOW: Token in bank %d different from token in data %d",token,local_token,0,0,0) ;
						}

						data = (u_short *) ((u_int) emcadcr + 40 + 4 + 128) ; 
					
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
							//LOG(ERR,"ESMD: Token in bank %d different from token in data %d",token,local_token,0,0,0) ;
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
					
	emc.check();					
	return bytes ;

}

