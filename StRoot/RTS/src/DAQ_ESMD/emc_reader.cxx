#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>

#include <rtsLog.h>
#include <rtsSystems.h>
#include <rts.h>

#include <daqFormats.h>


#include <DAQ_LEGACY/daq_legacy.h>
	

int emc_reader(char *m, int rts_id, char *retval[12], int retbytes[12]) 
{

	struct EMCP *emcp;
	struct EMCSECP *emcsecp ;
	struct EMCRBP *emcrbp ; 
	struct DUMMYDATA *emcadcr ;


	int len, off ;
	int i, j, k ;
	int cou, cou2 ;
	int subdet ;
	int swapemcp = 0;
	char *p, *secp, *rbp, *adcr ;

	emcp = (EMCP *) legacyDetp(rts_id, m) ;
	if(emcp == 0) return 0 ;


	switch(rts_id) {
	case ETOW_ID :
	case ESMD_ID :
		p = CHAR_EECP ;
		secp = CHAR_EECSECP ;
		rbp = CHAR_EECRBP ;
		adcr= CHAR_EECADCR ;
		break ;
	case BTOW_ID :
	case BSMD_ID :
		p = CHAR_EMCP ;
		secp = CHAR_EMCSECP ;
		rbp = CHAR_EMCRBP ;
		adcr= CHAR_EMCADCR ;
		break ;
	default :
		return 0 ;
	} ;


	switch(rts_id) {
	case ETOW_ID :
	case BTOW_ID :
		subdet = 0 ;
		break ;
	default :
		subdet = 1 ;
		break ;
	} ;




	if(legacyCheckBank(emcp->bh.bank_type,p) < 0) {
	  return 0 ;
	}
	
	if(emcp->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapemcp = 1;
		
	
	int found = 0 ;

	// let's see how many contributions (subdetectors) does this event have

	for(i=0;i<3;i++) {	// go through subdets: 
		// subdet 0: tower data
		// subdet 1: showermax
		// subdet 2: preshower

		len = qswap32(swapemcp, emcp->sec[i].len) ;
		off = qswap32(swapemcp, emcp->sec[i].off) ;

		if(len == 0) continue ;

		
		emcsecp = (struct EMCSECP *)((u_int *)emcp + off) ;

		if(legacyCheckBank(emcsecp->bh.bank_type,secp) < 0) {
		  continue ;
		}
			 
		cou = (b2h32(emcsecp->bh.length) - 10) / 2 ;	// contributions!
			 
		LOG(DBG,"ETOW: subdet/sector %d; fiber counts %d",i,cou) ;			 

		if(i != subdet) continue ;

		// go through fibers
		for(j=0;j<cou;j++) {
			len = b2h32(emcsecp->fiber[j].len) ;
			off = b2h32(emcsecp->fiber[j].off) ;

			if(len == 0) continue ;


			emcrbp = (struct EMCRBP *)((u_int *)emcsecp + off) ;


			if(legacyCheckBank(emcrbp->bh.bank_type,rbp) < 0) {
				continue ;
			}


			cou2 = (b2h32(emcrbp->bh.length) - 10) /2 ;

			if(cou2 > 1) {
				LOG(ERR,"I never coded this bank -- unused!") ;
				continue ;
			}

			for(k=0;k<cou2;k++) {
				len = b2h32(emcrbp->banks[k].len) ;
				off = b2h32(emcrbp->banks[k].off) ;

				if(len == 0) continue ;
				
				if(k!=0) {
					LOG(ERR,"Uncoded") ;
					continue ;
				}

				emcadcr = (struct DUMMYDATA *)((u_int *)emcrbp + off) ;
				if(legacyCheckBank(emcadcr->bh.bank_type,adcr) < 0) {
					continue ;
				}

				
				// get the header size...
				LOG(DBG,"fiber %d: Size of data %d, token %d",j,l2h32(emcadcr->bh.length),l2h32(emcadcr->bh.token)) ;
				if(l2h32(emcadcr->bh.length) < 3000) {	// FY04 data
					// 30 fees
				}
				else {
					// 48 fees
				}


				// skip bank header (10 words) + 1 dummy word due to the DAQ RB
				retval[j] = (char *)((int *)emcadcr + 11) ;	
				retbytes[j] = (l2h32(emcadcr->bh.length) - 11)* 4 ;
				
				found = 1 ;

#if 0
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
#endif
			}
		}
	}
						

	if(found) LOG(NOTE,"Found %s",rts2name(rts_id)) ;
	return found ;

}

