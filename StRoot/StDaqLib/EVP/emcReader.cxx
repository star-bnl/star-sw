#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>
#include <arpa/inet.h>

#include "rtsLog.h"
#include "rtsSystems.h"
#include "daqFormats.h"

#include "evpSupport.h"
#include "emcReader.h"


struct emc emc ;

int getEemcMapmt(int r) { // Added by Herbert Ward.
  return (int)(emc.esmd[r]);
}
int getEemcTower(int r) { // Added by Herbert Ward.
  return (int)(emc.etow[r]);
}

int emcReader(char *m) 
{
	struct DATAP *datap = (struct DATAP *)m ;
	struct DATAPX *datapx ;
	struct EMCP *emcp ;
	struct EMCSECP *emcsecp ;
	struct EMCRBP *emcrbp ; 
	struct DUMMYDATA *emcadcr, *emcadcd ;
	char *p, *secp, *rbp, *adcr, *adcd ;

	int len, off ;
	int i, j, k ;
	int cou, cou2 ;
	int instance = 0 ;
	int type, id ;

	int bytes ;


	emc.btow_max_ch = 4800  ;
	emc.bsmd_max_ch = 8*4800 ;
	emc.bpre_max_ch = 4800 ;	// unknown...

	emc.etow_max_ch = 4800 ;	// unknown...
	emc.esmd_max_ch = 4800 ;	// unknown...
	emc.epre_max_ch = 4800 ;	// unknown...

	emc.btow_ch = 0 ;
	emc.bsmd_ch = 0 ;
	emc.bpre_ch = 0 ;

	emc.etow_ch = 0 ;
	emc.esmd_ch = 0 ;
	emc.epre_ch = 0 ;


	emc.btow_in = emc.bsmd_in = emc.bpre_in = 0 ;
	emc.etow_in = emc.esmd_in = emc.epre_in = 0 ;

	if(datap == NULL) return 0 ;

	bytes = 0 ;

	// let's first do the Barrel Tower
	for(type=0;type<2;type++) {	// 0 - Barrel, 1 - Endcap
		if(type==0) continue; // Added by Herb because Barrel is handled by Subhassis' code.
		if(type==0) {
			id = EMC_ID ;
			p = "EMCP" ;
			secp = "EMCSECP" ;
			rbp = "EMCRBP" ;
			adcr= "EMCADCR" ;
			adcd = "EMCADCD" ;


			len = b2h32(datap->det[id].len) * 4 ;
			if(len == 0) continue ;

			off = b2h32(datap->det[id].off) ;
			if(off == 0) continue ;

		}
		else {
			id = EEC_ID ;
			p = "EECP" ;
			secp = "EECSECP" ;
			rbp = "EECRBP" ;
			adcr= "EECADCR" ;
			adcd = "EECADCD" ;

			// EEC is in DATAPX...
			len = l2h32(datap->det[EXT_ID].len) ; // Herb changed this from b2h32 to l2h32.
			if(len == 0) continue ;	// not even a xtended det

			off = l2h32(datap->det[EXT_ID].off) ; // Herb changed this from b2h32 to l2h32;
			if(off == 0) continue ;

			datapx = (struct DATAPX *)(m + off*4) ;

			// verify bank
			// if(checkBank(datapx->bh.bank_type, CHAR_DATAPX) < 0) {
			//   continue ;
			// }


			len = l2h32(datapx->det[id-10].len) * 4 ; // Changed from b2h32 to l2h32 by Herb.
			if(len == 0) continue ;

			off = l2h32(datapx->det[id-10].off) ; // Changed from b2h32 to l2h32 by Herb.
			if(off == 0) continue ;


			// override "m"
			m = (char *)datapx ;
		}

	

		// Herb commented this. LOG(NOTE,"EMC %d: bytes %d, off %d",id,len,off,0,0) ;

		bytes += len  ;	// save

		emcp = (struct EMCP *)((u_int *)m + off) ;

		// if(checkBank(emcp->bh.bank_type,p) < 0) {
			// return -1 ;
		// }


		// let's see how many contributions (subdetectors) does this event have

		for(i=0;i<3;i++) {	// go through subdets
			len = l2h32(emcp->sec[i].len) ; // Changed from b2h32 to l2h32 by Herb
			if(len == 0) continue ;

			instance = i + 1 ;	// EMC subinstances star from 1...

			off = l2h32(emcp->sec[i].off) ; // Changed from b2h32 to l2h32 by Herb.

			emcsecp = (struct EMCSECP *)((u_int *)emcp + off) ;

			// if(checkBank(emcsecp->bh.bank_type,secp) < 0) {
				// continue ;
			// }

			cou = (b2h32(emcsecp->bh.length) - 10) / 2 ;	// contributions!


			// Herb commented this. LOG(DBG,"EMC %d: instance %d: %d fibers used",id,instance,cou,0,0) ;

			for(j=0;j<cou;j++) {
				len = b2h32(emcsecp->fiber[j].len) ;

				if(len == 0) continue ;

				off = b2h32(emcsecp->fiber[j].off) ;

				emcrbp = (struct EMCRBP *)((u_int *)emcsecp + off) ;

				// if(checkBank(emcrbp->bh.bank_type,rbp) < 0) {
					// continue ;
				// }


				cou2 = (b2h32(emcrbp->bh.length) - 10) /2 ;

				// Herb commented this. LOG(DBG,"EMC %d: instance %d: fiber %d: %d banks used",id,instance,j+1,cou2,0) ;

				emcadcr = emcadcd = NULL ;

				for(k=0;k<cou2;k++) {
					len = b2h32(emcrbp->banks[k].len) ;

					if(len == 0) continue ;

					off = b2h32(emcrbp->banks[k].off) ;

					switch(k) {
					case 0 :	// Raw
						emcadcr = (struct DUMMYDATA *)((u_int *)emcrbp + off) ;
						// if(checkBank(emcadcr->bh.bank_type,adcr) < 0) {
							// continue ;
						// }

						break ;
					case 1 :	// zero-suppressed...
						emcadcd = (struct DUMMYDATA *)((u_int *)emcrbp + off) ;
						// if(checkBank(emcadcr->bh.bank_type,adcd) < 0) {
							// continue ;
						// }

						break ;
					default :
						// Herb commented this. LOG(ERR,"Unknown subbank %d in EMCRBP!",k,0,0,0,0) ;
						continue ;
					}

					// I currently only know about RAW data
					if(emcadcr == NULL) {
						// Herb commented this. LOG(WARN,"EMC %d: instance %d, format %d is not implemented yet!",
						    // Herb commented this. id,instance, k,0,0) ;
						continue ;
					}


					if((id==EMC_ID) && (instance == BTOW_INSTANCE)) {
						u_short *data ;
						int l ;

						emc.btow_in = 1;
						// get to the data: 40 bytes bank header, 4 bytes dummy,
						// 128 bytes fiber header...
						data = (u_short *) ((u_int) emcadcr + 40 + 4 + 128) ; 
					
						emc.btow_raw = data ;

						data += 120 ;	// skip the preamble of 4X30 shorts

						for(l=0;l<4800;l++) {
							emc.btow[l] = l2h16(*data++) ;
							if(emc.btow[l] > 0) emc.btow_ch++ ;
						}
					}
					else if((id==EMC_ID) && (instance == BSMD_INSTANCE)) {
						
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
						}

					}
					else if((id==EEC_ID) && (instance == ETOW_INSTANCE)) {

						// Let's ensure that we're really at the EECADCR bank.
						char *h=(char*)emcadcr; // The var is named emcadcr because code was written
						assert(h[0]=='E');      // originally for the barrel EMC.
						assert(h[1]=='E');
						assert(h[2]=='C');
						assert(h[3]=='A');
						assert(h[4]=='D');
						assert(h[5]=='C');
						assert(h[6]=='R');
						u_short *data ;
						int l ;

						emc.etow_in = 1;
						// get to the data: 40 bytes bank header, 4 bytes dummy,
						// 128 bytes fiber header...
						data = (u_short *) ((u_int) emcadcr + 40 + 4 + 128) ; 
					
						emc.etow_raw = data ;

						data += 120 ;	// skip the preamble of 4X30 shorts

						for(l=0;l<4800;l++) {
							emc.etow[l] = l2h16(*data++) ;
							if(emc.etow[l] > 0) emc.etow_ch++ ;
						}
					}
					else if((id==EEC_ID) && (instance == ESMD_INSTANCE)) {
						
						u_short *data ;
						int l ;

						emc.esmd_in = 1;
						// get to the data: 40 bytes bank header, 4 bytes dummy,
						// 256 bytes fiber header...
						data = (u_short *) ((u_int) emcadcr + 40 + 4 + 256) ; 
					

						emc.esmd_cap[j] = *(u_char *)((u_int)emcadcr + 40 + 4 + 4*16) ;
						for(l=0;l<4800;l++) {
							emc.esmd[l] = l2h16(*data++) ;
							if(emc.esmd[l] > 0) emc.esmd_ch++ ;
						}

					}



				}
			}
		}
	}
					
						
	return bytes ;

}

