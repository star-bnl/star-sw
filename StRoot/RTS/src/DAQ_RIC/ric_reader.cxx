#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>

#include <rtsLog.h>
#include <rtsSystems.h>
#include <daqFormats.h>
#include <rts.h>

#include "daq_ric.h"


int ric_reader(char *m, struct ric_t *ric, u_int driver) 
{
	struct DATAP *datap = (struct DATAP *)m ;
	struct RICP *ricp ;
	struct RICCRAMP *riccramp ;
	struct RICDATAD *ricdatad ;

	int len, off ;

	ric->mode = 1 ;
	ric->max_channels = 16*960 ;
	ric->channels = 0 ;

	if(datap == NULL) return 0 ;

	len = ntohl(datap->det[RIC_ID].len) * 4 ;
	if(len == 0) return 0 ;

	off = ntohl(datap->det[RIC_ID].off) ;
	if(off == 0) return 0 ;


	LOG(DBG,"RIC len %d, off %d",len,off,0,0,0) ;

	ricp = (struct RICP *)((u_int *)m + off) ;

	if(checkBank(ricp->bh.bank_type,CHAR_RICP) < 0) {
		return -1 ;
	}

#define RUN_ME
#ifdef RUN_ME

	memset(ric->adc,0,sizeof(ric->adc)) ;


	int cram ;

	for(cram=0;cram<16;cram++) {
		int banks ;

		if(ricp->crams[cram].len <= 0) continue ;

		riccramp = (struct RICCRAMP *)((u_int *)ricp + b2h32(ricp->crams[cram].off)) ;

		if(checkBank(riccramp->bh.bank_type,CHAR_RICCRAMP) < 0) {
			return -1 ;
		}

		for(banks=0;banks<8;banks++) {
			u_int cou ;
			u_int i ;

			if(riccramp->banks[banks].len <= 0) continue ;

			if(banks != RIC_BANK_DATAD) {
				LOG(DBG,"Found non-DATAD bank %d - skipping...",banks,0,0,0,0) ;
				continue ;
			}



			ricdatad = (struct RICDATAD *)((u_int *)riccramp + b2h32(riccramp->banks[banks].off)) ;

			
			LOG(DBG,"Bank id is %d...",banks,0,0,0,0) ;

			if(checkBank(ricdatad->bh.bank_type,CHAR_RICDATAD) < 0) {
				return -1 ;
			}


			cou = b2h32(ricdatad->bh.length) - sizeof(struct bankHeader)/4 ;

			LOG(DBG,"RICH bank %d, CRAM %d has %d entries...",banks,cram,cou,0,0) ;

			if(cou == 0) continue ;


			for(i=0;i<cou*2;i+=2) {
				u_short adc, ch ;

				ch = b2h16(ricdatad->data[i]) ;
				adc = b2h16(ricdatad->data[i+1]) ;

				if(adc > 1023) adc = 1023 ;

				if(adc == 0) continue ;

				// sanity
				if(ch >= 960) {
					LOG(ERR,"RICH channel %d >= 960!",ch,0,0,0,0) ;
					return -1 ;
				}

				ric->adc[cram][ch] = adc ;

				ric->channels++ ;
			}

		}

	}


#endif
	return len ;

}

