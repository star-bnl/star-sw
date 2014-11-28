#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>

#include <rtsLog.h>
#include <rtsSystems.h>
#include <daqFormats.h>
#include <rts.h>


#include "daq_pmd.h"

static pmd_t *pmd_p ;
static int adcReader(int sec, struct PMDADCD *adcd) ;
static int pedReader(int sec, int type, struct PMDPEDR *pedr) ;


int pmd_reader(char *m, struct pmd_t *pmd, u_int driver)
{
	struct DATAP *datap = (struct DATAP *)m ;
	struct DATAPX *datapx  ;
	struct PMDP *pmdp = 0;
	struct PMDSECP *secp = 0;
	struct PMDADCD *adcd ;
	struct PMDPEDR *pedr ;
	struct PMDRMSR *rmsr ;
	struct PMDTHRR *thrr ;

	int off ;
	int len = 0 ;

	int start_sec, stop_sec ;
	int sec, type ;
	int ret ;
	int swapdatap = 0;
	int swapdatapx = 0;

	pmd_p = pmd ;	// need this for support routines!

	if(pmd_p->max_channels) ;	// already set...
	else {
		pmd_p->channels = 0 ;
		pmd_p->max_channels = 2*PMD_CRAMS_MAX*2*PMD_CRAMS_CH_MAX ;
		pmd_p->mode = 0 ;
		pmd_p->status[0] = pmd_p->status[1] = 0 ;
	}

	LOG(DBG,"PMD reader: %p: sector %d",datap,driver) ;

	if(datap == NULL) return 0 ;	// no datap and no request from SFS

	if(driver != 0) {
		secp = (struct PMDSECP *) m ;	// this comes directly from SFS!
		start_sec = stop_sec = driver - 1 ;
		goto do_sectors ;
	}
	else {
		start_sec = 0 ;
		stop_sec = 1 ;
	}



	if(datap->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapdatap = 1;

	off = qswap32(swapdatap, datap->det[EXT_ID].len) ;
	if(off == 0) return 0 ;	// not even a xtended det

	off = qswap32(swapdatap, datap->det[EXT_ID].off) ;


	datapx = (struct DATAPX *)(m + off*4) ;

	// verify bank
	if(checkBank(datapx->bh.bank_type, CHAR_DATAPX) < 0) {
		return 0 ;
	}


	if(datapx->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swapdatapx = 1;

	len = qswap32(swapdatapx, datapx->det[PMD_ID-10].len) * 4 ;

	if(len == 0) return 0 ;


	off = qswap32(swapdatapx, datapx->det[PMD_ID-10].off)*4 ;

	pmdp = (struct PMDP *)((char *)datapx + off) ;

	// verify bank
	if(checkBank(pmdp->bh.bank_type, CHAR_PMDP) < 0) {
		return -1 ;
	}
	

//	memset(pmd_p->adc,0,sizeof(pmd_p->adc)) ;


	do_sectors:;
	for(sec=start_sec;sec<=stop_sec;sec++) {	// 2 possible sectors
		
		if(driver == 0) {
			LOG(DBG,"PMD %d, len %d, offset %u",sec,b2h32(pmdp->sec[sec].len),b2h32(pmdp->sec[sec].off),0,0) ;

			if(pmdp->sec[sec].len == 0) continue ;

		
			secp = (struct PMDSECP *)((char *)pmdp + b2h32(pmdp->sec[sec].off)*4) ;
		}

		if(checkBank((char *)secp,CHAR_PMDSECP) < 0) return -1 ;

		pmd_p->status[sec] = b2h32(secp->bh.format_number) ;

		for(type=0;type<4;type++) {
			// hack!
			if(secp->type[type].len == 0) continue ;

			switch(type) {
			case PMD_ADCD_N :
				adcd = (struct PMDADCD *)((char *)secp + b2h32(secp->type[type].off)*4) ;
				if(checkBank(adcd->bh.bank_type, CHAR_PMDADCD) < 0) return -1 ;

				ret = adcReader(sec, adcd) ;
				if(ret < 0) return -1 ;

				pmd_p->channels += ret ;

				break ;
			case PMD_PEDR_N :
				pedr = (struct PMDPEDR *)((char *)secp + b2h32(secp->type[type].off)*4) ;
				if(checkBank(pedr->bh.bank_type, CHAR_PMDPEDR) < 0) return -1 ;

				ret = pedReader(sec, type, pedr) ;
				if(ret < 0) return -1 ;

				pmd_p->mode = 1 ;

				pmd_p->channels += ret ;


				break ;
			case PMD_RMSR_N :
				rmsr = (struct PMDRMSR *)((char *)secp + b2h32(secp->type[type].off)*4) ;
				if(checkBank(rmsr->bh.bank_type, CHAR_PMDRMSR) < 0) return -1 ;

				ret = pedReader(sec, type, (struct PMDPEDR *)rmsr) ;
				if(ret < 0) return -1 ;

				pmd_p->mode = 1 ;


				break ;
			case PMD_THRR_N :
				thrr = (struct PMDTHRR *)((char *)secp + b2h32(secp->type[type].off)*4) ;
				if(checkBank(thrr->bh.bank_type, CHAR_PMDTHRR) < 0) return -1 ;

				ret = pedReader(sec, type, (struct PMDPEDR *)thrr) ;
				if(ret < 0) return -1 ;

				pmd_p->mode = 1 ;

				break ;

			}
		}

	}

	return len ;

}

static int adcReader(int sec, struct PMDADCD *adcd)
{
	int items = b2h32(adcd->bh.length) - sizeof(adcd->bh)/4 ;
	u_int *datum ;
	u_int j ;
	u_int rb, mz ;
	u_int ch_num ;
	u_int ch ;

	if(items <= 0) return 0 ;


	datum = adcd->data ;

	ch_num = 0 ;

	LOG(DBG,"ADC: PMD sec %d: items %d",sec,items,0,0,0) ;

	u_int *end_datum = datum + items ;

	for(end_datum=datum+items;datum<end_datum;) {
		u_int tmp = b2h32(*datum) ;
		datum++ ;

		// DAQ pseudo-header
		rb = (tmp & 0xFF000000) >> 24 ;
		mz = (tmp & 0x00FF0000) >> 16 ;
		ch = (tmp & 0x0000FFFF) ;


		//printf("  RB %d, MZ %d, ch_num %d\n",rb,mz,ch) ;

		if(rb >= PMD_CRAMS_MAX) {
			LOG(ERR,"PMD: Bad RB number %d",rb,0,0,0,0) ;
			return -1 ;
		}

		if((mz != 0) && (mz != 1)) {
			LOG(ERR,"PMD: Bad mezzanine %d in RB %d",mz,rb,0,0,0) ;
			return -1 ;
		}

		if(ch > PMD_CRAMS_CH_MAX) {
			LOG(ERR,"PMD: too many channels in RB %d, MZ %d: %d",rb,mz,ch,0,0) ;
			return -1 ;
		}

		LOG(DBG,"PMD ADCD: sec %d, rb %d, mz %d, channels %d",sec,rb,mz,ch,0) ;

		for(j=0;j<ch;j++) {
			u_int val ;

			val = b2h32(*datum) ;
			datum++ ;


			if(val & 0x40000000) {	// zero suppressed...
				u_int channel = (val & 0x007ff000) >> 12 ;
				val &= 0xFFF ;

				//printf("        %4d: %4d == %4d\n",j,channel,val) ;

				if(channel >= PMD_CRAMS_CH_MAX) {
					LOG(ERR,"PMD: channel too big %d in RB %d, MZ %d: %d",rb,mz,channel,0,0) ;
					return -1 ;
				}

				pmd_p->adc[sec][rb][mz][channel] = val ;
				ch_num++ ;
			}
			else {
			  static const int maxCounts = 13;
			  static int Counts = 0;
			  if (Counts < maxCounts) {
			    Counts++;
			    LOG(ERR,"PMD %d: RB %d, MZ %d: illegal value in channel %d: 0x%08X",sec,rb,mz,j,val) ;
			    if (Counts == maxCounts) {LOG(ERR,"PMD stop print out after %i counts",Counts) ;}
			  }
			}
		}
	}

	return ch_num ;
}

static int pedReader(int sec, int type, struct PMDPEDR *pedr)
{
	int items ;
	u_short *datum ;
	int rb, mz, ch ;
	int ch_num ;
	int valid ;


	items = (b2h32(pedr->bh.length) - sizeof(pedr->bh)/4)*2 ;

	if(items <= 0) return 0 ;


	datum = pedr->data ;

	ch_num = 0 ;

	//printf("PMD sec %d: items %d\n",sec,items) ;

	u_short *end_datum = datum + items ;

	for(end_datum=datum+items;datum<end_datum;) {
		int channel ;
		u_int tmp = b2h16(*datum) ;
		datum++ ;

		rb = (tmp & 0x7F00) >> 8 ;
		mz = (tmp & 0x00FF)  ;

		valid = tmp & 0x8000 ;


		ch = PMD_CRAMS_CH_MAX ;

		if(!valid) {
			LOG(WARN,"Type %d: datum not valid at %u == 0x%04X",type,datum-pedr->data, (u_int)tmp,0,0) ;

			datum += ch ;
			continue ;
		}


		if(rb >= PMD_CRAMS_MAX) {
			LOG(ERR,"PMD: Bad RB number %d",rb,0,0,0,0) ;
			return -1 ;
		}

		if((mz != 0) && (mz != 1)) {
			LOG(ERR,"PMD: Bad mezzanine %d in RB %d",mz,rb,0,0,0) ;
			return -1 ;
		}

		LOG(DBG,"  %d: SEC %d, RB %d, MZ %d: channel count %d...",type,sec,rb,mz,ch) ;



		
		for(channel=0;channel<ch;channel++) {
			u_int val ;
			u_short blah ;

			blah = *datum ;
			val = b2h16(blah) ;
			datum++ ;


			//printf("   t %d: crate %d: rb %d, mz %d, ch %4d == 0x%04X ",type,sec,rb,mz,channel,val) ;
			//printf("%f\n",(double)val/16.0) ;

			switch(type) {
			case PMD_PEDR_N :
				pmd_p->ped[sec][rb][mz][channel] = val ;
				break ;
			case PMD_RMSR_N :
				pmd_p->rms[sec][rb][mz][channel] = val ;
				break ;
			case PMD_THRR_N :
				pmd_p->thr[sec][rb][mz][channel] = val ;
				break ;
			}
			ch_num++ ;
					
		}
	}

	return ch_num ;
}
