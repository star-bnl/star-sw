#include <stdio.h>
#include <sys/types.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <unistd.h>

#include <rtsLog.h>
#include <daqModes.h>
#include <rtsSystems.h>

#include <DAQ_READER/daq_dta.h>

#include "fgtPed.h"



fgtPed::fgtPed(int rts)
{
	valid = 0 ;
	rb_mask = 0 ;	

	memset(fgt_stat,0,sizeof(fgt_stat)) ;


	k_seq = 0 ;	
	n_sigma = 0.0 ;

	rts_id = rts ;

	tb_cou_xpect = -1 ;
	tb_cou_ped = -1 ;

	memset(ch_status,0,sizeof(ch_status)) ;


	switch(rts_id) {
	case IST_ID :
	case FST_ID :
		for(int r=0;r<6;r++) {
		for(int ar=0;ar<6;ar++) {
		for(int ap=0;ap<24;ap++) {
		for(int c=0;c<128;c++) {
			ch_status[r][ar][ap][c] = FGT_CH_STAT_SHOULD ;
		}}}
		fgt_rdr[r] = new daq_fgt(0) ;
		}
		rb_mask = 0x3F ;

		
		break ;
	case GMT_ID :
		for(int ar=0;ar<2;ar++) {
			u_int ap_mask = 0x00F00F ;
			for(int ap=0;ap<24;ap++) {
				if(ap_mask & (1<<ap)) ;
				else continue ;

				for(int c=0;c<128;c++) {
					ch_status[0][ar][ap][c] = FGT_CH_STAT_SHOULD ;
				}
			}
		}
		fgt_rdr[0] = new daq_fgt(0) ;

		rb_mask = 1 ;
		break ;
	case FGT_ID :
		for(int r=0;r<2;r++) {
		for(int ar=0;ar<6;ar++) {

		u_int ap_mask = 0x3FF3FF ;
		for(int ap=0;ap<24;ap++) {
			if(ap_mask & (1<ap)) ;
			else continue ;

			for(int c=0;c<128;c++) {
				ch_status[r][ar][ap][c] = FGT_CH_STAT_SHOULD ;
			}
		}}
		fgt_rdr[r] = new daq_fgt(0) ;
		}
		rb_mask = 0x3 ;
		break ;
		
	}


	return ;
}



#if 0
static int fgt_bad_heuristics(int rts_id, double ped, double rms)
{
	switch(rts_id) {
	case IST_ID :
	case FST_ID :
		if((rms < 5.0)||(rms>50.0)||(ped>2000.0)||(ped<5.0)) return -1 ;
		break ;
	default :
		if((rms<=0.0) || (ped<0.0)) return -1 ;
		break ;
	}

	return 0 ;	// take it!
}
#endif

int fgtPed::run_stop()
{
	for(int r=0;r<FGT_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;

		int evts = fgt_stat[r].evts ;

		if(fgt_stat[r].err) {
			LOG(ERR,"RDO %d: %d errors in %d events",r+1,fgt_stat[r].err,evts) ;
		}

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
			if(fgt_stat[r].arm_mask & (1<<arm)) ;
			else continue ;

			for(int apv=0;apv<FGT_APV_COU;apv++) {
				int errs = fgt_stat[r].err_apv[arm][apv] ;
				int cou = fgt_stat[r].cou_apv[arm][apv] ;

				if(errs || (cou && (cou!=evts))) {
					LOG(ERR,"RDO %d: ARM %d, APV %2d: %d errors, in %d/%d events",
					    r+1,arm,apv,errs,cou,evts) ;
				}
			}
		}
	}

	return 0 ;
}


void fgtPed::init(int active_rbs)
{
	memset(fgt_stat,0,sizeof(fgt_stat)) ;

	rb_mask = active_rbs ;

}


int fgtPed::do_zs(char *src, int in_bytes, char *dst, int rdo1)
{
	int ks = k_seq ;
	int dumped_cou = 0 ;
	int all_cou = 0 ;

	short *d16 = (short *) dst ;
	u_int *d32 = (u_int *)dst ;

	fgt_stat[rdo1-1].evts++ ;
	
	daq_dta *dd = fgt_rdr[rdo1-1]->handle_adc(0, rdo1, src) ;


	// create meta
	apv_meta_zs_t meta_zs ;
	apv_meta_t *meta = (apv_meta_t *) dd->meta ;

	memset(&meta_zs,0,sizeof(meta_zs)) ;

	int t_xpect = tb_cou_ped ;

	int max_tb = -1 ;

	total_charge = 0 ;

	for(int arm=0;arm<FGT_ARM_COU;arm++) {
	for(int apv=0;apv<FGT_APV_COU;apv++) {
#if 0
		printf("ARC %d, ARM %d, APV %d: %d %d %d %d\n",rdo1,arm,apv,
		       meta->arc[rdo1].arm[arm].apv[apv].present,
		       meta->arc[rdo1].arm[arm].apv[apv].ntim,
		       meta->arc[rdo1].arm[arm].apv[apv].error,
		       meta->arc[rdo1].arm[arm].error) ;
#endif
		if(meta->arc[rdo1].arm[arm].apv[apv].present) {
			int err = 0 ;

			meta_zs.status[arm][apv] |= 1 ;

			int t_cou = meta->arc[rdo1].arm[arm].apv[apv].ntim ;

			if(t_cou != t_xpect) {
				if(fgt_stat[rdo1-1].err < 12) {
					LOG(WARN,"wrong tb_cou %d(expect %d): ARC %d, ARM %d, APV %d",t_cou,t_xpect,rdo1,arm,apv) ;
				}
				fgt_stat[rdo1-1].err++ ;
				err |= 2 ;
			}

			if(t_cou > max_tb) max_tb = t_cou ;


			if(err) {
				meta_zs.status[arm][apv] |= err ;
			}
		}

		if(meta->arc[rdo1].arm[arm].apv[apv].error || meta->arc[rdo1].arm[arm].error) {
			meta_zs.status[arm][apv] |= 4 ;	// error ;
		}

		if(meta_zs.status[arm][apv] & 1) {
			fgt_stat[rdo1-1].cou_apv[arm][apv]++ ;
		}

		if(meta_zs.status[arm][apv] & 0xFE) {
			fgt_stat[rdo1-1].err_apv[arm][apv]++ ;
		}

		switch(meta_zs.status[arm][apv]) {
		case 0 :	// not present
		case 1 :	// present
		case 3 :	// present but wrong tb
			break ;
		default :
			if(fgt_stat[rdo1-1].err < 100) {
				LOG(ERR,"ARC %d, ARM %d, APV %d: meta_zs 0x%X",rdo1,arm,apv,meta_zs.status[arm][apv]) ;
			}
			fgt_stat[rdo1-1].err++ ;
			break ;
		}

	}}

	// cut data!
	if(max_tb > tb_cou_ped) max_tb = tb_cou_ped ;	// FORCED!
	
	meta_zs.tb_cou = max_tb ;

	// roundoff to a 16 bit word...
	int meta_bytes = sizeof(meta_zs) ;
	while(meta_bytes % 2) meta_bytes++ ;

	*d32++ = 0xFEEDBEEF ;	// signature

	int do_ped_sub = 0 ;	// non ped-sub
	switch(rts_id) {
	default :
	case FGT_ID :
		*d32++ = META_ZS_VERSION ;		// version
		break ;
	case IST_ID :
	case FST_ID :
		do_ped_sub = 1 ;
		*d32++ = META_PED_ZS_VERSION ;	// zs _AND_ ped subtracted
		break ;		
	case GMT_ID :
		do_ped_sub = 1 ;	// subtract peds
		*d32++ = META_PED_ZS_VERSION ;	// zs _AND_ ped subtracted
		break ;
	}

	u_int *dta_bytes = d32++ ;	// reserve space
	*d32++ = meta_bytes ;

	memcpy(d32,&meta_zs,meta_bytes) ;
	d16 = (short *)d32 + meta_bytes/2 ;

	while(dd && dd->iterate()) {
		int arc, arm, apv ;

		arc = dd->rdo ;
		arm = dd->sec ;
		apv = dd->pad ;

//		printf("**** ARC %d, ARM %d, APV %d: entries %d\n",
//		      arc,arm,apv,dd->ncontent) ;

		fgt_adc_t *f = (fgt_adc_t *) dd->Void ;

		struct peds_t *p_thr = &(peds[arc-1]) ;

		int i_save = -1 ;
		int cou = 0 ;
		int dump = 0 ;
		int thr = 0 ;
		int ch = -1 ;
		int cou_tb = 0 ;

		*d16++ = 0xAB00 | arc ;
		*d16++ = (arm << 8) | apv ;


		for(u_int i=0;i<dd->ncontent;i++) {
			int tb = f[i].tb ;
			int adc = f[i].adc ;

			// if the timebin of the data is larger than the timebin of 
			// the available pedestals/thresholds -- silently drop!!!

			if(tb >= max_tb) continue ;

			if(tb==0) {
				if(dump) {
//					printf("*** dump ARC %d, ARM %d, APV %d, CH %d in %d tb\n",arc,arm,apv,ch,cou_tb) ;

					*d16++ = (cou_tb << 8) | ch ;
					dumped_cou++ ;

					for(int i=0;i<cou_tb;i++) {

						if(do_ped_sub) {
							short adc = (short) ((float)f[i_save+i].adc - p_thr->ped[arm][apv][ch][i] + 0.5);	// had bug, was "+ 0.2"!
							*d16++ = adc ;

							if(adc > 0) total_charge++ ;
							
						}
						else {
							*d16++ = f[i_save+i].adc ;
						}
						


					}
				}

				ch = f[i].ch ;

				i_save = i ;
				cou = 0 ;
				dump = 0 ;
				thr = p_thr->thr[arm][apv][ch] ;
				cou_tb = 0 ;


				all_cou++ ;

			}


			if(adc > thr) {
				cou++ ;
				if(cou >= ks) {
					dump = 1 ;
				}
			}					
			else cou = 0 ;

			cou_tb++ ;

//			printf("  %4d: CH %3d, TB %d == %d, thr %d\n",
//				i,f[i].ch,f[i].tb, f[i].adc,thr) ;

		}

		// last guy
		if(dump) {
//			printf("*** LAST dump ARC %d, ARM %d, APV %d, CH %d in %d tb\n",arc,arm,apv,ch,cou_tb) ;

			*d16++ = (cou_tb <<8) | ch  ;
			dumped_cou++ ;

			for(int i=0;i<cou_tb;i++) {


				if(do_ped_sub) {
					short adc = (short) ((float)f[i_save+i].adc - p_thr->ped[arm][apv][ch][i] + 0.5);	// had bug, was "+ 0.2"!
					*d16++ = adc ;
				}
				else {
					*d16++ = f[i_save+i].adc ;
				}

			}
		}

	}

	*d16++ = dumped_cou ;

	
	int out_bytes = (char *)d16 - (char *)dst ;

	*dta_bytes = out_bytes ;

	// make sure we are on a 32bit boundary!
	while(out_bytes % 4) {
		*d16++ = 0xBABA ;
		out_bytes += 2 ;
	}

	//LOG(TERR,"ARC %d: dumped %d/%d, %d bytes",rdo1,dumped_cou,all_cou,out_bytes) ;

	return out_bytes ;
}


/*
	Called per event, per RDO. evbbuff is the raw RDO contribuition.
	rdo counts from 1.
*/
void fgtPed::accum(char *evbuff, int bytes, int rdo1)
{

	int rdo = rdo1 - 1 ;	// since rdo1 is from 1



	fgt_stat[rdo].evts++ ;

	// skip first few events!
//	if(fgt_stat[rdo].evts <= 3) {
//		return ;
//	}

	if(fgt_stat[rdo].evts > 0xFF00) return ;	// don't allow more than 16bits worth...

	struct peds_t *p = peds + rdo ;

	daq_dta *dd = 0 ;
	dd = fgt_rdr[rdo1-1]->handle_adc(0,rdo1, evbuff) ;


	int t_xpect = tb_cou_xpect ;

	char need[FGT_ARM_COU][FGT_APV_COU] ;
	memset(need,0,sizeof(need)) ;

	if(dd && dd->meta) {
		apv_meta_t *meta = (apv_meta_t *)dd->meta ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
			if(meta->arc[rdo1].arm[arm].apv[apv].present == 0) continue ;

			
			if(meta->arc[rdo1].arm[arm].apv[apv].ntim != t_xpect) {
				if(fgt_stat[rdo1-1].err < 10) {
					LOG(WARN,"evt %d: RDO %d, ARM %d, APV %d: ntim %d, expect %d??",fgt_stat[rdo].evts,rdo1,arm,apv,meta->arc[rdo1].arm[arm].apv[apv].ntim,t_xpect) ;
				}
				fgt_stat[rdo1-1].err++ ;
			}

			need[arm][apv] |= 1 ;
			
			if(meta->arc[rdo1].arm[arm].apv[apv].apv_id != apv) {
				LOG(ERR,"RDO %d, ARM %d, APV %d: %d",rdo1,arm,apv,meta->arc[rdo1].arm[arm].apv[apv].apv_id) ;
				need[arm][apv] |= 2 ;	// error
			}


		}
		}
	}

	while(dd && dd->iterate()) {
		if(dd->rdo != rdo1) continue ;

		int arm = dd->sec ;
		int apv = dd->pad ;

		fgt_adc_t *f = (fgt_adc_t *) dd->Void ;

		need[arm][apv] |= 4 ;

		for(u_int i=0;i<dd->ncontent;i++) {
			int adc ;
			int ch ;
			int tb ;

			ch = f[i].ch ;
			adc = f[i].adc ;
			tb = f[i].tb ;

			p->ped[arm][apv][ch][tb] += (float) adc ;
			p->rms[arm][apv][ch][tb] += (float) (adc * adc) ;
			if(tb==0) p->cou[arm][apv][ch]++ ;
		}
	}


	// the following is just an error dump...
	for(int arm=0;arm<FGT_ARM_COU;arm++) {
	for(int apv=0;apv<FGT_APV_COU;apv++) {
		if(need[arm][apv] == 0) continue ;

		switch(need[arm][apv]) {
		case 1 :	// missing in data; will capture the error later
		case 5 :	// all OK
			break ;
		default :
			LOG(ERR,"ARC %d, ARM %d, APV %d: need is 0x%X",rdo1,arm,apv,need[arm][apv]) ;
		}

	}
	}


	return ;

}



double fgtPed::do_thresh(double ns, int k, int do_log)
{
	// suspect
	// tb_cou set in from_cache



	n_sigma = ns ;
	k_seq = k ;


	LOG(INFO,"do_thresh: n-sigma %f, k-seq %d",n_sigma, k_seq) ;


	for(int r=0;r<FGT_RDO_COU;r++) {
		struct peds_t *p = peds + r ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
		for(int c=0;c<FGT_CH_COU;c++) {

		if(ch_status[r][arm][apv][c] & FGT_CH_STAT_SHOULD) ;
		else continue ;


		double ped = 0.0 ;
		double rms = 0.0 ;
		int cou = 0 ;

		for(int t=0;t<tb_cou_ped;t++) {
			double pp = p->ped[arm][apv][c][t] ;
			double rm = p->rms[arm][apv][c][t] ;

			if(rm < 0.0) continue ;	// should be here but not found in data, damn...

			ped += pp ;
			rms += rm ;
			cou++ ;
		}

		if(cou == 0) {	// channel should have been present but it wasn't in the pedestal file...
			p->thr[arm][apv][c] = 0xFFFD ;
			continue ;
		}

		ped /= cou ;
		rms /= cou ;

		p->thr[arm][apv][c] = (u_short) (ped + rms * n_sigma + 0.5) ;

		}
		}
		}
	}

	// kill bad
	int b_all_all = 0 ;
	int b_bad_all = 0 ;

	for(int r=0;r<FGT_RDO_COU;r++) {
		struct peds_t *p = peds + r ;

		int b_ped_0 = 0 ;
		int b_bad  = 0 ;
		int b_masked  = 0 ;
		int b_misconfigured  = 0 ;
		int b_all = 0 ;
		int b_unknown = 0 ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
		for(int c=0;c<FGT_CH_COU;c++) {
			if(ch_status[r][arm][apv][c] & FGT_CH_STAT_SHOULD) ;
			else {
				p->thr[arm][apv][c] = 0xFFFE ;	// not physically present...
				continue ;
			}

			b_all++ ;

			// kill any channel marked odd!
			if(ch_status[r][arm][apv][c] & 0xFE) {
				p->thr[arm][apv][c] = 0xFFFF ;
				b_bad_all++ ;
			}
			else if(p->thr[arm][apv][c] >= 0xFFFD) {
				LOG(WARN,"Killed but not marked: %d %d %d %d",r,arm,apv,c) ;
				b_unknown++ ;
				b_bad_all++ ;
			}
			
			if(ch_status[r][arm][apv][c] & FGT_CH_STAT_NO_CONFIG) {
				b_masked++ ;
			}
			if(ch_status[r][arm][apv][c] & FGT_CH_STAT_NO_RESPONSE) {
				b_misconfigured++ ;
			}
			if(ch_status[r][arm][apv][c] & FGT_CH_STAT_BAD) {
				b_bad++ ;
			}
			if(ch_status[r][arm][apv][c] & FGT_CH_STAT_PED_UNKNOWN) {
				b_ped_0++ ;
			}
			

			
		}}}

		b_all_all += b_all ;

		LOG(WARN,"ARC %d: masked %d, misconfigd %d, bad %d, bad ped %d, unknown %d of %d all",
		    r+1,b_masked,b_misconfigured,b_bad,b_ped_0,b_unknown,b_all) ;

	}

	double perc = (double)b_bad_all/(double)b_all_all ;
	perc *= 100.0 ;

	return perc ;

}

void fgtPed::calc()
{

	const int MIN_EVENTS =  1000 ;


	LOG(NOTE,"Calculating pedestals") ;

	tb_cou_ped = -1 ;

	int bad = 0 ;



	for(int r=0;r<FGT_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;



		struct peds_t *ped = peds + r ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
		for(int ch=0;ch<FGT_CH_COU;ch++) {

		int cou = ped->cou[arm][apv][ch] ;
		
		for(int t=0;t<tb_cou_xpect;t++) {

			if(cou == 0) {	// never seen in the data!
				ped->ped[arm][apv][ch][t] = 0 ;
				ped->rms[arm][apv][ch][t] = -1.0 ;
			}
			else {
				double pp, rr ;

		
				pp = ped->ped[arm][apv][ch][t] / (double) cou ;
				rr = ped->rms[arm][apv][ch][t] / (double) cou ;

				// due to roundoff I can have super small negative numbers
				if(rr < (pp*pp)) rr = 0.0 ;
				else rr = sqrt(rr - pp*pp) ;

				ped->ped[arm][apv][ch][t] = pp ;
				ped->rms[arm][apv][ch][t] = rr ;

				if(t > tb_cou_ped) tb_cou_ped = t ;

//				LOG(TERR,"RDO %d, ARM %d, APV %d, CH %d, TB %d: %f +- %f, cou %d",
//				    r,arm,apv,ch,t,pp,rr,ped->cou[arm][apv][ch][t]) ;
			}
		}
		}

		}
		}
	}

	tb_cou_ped++ ;	// need to increment...


	int not_enough = 0 ;
	for(int r=0;r<FGT_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;

		if(fgt_stat[r].evts < MIN_EVENTS) {
			not_enough = 1 ;
		}
	}



	LOG(TERR,"Pedestals calculated. tb_count %d, RDO counts: %u",tb_cou_ped,fgt_stat[0].evts) ;
	valid = 1 ;	// assume all OK...

	if(not_enough) valid = 0 ;


	if(!valid) {
		LOG(ERR,"FGT pedestals not good: APVs bad %d, events %d",bad,fgt_stat[0].evts) ;
	}

	return ;
}


int fgtPed::to_evb(char *buff)
{
	int r, arm, apv, c, t ;
	u_short *dta = (u_short *) buff ;	


	if(!valid) {
		// log error but continue...
		LOG(ERR,"ped::to_evb peds are bad: valid %d",valid) ;
	}

	LOG(NOTE,"Preparing pedestals for later EVB...") ;

	*dta++ = 0xBEEF ;		// signature
	*dta++ = 0x0001 ;		// version
	*dta++ = FGT_ARM_COU ;		// ARM
	*dta++ = FGT_APV_COU ;
	*dta++ = FGT_CH_COU ;		// channel count
	*dta++ = tb_cou_ped ;		// timebin count


	for(r=0;r<FGT_RDO_COU;r++) {
		if(rb_mask && (1<<r)) ;
		else continue ;

		struct peds_t *ped = peds + r ;

		*dta++ = r+1 ;			// ARC, from 1
		u_short *apv_cou = dta++ ;
		*apv_cou = 0 ;

		// need to dump the apv_meta_zs_t bank!!!

		for(arm=0;arm<FGT_ARM_COU;arm++) {
		for(apv=0;apv<FGT_APV_COU;apv++) {



		*dta++ = arm ;
		*dta++ = apv ;
		(*apv_cou)++ ;

		for(c=0;c<FGT_CH_COU;c++) {
		for(t=0;t<tb_cou_ped;t++) {

				u_short pp ;

				pp = (u_short)(ped->ped[arm][apv][c][t]*16.0 + 0.5)  ;
				*dta++ = pp;

				pp = (u_short)(ped->rms[arm][apv][c][t]*16.0  + 0.5) ;

				*dta++ = pp ;
		}
		}
		}
		}

	}

	LOG(TERR,"Pedestals prepared for later EVB, %d bytes",(char *)dta-buff) ;

	return ((char *)dta-buff) ;
}

void fgtPed::clear()
{
	memset(peds,0,sizeof(peds)) ;
}

void fgtPed::clear_from_cache()
{
	for(int r=0;r<FGT_RDO_COU;r++) {
		struct peds_t *p = peds_from_cache + r ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
		for(int c=0;c<FGT_CH_COU;c++) {
			for(int t=0;t<FGT_TB_COU;t++) {
				p->ped[arm][apv][c][t] = 0.0 ;
				p->rms[arm][apv][c][t] = -2.0 ;
			}
			p->cou[arm][apv][c] = 0 ;
			p->thr[arm][apv][c] = 0xFFFF ;
		}
		}
		}
	}


}

int fgtPed::from_cache(char *fname) 
{
	FILE *f ;
	const char *fn ;
	

	tb_cou_ped = - 1;

	clear() ;
	clear_from_cache() ;


	// trivial load from disk...
	if(fname) {
		fn = fname ;
		f = fopen(fname,"r") ;
	}
	else {
		fn = "/RTScache/pedestals.txt" ;
		f = fopen(fn,"r") ;
	}

	if(f==0) {
		LOG(U_TONKO,"ped::from_cache can't open input file \"%s\" [%s]",fn,strerror(errno)) ;
		return -1 ;
	}


	LOG(NOTE,"Loading pedestals from cache \"%s\"...",fn) ;

	

	while(!feof(f)) {
		int r, arm, apv, ch, tb ;
		float pp, rr ;
		char buff[256] ;

		if(fgets(buff,sizeof(buff),f) == 0) continue ;
		
		switch(buff[0]) {
		case '#' :
		case '!' :
		case '*' :
		case '/' :
		case '.' :
			continue ;
		}

		   
		int ret = sscanf(buff,"%d %d %d %d %d %f %f",&r,&arm,&apv,&ch,&tb,&pp,&rr) ;
		if(ret != 7) continue ;


		struct peds_t *ped = peds + (r-1) ;

		ped->ped[arm][apv][ch][tb] = pp ;
		ped->rms[arm][apv][ch][tb] = rr ;

		struct peds_t *ped_fc = peds_from_cache + (r-1) ;

		ped_fc->ped[arm][apv][ch][tb] = pp ;
		ped_fc->rms[arm][apv][ch][tb] = rr ;
		
		if(tb > tb_cou_ped) {
			tb_cou_ped = tb ;
		}


		if(tb==0) {
			if(pp<=0.0 || rr<=0.0) {
				ch_status[r-1][arm][apv][ch] |= FGT_CH_STAT_PED_UNKNOWN ;
			}
		}
	}

	fclose(f) ;

	tb_cou_ped++ ;	// to get to the ntimbins

	if(tb_cou_ped != tb_cou_xpect) {
		LOG(ERR,"Pedestals loaded from cache \"%s\" but have %d timebins != expect %d!",fn,
		    tb_cou_ped,tb_cou_xpect) ;
	}
	else {
		LOG(INFO,"Pedestals loaded from cache \"%s\", %d timebins OK",fn,tb_cou_ped) ;
	}

	valid = 1 ;

	return valid ;
}

int fgtPed::to_cache(char *fname, u_int run, int dont_cache)
{
	FILE *f ;
	char f_fname[128] ;
	int do_ln = 1 ;

	if(!valid) {
		LOG(ERR,"ped::to_cache peds are bad: valid %d -- caching anyway...",valid) ;
		do_ln = 0 ;
		//LOG(CAUTION,"Pedestals are not valid -- not caching!") ;
	}

	if(fname) {
		strcpy(f_fname,fname) ;
		do_ln = 0 ;
	}
	else {
		sprintf(f_fname,"/RTScache/%s_pedestals_%08u.txt",rts2name(rts_id),run) ;
	}


	f = fopen(f_fname,"w") ;
	if(f==0) {
		LOG(ERR,"ped::to_cache can't open output file \"%s\" [%s]",f_fname,strerror(errno)) ;
		return -1 ;
	}


	if(tb_cou_ped != tb_cou_xpect) {
		LOG(CAUTION,"Writing pedestals to cache \"%s\" [valid %d] but data has %d timebins != expect %d!",f_fname,valid,
		    tb_cou_ped,tb_cou_xpect) ;
	}
	else {
		LOG(TERR,"Writing pedestals to cache \"%s\" [valid %d], ntimebins %d",f_fname,valid,tb_cou_ped) ;
	}

	time_t tim = time(0) ;
	fprintf(f,"# Detector %s\n",rts2name(rts_id)) ;
	fprintf(f,"# Run %08u\n",run) ;
	fprintf(f,"# Date %s",ctime(&tim)) ;
	fprintf(f,"# Timebins %d\n",tb_cou_ped) ;
	fprintf(f,"\n") ;

	for(int r=0;r<FGT_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;


		struct peds_t *ped = peds + r ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {


		for(int c=0;c<FGT_CH_COU;c++) {

		// dump only the ones which need to exist!
		if(ch_status[r][arm][apv][c] & FGT_CH_STAT_SHOULD) ;
		else continue ;

		for(int t=0;t<tb_cou_ped;t++) {

			fprintf(f,"%d %d %2d %3d %2d %7.3f %.3f\n",r+1,arm,apv,c,t,
				ped->ped[arm][apv][c][t],
				ped->rms[arm][apv][c][t]) ;
		}
		}
		}
		}
	}

	fclose(f) ;	


	if(dont_cache) {
		LOG(CAUTION,"Pedestals NOT cached for run %08u: bad run conditions 0x%X!",run,dont_cache) ;
		return 0 ;
	}

	
	f = fopen("/RTScache/pedestals.txt","w") ;
	if(f==0) {
		LOG(ERR,"ped::to_cache can't open output file \"%s\" [%s]","/RTScache/pedestals.txt",strerror(errno)) ;
		return -1 ;
	}

	fprintf(f,"# Detector %s\n",rts2name(rts_id)) ;
	fprintf(f,"# Run %08u\n",run) ;
	fprintf(f,"# Date %s",ctime(&tim)) ;
	fprintf(f,"# Timebins %d\n",tb_cou_ped) ;
	fprintf(f,"\n") ;

	int all_cou = 0 ;
	int non_cached_cou = 0 ;

	for(int r=0;r<FGT_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;


		struct peds_t *ped = peds + r ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {


		for(int c=0;c<FGT_CH_COU;c++) {

		// dump only the ones which need to exist!
		if(ch_status[r][arm][apv][c] & FGT_CH_STAT_SHOULD) ;
		else continue ;

		struct peds_t *use_ped ;

		all_cou++ ;
		// use just he lower bits of status!!!
		if((ch_status[r][arm][apv][c] & 0x0E) || (ped->cou[arm][apv][c]==0)) {
			//use_ped = peds_from_cache ; Major bug before Aug 2014!!! All peds_from_cache were taken from RDO1
			//FIXED in Aug 2014:
			use_ped = peds_from_cache + r ;
			non_cached_cou++ ;
		}
		else {
			use_ped = ped ;
		}

		for(int t=0;t<tb_cou_ped;t++) {
			
			fprintf(f,"%d %d %2d %3d %2d %7.3f %.3f\n",r+1,arm,apv,c,t,
				use_ped->ped[arm][apv][c][t],
				use_ped->rms[arm][apv][c][t]) ;
		}
		}
		}
		}
	}

	fclose(f) ;	
	
	{
		char cmd[128] ;

		sprintf(cmd,"/bin/cp /RTScache/pedestals.txt /RTScache/%s_pedestals_%08u_loaded.txt",rts2name(rts_id),run) ;
		system(cmd) ;
	}

			

//	if(do_ln) {
//		char cmd[128] ;
//		sprintf(cmd,"/bin/ln -f -s %s_pedestals_%08u.txt /RTScache/pedestals.txt",rts2name(rts_id),run) ;
//		system(cmd) ;
//	}

	double perc = 100.0 * (double)non_cached_cou / (double)all_cou ;
	if(perc > 10.0) {
		LOG(U_TONKO,"Pedestals cached for run %08u: skipped %d/%d (%d%%) channels.",run,non_cached_cou,all_cou,(int)perc) ;
	}
	else {
		LOG(TERR,"Pedestals cached for run %08u: skipped %d/%d channels.",run,non_cached_cou,all_cou) ;
	}

	return 1 ;
}



int fgtPed::bad_from_cache(char *fname) 
{
	FILE *f ;
	const char *fn ;
	
	int b_cou = 0 ;
	
	
	// trivial load from disk...
	if(fname) {
		fn = fname ;
		f = fopen(fname,"r") ;
	}
	else {
		switch(rts_id) {
		case GMT_ID :
			fn = "/RTS/conf/gmt/gmt_bad_channels.txt" ;
			break ;
		case IST_ID :
			fn = "/RTS/conf/ist/ist_bad_channels.txt" ;
			break ;
		case FST_ID :
			fn = "/RTS/conf/fst/fst_bad_channels.txt" ;
			break ;
		case FGT_ID :
		default:
			fn = "/RTS/conf/fgt/fgt_bad_channels.txt" ;
			break ;
		}

		f = fopen(fn,"r") ;
	}

	if(f==0) {
		LOG(ERR,"ped::bad_from_cache can't open input file \"%s\" [%s]",fn,strerror(errno)) ;
		return -1 ;
	}


	LOG(NOTE,"Loading bad from cache \"%s\"...",fn) ;

	

	while(!feof(f)) {
		int r, arm, apv, ch  ;
		char buff[256] ;

		if(fgets(buff,sizeof(buff),f) == 0) continue ;
		
		switch(buff[0]) {
		case '#' :
		case '!' :
		case '*' :
		case '/' :
		case '.' :
			continue ;
		}

		   
		int ret = sscanf(buff,"%d %d %d %d",&r,&arm,&apv,&ch) ;
		if(ret != 4) continue ;
		//check for negative 0!

		char ca[4][16] ;
		char n[4] ;
		memset(n,0,sizeof(n)) ;
		sscanf(buff,"%s %s %s %s",ca[0],ca[1],ca[2],ca[3]) ;
		for(int i=0;i<4;i++) {
			int dummy ;
			if(sscanf(ca[i],"%d",&dummy)!=1) continue ;
			if(dummy==0) {
				if(index(ca[i],'-')) n[i] = '-' ;
				else n[i] = '+' ;
			}
			else {
				if(dummy<0) n[i] = '-' ;
				else n[i] = '+' ;
			}
		}

		if(r<0) r *= -1 ;
		if(arm < 0) arm *= -1 ;
		if(apv < 0) apv *= -1 ;
		if(ch < 0) ch *= -1 ;



		if(n[1]=='-') {	//nix ARM
			for(int a=0;a<FGT_APV_COU;a++) {
			for(int c=0;c<FGT_CH_COU;c++) {
				ch_status[r-1][arm][a][c] |= FGT_CH_STAT_BAD ;
			}
			}
		}
		else if(n[2]=='-') {	//nix APV
			for(int c=0;c<FGT_CH_COU;c++) {
				ch_status[r-1][arm][apv][c] |= FGT_CH_STAT_BAD ;
			}
		}
		else {
			ch_status[r-1][arm][apv][ch] |= FGT_CH_STAT_BAD ;
		}
		

	}

	fclose(f) ;

	for(int r=0;r<FGT_RDO_COU;r++) {
	for(int m=0;m<FGT_ARM_COU;m++) {
	for(int a=0;a<FGT_APV_COU;a++) {
	for(int c=0;c<FGT_CH_COU;c++) {
		if(ch_status[r][m][a][c] & FGT_CH_STAT_BAD) b_cou++ ;
	}
	}
	}
	}

	LOG(INFO,"Loaded %d bad channels from \"%s\"",b_cou,fn) ;

	return b_cou ;
}
