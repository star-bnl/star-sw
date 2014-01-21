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



fgtPed::fgtPed()
{
	valid = 0 ;
	rb_mask = 0x03 ;	// assume all..

	memset(fgt_stat,0,sizeof(fgt_stat)) ;

	
	sizeof_ped = sizeof(struct peds) * FGT_RDO_COU ;	// for FGT_RDO_COU RDOs

	
	ped_store = 0 ;	// unassigned!

	memset(fgt_rdr,0,sizeof(fgt_rdr)) ;

	k_seq = 0 ;	
	n_sigma = 0.0 ;

	rts_id = 0 ;

	tb_cou_xpect = 0 ;

	return ;
}


fgtPed::~fgtPed()
{
	if(ped_store) {
		free(ped_store) ;
		ped_store = 0 ;
	}

	for(int i=0;i<FGT_RDO_COU;i++) {
		if(fgt_rdr[i]) {
			delete fgt_rdr[i] ;
			fgt_rdr[i] = 0 ;	
		}
	}


	return ;
}


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


void fgtPed::init(int active_rbs, int rts)
{
	valid = 0 ;

	memset(fgt_stat,0,sizeof(fgt_stat)) ;

	rb_mask = active_rbs ;

	if(ped_store == 0) {
		ped_store = (struct peds *) malloc(sizeof_ped) ;
	}

	for(int i=0;i<FGT_RDO_COU;i++) {
		if((rb_mask & (1<<i)) && (fgt_rdr[i]==0)) {
			fgt_rdr[i] = new daq_fgt(0) ;
		}
	}


	rts_id = rts ;

	if(tb_cou_xpect) {
	}
	else {
	switch(rts_id) {
	case FGT_ID :
		tb_cou_xpect = 8 ;
		break ;
	case GMT_ID :
		tb_cou_xpect = 15 ;
		break ;
	case IST_ID :
		tb_cou_xpect = 7 ;
		break ;
	}
	}

	memset(ped_store,0,sizeof_ped) ;

	LOG(TERR,"Pedestals zapped: rb_mask 0x%02X, expected tb count %d",rb_mask,tb_cou_xpect) ;
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

	int max_tb = -1 ;

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

			int tb_cou = meta->arc[rdo1].arm[arm].apv[apv].ntim ;

			if(tb_cou != tb_cou_xpect) {
				if(fgt_stat[rdo1-1].err < 12) {
					LOG(WARN,"wrong tb_cou %d(expect %d): ARC %d, ARM %d, APV %d",tb_cou,tb_cou_xpect,rdo1,arm,apv) ;
				}
				fgt_stat[rdo1-1].err++ ;
			}

			if(tb_cou == 0) {
				// fully missing!
				err = 1 ;
			}
			else if(max_tb >= 0) {
				if(tb_cou != max_tb) {
					err = 1 ;
				}
			}
			else {
				max_tb = tb_cou ;
			}

			if(err) {
				meta_zs.status[arm][apv] |= 2 ;
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

		struct peds *p_thr = ped_store + (arc-1) ;

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
			
			if(tb==0) {
				if(dump) {
//					printf("*** dump ARC %d, ARM %d, APV %d, CH %d in %d tb\n",arc,arm,apv,ch,cou_tb) ;

					*d16++ = (cou_tb << 8) | ch ;
					dumped_cou++ ;

					for(int i=0;i<cou_tb;i++) {

						if(do_ped_sub) {
							*d16++ = (short)((float)f[i_save+i].adc - p_thr->ped[arm][apv][ch][i] + 0.5);	// had bug, was "+ 0.2"!
						}
						else {
							*d16++ = f[i_save+i].adc ;
						}
						

//						printf("   *** ch %d: %d %d\n",
//						       f[i_save+i].ch,
//						       f[i_save+i].tb,
//						       f[i_save+i].adc) ;
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


			if(adc >= thr) {
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
					*d16++ = (short)((float)f[i_save+i].adc - p_thr->ped[arm][apv][ch][i] + 0.5);	// had bug, was "+ 0.2"!
				}
				else {
					*d16++ = f[i_save+i].adc ;
				}

				//*d16++ = f[i_save+i].adc ;

//				printf("   *** ch %d: %d %d\n",
//				       f[i_save+i].ch,
//				       f[i_save+i].tb,
//				       f[i_save+i].adc) ;
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

	LOG(NOTE,"ARC %d: dumped %d/%d, %d bytes",rdo1,dumped_cou,all_cou,out_bytes) ;

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
//	if(evts[rdo] <= 3) {
//		LOG(NOTE,"RDO %d: skipping event %d < 3",rdo,evts[rdo]) ;
//		return ;
//	}

	if(fgt_stat[rdo].evts > 0xFF00) return ;	// don't allow more than 16bits worth...



	struct peds *p = ped_store + rdo ;

//	LOG(TERR,"Hello %p",fgt_rdr[0]) ;

	daq_dta *dd = 0 ;
	dd = fgt_rdr[rdo1-1]->handle_adc(0,rdo1, evbuff) ;

//	LOG(TERR,"Herein") ;

	char need[FGT_ARM_COU][FGT_APV_COU] ;
	memset(need,0,sizeof(need)) ;

	if(dd && dd->meta) {
		apv_meta_t *meta = (apv_meta_t *)dd->meta ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
			if(meta->arc[rdo1].arm[arm].apv[apv].present == 0) continue ;

			
			if(meta->arc[rdo1].arm[arm].apv[apv].ntim != tb_cou_xpect) {
				if(fgt_stat[rdo1-1].err < 10) {
					LOG(WARN,"evt %d: RDO %d, ARM %d, APV %d: ntim %d??",fgt_stat[rdo].evts,rdo1,arm,apv,meta->arc[rdo1].arm[arm].apv[apv].ntim) ;
				}
				fgt_stat[rdo1-1].err++ ;
			}

			need[arm][apv] |= 1 ;
			
			p->expect_cou[arm][apv]++ ;



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
			p->cou[arm][apv][ch][tb]++ ;
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

static int fgt_bad_heuristics(int rts_id, double ped, double rms)
{
	switch(rts_id) {
	case IST_ID :
		if((rms < 5.0)||(rms>50.0)||(ped>2000.0)||(ped<5.0)) return -1 ;
		break ;
	default :
		if((rms<=0.0) || (ped<0.0)) return -1 ;
		break ;
	}

	return 0 ;	// take it!
}


double fgtPed::do_thresh(double ns, int k)
{
	tb_cou = tb_cou_xpect ;
	char bad[FGT_RDO_COU][FGT_ARM_COU][FGT_APV_COU][FGT_CH_COU] ;

	if(!ped_store || !valid) {
		LOG(ERR,"fgt:do_thresh invalid") ;
		return -1.0 ;
	}

	n_sigma = ns ;
	k_seq = k ;

	memset(bad,0,sizeof(bad)) ;

	LOG(TERR,"do_thresh: n-sigma %f, k-seq %d",n_sigma, k_seq) ;

	// use the 0th timebin! For what???
	for(int r=0;r<FGT_RDO_COU;r++) {
		struct peds *p = ped_store + r ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
		for(int c=0;c<FGT_CH_COU;c++) {

		p->thr[arm][apv][c] = 0xFFFF ;	// max it out

		if(p->rms[arm][apv][c][0] < -1.5) continue ;	// not necessary at all

		double ped = 0.0 ;
		double rms = 0.0 ;
		int cou = 0 ;

		int c_bad = 0 ;
		for(int t=0;t<tb_cou;t++) {
			double pp = p->ped[arm][apv][c][t] ;
			double rm = p->rms[arm][apv][c][t] ;

	
			if(fgt_bad_heuristics(rts_id,pp,rm)) {
				//LOG(TERR,"%d %d %d %d %d %f %f\n",r,arm,apv,c,t,pp,rm) ;
				c_bad = 1 ;
			}

			if(rm < 0.0) continue ;	// should be here but not found in data, damn...

			//if(cou == 7) continue ;

			ped += pp ;
			rms += rm ;
			cou++ ;
		}

		if(c_bad) bad[r][arm][apv][c] = 1 ;

		if(cou == 0) continue ;

		if(tb_cou != cou) {
			LOG(WARN,"%d %d %d %d: expect %d timebins but have %d",r,arm,apv,c,tb_cou,cou) ;
		}


		ped /= cou ;
		rms /= cou ;

		p->thr[arm][apv][c] = (u_short) (ped + rms * n_sigma + 0.5) ;

		int aa = apv ;
		int off_id = (r)*6*20*128 + arm*20*128 ;
		if(apv >= 12) aa = apv - 2 ;

		off_id += aa*128+c ;

		//printf("TH %d: %f %f %d\n",off_id,ped,rms,cou) ;
		}
		}
		}
	}

	// kill bad
	int all_cou = 0 ;
	int bad_cou = 0 ;
	for(int r=0;r<FGT_RDO_COU;r++) {
		struct peds *p = ped_store + r ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {

		int b_cou = 0 ;
		for(int c=0;c<FGT_CH_COU;c++) {
			if(p->rms[arm][apv][c][0] < -1.5) continue ;	// not necessary at all
			if(bad[r][arm][apv][c]) {
				//p->thr[arm][apv][c] = 0xFFFF ;	// max it out
				b_cou++ ;
			}
			all_cou++ ;
		}

		if(b_cou) {
			bad_cou += b_cou ;
			LOG(WARN,"ARC %d, ARM %d, APV %2d: has %d bad channels",r,arm,apv,b_cou) ;
		}

		}
		}
	}
	double perc = 100.0*(double)bad_cou/(double)all_cou ;

	return  perc ;

}

void fgtPed::calc()
{

	const int MIN_EVENTS = 100 ;


	LOG(NOTE,"Calculating pedestals") ;

	tb_cou = 0 ;

	int bad = 0 ;
	int real_bad = 0 ;


	for(int r=0;r<FGT_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;



		struct peds *ped = ped_store + r ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {

		int expect = ped->expect_cou[arm][apv] ;
		int got_cou = ped->cou[arm][apv][0][0] ;

		if(got_cou != expect) {
			bad++ ;
			real_bad++ ;

			LOG(ERR,"ARC %d, ARM %d, APV %2d: got %d, expect %d",
			    r+1,arm,apv,got_cou,expect) ;
		}

		for(int ch=0;ch<FGT_CH_COU;ch++) {
		for(int t=0;t<FGT_TB_COU;t++) {

			if(ped->cou[arm][apv][ch][t] == 0) {
				ped->ped[arm][apv][ch][t] = 0 ;
				ped->rms[arm][apv][ch][t] = -1.0 ;
			}
			else {
				double pp, rr ;

		
				pp = ped->ped[arm][apv][ch][t] / (double) ped->cou[arm][apv][ch][t] ;
				rr = ped->rms[arm][apv][ch][t] / (double) ped->cou[arm][apv][ch][t] ;

				// due to roundoff I can have super small negative numbers
				if(rr < (pp*pp)) rr = 0.0 ;
				else rr = sqrt(rr - pp*pp) ;

				ped->ped[arm][apv][ch][t] = pp ;
				ped->rms[arm][apv][ch][t] = rr ;

				if(t > tb_cou) tb_cou = t ;

//				LOG(TERR,"RDO %d, ARM %d, APV %d, CH %d, TB %d: %f +- %f, cou %d",
//				    r,arm,apv,ch,t,pp,rr,ped->cou[arm][apv][ch][t]) ;
			}
		}
		}
		}
		}

	}


	tb_cou++ ;	// need to increment...


	int not_enough = 0 ;
	for(int r=0;r<FGT_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;

		if(fgt_stat[r].evts < MIN_EVENTS) not_enough = 1 ;
	}


	LOG(TERR,"Pedestals calculated. tb_count %d, RDO counts: %u",tb_cou,fgt_stat[0].evts) ;
	valid = 1 ;	// assume all OK...

	if(not_enough) valid = 0 ;
	else valid = !bad ;

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
	*dta++ = tb_cou ;		// timebin count


	for(r=0;r<FGT_RDO_COU;r++) {
		if(rb_mask && (1<<r)) ;
		else continue ;

		struct peds *ped = ped_store + r ;

		*dta++ = r+1 ;			// ARC, from 1
		u_short *apv_cou = dta++ ;
		*apv_cou = 0 ;

		// need to dump the apv_meta_zs_t bank!!!

		for(arm=0;arm<FGT_ARM_COU;arm++) {
		for(apv=0;apv<FGT_APV_COU;apv++) {

		if(ped->expect_cou[arm][apv] == 0) continue ;	// no hits at all...

		*dta++ = arm ;
		*dta++ = apv ;
		(*apv_cou)++ ;

		for(c=0;c<FGT_CH_COU;c++) {
		for(t=0;t<tb_cou;t++) {

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

int fgtPed::from_cache(char *fname) 
{
	FILE *f ;
	char *fn ;

	// zap rmses to negative!
	for(int r=0;r<FGT_RDO_COU;r++) {
		struct peds *ped = ped_store + r ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
		for(int c=0;c<FGT_CH_COU;c++) {

		for(int t=0;t<FGT_TB_COU;t++) {
			ped->ped[arm][apv][c][t] = 0.0 ;
			ped->rms[arm][apv][c][t] = -2.0 ;	// mark as not needed...
		}

		ped->thr[arm][apv][c] = 0xFFFF ;

		}
		}
		}
	}	
	
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
		LOG(ERR,"ped::from_cache can't open input file \"%s\" [%s]",fn,strerror(errno)) ;
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

		struct peds *peds = ped_store + (r-1) ;

		peds->ped[arm][apv][ch][tb] = pp ;
		peds->rms[arm][apv][ch][tb] = rr ;
	}

	fclose(f) ;
	LOG(TERR,"Pedestals loaded from cache \"%s\"",fn) ;


	valid = 1 ;

	return valid ;
}

int fgtPed::to_cache(char *fname, u_int run)
{
	FILE *f ;
	char f_fname[128] ;
	int do_ln = 1 ;

	if(!valid) {
		LOG(ERR,"ped::to_cache peds are bad: valid %d -- caching anyway...",valid) ;
		//do_ln = 0 ;

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


	LOG(TERR,"Writing pedestals to cache \"%s\" [valid %d]...",f_fname,valid) ;

	time_t tim = time(0) ;
	fprintf(f,"# Detector %s\n",rts2name(rts_id)) ;
	fprintf(f,"# Run %08u\n",run) ;
	fprintf(f,"# Date %s",ctime(&tim)) ;
	fprintf(f,"# Timebins %d\n",tb_cou) ;
	fprintf(f,"\n") ;

	for(int r=0;r<FGT_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;


		struct peds *peds = ped_store + r ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {

		if(peds->expect_cou[arm][apv] == 0) continue ;

		for(int c=0;c<FGT_CH_COU;c++) {
		for(int t=0;t<tb_cou;t++) {
//			if(peds->rms[arm][apv][c][t] < 0.0) continue ;

			fprintf(f,"%d %d %2d %3d %2d %7.3f %.3f\n",r+1,arm,apv,c,t,
				peds->ped[arm][apv][c][t],
				peds->rms[arm][apv][c][t]) ;
		}
		}
		}
		}
	}

	fclose(f) ;	

	if(do_ln) {
		char cmd[128] ;
		sprintf(cmd,"/bin/ln -f -s %s_pedestals_%08u.txt /RTScache/pedestals.txt",rts2name(rts_id),run) ;
		system(cmd) ;
	}

	LOG(TERR,"Pedestals written to cache \"%s\"",f_fname) ;

	return 1 ;
}

int fgtPed::special_setup(int run_type, int sub_type)
{

	// we do nothing here for FGT-like detectors...
	return 1 ;
}



