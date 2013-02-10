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

#include <DAQ_READER/daq_dta.h>

#include "fgtPed.h"



fgtPed::fgtPed()
{
	valid = 0 ;
	rb_mask = 0x03 ;	// assume all..

	memset(evts,0,sizeof(evts)) ;
	memset(valid_evts,0,sizeof(valid_evts)) ;
	
	sizeof_ped = sizeof(struct peds) * FGT_RDO_COU ;	// for FGT_RDO_COU RDOs

	
	ped_store = 0 ;	// unassigned!

	memset(fgt_rdr,0,sizeof(fgt_rdr)) ;

	k_seq = 0 ;	
	n_sigma = 0.0 ;

	return ;
}


fgtPed::~fgtPed()
{
	if(ped_store) {
		free(ped_store) ;
		ped_store = 0 ;
	}

	if(fgt_rdr[0]) {
		delete fgt_rdr[0] ;
		delete fgt_rdr[1] ;
		fgt_rdr[0] = 0 ;
		fgt_rdr[1] = 0 ;
	}

	return ;
}



void fgtPed::init(int active_rbs)
{
	valid = 0 ;

	memset(evts,0,sizeof(evts)) ;
	memset(valid_evts,0,sizeof(valid_evts)) ;

	rb_mask = active_rbs ;

	if(ped_store == 0) {
		ped_store = (struct peds *) malloc(sizeof_ped) ;
	}

	if(fgt_rdr[0] == 0) {
		fgt_rdr[0] = new daq_fgt(0) ;
		fgt_rdr[1] = new daq_fgt(0) ;
	}


	memset(ped_store,0,sizeof_ped) ;

/* wrong! Do it in from_cache
	for(int r=0;r<FGT_RDO_COU;r++) {
		struct peds *p = ped_store + r ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
		for(int c=0;c<FGT_CH_COU;c++) {
		for(int t=0;t<FGT_TB_COU;t++) {
			p->rms[arm][apv][c][t] = -1.0 ;
		}
		}
		}
		}
	}
*/



	LOG(TERR,"Pedestals zapped: rb_mask 0x%02X",rb_mask) ;
}


int fgtPed::do_zs(char *src, int in_bytes, char *dst, int rdo1)
{
	int ks = k_seq ;
	int dumped_cou = 0 ;
	int all_cou = 0 ;

	u_short *d16 = (u_short *) dst ;
	u_int *d32 = (u_int *)dst ;



	daq_dta *dd = fgt_rdr[rdo1-1]->handle_adc(0, rdo1, src) ;


	// create meta
	apv_meta_zs_t meta_zs ;
	apv_meta_t *meta = (apv_meta_t *) dd->meta ;

	memset(&meta_zs,0,sizeof(meta_zs)) ;

	int max_tb = -1 ;

	for(int arm=0;arm<FGT_ARM_COU;arm++) {
	for(int apv=0;apv<FGT_APV_COU;apv++) {
		printf("ARC %d, ARM %d, APV %d: %d %d %d %d\n",rdo1,arm,apv,
		       meta->arc[rdo1].arm[arm].apv[apv].present,
		       meta->arc[rdo1].arm[arm].apv[apv].ntim,
		       meta->arc[rdo1].arm[arm].apv[apv].error,
		       meta->arc[rdo1].arm[arm].error) ;

		if(meta->arc[rdo1].arm[arm].apv[apv].present) {
			int err = 0 ;

			meta_zs.status[arm][apv] |= 1 ;

			int tb_cou = meta->arc[rdo1].arm[arm].apv[apv].ntim ;

			if(tb_cou == 0) {
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

		switch(meta_zs.status[arm][apv]) {
		case 0 :
		case 1 :
			break ;
		default :
			LOG(ERR,"ARC %d, ARM %d, APV %d: meta_zs 0x%X",rdo1,arm,apv,meta_zs.status[arm][apv]) ;
		}

	}}

	meta_zs.tb_cou = max_tb ;

	// roundoff to a 16 bit word...
	int meta_bytes = sizeof(meta_zs) ;
	while(meta_bytes % 2) meta_bytes++ ;

	*d32++ = 0xFEEDBEEF ;	// signature
	*d32++ = META_ZS_VERSION ;		// version
	u_int *dta_bytes = d32++ ;	// reserve space
	*d32++ = meta_bytes ;

	memcpy(d32,&meta_zs,meta_bytes) ;
	d16 = (u_short *)d32 + meta_bytes/2 ;

	while(dd && dd->iterate()) {
		int arc, arm, apv ;

		arc = dd->rdo ;
		arm = dd->sec ;
		apv = dd->pad ;

//		printf("ARC %d, ARM %d, APV %d: entries %d\n",
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

//		printf("*** ARC %d %d %d\n",arc,arm,apv) ;
//		printf("*** ARM %d, APV %d\n",arm,apv) ;

		for(u_int i=0;i<dd->ncontent;i++) {
			int tb = f[i].tb ;
			int adc = f[i].adc ;
			
			if(tb==0) {
				if(dump) {
//					printf("*** dump CH %d in %d tb\n",ch,cou_tb) ;
					*d16++ = (cou_tb << 8) | ch ;
					dumped_cou++ ;

					for(int i=0;i<cou_tb;i++) {

						*d16++ = f[i_save+i].adc ;

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
//			printf("*** LAST dump CH %d in %d tb\n",ch,cou_tb) ;
			*d16++ = (cou_tb <<8) | ch  ;
			dumped_cou++ ;

			for(int i=0;i<cou_tb;i++) {

				*d16++ = f[i_save+i].adc ;

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



	evts[rdo]++ ;

	// skip first few events!
//	if(evts[rdo] <= 3) {
//		LOG(NOTE,"RDO %d: skipping event %d < 3",rdo,evts[rdo]) ;
//		return ;
//	}

	if(valid_evts[rdo] > 0xFF00) return ;	// don't allow more than 16bits worth...

	valid_evts[rdo]++ ;


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


			if(meta->arc[rdo1].arm[arm].apv[apv].ntim != 15) {
				LOG(WARN,"evt %d: RDO %d, ARM %d, APV %d: ntim %d??",evts[rdo],rdo1,arm,apv,meta->arc[rdo1].arm[arm].apv[apv].ntim) ;
			}

			need[arm][apv] |= 1 ;
			
			p->expect_cou[arm][apv]++ ;



			if(meta->arc[rdo1].arm[arm].apv[apv].apv_id != apv) {
				LOG(WARN,"RDO %d, ARM %d, APV %d: %d",rdo1,arm,apv,meta->arc[rdo1].arm[arm].apv[apv].apv_id) ;
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

void fgtPed::do_thresh(double ns, int k)
{
	tb_cou = -1 ;

	if(!ped_store || !valid) {
		LOG(ERR,"fgt:do_thresh invalid") ;
		return ;
	}

	n_sigma = ns ;
	k_seq = k ;

	LOG(TERR,"do_thresh: n-sigma %f, k-seq %d",n_sigma, k_seq) ;

	// use the 0th timebin!
	for(int r=0;r<FGT_RDO_COU;r++) {
		struct peds *p = ped_store + r ;

		for(int arm=0;arm<FGT_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
		for(int c=0;c<FGT_CH_COU;c++) {

		p->thr[arm][apv][c] = 0xFFFF ;	// max it out

		double ped = 0.0 ;
		double rms = 0.0 ;
		int cou = 0 ;

		for(int t=0;t<FGT_TB_COU;t++) {
			double pp = p->ped[arm][apv][c][t] ;
			double rm = p->rms[arm][apv][c][t] ;

			if(rm < 0.0) continue ;
			//if(cou == 7) continue ;

			ped += pp ;
			rms += rm ;
			cou++ ;
		}

		if(cou==0) continue ;

		if(tb_cou < 0) tb_cou = cou ;
		else {
			if(tb_cou != cou) {
				LOG(WARN,"%d %d %d %d: expect %d timebins but have %d",r,arm,apv,c,tb_cou,cou) ;
			}
		}

		ped /= cou ;
		rms /= cou ;

		p->thr[arm][apv][c] = (u_short) (ped + rms * n_sigma + 0.5) ;

		int aa = apv ;
		int off_id = (r)*6*20*128 + arm*20*128 ;
		if(apv >= 12) aa = apv - 2 ;

		off_id += aa*128+c ;

		printf("TH %d: %f %f %d\n",off_id,ped,rms,cou) ;
		}
		}
		}
	}

	return ;

}

void fgtPed::calc()
{

	const u_int MIN_EVENTS = 2 ;


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

		if(valid_evts[r] < MIN_EVENTS) not_enough = 1 ;
	}


	LOG(TERR,"Pedestals calculated. tb_count %d, RDO counts: %u %u",tb_cou,valid_evts[0],valid_evts[1]) ;

	valid = 1 ;	// assume all OK...

	if(not_enough) valid = 0 ;
	else valid = !bad ;

	if(!valid) {
		LOG(ERR,"FGT pedestals not good: APVs bad %d, events %d %d",bad,valid_evts[0],valid_evts[1]) ;
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
			ped->rms[arm][apv][c][t] = -1.0 ;
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
	char *fn ;


	if(!valid) {
		LOG(ERR,"ped::to_cache peds are bad: valid %d -- not caching",valid) ;
		return -1 ;
	}

	if(fname) {
		fn = fname ;
	}
	else {
		fn = "/RTScache/pedestals.txt" ;
	}


	f = fopen(fn,"w") ;
	if(f==0) {
		LOG(ERR,"ped::to_cache can't open output file \"%s\" [%s]",fn,strerror(errno)) ;
		return -1 ;
	}


	LOG(NOTE,"Writing pedestals to cache \"%s\"...",fn) ;

	time_t tim = time(0) ;
	fprintf(f,"### Detector %s, Run number %u, Date %s",fgt_rdr[0]->name,run,ctime(&tim)) ;
	fprintf(f,"### Timebin %d\n",tb_cou) ;

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

	LOG(TERR,"Pedestals written to cache \"%s\"",fn) ;

	return 1 ;
}

int fgtPed::special_setup(int run_type, int sub_type)
{

	// we do nothing here for FGT-like detectors...
	return 1 ;
}



