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

#include "fgtPed.h"	// for some defines
#include "fstPed.h"


// real time in useconds
static u_int t_mark()
{
        timespec ts ;

        clock_gettime(CLOCK_MONOTONIC,&ts) ;

        return (u_int)(ts.tv_sec*1000000+ts.tv_nsec/1000) ;
}

// returns microseconds; good enough
static u_int t_delta(u_int mark)
{
        return (unsigned int)((t_mark() - mark)) ;
}


char fstPed::cmnGroup[24][128] ;




int fstPed::load_group(const char *fname)
{
	if(fname==0) fname = "/RTS/conf/fst/cmnGroup.txt" ;

	memset(cmnGroup,0xFF,sizeof(cmnGroup)) ;	// set to -1

	FILE *f = fopen(fname,"r") ;
	if(f==0) {
		LOG(ERR,"Group file %s [%s]",fname,strerror(errno)) ;
		return -1 ;
	}

	while(!feof(f)) {
		char buff[32] ;
		int apv, ch, grp ;

		if(fgets(buff,sizeof(buff),f)==0) continue ;

		int ret = sscanf(buff,"%d %d %d",&apv,&ch,&grp) ;
		if(ret != 3) continue ;

		cmnGroup[apv][ch] = grp ;	// is negative if it doesn't exist!
	}
	fclose(f) ;

//	for(int i=0;i<24;i++) {
//		LOG(TERR,"cmnGroup: APV %d, ch 0: %d",i,cmnGroup[i][0]) ;
//	}

	LOG(INFO,"cmnGroup %s loaded.",fname) ;

	return 0 ;


}

fstPed::fstPed(int rts)
{
	valid = 0 ;
	rb_mask = 0 ;	

	memset(fgt_stat,0,sizeof(fgt_stat)) ;


	k_seq = 0 ;	
	n_sigma = 0.0 ;


	tb_cou_xpect = -1 ;
	tb_cou_ped = -1 ;

	memset(ch_status,0,sizeof(ch_status)) ;

	evts_for_rms = 500 ;
	evts_for_cmn = 500 ;

	for(int r=0;r<6;r++) {
	for(int ar=0;ar<6;ar++) {
	for(int ap=0;ap<24;ap++) {
	for(int c=0;c<128;c++) {
		ch_status[r][ar][ap][c] = FGT_CH_STAT_SHOULD ;
	}}}

	}
	rb_mask = 0x3F ;	// this will depend

	for(int t=0;t<8;t++) {	// for 8 threads!
		fgt_rdr[t] = new daq_fgt(0) ;
	}

	// reasomable defaults
	nSigmaCut = 3.0 ;

	nPedsCut1 = 3.5 ;
	nPedsTbs1 = 1 ;

	nPedsCut2 = 2.5 ;
	nPedsTbs2 = 2 ;

	nSeedsCut = 4.0 ;
	nSeedsTbs = 2 ;
	
	return ;
}



int fstPed::run_stop()
{
	for(int r=0;r<FGT_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;

		int evts = fgt_stat[r].evts ;

		if(fgt_stat[r].err) {
			LOG(WARN,"RDO %d: %d errors in %d events [but this is questionable]",r+1,fgt_stat[r].err,evts) ;
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


void fstPed::init(int active_rbs)
{
	memset(fgt_stat,0,sizeof(fgt_stat)) ;

	rb_mask = active_rbs ;

}


int fstPed::do_zs(char *src, int in_bytes, char *dst, int rdo1, int id)
{
//	int ks = k_seq ;
	int dumped_cou = 0 ;
//	int all_cou = 0 ;

	u_int mark = t_mark() ;

	short *d16 = (short *) dst ;
	u_int *d32 = (u_int *)dst ;

	fgt_stat[rdo1-1].evts++ ;
//	int evt = fgt_stat[rdo1-1].evts ;

	daq_dta *dd = fgt_rdr[id]->handle_adc(sector, rdo1, src) ;


	// create meta
	apv_meta_zs_t meta_zs ;
	apv_meta_t *meta = (apv_meta_t *) dd->meta ;

	memset(&meta_zs,0,sizeof(meta_zs)) ;

	int t_xpect = tb_cou_ped ;

	int max_tb = -1 ;

	total_charge = 0 ;

	for(int arm=0;arm<FST_ARM_COU;arm++) {
	for(int apv=0;apv<FGT_APV_COU;apv++) {
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

//	int do_ped_sub = 0 ;	// non ped-sub

//	do_ped_sub = 1 ;
	*d32++ = META_PED_ZS_VERSION ;	// zs _AND_ ped subtracted

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

		struct peds_t *p = &(peds[arc-1]) ;

//		int i_save = -1 ;
//		int cou = 0 ;
//		int dump = 0 ;
//		int thr = 0 ;
//		int ch = -1 ;
//		int cou_tb = 0 ;

		*d16++ = 0xAB00 | arc ;
		*d16++ = (arm << 8) | apv ;


		struct grp_t {
			double mean ;
			int cou ;
		} grp[4][9] ;

		memset(grp,0,sizeof(grp)) ;
		int bad_group = 0 ;

		u_short dta[128][9] ;	// cache


		//u_int mark = t_mark() ;

		// first pass, accumulate for the CMN
		for(u_int i=0;i<dd->ncontent;i++) {
			int ch = f[i].ch ;

			int g = cmnGroup[apv][ch] ;
			if(g<0) continue ;


			int tb = f[i].tb ;

			// if the timebin of the data is larger than the timebin of 
			// the available pedestals/thresholds -- silently drop!!!

			if(tb >= max_tb) continue ;

			int adc = f[i].adc ;



			double mean = p->ped[arm][apv][ch][tb] ;
			double rms = p->rms[arm][apv][ch][tb] ;


//			printf("     ch %d, tb %d: mean %f, rms %f: adc = %d\n",ch,tb,mean,rms,adc) ;

			dta[ch][tb] = adc ;

			if(adc < (mean+nSigmaCut*rms)) {
				grp[g][tb].mean += adc - mean ;
				grp[g][tb].cou++ ;
			}

		}
		
		//mark = t_delta(mark) ;
		//printf("pass1 %u\n",mark) ;
		//mark = t_mark() ;

		// second pass: calculate CMN
		for(int g=0;g<4;g++) {
		for(int tb=0;tb<9;tb++) {
			if(grp[g][tb].cou) grp[g][tb].mean /= grp[g][tb].cou ;
			else {
				bad_group ++ ;	// and now what????
				grp[g][tb].mean = 10000000.0 ;
			}
		}}

		//mark = t_delta(mark) ;
		//printf("pass2 %u\n",mark) ;
		//mark = t_mark() ;

		// 3rd pass do the hits pass
		for(int ch=0;ch<128;ch++) {
			int g = cmnGroup[apv][ch] ;
			if(g<0) continue ;

			int tb_cou[3] ;
			double d[9] ;

			tb_cou[0] = tb_cou[1] = tb_cou[2] = 0 ;

			int t_cou = 0 ;
			int pass = 0 ;

			for(int tb=0;tb<9;tb++) {
				if(tb>=max_tb) continue ;

				if(grp[g][tb].mean > 10000.0) {
					pass |= 8 ;
					break ;
				}
			}

			for(int tb=0;tb<9;tb++) {
				double dd ;

				if(tb >= max_tb) continue ;

				t_cou++ ;

				double cmn_mean = grp[g][tb].mean ;
				double mean = p->ped[arm][apv][ch][tb] ;
				double cmn_rms = p->cmn_rms[arm][apv][ch][tb] ;

				if(pass) {	// NOTE: special ERROR case!!!
					dd = dta[ch][tb] - mean ;
				}
				else {
					dd = dta[ch][tb] - mean - cmn_mean ;
				}


				if(dd > nSeedsCut*cmn_rms) tb_cou[0]++ ;
				if(dd > nPedsCut1*cmn_rms) tb_cou[1]++ ;
				if(dd > nPedsCut2*cmn_rms) tb_cou[2]++ ;

				d[tb] = dd ;
			}


			if(tb_cou[0]>=nSeedsTbs) pass |= 1 ;
			if(tb_cou[1]>=nPedsTbs1) pass |= 2 ;
			if(tb_cou[2]>=nPedsTbs2) pass |= 4 ;

			// dump out
			if(pass) {
				*d16++ = (t_cou<<8)|ch ;
				dumped_cou++ ;

				for(int tb=0;tb<t_cou;tb++) {
					int id ;

					if(d[tb]<0) id = 0 ;
					else id = ((int)(d[tb]+0.5))&0xFFF ;	// roundoff ;

					*d16++ = (pass<<12)|id ;
				}
			}
		}

		//mark = t_delta(mark) ;
		//printf("pass3 %u\n",mark) ;
		//mark = t_mark() ;

//		if(bad_group >10) {	// this happens often!
//			LOG(WARN,"%d: EVT %d: ARM %d, APV %d: %d bad CMN",rdo1,evt,arm,apv,bad_group) ;
//		}
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

//	printf("RDO: %d us\n",t_delta(mark)) ;

	return out_bytes ;
}


/*
	Called per event, per RDO. evbbuff is the raw RDO contribuition.
	rdo counts from 1.
*/
void fstPed::accum(char *evbuff, int bytes, int rdo1)
{
	int do_cmn = 0 ;
	int rdo = rdo1 - 1 ;	// since rdo1 is from 1



	fgt_stat[rdo].evts++ ;

	int evt = fgt_stat[rdo].evts ;

	if(fgt_stat[rdo].evts < evts_for_rms) do_cmn = 0 ;
	else if(fgt_stat[rdo].evts==evts_for_rms) do_cmn = 1 ;	// CALC
	else do_cmn = 2 ;

	// skip first few events!
//	if(fgt_stat[rdo].evts <= 3) {
//		return ;
//	}

	if(fgt_stat[rdo].evts > 0xFF00) return ;	// don't allow more than 16bits worth...

	struct peds_t *p = peds + rdo ;

	daq_dta *dd = 0 ;
	dd = fgt_rdr[rdo1-1]->handle_adc(sector,rdo1, evbuff) ;


	int t_xpect = tb_cou_xpect ;

	char need[FST_ARM_COU][FGT_APV_COU] ;
	memset(need,0,sizeof(need)) ;

	if(dd && dd->meta) {
		apv_meta_t *meta = (apv_meta_t *)dd->meta ;

		for(int arm=0;arm<FST_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
			if(meta->arc[rdo1].arm[arm].apv[apv].present == 0) continue ;

			if(meta->arc[rdo1].arm[arm].apv[apv].ntim != t_xpect) {
				if(fgt_stat[rdo1-1].err <= 10) {
					LOG(WARN,"evt %d: RDO %d, ARM %d, APV %d: ntim %d, expect %d??",fgt_stat[rdo].evts,rdo1,arm,apv,meta->arc[rdo1].arm[arm].apv[apv].ntim,t_xpect) ;
					if(fgt_stat[rdo1-1].err==10) {
						LOG(WARN,"evt %d: RDO %d: NO more warnings",fgt_stat[rdo].evts,rdo1) ;
					}
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

	if(do_cmn==1) {
		LOG(INFO,"%d: calculating pedestals at event %d",rdo1,fgt_stat[rdo].evts) ;
	}

	struct fst_grp_t {
		double mean ;
		int cou ;
	} fst_grp[4][9] ;

	u_short grp_dta[128][9] ;

	memset(fst_grp,0,sizeof(fst_grp)) ;

	int good_cmn = 0 ;

	while(dd && dd->iterate()) {
		if(dd->rdo != rdo1) continue ;

		int arm = dd->sec ;
		int apv = dd->pad ;

		fgt_adc_t *f = (fgt_adc_t *) dd->Void ;

		need[arm][apv] |= 4 ;

		//if(do_cmn==2) {
		//	LOG(TERR,"%d: evt %d: cmd1: arm %d, apv %d",rdo1,evt,arm,apv) ;
		//}


		int for_cmn = 0 ;

		if(do_cmn==2) memset(fst_grp,0,sizeof(fst_grp)) ;

		for(u_int i=0;i<dd->ncontent;i++) {
			int adc ;
			int ch ;
			int tb ;

			ch = f[i].ch ;
			adc = f[i].adc ;
			tb = f[i].tb ;

			
			if(do_cmn==0) {
				p->ped[arm][apv][ch][tb] += adc ;
				p->rms[arm][apv][ch][tb] += (adc * adc) ;
				p->cou[arm][apv][ch][tb]++ ;
				
				//if(arm==0 && apv==0 && ch==0 && tb==0) {
				//	LOG(TERR,"do_cmn=0  %d",adc) ;
				//}
				
			}
			else if(do_cmn==1) {
				int cou = p->cou[arm][apv][ch][tb] ;
				double mean, rms ;

				if(cou==0) {
					mean = -1.0 ;
					rms = 0.0 ;
				}
				else {
					mean = p->ped[arm][apv][ch][tb] / cou ;
					rms = p->rms[arm][apv][ch][tb] / cou ;

					rms = sqrt(rms-mean*mean) ;
				}

				p->ped[arm][apv][ch][tb] = mean ;
				p->rms[arm][apv][ch][tb] = rms ;

				//if(arm==0 && apv==0 && ch==0 && tb==0) {	// executes twice!!!
				//	LOG(TERR," PED mean %f, rms %f",mean,rms) ;
				//}


				//printf("%d %d %d %d %d = %f %f\n",rdo1,arm,apv,ch,tb,mean,rms) ;

				p->cou[arm][apv][ch][tb] = 0 ;	// restart for next step
			}
			else {
				int g = cmnGroup[apv][ch] ;
				if(g<0) continue ;

				double mean = p->ped[arm][apv][ch][tb] ;
				double rms = p->rms[arm][apv][ch][tb] ;

				grp_dta[ch][tb] = adc ;	// cache data so we don't need to pass again

				//printf("%d %d %d %d %d = %f %f; %d\n",rdo1,arm,apv,ch,tb,mean,rms,adc) ;

				if(mean<0.0) continue ;

				if(adc < (mean+nSigmaCut*rms)) {
					for_cmn++ ;
					fst_grp[g][tb].mean += adc - mean ;
					fst_grp[g][tb].cou++ ;
				}

			}
		

		}

		if(do_cmn==2) {
			if(for_cmn < 10) LOG(WARN,"%d: evt %d: ARM %d, APV %d: for cmn %d",rdo1,evt,arm,apv,for_cmn) ;

			//int good = 0 ;

			for(int g=0;g<4;g++) {
			for(int tb=0;tb<9;tb++) {
				if(fst_grp[g][tb].cou) fst_grp[g][tb].mean /= fst_grp[g][tb].cou ;
				else {
					//LOG(WARN,"%d: evt %d: ARM %d, APV %d, cmn_group %d, tb %d not there?",rdo1,evt,arm,apv,g,tb) ;
					fst_grp[g][tb].mean = 1000000.0 ;
				}
			}}

			for(int ch=0;ch<128;ch++) {
				int g = cmnGroup[apv][ch] ;

				if(g<0) continue ;

				for(int tb=0;tb<9;tb++) {
					int d = grp_dta[ch][tb] ;

					double g_mean = fst_grp[g][tb].mean ;
				
					if(g_mean > 1024) continue ;

					double mean = p->ped[arm][apv][ch][tb] ;
					double rms = p->rms[arm][apv][ch][tb] ;
					
					if(mean<0.0) continue ;

					//if(arm==0 && apv==0 && ch==0 && tb==0) {
					//	LOG(TERR,"  mean %f, rms %f, g_mean %f: adc %d",mean,rms,g_mean,d) ;
					//}

					if(d < (mean+nSigmaCut*rms)) {
						p->cmn_ped[arm][apv][ch][tb] += d-mean-g_mean ;
						p->cmn_rms[arm][apv][ch][tb] += (d-mean-g_mean)*(d-mean-g_mean) ;
						p->cou[arm][apv][ch][tb]++ ;
						good_cmn++ ;
					}
				}
			}

			//if(good < 10) LOG(WARN,"%d: EVT %d: ARM %d, APV %d: good CMN %d",rdo1,evt,arm,apv,good) ;
		}
		
	}

	if(do_cmn==2 && good_cmn<40000) LOG(WARN,"%d: evt %d: good_cmn %d",rdo1,evt,good_cmn) ;

	// the following is just an error dump...
	for(int arm=0;arm<FST_ARM_COU;arm++) {
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



double fstPed::do_thresh(double ns, int k, int do_log)
{
	// suspect
	// tb_cou set in from_cache
	


	n_sigma = ns ;
	k_seq = k ;


	LOG(INFO,"do_thresh: n-sigma %f, k-seq %d",n_sigma, k_seq) ;


	for(int r=0;r<FGT_RDO_COU;r++) {
		struct peds_t *p = peds + r ;

		for(int arm=0;arm<FST_ARM_COU;arm++) {
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
			/////////////p->thr[arm][apv][c] = 0xFFFD ;
			continue ;
		}

		ped /= cou ;
		rms /= cou ;

		//////////p->thr[arm][apv][c] = (u_short) (ped + rms * n_sigma + 0.5) ;

		}
		}
		}
	}

	// kill bad
	int b_all_all = 0 ;
	int b_bad_all = 0 ;

	for(int r=0;r<FGT_RDO_COU;r++) {
//		struct peds_t *p = peds + r ;

		if(rb_mask & (1<<r)) ;
		else continue ;

		int b_ped_0 = 0 ;
		int b_bad  = 0 ;
		int b_masked  = 0 ;
		int b_misconfigured  = 0 ;
		int b_all = 0 ;
		int b_unknown = 0 ;

		int err = 0 ;

		for(int arm=0;arm<3;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
		for(int c=0;c<FGT_CH_COU;c++) {
			if(ch_status[r][arm][apv][c] & FGT_CH_STAT_SHOULD) ;
			else {
				/////////p->thr[arm][apv][c] = 0xFFFE ;	// not physically present...
				continue ;
			}

//			if(cmnGroup[apv][c]<0) continue ;

			b_all++ ;

			// kill any channel marked odd!
			if(ch_status[r][arm][apv][c] & 0xFE) {
				///////////p->thr[arm][apv][c] = 0xFFFF ;
				b_bad_all++ ;
			}
/////			else if(p->thr[arm][apv][c] >= 0xFFFD) {
/////				//LOG(WARN,"Killed but not marked: %d %d %d %d",r,arm,apv,c) ;
/////				b_unknown++ ;
/////				b_bad_all++ ;
/////			}
			
			if(ch_status[r][arm][apv][c] & FGT_CH_STAT_NO_CONFIG) {
				b_masked++ ;
			}
			if(ch_status[r][arm][apv][c] & FGT_CH_STAT_NO_RESPONSE) {
				b_misconfigured++ ;
				err = 1 ;
			}
			if(ch_status[r][arm][apv][c] & FGT_CH_STAT_BAD) {
				b_bad++ ;
				err = 1 ;
			}
			if(ch_status[r][arm][apv][c] & FGT_CH_STAT_PED_UNKNOWN) {
				b_ped_0++ ;
				err = 1 ;
			}
			

			
		}}}

		b_all_all += b_all ;

		if(err) {
			LOG(WARN,"ARC %d: masked %d, misconfigd %d, bad %d, bad ped %d, unknown %d of %d all",
			    r+1,b_masked,b_misconfigured,b_bad,b_ped_0,b_unknown,b_all) ;

		}
		else {
			LOG(WARN,"ARC %d: masked %d, misconfigd %d, bad %d, bad ped %d, unknown %d of %d all",
			    r+1,b_masked,b_misconfigured,b_bad,b_ped_0,b_unknown,b_all) ;
		}

	}

	double perc = (double)b_bad_all/(double)b_all_all ;
	perc *= 100.0 ;

	return perc ;

}

void fstPed::calc()
{

	const int MIN_EVENTS =  1000 ;


	LOG(NOTE,"Calculating pedestals") ;

	tb_cou_ped = -1 ;

	int bad = 0 ;



	for(int r=0;r<FGT_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;



		struct peds_t *ped = peds + r ;

		for(int arm=0;arm<FST_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
		for(int ch=0;ch<FGT_CH_COU;ch++) {


		
		for(int t=0;t<tb_cou_xpect;t++) {
			int cou = ped->cou[arm][apv][ch][t] ;

			if(cou == 0) {	// never seen in the data!
				ped->cmn_ped[arm][apv][ch][t] = -1.0 ;
				ped->cmn_rms[arm][apv][ch][t] = 0 ;
			}
			else {
				double pp, rr ;

		
				pp = ped->cmn_ped[arm][apv][ch][t] / cou ;
				rr = ped->cmn_rms[arm][apv][ch][t] / cou ;


				// due to roundoff I can have super small negative numbers
				if(rr < (pp*pp)) rr = 0.0 ;
				else rr = sqrt(rr - pp*pp) ;

				ped->cmn_ped[arm][apv][ch][t] = pp ;
				ped->cmn_rms[arm][apv][ch][t] = rr ;

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


int fstPed::to_evb(char *buff)
{
	int r, arm, apv, c, t ;
	u_short *dta = (u_short *) buff ;	


	if(!valid) {
		// log error but continue...
		LOG(ERR,"ped::to_evb peds are bad: valid %d",valid) ;
	}

	LOG(NOTE,"Preparing pedestals for later EVB...") ;

	*dta++ = 0xBEEF ;		// signature
	*dta++ = 0x0002 ;		// version: 2 for FST!
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

		for(arm=0;arm<FST_ARM_COU;arm++) {
		for(apv=0;apv<FGT_APV_COU;apv++) {

		if(cmnGroup[apv][0]<0) continue ;

		*dta++ = arm ;
		*dta++ = apv ;
		(*apv_cou)++ ;

		for(c=0;c<FGT_CH_COU;c++) {
		for(t=0;t<tb_cou_ped;t++) {

				u_short pp ;

				pp = (u_short)(ped->ped[arm][apv][c][t]*16.0 + 0.5)  ;
				*dta++ = pp;

				pp = (u_short)(ped->rms[arm][apv][c][t]*16.0 + 0.5) ;
				*dta++ = pp ;

				pp = (u_short)(ped->cmn_rms[arm][apv][c][t]*16.0 + 0.5) ;
				*dta++ = pp ;

		}
		}
		}
		}

	}

	LOG(TERR,"Pedestals prepared for later EVB, %d bytes",(char *)dta-buff) ;

	return ((char *)dta-buff) ;
}

void fstPed::clear()
{
	memset(peds,0,sizeof(peds)) ;
}

void fstPed::clear_from_cache()
{
	return ;	
#if 0
	for(int r=0;r<FGT_RDO_COU;r++) {
		struct peds_t *p = peds_from_cache + r ;

		for(int arm=0;arm<FST_ARM_COU;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
		for(int c=0;c<FGT_CH_COU;c++) {
			for(int t=0;t<FST_TB_COU;t++) {
				p->ped[arm][apv][c][t] = 0.0 ;
				p->rms[arm][apv][c][t] = -2.0 ;
				p->cmn_rms[arm][apv][c][t] = -2.0 ;
				p->cou[arm][apv][c][t] = 0 ;
			}
			////p->thr[arm][apv][c] = 0xFFFF ;
		}
		}
		}
	}

#endif

}

int fstPed::from_cache(char *fname) 
{
	FILE *f ;
	const char *fn ;
	char file[64] ;

	tb_cou_ped = - 1;

	clear() ;
	clear_from_cache() ;


	if(fname) {
		fn = fname ;
	}
	else {
		sprintf(file,"/RTScache/fst_s%d_pedestals.txt",sector) ;
		fn = file ;
	}

	f = fopen(fn,"r") ;
	if(f==0) {
		LOG(U_TONKO,"ped::from_cache can't open input file \"%s\" [%s]",fn,strerror(errno)) ;
		return -1 ;
	}


	LOG(INFO,"Loading pedestals from cache \"%s\"...",fn) ;

	

	while(!feof(f)) {
		int r, arm, apv, ch, tb ;
		float pp, rr, cmn_rms ;
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

		   
		int ret = sscanf(buff,"%d %d %d %d %d %f %f %f",&r,&arm,&apv,&ch,&tb,&pp,&rr,&cmn_rms) ;
		if(ret != 8) continue ;


		struct peds_t *ped = peds + (r-1) ;

		ped->ped[arm][apv][ch][tb] = pp ;
		ped->rms[arm][apv][ch][tb] = rr ;
		ped->cmn_rms[arm][apv][ch][tb] = cmn_rms ;
#if 0
		struct peds_t *ped_fc = peds_from_cache + (r-1) ;

		ped_fc->ped[arm][apv][ch][tb] = pp ;
		ped_fc->rms[arm][apv][ch][tb] = rr ;
		ped_fc->cmn_rms[arm][apv][ch][tb] = cmn_rms ;
#endif		
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

int fstPed::to_cache(char *fname, u_int run, int dont_cache)
{
	FILE *f ;
	char f_fname[128] ;

	int bad = 0 ;
	for(int r=0;r<FGT_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;


		if(fgt_stat[r].evts < 1000) {
			LOG(ERR,"%d: not enough events %d",r+1,fgt_stat[r].evts) ;
			bad = 1 ;
		}
	}

	if(bad || dont_cache || !valid) bad = 1 ;	

	if(bad) {
		LOG(ERR,"ped::to_cache peds are bad: valid %d %d %d-- caching anyway...",bad,dont_cache,valid) ;
		//LOG(CAUTION,"Pedestals are not valid -- not caching!") ;
	}

	if(fname) {
		strcpy(f_fname,fname) ;
	}
	else {
		sprintf(f_fname,"/RTScache/fst_s%d_pedestals_%08u_%s.txt",sector,run,bad?"BAD":"GOOD") ;
	}


	f = fopen(f_fname,"w") ;
	if(f==0) {
		LOG(ERR,"ped::to_cache can't open output file \"%s\" [%s]",f_fname,strerror(errno)) ;
		return -1 ;
	}


	if(tb_cou_ped != tb_cou_xpect) {
		LOG(ERR,"Writing pedestals to cache \"%s\" [valid %d] but data has %d timebins != expect %d!",f_fname,valid,
		    tb_cou_ped,tb_cou_xpect) ;
	}
	else {
		LOG(INFO,"Writing pedestals to cache \"%s\" [valid %d], ntimebins %d",f_fname,valid,tb_cou_ped) ;
	}

	time_t tim = time(0) ;
	fprintf(f,"# Detector FST%d\n",sector) ;
	fprintf(f,"# Run %08u\n",run) ;
	fprintf(f,"# Date %s",ctime(&tim)) ;
	fprintf(f,"# Timebins %d\n",tb_cou_ped) ;
	fprintf(f,"\n") ;


	for(int r=0;r<FGT_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;


		struct peds_t *ped = peds + r ;

		for(int arm=0;arm<3;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
		for(int c=0;c<FGT_CH_COU;c++) {

			// dump only the ones which need to exist!
			if(ch_status[r][arm][apv][c] & FGT_CH_STAT_SHOULD) ;
			else continue ;

			if(cmnGroup[apv][c]<0) continue ;

			for(int t=0;t<tb_cou_ped;t++) {

				fprintf(f,"%d %d %2d %3d %2d %7.3f %.3f %.3f\n",r+1,arm,apv,c,t,
					ped->ped[arm][apv][c][t],
					ped->rms[arm][apv][c][t],
					ped->cmn_rms[arm][apv][c][t]) ;
			}
		}
		}
		}
	}

	fclose(f) ;	

	if(fname) {
		strcpy(f_fname,fname) ;
	}
	else {
		sprintf(f_fname,"/RTScache/fst_s%d_pedestals_cmn_%08u_%s.txt",sector,run,bad?"BAD":"GOOD") ;
	}


	f = fopen(f_fname,"w") ;
	if(f==0) {
		LOG(ERR,"ped::to_cache can't open output file \"%s\" [%s]",f_fname,strerror(errno)) ;
		return -1 ;
	}


	if(tb_cou_ped != tb_cou_xpect) {
		LOG(ERR,"Writing pedestals to cache \"%s\" [valid %d] but data has %d timebins != expect %d!",f_fname,valid,
		    tb_cou_ped,tb_cou_xpect) ;
	}
	else {
		LOG(INFO,"Writing pedestals to cache \"%s\" [valid %d], ntimebins %d",f_fname,valid,tb_cou_ped) ;
	}

	tim = time(0) ;
	fprintf(f,"# Detector FST%d\n",sector) ;
	fprintf(f,"# Run %08u\n",run) ;
	fprintf(f,"# Date %s",ctime(&tim)) ;
	fprintf(f,"# Timebins %d\n",tb_cou_ped) ;
	fprintf(f,"\n") ;


	for(int r=0;r<FGT_RDO_COU;r++) {
		if(rb_mask & (1<<r)) ;
		else continue ;


		struct peds_t *ped = peds + r ;

		for(int arm=0;arm<3;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {
		for(int c=0;c<FGT_CH_COU;c++) {

			// dump only the ones which need to exist!
			if(ch_status[r][arm][apv][c] & FGT_CH_STAT_SHOULD) ;
			else continue ;

			if(cmnGroup[apv][c]<0) continue ;

			for(int t=0;t<tb_cou_ped;t++) {

				fprintf(f,"%d %d %2d %3d %2d %7.3f %.3f\n",r+1,arm,apv,c,t,
					ped->cmn_ped[arm][apv][c][t],
					ped->cmn_rms[arm][apv][c][t]) ;
			}
		}
		}
		}
	}

	fclose(f) ;	

	
	

	if(bad || dont_cache || !valid) {
		LOG(CAUTION,"Pedestals NOT cached for run %08u",run) ;
		LOG(ERR,"Pedestals NOT cached for run %08u: %d:%d:0x%X!",run,bad,valid,dont_cache) ;
		return 0 ;
	}

	
	sprintf(f_fname,"/RTScache/fst_s%d_pedestals.txt",sector) ;
	f = fopen(f_fname,"w") ;
	if(f==0) {
		LOG(ERR,"ped::to_cache can't open output file \"%s\" [%s]",f_fname,strerror(errno)) ;
		return -1 ;
	}

	fprintf(f,"# Detector FST%d\n",sector) ;
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

		for(int arm=0;arm<3;arm++) {
		for(int apv=0;apv<FGT_APV_COU;apv++) {


		for(int c=0;c<FGT_CH_COU;c++) {

		// dump only the ones which need to exist!
		if(ch_status[r][arm][apv][c] & FGT_CH_STAT_SHOULD) ;
		else continue ;

		if(cmnGroup[apv][c]<0) continue ;

		struct peds_t *use_ped = ped ;

		all_cou++ ;
#if 0
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
#endif
		for(int t=0;t<tb_cou_ped;t++) {
			
			fprintf(f,"%d %d %2d %3d %2d %7.3f %.3f %.3f\n",r+1,arm,apv,c,t,
				use_ped->ped[arm][apv][c][t],
				use_ped->rms[arm][apv][c][t],
				use_ped->cmn_rms[arm][apv][c][t]) ;
		}
		}
		}
		}
	}

	fclose(f) ;	

#if 0	
	{
		char cmd[128] ;

		sprintf(cmd,"/bin/cp /RTScache/pedestals.txt /RTScache/%s_pedestals_%08u_loaded.txt","fst",run) ;
		system(cmd) ;
	}
#endif

	double perc = 100.0 * (double)non_cached_cou / (double)all_cou ;
	if(perc > 10.0) {
		LOG(U_TONKO,"Pedestals cached for run %08u: skipped %d/%d (%d%%) channels.",run,non_cached_cou,all_cou,(int)perc) ;
	}
	else {
		LOG(TERR,"Pedestals cached for run %08u: skipped %d/%d channels.",run,non_cached_cou,all_cou) ;
	}

	return 1 ;
}



int fstPed::bad_from_cache(char *fname) 
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
		fn = "/RTS/conf/fst/fst_bad_channels.txt" ;
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
	for(int m=0;m<FST_ARM_COU;m++) {
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
