/*
	This code is property of Brookhaven National Laboratory, USA.	
	Authored by Tonko Ljubicic.

	No modifications are to be made without the express consent of
	the author.

	Version 6.00	6/18/2007	Brand new TPX version
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <rtsLog.h>
#include <I386/i386Lib.h>

#include <TPC/rowlen.h>

#include "tpxGain.h"
#include "tpxFCF.h"

#include <DAQ_READER/daq_dta_structs.h>

#define likely(x)       __builtin_expect((x),1)
#define unlikely(x)     __builtin_expect((x),0)

int tpxFCF::afterburner(int cou, daq_cld *store[])
{
	int merged ;

	if(likely(cou == 0)) return 0 ;	// most cases like this...


	merged = 0 ;

	for(int i=0;i<cou;i++) {
		daq_cld *l = store[i] ;	// left guy
		int merge_ix = -1 ;



		if(l->charge == 0) continue ;	// already merged
		
		
		for(int j=0;j<cou;j++) {
			int ok = 0 ;
			if(i==j) continue ;

			daq_cld *r = store[j] ;	// right guy
		
			if(r->charge == 0) continue ;	// already merged!


			if(r->p1==13) {	// right
				if(l->p2 != 12) continue ;
			}
			else if(r->p2==12) {
				if(l->p1 != 13) continue ;
			}
			else if(r->p1==131) {
				if(l->p2 != 130) continue ;
			}
			else if(r->p2==130) {
				if(l->p1 != 131) continue ;
			}



			if((r->t1 >= l->t1) && (r->t1 <= l->t2)) ok = 1 ;
			if((r->t2 >= l->t1) && (r->t2 <= l->t2)) ok = 1 ;

			if(ok==0) continue ;

			merge_ix = j ;
			break ;
			
			

		}

		if(merge_ix < 0) {	// no merge -- let it go...
			
		}
		else {
			// merge...
			merged++ ;
			
			// merge
			daq_cld *r = store[merge_ix] ;
			double charge = l->charge + r->charge ;

			/*
			printf("can merge: L: [%d:%d] %f, R: [%d:%d] %f\n",
			       l->p1,l->p2,l->tb,
			       r->p1,r->p2,r->tb) ;
			*/

			l->tb = (l->tb * l->charge + r->tb * r->charge) / charge ;
			l->pad = (l->pad * l->charge + r->pad * r->charge) / charge ;
			
			if(r->t1 < l->t1) l->t1 = r->t1 ;
			if(r->t2 > l->t2) l->t2 = r->t2 ;

			if(r->p1 < l->p1) l->p1 = r->p1 ;
			if(r->p2 > l->p2) l->p2 = r->p2 ;

			l->flags |= r->flags ;		// merge flags
			l->flags &= ~FCF_ONEPAD ;	// remove onepad, by dedfinition.

			if(charge > (double)0x7FFF) {
				l->charge = (u_short) (charge / 1024.0) ;
				l->flags |= FCF_BIG_CHARGE ;
			}
			else {
				l->charge = (u_short) charge ;
			}

			// and kill the right guy!
			r->flags |= FCF_DEAD_EDGE  ;
			r->charge = 0 ;
		}


		
	}

	// cleanup flags
	for(int i=0;i<cou;i++) {
		daq_cld *l = store[i] ;	// left guy

//		printf("AfterB: %2d: P [%d:%d], T [%d:%d], flags %d, charge %d\n",i,l->p1,l->p2,l->t1,l->t2,l->flags,l->charge) ;

		if(l->flags & (FCF_ONEPAD | FCF_DEAD_EDGE)) {	// is this is still on, kill it with dead edge
			l->flags |= FCF_DEAD_EDGE ;
		}
		else {
			l->flags &= ~FCF_BROKEN_EDGE ;	// remove this flag since we ran the afterburner
		}

	}

	return merged ;
}

int tpxFCF::fcf_decode(u_int *p_buff, daq_cld *dc, u_short version)
{
	double p, t ;
	int p1,p2,t1,t2,cha,fla ;
	int ptmp ;

	fla = 0 ;

	// pad
	u_int tmp = *p_buff & 0xFFFF ;
	if(tmp & 0x8000) fla |= FCF_MERGED ;
	if(tmp & 0x4000) fla |= FCF_DEAD_EDGE ;

	if(version==FCF_V_FY08) {
		p = (double)(tmp & 0x3FFF) / 32.0 ;
	}
	else {
		p = (double)(tmp & 0x3FFF) / 64.0 ;
	}			

	// time
	tmp = *p_buff >> 16 ;
	if(tmp & 0x8000) fla |= FCF_ONEPAD ;

	if(version == FCF_V_FY08) {
		t = (double)(tmp & 0x7FFF) / 32.0 ;
	}
	else {
		t = (double)(tmp & 0x7FFF) / 64.0 ;
	}			

	p_buff++ ;	// advance to next word
	cha = *p_buff >> 16 ;

	if(cha >= 0x8000) {	// special case of very large charge...
		fla |= FCF_BIG_CHARGE ;
		cha = (cha & 0x7FFF) * 1024 ;
		if(cha == 0) cha = 0x8000;	// exaclty
	}

	ptmp = *p_buff & 0xFFFF ;

	if(ptmp & 0x8000) fla |= FCF_ROW_EDGE ;
	if(ptmp & 0x4000) fla |= FCF_BROKEN_EDGE ;

	t1 = ptmp & 0xF ;
	t2 = (ptmp >> 4) & 0xF ;


	p1 = (ptmp >> 8) & 0x7 ;
	p2 = (ptmp >> 11) & 0x7 ;

	t1 = (u_int)t - t1 ;
	t2 = (u_int)t + t2 ;
	p1 = (u_int)p - p1 ;
	p2 = (u_int)p + p2 ;

	dc->t1 = t1 ;
	dc->t2 = t2 ;
	dc->p1 = p1 ;
	dc->p2 = p2 ;
	dc->charge = cha ;
	dc->flags = fla ;
	dc->pad = p ;
	dc->tb = t ;


	return 2 ;	// 2 u_ints used

}

int tpxFCF::fcf_decode(u_int *p_buff, daq_sim_cld *sdc, u_short version)
{
	int skip = fcf_decode(p_buff,&(sdc->cld), version) ;

	sdc->track_id = 0 ;
	sdc->quality = 0 ;

	
	p_buff += skip ;


	sdc->quality = (*p_buff) >> 16 ;
	sdc->track_id = (*p_buff) & 0xFFFF ;


	//LOG(TERR,"Q %d, T %d",sdc->quality, sdc->track_id) ;

	// took one int so...
	skip++ ;


	return skip ;
}


tpxFCF::tpxFCF() 
{
	rbs = 0 ;
	sector = 0 ;
	gains = 0 ;
	cl_marker = 0 ;

	run_compatibility = 9 ;	// run 9
	do_cuts = 2 ;	// 1 means always, 2 means don't cut edges (for i.e. pulser run), 0 means don't...
	ch_min = 10 ;


	memset(row_ix,0xFF,sizeof(row_ix)) ;
	storage = 0 ;


	read_version = 0 ;
	do_version = FCF_V_FY09 ;

	return ;
}


tpxFCF::~tpxFCF()
{
	if(storage) {
		free(storage) ;
		storage = 0 ;
	}

	return ;
}

/*
	Called once at startup
*/
void tpxFCF::config(u_int mask, int mode)
{
	rbs = mask ;
	modes = mode ;

	int r ;
	int row, pad ;
	int a, ch ;

	for(r=0;r<=45;r++) {
		row_ix[r] = 0 ;	// nix...
	} 


	LOG(NOTE,"calling config: mask 0x%X, mode %d",mask,mode) ;

	// There is some amount of acrobatics involved so
	// bear with me...

	// First, we figure out which rows are needed from the RB mask
	for(r=0;r<6;r++) {
		if(rbs & (1<<r)) {
			for(a=0;a<256;a++) {
			for(ch=0;ch<16;ch++) {
				tpx_from_altro(r,a,ch,row,pad) ;

				if(row > 45) continue ;	// not in this RDO...
				if(row == 0) continue ;	// will nix row 0 as well...
				row_ix[row] = 1  ;	// mark as needed...
			}
			}
		}
	}

	// get the count of pads needed assuming _whole_ rows!
	int tot_count = 0 ;
	for(row=0;row<=45;row++) {
		if(row_ix[row]) {
			tot_count += tpc_rowlen[row] ;	// allocate whole rows!
		}
	}

	LOG(NOTE,"fcfconfig: RDO mask 0x%03X: allocated %d pads (%d bytes)",rbs,tot_count,tot_count * sizeof(struct stage1)) ;

	// allocate storage
	if(storage) {
		LOG(WARN,"Whoa! Storage already allocated!") ;
		free(storage) ;
	}

	storage = (struct stage1 *) valloc(tot_count * sizeof(struct stage1)) ;
	
	LOG(NOTE,"FCF for mask 0x%02X: alloced %d bytes",mask,tot_count * sizeof(struct stage1)) ;

	// clear storage
	memset(storage,0,tot_count * sizeof(struct stage1)) ;



	// re-create offsets which we use in the row+pad navigation
	tot_count = 0 ;	// re use...
	for(row=0;row<=45;row++) {
		if(row_ix[row] == 0) {
			row_ix[row] = -1 ;	// nix!
			continue ;
		}

		row_ix[row] = tot_count ;

		tot_count += tpc_rowlen[row] ;
	}

	// OK -- we have the storage and the navigation via get_stage1(row,pad) ready.
	// Now let's get the per-pad flags ready:

	// Mark pads which are there as present...
	for(r=0;r<6;r++) {
		if(rbs & (1<<r)) {
			for(a=0;a<256;a++) {
			for(ch=0;ch<16;ch++) {
				tpx_from_altro(r,a,ch,row,pad) ;

				if(row > 45) continue ;
				if(row == 0) continue ;

				get_stage1(row, pad)->f = 0x8000 ;


			}
			}
		}
	}

	return ;
}

/*
	This might me reapplied...
*/
void tpxFCF::apply_gains(int sec, tpxGain *gain)	
{
	sector = sec ;
	gains = gain ;

	int row, pad ;

	LOG(NOTE,"Applying gains to sector %d [%p ?]",sec,gain) ;

	// clear all flags but the existing ones
	for(row=1;row<=45;row++) {
		if(row_ix[row] < 0) continue ;

		for(pad=1;pad <= tpc_rowlen[row]; pad++) {

			get_stage1(row, pad)->f &= 0xFF00 ;	// clear, keeping upper bits intact...
		}
	}


	// put gains & flags
	for(row=1;row<=45;row++) {
		if(row_ix[row] < 0) continue ;

		for(pad=1;pad<=tpc_rowlen[row];pad++) {	
			stage1 *s = get_stage1(row, pad) ;

			s->g = gains->get_gains(sector,row,pad)->g ;
			s->t0 = gains->get_gains(sector,row,pad)->t0 ;

			u_int fl = 0 ;

			if(!(s->f & 0x8000)) {	// not really here
				//LOG(WARN,"Applying broken edge to row:pad %d:%d",row,pad) ;
				fl |= FCF_BROKEN_EDGE ;
			}

			if(s->g == 0.0) {	// dead
				fl |= FCF_DEAD_EDGE ;
			}


			// for these we must mark adjacent pads!
			if(fl) {
				if(pad>1) {
					get_stage1(row,pad-1)->f |= fl ;	// bad_edge
				}
				if(pad < tpc_rowlen[row]) {
					get_stage1(row,pad+1)->f |= fl ;	// bad_edge ;
				}

			}
#if 0
			if(row==8) {
				switch(pad) {
				case 12 :
				case 13 :
				case 130 :
				case 131 :
					fl |= FCF_BROKEN_EDGE ;
					break ;
				}
			}
#endif
			// marks the first and last pad as "edge"
			if((pad==1) || (pad==tpc_rowlen[row])) {
				fl |= FCF_ROW_EDGE ;
			}
			
			s->f |= fl | FCF_ONEPAD ;

			LOG(DBG,"FCF gains: row %2d, pad %3d: gain %f, flags 0x%04X",row,pad,s->g,s->f) ;

		}


	}
		
	
	return ;
}

void tpxFCF::start_evt()
{
	cl_marker = 10000 ;	// used to mark unique clusters sector...

	for(int r=1;r<=45;r++) {
		if(row_ix[r] < 0) continue ;

// BUG IN FY08 run:		for(int p=1;p<tpc_rowlen[r];p++) {
		for(int p=1;p<=tpc_rowlen[r];p++) {
			struct stage1 *o ;
			o = get_stage1(r, p) ;
			if(unlikely(o==0)) {
				LOG(ERR,"No row pad %d:%d???",r,p) ;
			}
			else {
				o->count = 0 ;
			}
		}
	}

	
	return ;
}



int tpxFCF::do_pad(tpx_altro_struct *a, daq_sim_adc_tb *sim_adc)
{
	struct stage1 *s ;

	if(unlikely(a->row > 45)) return 0 ;
	if(unlikely(a->row == 0)) return 0 ;



	s = get_stage1(a->row, a->pad) ;

	if(unlikely(s==0)) {
		LOG(ERR,"Whoa -- no row:pad %d:%d???",a->row,a->pad) ;
		return 0 ;
	}


	s->count = 0 ;

	if(unlikely(a->count<=1)) return 0 ;	// no point....


	// HACK put in on Mar 6th, 2009 to suppress those
	// long strips (>=415 tb) of unknown nature (to that date).
	// After run 10065096.
	// Tonko.
	// Actually, I will leave this cut in and lower to 400
	if(unlikely(a->count >= 400)) {
		//LOG(WARN,"count %d on r:p %d:%d",a->count,a->row,a->pad) ;
		return 0 ;
	}

	u_int t_ave, charge ;
	u_int tb_start ;
	u_int last_falling, last_adc ;
	u_int flags ;
	u_int tb_prev ;
	int new_cluster ;

	struct tpxFCF_cl *cl, *cl_max ;


	cl = s->cl ;	// start...
	cl_max = &(s->cl[FCF_MAX_CL]) ;	// end of cl sentinel...
	

	u_int max_adc = 0 ;

	last_falling = last_adc = 0 ;
	t_ave = charge = 0 ;

	tb_prev = tb_start = a->tb[0] ;
	flags =  s->f & 0xFF ;
	new_cluster = 0 ;


	// start the loop over raw pixels in this pad...
	for(int i=0;likely( i < a->count );i++) {
		u_int adc, tb ;


		adc = a->adc[i] ;
		tb = a->tb[i] ;

		if(unlikely( adc <= 1 )) {	// possible due to ALTROs way of sticking sequences...
			continue ;
		}

		if(unlikely( tb < (tb_prev - 1) )) {	// end of previous sequence
			new_cluster |= FCF_ONEPAD ;
		}

		if(unlikely( last_falling )) {
			if(unlikely( last_falling > FCF_MIN_WIDTH )) {
				if(unlikely( adc > (last_adc + FCF_ADC_NOISE) )) {
					flags |= FCF_DOUBLE_T ;
					new_cluster |= FCF_DOUBLE_T ;

				}
			}
			else {
				last_falling++ ;
			}

		}
		else {
			if(unlikely( (adc + FCF_ADC_NOISE) < last_adc )) {
				last_falling = 1 ;
			}
		}


		if(unlikely( new_cluster )) {
			if(likely(max_adc >= FCF_ADC_MIN)) {
				cl->t_ave = t_ave ;
				cl->charge = charge ;

				cl->t1 = tb_prev;	// put previous; t1 is the lower value
				cl->t2 = tb_start ;
		
				cl->flags = flags ;
	

				cl++ ;

				// protect storage!
				if(unlikely( cl >= cl_max )) goto pad_done ;	// to many!
			}

			// prepare for new one...
			if(unlikely( new_cluster & FCF_DOUBLE_T )) ;
			else flags = s->f & 0xFF ;

			t_ave = charge = 0 ;
			tb_start = tb ;
			last_falling = 0 ;
			max_adc = 0 ;

			new_cluster = 0 ;
		}


		if(unlikely( adc > max_adc )) {
			max_adc = adc ;
		}

		last_adc = adc ;
		tb_prev = tb ;

		t_ave += adc * tb ;
		charge += adc ;

		   
	}	// scan over the sequence
	
	// finish off the rest...
	if(likely(max_adc >= FCF_ADC_MIN)) {
		cl->t1 = tb_prev  ;	// t1 is the lower value
		cl->t2 = tb_start ;

		cl->flags = flags ;

		cl->charge = charge ;
		cl->t_ave = t_ave ;

		cl++ ;
	}

	// goto target...
	pad_done: ;


	s->count = cl - s->cl ;	// count of 1d clusters


	if(unlikely( s->count == 0 )) {
		//LOG(DBG,"No 1D clusters?") ;
	}
	else {
		if(unlikely( s->count >= FCF_MAX_CL )) {
			LOG(NOTE,"Row %d:%d, %d clusters [max!]",a->row,a->pad,s->count) ;
		}
		else {
			//LOG(DBG,"Row %d:%d, %d clusters",a->row,a->pad,s->count) ;
		}
	}

#ifdef FCF_DEBUG
	// debugging dump
	cl = s->cl ;
	for(int j=0;j<s->count;j++) {
		LOG(DBG,"C: %d:%d\t %3d %3d, ch %5d, fl 0x%X",a->row,a->pad,cl->t1,cl->t2,cl->charge,cl->flags) ;	
		cl++ ;
	}

#endif

	if(unlikely(sim_adc && modes)) {	// FCF_ANNOTATION for simulated data!
		cl = s->cl ;
		
		for(int j=0;j<s->count;j++) {
		
			int t_lo = cl->t1 ;
			int t_hi = cl->t2 ;
			int adc_min = 0 ;

			cl->track_id = 0 ;
			cl->quality = 0 ;

			u_int adc_sum = 0 ;
			u_int t_sum = 0 ;

			// position this clusters sim data...
			int i_min = 0xFFFFFF ;
			cl->sim = 0 ;
			cl->sim_length = 0 ;
			for(int i=0;i<a->count;i++) {
				if((a->tb[i] >= t_lo) && (a->tb[i]<=t_hi)) {	// belong to this sequence
					if(i < i_min) i_min = i ;		// need the smallest I
					cl->sim_length++ ;

					// in case of non-geant data, overwrite the track ids with the current marker...
					if(sim_adc[i].track_id == 0xFFFF) {
						modes |= 2 ;		// heuristic to set the correct mode
						sim_adc[i].track_id = cl_marker ;
					}
				}
			}
			cl->sim = sim_adc + i_min ;	// this is where the sim data starts...

			
			// get thr track id from the pixel with the maximum ADC
			for(int i=0;i<a->count;i++) {
				if((a->tb[i] >= t_lo) && (a->tb[i]<=t_hi)) {	// belong to this sequence
					adc_sum += a->adc[i] ;

					if(a->adc[i] >= adc_min) {
						adc_min = a->adc[i] ;	// bug before Oct 09!
						cl->track_id = sim_adc[i].track_id ;
					}
				}
			}

			// sum the pixels whic belong to this track id only
			for(int i=0;i<a->count;i++) {
				if((a->tb[i] >= t_lo) && (a->tb[i]<=t_hi)) {	// belong to this sequence

					if(cl->track_id == sim_adc[i].track_id) {
						t_sum += a->adc[i] ;
					}
				}
			}

			if(adc_sum) {
				cl->quality = (t_sum * 100) / adc_sum ;
			
			}
			else {
				cl->quality = 0 ;
			}


			//LOG(TERR,"%d: track id %d, qua %d",cl_marker,cl->track_id, cl->quality) ;

			cl_marker++ ;
			cl++ ;

		}

	}

	
	return s->count ;	// returns count...
}


int tpxFCF::stage2(u_int *outbuff, int max_bytes)
{
	int r, p, c ;
	struct stage1 *old1, *cur1 ;
	tpxFCF_cl *old, *cur, *old_end, *cur_end ;
	
	loc_buff = outbuff ;	// copy over


	for(r=0;r<=45;r++) {
		if(row_ix[r] < 0) {
			continue ;
		}


		cur_row = r ;
		if(cur_row == 0) {
			LOG(WARN,"Can;t be -- row 0; row_ix %d",row_ix[r]) ;
		}

		int bytes_so_far = (loc_buff - outbuff)*4 ;
		if(bytes_so_far > (max_bytes-1024)) {
			LOG(WARN,"row %d: approaching limit: is %d, max %d",cur_row,bytes_so_far,max_bytes) ;
		}


		u_int *row_cache = loc_buff++ ;	// remember where the row goes...
		u_int *clust_count_cache = loc_buff++ ;	// remember where the cluster count goes...

		
		cur_row_clusters = 0 ;

		//LOG(DBG,"going into row %d",r) ;

		// scan from first pad to one before the last...
		for(p=1;p<tpc_rowlen[r];p++) {

			old1 = get_stage1(r,p) ;
			old = old1->cl ;

			cur1 = get_stage1(r,p+1) ;
			cur = cur1->cl ;

			//LOG(DBG,"row %d: pad %d:%d, pad++ %d:%d",r,p,old1->count,p+1,cur1->count) ;

			//LOG(DBG,"RP %d:%d at 0x%08X, next at 0x%08X",r,p,old1,cur1) ;

			// counters
			//if(old1->count) LOG(DBG,"pad %d: count %d",p,old1->count) ;

			//end sentinel
			cur_end = cur + cur1->count ;
			old_end = old + old1->count ;
			

			while(old<old_end) {	// loop over old
				int merge ;

				merge = 0 ;
				

				// loop over next, called "cur"
				while(likely(cur<cur_end)) {

					//LOG(DBG,"Row %d: old pad %d[%d]: %d..%d 0x%X, new %d..%d 0x%X [%d]",r,p,old_end-old,
					//    old->t1,old->t2,old->flags,cur->t1,cur->t2,cur->flags,cur_end-cur) ;

					// every "new" sequence MUST be one_pad!
					if(cur->flags & FCF_ONEPAD) ;
					else {
						LOG(WARN,"Can;t be without ONEPAD %d:%d 0x%X?! %d..%d",r,p+1,cur->flags,cur->p1,cur->p2) ;
					}

					//LOG(DBG,"p %d: %d %d",p,old->t1,cur->t2) ;
					

					if(unlikely((old->t1 - cur->t2) >= 1)) {
						// the old guy is left behind and thus
						// we can only dump it oiut at this point
						merge = 2 ;
						
						// keep the current guy

						break ;	// this dumps the old guy
					}
					else if(unlikely((cur->t1 - old->t2) > 1)) {
						// don't use the new guy ever again...

						cur++ ;
						continue ;
					}
					else {	// we are merging here into cur for sure!

						// it only depends to see if we just merge and blow "old"
						// or
						// merge part of "old" and dump "old"
						//LOG(DBG,"Merge") ;

						// move to double and gain correct!
						if(unlikely(cur->flags & FCF_IN_DOUBLE)) {

						}
						else {
							cur->flags |= FCF_IN_DOUBLE ;

							cur->f_charge = cur->charge * cur1->g ;
							cur->scharge = cur->f_charge ;
							
							cur->f_t_ave = cur->t_ave * cur1->g ;
							cur->f_t_ave += cur1->g * cur1->t0 ;

							cur->p1 = p+1 ;
							cur->p_ave = (p+1) * cur->f_charge ;

						}
						

						// also correct the charge if the old guy is still no corrected -- first pad in a sequence
						if(likely(old->flags & FCF_IN_DOUBLE)) {

						}
						else {
							// correct charge here
							old->flags |= FCF_IN_DOUBLE ;

							old->f_charge = old->charge * old1->g ;
							old->scharge = old->f_charge ;

							old->f_t_ave = old->t_ave * old1->g ;
							old->f_t_ave += old1->g * old1->t0 ;

							old->t_min = old->t1 ;
							old->t_max = old->t2 ;

							old->p1 = p ;
							old->p_ave = p * old->f_charge ;
						}


	
						if(unlikely(old->flags & FCF_FALLING)) {
							
							if(unlikely(old->flags & FCF_ONEPAD)) {
								LOG(ERR,"Can't be on a merge!") ;
							}
							if(unlikely(!(old->flags & FCF_IN_DOUBLE))) {
								LOG(ERR,"Can't be on a merge!!!") ;
							}


							if(unlikely(cur->scharge > (old->scharge + FCF_MIN_ADC_PAD_C))) {

								// we are splitting here!
								double sc_tmp, sc_p_tmp ;

								// take half of old scharge

								sc_tmp = old->scharge * 0.5 ;
								sc_p_tmp = sc_tmp * (p+1) ;


								cur->flags |= FCF_DOUBLE_PAD ;
								cur->f_charge += sc_tmp ;
								cur->p_ave += sc_p_tmp ;
								cur->f_t_ave += ((cur->t1 + cur->t2) * sc_tmp) * 0.5 ;
								
								
								old->flags |= FCF_DOUBLE_PAD ;
								old->f_charge -= sc_tmp ;
								old->p_ave -= sc_p_tmp ;
								old->f_t_ave -= ((old->t1 + old->t2) * sc_tmp) * 0.5 ;
								

								//LOG(DBG,"Split!") ;

								

								cur++ ;	// don't use the new guy again...
								merge = 3 ;
								break ;	// this dumps the old guy, or does it?
							}

							cur->flags |= FCF_FALLING ;
						}
						else {
							if(unlikely(cur->scharge  < (old->scharge - FCF_MIN_ADC_PAD_C))) {
								cur->flags |= FCF_FALLING ;
							}
						}

						// merge!
						merge = 1 ;

						//LOG(DBG,"Typical merge") ;

						// sanity check: the new 1d-clusters can only be
						// "fresh" i.e. 1pad wide...
						if(unlikely(!(cur->flags & FCF_ONEPAD))) {
							LOG(ERR,"Can;t be -- here I can have only ONEPADS!!") ;
						}
						if(unlikely(!(cur->flags & FCF_IN_DOUBLE))) {
							LOG(ERR,"Can't be -- I must be in double here!") ;

						}						

						
						cur->flags |= old->flags ;
						cur->flags &= ~FCF_ONEPAD ;

						if(unlikely(modes)) {	// merge annotated stuff
							double qua ;

							if(cur->track_id == old->track_id) {
								qua = cur->quality * cur->f_charge + old->quality * old->f_charge ;
							}
							else if(cur->f_charge < old->f_charge) {
								cur->track_id = old->track_id ;

								qua = old->quality * old->f_charge ;

							}
							else {
								qua = cur->quality * cur->f_charge ;
							}
							
							qua /= (cur->f_charge + old->f_charge) ;

							cur->quality = (u_int )qua ;

							// need to know what to do here when we 
							if(modes & 2) {	// local annotation back to track id!
								cur->track_id = old->track_id ;
								for(int i=0;i<cur->sim_length;i++) {
									cur->sim[i].track_id = old->track_id ;
								}
							}
						}



						cur->f_charge += old->f_charge ;
						cur->f_t_ave += old->f_t_ave ;
						cur->p_ave += old->p_ave ;
						cur->p1 = old->p1 ;


						if(cur->t1 > old->t_min) {
							cur->t_min = old->t_min ;
						}
						else {
							cur->t_min = cur->t1 ;
						}

						if(cur->t2 < old->t_max) {
							cur->t_max = old->t_max ;
						}
						else {
							cur->t_max = cur->t2 ;
						}

						

						cur++ ;	// don't use the new guy again...
						break ;
					}
				}	// end of loop over new....


				//LOG(DBG,"Merge %d:%d = merge %d, t1..t2 %d:%d",r,p,merge,old->t1,old->t2) ;


				switch(merge) {
				case 1 :
					// this is a real merge so keep the old guy...
					break ;
				default :
					if(old->flags & FCF_ONEPAD) {
						if(merge==3) {
							LOG(ERR,"Can;t be!") ;
						}
						// will dump for onepad
						old->p2 = p ;
						old->p1 = 201 ;
						dump(old,r) ;
					}
					else {
						// dump OLD!
						old->p2 = p  ;
						dump(old,r) ;
					}
					
					break ;
				}

				old++ ;

			}	// end of loop over old...

			

		}	// loop over pads; until one before the last

		// dump the last pad; p is at row->pad_stop
		old1 = get_stage1(r,p) ;
		old = old1->cl ;

		//LOG(DBG,"end of row %d: pad %d, leftower count %d",r,p,old1->count) ;
		//if(old1->count) {
		//	LOG(DBG,"Leftover at row %d:%d, %d",r,p,old1->count) ;
		//}

		for(c=0;c<old1->count;c++) {	// loop over old

			old->p2 = p ;
			dump(old,r);

			old++ ;
		}


		// end of row. We either enter the count in the header or go back 1 int...
		if(cur_row_clusters) {
			*row_cache = (do_version<<16) | r ;
			*clust_count_cache = cur_row_clusters ;
			//LOG(DBG,"In row %2d: found %d clusters",r,cur_row_clusters) ;
		}
		else {
			loc_buff -= 2 ;	// go back, nothing found!
		}

	}	// loop over rows...

	return loc_buff - (u_int *)outbuff ;	// return number of ints stored!
}


void tpxFCF::dump(tpxFCF_cl *cl, int row)
{
	u_int fla = cl->flags ;
	u_int tmp_p, tmp_fl ;
	u_int pad_c ;
	int time_c ;	// time_c can go negative due to T0 corrections!
	double dp, dt ;
	u_int p1, p2 ;
	int div_fact ;

	

	switch(run_compatibility) {
	case 9 :	//FY09	// THIS WAS IN RUN09, right or wrong!

		// first set of pre-cuts
		if(likely(do_cuts)) {
			if(unlikely(fla & FCF_BROKEN_EDGE)) ;	// pass hits on row 8 even when dead because of the afterburner!
			else if((do_cuts == 1) && (fla & (FCF_ROW_EDGE | FCF_DEAD_EDGE))) return ;	// kill clusters touching the edge or a dead pad
		}


		// next we will kill one pad wide clusters but not the ones close to the padplane (MWPC studies)!
		if((fla & FCF_ONEPAD) && (cl->t1 < 15)) {	// for any hit below the trigger, we pass -- for MWPC studies!
			if((cl->t2 - cl->t1)<=1) return ;	// I won't pass hits with less than 2 timebins
				if(cl->t2 > 30) return ;		// too long, noise


				//LOG(INFO,"dt %.1f: row %d: %d %d %d %d : %d %f 0x%X",dt,cur_row,cl->p1,cl->p2,cl->t1,cl->t2,cl->charge,cl->f_charge,cl->flags) ;
				return ;	// still
			}
		else if(likely(do_cuts)) {

			if(fla & FCF_BROKEN_EDGE) {	// always pass!
				;
			}
			else if(fla & FCF_ONEPAD) return ;
			else if((cl->t_max - cl->t_min) <= 2) return ;	// kill if the cluster length in time is small

		}



		//now, I can have ONEPAD hits here:
		//	1) because of the low timebin, MWPC studies
		//	2) because of row 8 aka BROKEN_EDGE

		if(!(fla & FCF_IN_DOUBLE)) {	// the values were not turned to double
			if(fla & FCF_ONEPAD) {	// this is the only valid case
				cl->p1 = cl->p2 ;	// adjust the start pad...



				// I don't have the gain corrections here yet (in FY09) so I use a 1.0!
				cl->f_charge = (float)cl->charge * 1.0 ;
				cl->p_ave = cl->p1 * cl->f_charge ;
				cl->f_t_ave = (float)cl->t_ave * 1.0 ;

			}
			else {
				LOG(WARN,"WTF? not double: row %d: %d-%d, %d-%d (%d-%d): charge %d (%.3f): 0x%X",cur_row,cl->p1,cl->p2,cl->t1,cl->t2,cl->t_min,cl->t_max,cl->charge,cl->f_charge,cl->flags) ;
				return ;
			}
		}

		if(cl->f_charge < 1.0) {
			if(!(fla & FCF_DEAD_EDGE)) LOG(WARN,"WTF? no charge: row %d: %d-%d, %d-%d (%d-%d): charge %d (%.3f): 0x%X",cur_row,cl->p1,cl->p2,cl->t1,cl->t2,cl->t_min,cl->t_max,cl->charge,cl->f_charge,cl->flags) ;
			return ;
		}

		break ;
	default :
		// run 10 ;
		if(unlikely(fla & FCF_BROKEN_EDGE)) goto keep ;	// always keep

		if(unlikely(do_cuts == 0)) goto keep ;	// obviously always keep

		if(likely(do_cuts != 2)) {		// junk them unless do_cuts is 2
			if(fla & (FCF_DEAD_EDGE | FCF_ROW_EDGE)) return ;	// never keep!
		}

		if(likely(fla & FCF_IN_DOUBLE)) {
			if((cl->t_max - cl->t_min)<=2) return ;	// kill if TB width <= 3
			if(cl->f_charge < 10.0) return ;	// kill small charge
		}
		else {
			
			if(likely(fla & FCF_ONEPAD)) ;
			else {
				LOG(WARN,"WTF? not onebad but not in double!?") ;
			}

			if((cl->t2 - cl->t1)<=2) return ;
			if(cl->charge < 10) return ;

			
		}

		keep:;

		if(likely(fla & FCF_IN_DOUBLE)) ;
		else {
			struct stage1 *gain = get_stage1(row, cl->p2) ;

			cl->p1 = cl->p2 ;

			cl->f_charge = cl->charge * gain->g ;
			// don't need scharge

			cl->f_t_ave = cl->t_ave * gain->g ;
			cl->f_t_ave += gain->g * gain->t0 ;

			cl->p_ave = cl->p2 * cl->f_charge ;

			cl->t_min = cl->t1 ;
			cl->t_max = cl->t2 ;

		}

	}



	int cha = (int) (cl->f_charge + 0.5) ;	// integerized version; 0.5 is for correct roundoff

	// clear the FALLING flag 
	fla &= (~FCF_FALLING) ;

	dt = cl->f_t_ave / cl->f_charge ;
	dp = cl->p_ave / cl->f_charge ;


	if(do_version == FCF_V_FY08) {
		div_fact = 32 ;
	}
	else {
		div_fact = 64 ;
		//LOG(DBG,"Using div fact %d",div_fact) ;
	}

	// we dump an integerized version ("fixed point")
	time_c = (u_int) (dt * div_fact + 0.5) ;	// 0.5 for correct roundoff
	pad_c = (u_int) (dp * div_fact + 0.5) ;

	// get pad extents
	tmp_p  = pad_c / div_fact   ;
		
	p1 = tmp_p - cl->p1 ;
	p2 = cl->p2 - tmp_p ;

	if(p1 > 7) p1 = 7 ;
	if(p2 > 7) p2 = 7 ;

	tmp_fl = (p1 << 8) | (p2 << 11)  ;


	// get timebin extents
	tmp_p = time_c / div_fact   ;
		
	p1 = tmp_p - cl->t_min ;
	p2 = cl->t_max - tmp_p ;

	if(p1 > 15) p1 = 15 ;
	if(p2 > 15) p2 = 15 ;


	tmp_fl |= (p2 << 4) | p1 ;

	
	if(fla & FCF_ONEPAD) time_c |= 0x8000 ;

	if(fla & (FCF_DOUBLE_T | FCF_DOUBLE_PAD)) pad_c |= 0x8000 ;
	if(fla & FCF_DEAD_EDGE) pad_c |= 0x4000 ;

	if(fla & FCF_ROW_EDGE) tmp_fl |= 0x8000 ;
	if(fla & FCF_BROKEN_EDGE) tmp_fl |= 0x4000 ;

	if(cha > 0x7FFF) cha = 0x8000 | (cha/1024) ;


	// watchout for ordering!
	*loc_buff++ = (time_c << 16) | pad_c ;
	*loc_buff++ = (cha << 16) | tmp_fl ;


	if(modes) {	// FCF Annotation
		*loc_buff++ = (cl->quality << 16) | cl->track_id ;
	}


	cur_row_clusters++ ;

	
	return ;
}
