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

#include <DAQ_TPX/tpxGain.h>
#include <DAQ_TPX/tpxFCF.h>

#define likely(x)       __builtin_expect((x),1)
#define unlikely(x)     __builtin_expect((x),0)

tpxFCF::tpxFCF() 
{
	rbs = 0 ;
	sector = 0 ;
	gains = 0 ;


	do_cuts = 1 ;
	ch_min = 10 ;

	memset(row_ix,0xFF,sizeof(row_ix)) ;
	storage = 0 ;

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
void tpxFCF::config(u_int mask)
{
	rbs = mask ;


	int r ;
	int row, pad ;
	int a, ch ;

	for(r=0;r<=45;r++) {
		row_ix[r] = 0 ;	// nix...
	} 


	// There is some amount of acrobatics involved so
	// bear with me...

	// First, we figure out which rows are needed from the RB mask
	for(r=0;r<6;r++) {
		if(rbs & (1<<r)) {
			for(a=0;a<256;a++) {
			for(ch=0;ch<16;ch++) {
				tpx_from_altro(r,a,ch,row,pad) ;

				if(row > 45) continue ;	// not in this RDO...

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

	// clear all flags but the existing ones
	for(row=0;row<=45;row++) {
		if(row_ix[row] < 0) continue ;

		for(pad=1;pad <= tpc_rowlen[row]; pad++) {

			get_stage1(row, pad)->f &= 0xFF00 ;	// clear, keeping upper bits intact...
		}
	}


	// put gains & flags
	for(row=0;row<=45;row++) {
		if(row_ix[row] < 0) continue ;

		for(pad=1;pad<=tpc_rowlen[row];pad++) {	
			stage1 *s = get_stage1(row, pad) ;

			s->g = gains->get_gains(sector,row,pad)->g ;
			s->t0 = gains->get_gains(sector,row,pad)->t0 ;

			u_int fl = 0 ;

			if(!(s->f & 0x8000)) {	// not really here
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

			// marks the first and last pad as "edge"
			if((pad==1) || (pad==tpc_rowlen[row])) {
				fl |= FCF_ROW_EDGE ;
			}
			
			s->f |= fl | FCF_ONEPAD ;

			//LOG(DBG,"FCF gains: row %2d, pad %3d: gain %f, flags 0x%04X",row,pad,s->g,s->f) ;

		}


	}
		
	
	return ;
}

void tpxFCF::start_evt()
{
// possibly unnecessary
	for(int r=0;r<=45;r++) {
		if(row_ix[r] < 0) continue ;

		for(int p=1;p<tpc_rowlen[r];p++) {
			struct stage1 *o ;
			o = get_stage1(r, p) ;
			if(o==0) {
				LOG(ERR,"No row pad %d:%d???",r,p) ;
			}
			else {
				o->count = 0 ;
			}
		}
	}

	
	return ;
}


int tpxFCF::do_pad(tpx_altro_struct *a)
{
	struct stage1 *s ;

	s = get_stage1(a->row, a->pad) ;
	if(s==0) {
		LOG(ERR,"Whoa -- no row:pad %d:%d???",a->row,a->pad) ;
		return 0 ;
	}
	else {
		//LOG(DBG,"RP %d:%d at 0x%08X",a->row,a->pad,s) ;
	}

	s->count = 0 ;


	//LOG(DBG,"Sector %d: rp %d:%d, %d values",sector,a->row,a->pad,a->count) ;

	if(a->row > 45) return 0 ;
	if(a->count==0) return 0 ;

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

		//printf("...%d:%d\t%2d: tb %3d, adc %3d: new %d, last_falling %d, tb %d - %d\n",
		//       a->row,a->pad,i,tb,adc,new_cluster,last_falling,tb_start,tb_prev) ;

		if(unlikely( new_cluster )) {
			if(max_adc >= FCF_ADC_MIN) {
				cl->t_ave = t_ave ;
				cl->charge = charge ;

				cl->t1 = tb_prev;	// put previous; t1 is the lower value
				cl->t2 = tb_start ;
		
				cl->flags = flags ;
	
				#ifdef FCF_EXTENDED
				cl->pix = cl->t2 - cl->t1 + 1 ;
				cl->max_adc = max_adc ;				
				#endif


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


		if(unlikely( adc > max_adc )) max_adc = adc ;


		last_adc = adc ;
		tb_prev = tb ;

		t_ave += adc * tb ;
		charge += adc ;

		   
	}	// scan over the sequence
	
	// finish off the rest...
	if(max_adc >= FCF_ADC_MIN) {
		cl->t1 = tb_prev  ;	// t1 is the lower value
		cl->t2 = tb_start ;

		cl->flags = flags ;

		cl->charge = charge ;
		cl->t_ave = t_ave ;

		#ifdef FCF_EXTENDED
		cl->pix = cl->t2 - cl->t1 + 1 ;
		cl->max_adc = max_adc ;
		#endif


		cl++ ;
	}

	// goto target...
	pad_done: ;


	s->count = cl - s->cl ;	// count of 1d clusters

	if(unlikely( s->count == 0 )) {
		LOG(DBG,"No 1D clusters?") ;
	}
	else {
		if(unlikely( s->count >= FCF_MAX_CL )) {
			LOG(NOTE,"Row %d:%d, %d clusters [max!]",a->row,a->pad,s->count) ;
		}
		else {
			//LOG(DBG,"Row %d:%d, %d clusters",a->row,a->pad,s->count) ;
		}
	}

#if 0
	// debugging dump
	cl = s->cl ;
	for(int j=0;j<s->count;j++) {
		LOG(DBG,"C: %d:%d\t %3d %3d, ch %5d, fl 0x%X",a->row,a->pad,cl->t1,cl->t2,cl->charge,cl->flags) ;	
		cl++ ;
	}

#endif

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

			//LOG(DBG,"RP %d:%d at 0x%08X, next at 0x%08X",r,p,old1,cur1) ;

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
							
						if(unlikely(old->flags & FCF_FALLING)) {
							
							if(old->flags & FCF_ONEPAD) {
								LOG(ERR,"Can't be on a merge!") ;
							}
							if(!(old->flags & FCF_IN_DOUBLE)) {
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
						if(!(cur->flags & FCF_ONEPAD)) {
							LOG(ERR,"Can;t be -- here I can have only ONEPADS!!") ;
						}
						if(!(cur->flags & FCF_IN_DOUBLE)) {
							LOG(ERR,"Can't be -- I must be in double here!") ;
						}						
						// correct our charge & t0 and move to float!!!

						//cur->f_charge = cur->charge * cur1->g ;
						//cur->scharge = cur->f_charge ;	// GAIN corrected!						

						//cur->f_t_ave = cur->t_ave * cur1->g ;
						//cur->f_t_ave += cur1->g * cur1->t0 ;
						//cur->p_ave = (p+1) * cur->f_charge + old->p_ave ;

						// also correct the charge if the old pad
						// is still 1pad wide...
						if(old->flags & FCF_ONEPAD) {
							if(old->flags & FCF_IN_DOUBLE) {
								// this happens on a merge
								//LOG(NOTE,"Corrected old pad %d:%d 0x%X",r,p,old->flags) ;
							}
							else {
								// correct charge here
								old->flags |= FCF_IN_DOUBLE ;
								old->f_charge = old->charge * old1->g ;
								old->scharge = old->f_charge ;
								old->f_t_ave = old->t_ave * old1->g ;
								old->f_t_ave += old1->g * old1->t0 ;

								old->p1 = p ;
								old->p_ave = p * old->f_charge ;
							}
						}

						
						cur->flags |= old->flags ;
						cur->flags &= ~FCF_ONEPAD ;



						cur->f_charge += old->f_charge ;
						cur->f_t_ave += old->f_t_ave ;
						cur->p_ave += old->p_ave ;
						cur->p1 = old->p1 ;

						


						if(cur->t1 > old->t1) {
							cur->t_min = old->t1 ;
						}
						else {
							cur->t_min = cur->t1 ;
						}

						if(cur->t2 < old->t2) {
							cur->t_max = old->t2 ;
						}
						else {
							cur->t_max = cur->t2 ;
						}


						#ifdef FCF_EXTENDED
						cur->pix += old->pix ;
						if(cur->max_adc < old->max_adc) cur->max_adc = old->max_adc ;
						#endif

						cur++ ;	// don't use the new guy again...
						break ;
					}
				}	// end of loop over new....


				//LOG(NOTE,"Merge %d:%d = merge %d, t1..t2 %d:%d",r,p,merge,old->t1,old->t2) ;


				switch(merge) {
				case 1 :

					// this is a real merge so keep the old guy...
					break ;
				default :
					if(old->flags & FCF_ONEPAD) {
						if(merge==3) {
							LOG(ERR,"Can;t be!") ;
						}
					}
					else {
						// dump OLD!
						old->p2 = p  ;
						dump(old) ;
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

		for(c=0;c<old1->count;c++) {	// loop over old

			old->p2 = p ;
			dump(old) ;

			old++ ;
		}


		// end of row. We either enter the count in the header or go back 1 int...
		if(cur_row_clusters) {
			*row_cache = r ;
			*clust_count_cache = cur_row_clusters ;
			//LOG(DBG,"In row %2d: found %d clusters",r,cur_row_clusters) ;
		}
		else {
			loc_buff -= 2 ;	// go back, nothing found!
		}

	}	// loop over rows...

	return loc_buff - (u_int *)outbuff ;	// return number of ints stored!
}


void tpxFCF::dump(tpxFCF_cl *cl)
{
	u_int fla = cl->flags ;
	u_int tmp_p, tmp_fl ;
	u_int pad_c ;
	int time_c ;	// time_c can go negative due to T0 corrections!
	double dp, dt ;
	u_int p1, p2 ;

	// integerize charge
	if(cl->flags & FCF_IN_DOUBLE) ;
	else {
		if(cl->flags & FCF_ONEPAD) return ;
		LOG(ERR,"Not in double?") ;
	}

	//LOG(DBG,"dump: row %d: %d %d %d %d %f 0x%X",cur_row,cl->p1,cl->p2,cl->t_min,cl->t_max,cl->f_charge,cl->flags) ;

	// cuts go here
	if(likely(do_cuts)) {

		if(fla & FCF_BROKEN_EDGE) {	// always pass!
			;
		}
		else if(fla & FCF_ONEPAD) return ;
		//else if(fla & (FCF_ROW_EDGE | FCF_DEAD_EDGE | FCF_ONEPAD)) return ;	// kill!
		//else if(cl->tmin == 0) return ;		// kill if they touch timebin 0
		else if((cl->t_max - cl->t_min) <= 2) return ;	// kill if the cluster length in time is small

	}

	int cha = (int) (cl->f_charge + 0.5) ;

	// clear the FALLING flag 
	fla &= (~FCF_FALLING) ;

	dp = cl->p_ave / cl->f_charge ;
	dt = cl->f_t_ave / cl->f_charge ;

	// we dump an integerized version ("fixed point") which is 32 times larges
	time_c = (u_int) (dt * 32.0 + 0.5) ;	// 0.5 for correct roundoff
	pad_c = (u_int) (dp * 32.0 + 0.5) ;


//	LOG(INFO,"\tCL: %f[%d:%d] %f[%d:%d] %f 0x%04X",dp,cl->p1,cl->p2,dt,cl->t_min,cl->t_max,cl->charge,fla) ;

	// get pad extents
	tmp_p  = pad_c / 32   ;
		
	p1 = tmp_p - cl->p1 ;
	p2 = cl->p2 - tmp_p ;

	if(p1 > 7) p1 = 7 ;
	if(p2 > 7) p2 = 7 ;

	tmp_fl = (p1 << 8) | (p2 << 11)  ;


	// get timebin extents
	tmp_p = time_c / 32   ;
		
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


#ifdef FCF_SIM_ON
	//if(fla==0) printf("PIX %d %d %d %d %d\n",cl->cl_id,fla,cha,cl->pix,cl->adc_max) ;

	{
	int i, j ;

	int quality = 1 ;

	if(simout) {

		u_int sim_cha, all_cha ;

		sim_cha = all_cha = 0 ;

		for(i=1;i<=182;i++) {
			for(j=0;j<512;j++) {
				if(pixStruct[i][j].cl_id == cl->cl_id) {
					if(cl->id == pixStruct[i][j].id_simtrk) {
						sim_cha += pixStruct[i][j].adc ;
					}
					all_cha += pixStruct[i][j].adc ;
				}
			}
		}

		if(all_cha) {
			quality = (int)(100.0*(double)sim_cha/(double)all_cha) ;
		}
		else {
			quality = 0 ;
		}


		struct FcfSimOutput *s = (struct FcfSimOutput *) simout ;

		s->id_simtrk = cl->id ;
		s->id_quality = quality ;

		s->cl_id = cl->cl_id ;	// put the local cluster id

		simout += sizeof(struct FcfSimOutput)/4 ;	// advance here
		
	}

#if defined(FCF_ANNOTATE_CLUSTERS) && !defined(__ROOT__)
	for(i=1;i<=182;i++) {
		for(j=0;j<512;j++) {
			if(pixStruct[i][j].adc) {
				fcfPixA[row-1][i-1][j] = pixStruct[i][j] ;
			}
		}
	}
#endif
	}
#endif	// FCF_SIM_ON!

	cur_row_clusters++ ;
	
	return ;
}
