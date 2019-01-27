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

char *tpxFCF::fcf_flags(u_char flags)
{
	static char c_flags[32] ;

	c_flags[0] = 0 ;	// empty string...

	if(flags & FCF_IN_DOUBLE) strcat(c_flags,"?") ;	// can't be

	if(flags & FCF_DEAD_EDGE) strcat(c_flags,"D") ;
	if(flags & FCF_BROKEN_EDGE) strcat(c_flags,"B") ;
	if(flags & FCF_ROW_EDGE) strcat(c_flags,"E") ;

	if(flags & FCF_BIG_CHARGE) strcat(c_flags,"c") ;
	if(flags & FCF_DOUBLE_T) strcat(c_flags,"!") ;	// can't be
	if(flags & FCF_MERGED) strcat(c_flags,"m") ;
	if(flags & FCF_ONEPAD) strcat(c_flags,"1") ;
	
	if(c_flags[0] == 0) strcpy(c_flags,"0") ;

	return c_flags ;
}


int tpxFCF::afterburner(int cou, daq_cld *store[])
{
	int merged ;

	if(likely(cou == 0)) return 0 ;	// most cases like this...

	//printf("Start afterburner: cou %d\n",cou) ;

	merged = 0 ;

	for(int i=0;i<cou;i++) {
		daq_cld *l = store[i] ;	// left guy
		int merge_ix = -1 ;



		if(l->charge == 0) continue ;	// already merged
		if(l->flags & FCF_BROKEN_EDGE) ;
		else continue ;

		int type = 0 ;
		
		for(int j=0;j<cou;j++) {
			int ok = 0 ;


			if(j<=i) continue ;

			daq_cld *r = store[j] ;	// right guy
		
			if(r->charge == 0) continue ;	// already merged!
			if(r->flags & FCF_BROKEN_EDGE) ;
			else continue ;

			if(r->p1==13) {	// right
				if(l->p2 != 12) continue ;
				type = 1 ;
			}
			else if(r->p2==12) {
				if(l->p1 != 13) continue ;
				type = 2 ;
			}
			else if(r->p1==131) {
				if(l->p2 != 130) continue ;
				type = 3 ;
			}
			else if(r->p2==130) {
				if(l->p1 != 131) continue ;
				type = 4 ;
			}
			else {	// can happen if one was deconvoluted...
				continue ;
				type = 5 ;
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
			printf("can merge %d: L%d: [%d:%d] %f, f 0x%02X, R%d: [%d:%d] %f f 0x%02X\n",
			       type,
			       i,l->p1,l->p2,l->tb,l->flags,
			       merge_ix,r->p1,r->p2,r->tb,r->flags) ;
			*/

			l->tb = (l->tb * l->charge + r->tb * r->charge) / charge ;
			l->pad = (l->pad * l->charge + r->pad * r->charge) / charge ;
			
			if(r->t1 < l->t1) l->t1 = r->t1 ;
			if(r->t2 > l->t2) l->t2 = r->t2 ;

			if(r->p1 < l->p1) l->p1 = r->p1 ;
			if(r->p2 > l->p2) l->p2 = r->p2 ;

			l->flags |= r->flags ;		// merge flags
			l->flags &= ~FCF_ONEPAD ;	// remove onepad, by dedfinition.

			// bug fixed...
			if(charge > (double)0x7FFF) {
				
				int ch = (int) (charge+1) ;	// roundoff

				ch /= 1024 ;	// do what FCF does...

				l->charge = (u_short) ch * 1024 ;	// do what decoder does...
				l->flags |= FCF_BIG_CHARGE ;
			}
			else {
				l->charge = (u_short) charge ;
			}

			l->flags &= ~FCF_BROKEN_EDGE ;

			// and kill the right guy!
			r->flags |= FCF_DEAD_EDGE  ;
			r->charge = 0 ;
		}


		
	}

	// cleanup flags
	for(int i=0;i<cou;i++) {
		daq_cld *l = store[i] ;	// left guy

//		printf("AfterB: %2d: P [%d:%d], T [%d:%d], flags %d, charge %d\n",i,l->p1,l->p2,l->t1,l->t2,l->flags,l->charge) ;

		if(l->flags & (FCF_ONEPAD | FCF_DEAD_EDGE | FCF_ROW_EDGE)) {	// is this is still on, kill it with dead edge
			l->flags |= FCF_DEAD_EDGE ;
			l->charge = 0 ;
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
	u_int p_tmp, t_tmp ;

	// pad
	p_tmp = *p_buff & 0xFFFF ;

	// time
	t_tmp = *p_buff >> 16 ;


	if(version==FCF_V_FY08) {	// had a bug in FY08...
		p = (double)(p_tmp & 0x3FFF) / 32.0 ;
		t = (double)(t_tmp & 0x7FFF) / 32.0 ;
	}
	else {
		p = (double)(p_tmp & 0x3FFF) / 64.0 ;
		t = (double)(t_tmp & 0x7FFF) / 64.0 ;
	}			

	fla = 0 ;
	if(p_tmp & 0x8000) fla |= FCF_MERGED ;
	if(p_tmp & 0x4000) fla |= FCF_DEAD_EDGE ;
	if(t_tmp & 0x8000) fla |= FCF_ONEPAD ;


	p_buff++ ;	// advance to next word
	cha = *p_buff >> 16 ;

	if(cha >= 0x8000) {	// special case of very large charge...
//printf("1: Big cha: 0x%08X %d; %f %f\n",cha,cha,p,t) ;

		fla |= FCF_BIG_CHARGE ;
		cha = (cha & 0x7FFF) * 1024 ;
//		if(cha == 0) cha = 0x8000;	// exaclty, but can't be I think...

//printf("2: Big cha: 0x%08X %d\n",cha,cha) ;

		// quasi fix of the very large problem...
		if(cha > 0xFFFF) cha = 0xFFFF ;	// because the daq_cld structure has charge as a short... damn...

	}

	p_tmp = *p_buff & 0xFFFF ;	// reuse p_tmp

	if(p_tmp & 0x8000) fla |= FCF_ROW_EDGE ;
	if(p_tmp & 0x4000) fla |= FCF_BROKEN_EDGE ;

	t1 = p_tmp & 0xF ;
	t2 = (p_tmp >> 4) & 0xF ;


	p1 = (p_tmp >> 8) & 0x7 ;
	p2 = (p_tmp >> 11) & 0x7 ;


	t1 = (int)t - t1 ;
	t2 = (int)t + t2 ;


	p1 = (int)p - p1 ;
	p2 = (int)p + p2 ;

	dc->t1 = t1 ;
	dc->t2 = t2 ;
	dc->p1 = p1 ;
	dc->p2 = p2 ;
	dc->charge = cha ;	// this is a problem for BIG_CHARGE... it will strip the upper bits... unsolved.
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


tpxFCF::s_static_storage *tpxFCF::gain_storage[24][256] ;



tpxFCF::tpxFCF() 
{
	rbs = 0 ;
	sector = 0 ;
	modes = 0 ;

	rdo = 0 ;
	fcf_style = 0 ;
	memset(working_storage,0,sizeof(working_storage)) ;

	cl_marker = 0 ;
	my_id = -1 ;

	run_compatibility = 9 ;	// run 9
	do_cuts = 2 ;	// 1 means always, 2 means don't cut edges (for i.e. pulser run), 0 means don't...
	ch_min = 10 ;

	row_count = 45 ;	// default for our usual TPC
	tpx_rowlen = tpc_rowlen ;	// default for out usual TPC
	tpx_padplane = 0 ;		// original padplane

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

	for(int s=0;s<24;s++) {
	for(int r=0;r<256;r++) {
		if(working_storage[s][r]) {
			free(working_storage[s][r]) ;
			working_storage[s][r] = 0 ;
		}
		if(gain_storage[s][r]) {
			free(gain_storage[s][r]) ;
			gain_storage[s][r] = 0;
		}
	}
	}

	return ;
}

/*
	Called once at startup to allocate the static storage
*/
void tpxFCF::config2(int sec1, int rdo1, int mode, int rows, unsigned char *rowlen)
{
	int rdo0 = rdo1 - 1 ;
	int sec0 = sec1 - 1 ;

	//temporary
	sector = sec1 ;

	modes = mode ;

	if((rowlen==0) || (rows<=0)) {
		tpx_rowlen = tpc_rowlen ;
		row_count = 45 ;	// force override
		tpx_padplane = 0 ;	// original
	}
	else {
		tpx_rowlen = rowlen ;
		row_count = rows ;
		tpx_padplane = 1 ;	// new padplane
	}


	int tot_count = 0 ;

	for(int a=0;a<256;a++) {
	for(int ch=0;ch<16;ch++) {
		int pad, row ;

		tpx_from_altro(rdo0,a,ch,row,pad) ;

		if(row > 250) continue ;	// not in this RDO...
		if(row == 0) continue ;	// will nix row 0 as well...

		int bytes = tpx_rowlen[row] * sizeof(s_static_storage) ;


		if(gain_storage[sec0][row]) ;
		else {


			gain_storage[sec0][row] = (s_static_storage *)valloc(bytes) ;
			//and clear
			memset(gain_storage[sec0][row],0, bytes) ;

			tot_count += tpx_rowlen[row] ;
			
		}



		s_static_storage *ss = get_static(row,pad) ;

		ss->f = FCF_NEED_PAD | FCF_ONEPAD ;
		ss->g = 1.0 ;
		ss->t0 = 0.0 ;
	}
	}

	LOG(TERR,"config2: S%2d, RDO %d: %d pads",sec1,rdo1,tot_count) ;
}


void tpxFCF::config(u_int mask, int mode, int rows, unsigned char *rowlen)
{
	rbs = mask ;
	modes = mode ;

	int r ;
	int row, pad ;
	int a, ch ;

	memset(row_ix,0,sizeof(row_ix)) ;

	if((rowlen==0) || (rows<=0)) {
		tpx_rowlen = tpc_rowlen ;
		row_count = 45 ;	// force override
		tpx_padplane = 0 ;	// original
	}
	else {
		tpx_rowlen = rowlen ;
		row_count = rows ;
		tpx_padplane = 1 ;	// new padplane
	}


	//LOG(WARN,"calling config: mask 0x%X, mode %d, rows %3d",mask,mode,row_count) ;

	// There is some amount of acrobatics involved so
	// bear with me...

	if(tpx_padplane) {	// some new padplane
		for(row=1;row<=row_count;row++) row_ix[row] = 1 ;
	}
	else {	// original TPC padplane
		// First, we figure out which rows are needed from the RB mask
		for(r=0;r<6;r++) {	// hell, let's assume 32 RDOs per sector...
		if(rbs & (1<<r)) {
			for(a=0;a<256;a++) {
			for(ch=0;ch<16;ch++) {
				tpx_from_altro(r,a,ch,row,pad) ;

				if(row > 250) continue ;	// not in this RDO...
				if(row == 0) continue ;	// will nix row 0 as well...
				row_ix[row] = 1  ;	// mark as needed...
			}
			}
		}
		}
	}

	// get the count of pads needed assuming _whole_ rows!
	int tot_count = 0 ;
	for(row=0;row<=row_count;row++) {
		if(row_ix[row]) {
			tot_count += tpx_rowlen[row] ;	// allocate whole rows!
		}
	}

	LOG(NOTE,"fcfconfig: RDO mask 0x%03X: allocated %d pads (%d bytes)",rbs,tot_count,tot_count * sizeof(struct stage1)) ;

	// allocate storage
	if(storage) {
		LOG(WARN,"Whoa! Storage already allocated!") ;
		free(storage) ;
	}

	storage = (struct stage1 *) valloc(tot_count * sizeof(struct stage1)) ;
	
	LOG(NOTE,"FCF for mask 0x%02X: alloced %d bytes for %d tot_count X %d; cleared gains",mask,
	    tot_count * sizeof(struct stage1),
	    tot_count,
	    sizeof(struct stage1)) ;

	// clear storage
	memset(storage,0,tot_count * sizeof(struct stage1)) ;



	// re-create offsets which we use in the row+pad navigation
	tot_count = 0 ;	// re use...
	for(row=0;row<=row_count;row++) {
		if(row_ix[row] == 0) {
			row_ix[row] = -1 ;	// nix!
			continue ;
		}

		row_ix[row] = tot_count ;

		tot_count += tpx_rowlen[row] ;
	}

	// OK -- we have the storage and the navigation via get_stage1(row,pad) ready.
	// Now let's get the per-pad flags ready:

	if(tpx_padplane) {
		for(row=1;row<=row_count;row++) {
			for(pad=1;pad<=tpx_rowlen[row];pad++) {
				get_stage1(row,pad)->f = FCF_NEED_PAD | FCF_ONEPAD ;
				get_stage1(row,pad)->g = 1.0 ;
				get_stage1(row,pad)->t0 = 0.0 ;
			}
		}
	}
	else {
		// Mark pads which are there as present...
		for(r=0;r<6;r++) {
			if(rbs & (1<<r)) {
			for(a=0;a<256;a++) {
			for(ch=0;ch<16;ch++) {
				tpx_from_altro(r,a,ch,row,pad) ;

				if(row > 250) continue ;
				if(row == 0) continue ;

				get_stage1(row, pad)->f = FCF_NEED_PAD | FCF_ONEPAD ;
				get_stage1(row, pad)->g = 1.0 ;
				get_stage1(row, pad)->t0 = 0.0 ;

			}
			}
			}
		}
	}

	return ;
}

/*
	This might me reapplied...
*/
void tpxFCF::apply_gains2(tpxGain *gain)	
{
//	gains = gain ;

	if(tpx_padplane) gain = 0 ;	// force it!

	if(gain == 0) {
		LOG(WARN,"Sector %2d, gains NULL",sector) ;
	}
	else {
		LOG(NOTE,"Applying gains to sector %d [%p ?]",sector,gain) ;
	}

	// clear all flags but the existing higher ones
	for(int s=0;s<24;s++) {
	for(int r=0;r<256;r++) {
		if(gain_storage[s][r] == 0) continue ;

		sector = s+1 ;
		rdo = r+1 ;

		for(int row=1;row<=row_count;row++) {
		for(int pad=1;pad<=tpx_rowlen[row];pad++) {
			s_static_storage *ss = get_static(row,pad) ;

			if(ss==0) continue ;

			ss->f &= 0xFF00 ;
		}
		}
	}
	}


	for(int s=0;s<24;s++) {
	for(int r=0;r<256;r++) {
		if(gain_storage[s][r] == 0) continue ;

		sector = s+1 ;
		rdo = r+1 ;

		for(int row=1;row<=row_count;row++) {
		for(int pad=1;pad<=tpx_rowlen[row];pad++) {
			s_static_storage *ss = get_static(row,pad) ;
			int kill = 0 ;

			if(ss==0) continue ;
			
			if(gain) {
				ss->g = gain->get_gains(sector,row,pad)->g ;
				ss->t0 = gain->get_gains(sector,row,pad)->t0 ;
			}
			else {
				ss->g = 1.0 ;
				ss->t0 = 0.0 ;
			}
			
			int fl = 0 ;

			if(tpx_fy16_map==0) {
				if(!(ss->f & FCF_NEED_PAD)) {	// not in this RDO i.e. row 8 is split between RDO1 and RDO2
					fl |= FCF_BROKEN_EDGE ;
					LOG(TERR,"Broken edge in RDO %d: RP %d:%d",rdo,row,pad) ;
				}
			}

			if(ss->g < 0.001) {
				fl |= FCF_DEAD_EDGE ;
				kill |= FCF_KILLED_PAD ;	// this pad is really dead!!!
			}

			if(fl) {	// apply the above flags to adjecant pads as well
				if(pad>1) {
					get_static(row,pad-1)->f |= fl ;
				}
				if(pad < tpx_rowlen[row]) {
					get_static(row,pad+1)->f |= fl ;
				}
			}

			// marks the first and last pad as "edge"
			if((pad==1) || (pad==tpx_rowlen[row])) {
				fl |= FCF_ROW_EDGE ;
			}
			
			ss->f |= fl | FCF_ONEPAD | kill ;
			
			//if(row<=13) {
			//	LOG(WARN,"S %d:%d, RP %d:%d is 0x%X",sector,rdo,row,pad,ss->f) ;
			//}
			
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
	gain = gain ;

	int row, pad ;

	if(tpx_padplane) gain = 0 ;	// force it!

	LOG(WARN,"apply_gains???") ;

	if(gain == 0) {
		LOG(WARN,"Sector %2d, gains NULL",sector) ;
	}
	else {
		LOG(NOTE,"Applying gains to sector %d [%p ?]",sector,gain) ;
	}

	// clear all flags but the existing ones
	for(row=1;row<=row_count;row++) {
		if(row_ix[row] < 0) continue ;

		for(pad=1;pad <= tpx_rowlen[row]; pad++) {

			get_stage1(row, pad)->f &= 0xFF00 ;	// clear, keeping upper bits intact...
		}
	}


	// put gains & flags
	for(row=1;row<=row_count;row++) {
		if(row_ix[row] < 0) continue ;

		for(pad=1;pad<=tpx_rowlen[row];pad++) {	
			stage1 *s = get_stage1(row, pad) ;

			if(gain) {
				s->g = gain->get_gains(sector,row,pad)->g ;
				s->t0 = gain->get_gains(sector,row,pad)->t0 ;
			}
			else {
				s->g = 1.0 ;
				s->t0 = 0.0 ;
			}

			u_int fl = 0 ;

			if(!(s->f & FCF_NEED_PAD)) {	// not really here; missing in the RDO...
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
				if(pad < tpx_rowlen[row]) {
					get_stage1(row,pad+1)->f |= fl ;	// bad_edge ;
				}

			}

			// marks the first and last pad as "edge"
			if((pad==1) || (pad==tpx_rowlen[row])) {
				fl |= FCF_ROW_EDGE ;
			}
			
			s->f |= fl | FCF_ONEPAD ;

			LOG(DBG,"FCF gains: row %2d, pad %3d: gain %f, flags 0x%04X",row,pad,s->g,s->f) ;

		}


	}
		
	
	return ;
}

void tpxFCF::start_evt2(int sec1, int rdo1)
{
	cl_marker = 10000 ;	// used to mark unique clusters sector...

//	LOG(TERR,"start_evt2: START: %d %d",sec1,rdo1) ;

	sector = sec1 ;
	rdo = rdo1 ;

	
	for(int r=1;r<=row_count;r++) {
		if(gain_storage[sector-1][r] == 0) continue ;	//this is an optimization thing...

		for(int p=1;p<=tpx_rowlen[r];p++) {
			struct stage1 *w ;
			w = get_working(r, p) ;
			if(unlikely(w==0)) {
				LOG(ERR,"[%d] S%02d:%d: no row pad %d:%d???",my_id,sector,rdo,r,p) ;
			}
			else {
				//LOG(TERR,"[%d] S%02d:%d: got rp %d:%d",my_id,sector,rdo,r,p) ;
				w->count = 0 ;
			}
		}
	}

//	LOG(TERR,"start_evt2: END: %d %d",sec1,rdo1) ;
	
	return ;
}


void tpxFCF::start_evt()
{
	cl_marker = 10000 ;	// used to mark unique clusters sector...


	for(int r=1;r<=row_count;r++) {
		if(row_ix[r] < 0) continue ;

		for(int p=1;p<=tpx_rowlen[r];p++) {
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
	
	if(unlikely(a->row > 250)) return 0 ;
	if(unlikely(a->row == 0)) return 0 ;
	if(unlikely(a->pad > tpx_rowlen[a->row])) return 0 ;

	if(fcf_style) {
		s = get_working(a->row,a->pad) ;
	}
	else {
		s = get_stage1(a->row, a->pad) ;


	}

	if(unlikely(s==0)) {
		LOG(ERR,"[%d] Whoa -- no row:pad %d:%d???",my_id,a->row,a->pad) ;
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

	u_int flags ;
	u_int orig_flags ;

	if(fcf_style) {
		struct s_static_storage *ss ;

		ss = get_static(a->row,a->pad) ;
		if(unlikely(ss->g <= 0.001)) {
			//LOG(WARN,"Killed %d:%d",a->row,a->pad) ;
			return 0 ;
		}

		orig_flags = flags =  ss->f & 0xFF ;
	}
	else {
		// kill, by hand, the pad which has 0.0 gain just to avoid confusion
		// when the gains i.e. in Offline are misapplied
		if(unlikely(s->g <= 0.001)) {
			//LOG(WARN,"Killed %d:%d",a->row,a->pad) ;
			return 0 ;
		}

		orig_flags = flags =  s->f & 0xFF ;
	}


//	if(a->row <= 13) {
//		LOG(TERR,"Doing %d:%d %d",a->row,a->pad,a->count) ;
//	}

	u_int t_ave, charge ;
	u_int tb_start ;
	u_int last_falling, last_adc ;

	u_int tb_prev ;
	int new_cluster ;

	struct tpxFCF_cl *cl, *cl_max ;


	cl = s->cl ;	// start...
	cl_max = &(s->cl[FCF_MAX_CL]) ;	// end of cl sentinel...
	

	u_int max_adc = 0 ;

	last_falling = last_adc = 0 ;
	t_ave = charge = 0 ;

	tb_prev = tb_start = a->tb[0] ;

	new_cluster = 0 ;


	// start the loop over raw pixels in this pad...
	for(int i=0;likely( i < a->count );i++) {
		u_int adc, tb ;


		adc = a->adc[i] ;
		tb = a->tb[i] ;

//		printf("........looking at %d %d %d %d\n",a->row,a->pad,tb,adc) ;

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
//			else flags = s->f & 0xFF ;
			else flags = orig_flags & 0xFF ;

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


	for(r=1;r<=row_count;r++) {
		if(fcf_style) {
			if(gain_storage[sector-1][r] == 0) continue ;
		}
		else {
			if(row_ix[r] < 0) continue ;
		}


		cur_row = r ;
		//if(cur_row == 0) {
		//	LOG(WARN,"Can;t be -- row 0; row_ix %d",row_ix[r]) ;
		//}

		int bytes_so_far = (loc_buff - outbuff)*4 ;
		if(bytes_so_far > (max_bytes-1024)) {
			LOG(WARN,"row %d: approaching limit: is %d, max %d",cur_row,bytes_so_far,max_bytes) ;
		}


		u_int *row_cache = loc_buff++ ;	// remember where the row goes...
		u_int *clust_count_cache = loc_buff++ ;	// remember where the cluster count goes...

		
		cur_row_clusters = 0 ;

		//LOG(TERR,"[%d S%02d:%d] going into row %d",my_id,sector,rdo,r) ;

		// scan from first pad to one before the last...
		for(p=1;p<tpx_rowlen[r];p++) {

			if(fcf_style) {
				struct s_static_storage *old_ss, *cur_ss ;

				old1 = get_working(r,p) ;

				if(old1->count == 0) continue ;

				cur1 = get_working(r,p+1) ;
				
				old_ss = get_static(r,p) ;
				cur_ss = get_static(r,p+1) ;

				//LOG(TERR,"%p %p %p %p",old1,cur1,old_ss,cur_ss) ;

				old1->g = old_ss->g ;
				old1->t0 = old_ss->t0 ;
				//old1->f = 0 ;

				cur1->g = cur_ss->g ;
				cur1->t0 = cur_ss->t0 ;
				//cur1->f = 0 ;


				
			}
			else {
				old1 = get_stage1(r,p) ;
				cur1 = get_stage1(r,p+1) ;
			}

			//if(r<=13) LOG(TERR,"[%d] rp %d:%d : gain %f, t0 %f, flags 0x%X",my_id,r,p,old1->g,old1->t0,old1->f) ;

			old = old1->cl ;
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
							//BUG in FY10!!!
							//cur->f_t_ave += cur1->g * cur1->t0 ;
							cur->f_t_ave += cur->f_charge * cur1->t0 ;
							cur->p1 = p+1 ;
							cur->p_ave = (p+1) * cur->f_charge ;

							// Added in FY12
							cur->t_min = cur->t1 ;
							cur->t_max = cur->t2 ;

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
							//BUG in FY10!!!
							//old->f_t_ave += old1->g * old1->t0 ;
							old->f_t_ave += old->f_charge * old1->t0 ;

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
							if (cur->f_charge + old->f_charge > 0)
							qua /= (cur->f_charge + old->f_charge) ;
							else qua = 0;
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
						//old->p1 = 201 ; was in FY11
						old->p1 = p ;
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
		if(fcf_style) {
			old1 = get_working(r,p) ;
		}
		else {
			old1 = get_stage1(r,p) ;
		}

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
//		LOG(TERR,"[%d] row %d, clusters %d",my_id,r,cur_row_clusters) ;

		if(cur_row_clusters) {
			*row_cache = (do_version<<16) | r ;
			*clust_count_cache = cur_row_clusters ;
			//LOG(DBG,"In row %2d: found %d clusters",r,cur_row_clusters) ;
		}
		else {
			loc_buff -= 2 ;	// go back, nothing found!
		}

	}	// loop over rows...

//	LOG(TERR,"[%d] returning %d",loc_buff-(u_int *)outbuff) ;

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


//LOG(TERR,"[%d] Dump: fl 0x%X: p %d,%d, t %d,%d",my_id,fla,cl->p1,cl->p2,cl->t1,cl->t2) ;

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
			struct stage1 ggg, *gain ;

			if(likely(fla & FCF_ONEPAD)) ;
			else {
				LOG(WARN,"WTF? not onebad but not in double!?") ;
			}

//			printf("ONEPAD: %d %d %d %d %d\n",cl->p1,cl->p2,cl->t1,cl->t2,fla) ;


			
			if(fcf_style) {
				gain = &ggg ;

				gain->g = get_static(row, cl->p2)->g ;
				gain->t0 = get_static(row, cl->p2)->t0 ;


			}
			else {
				gain = get_stage1(row, cl->p2) ;
			}

			cl->p1 = cl->p2 ;

			cl->f_charge = cl->charge * gain->g ;
			// don't need scharge

			cl->f_t_ave = cl->t_ave * gain->g ;
			// BUG in FY10!!!
			//cl->f_t_ave += gain->g * gain->t0 ;
			cl->f_t_ave += cl->f_charge * gain->t0 ;

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
