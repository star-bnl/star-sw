#include <stdio.h>
#include <sys/types.h>
#include <math.h>
#include <string.h>

#ifdef __ROOT__

#define LOG(x,a,v1,v2,v3,v4,v5)

#else
#include <rtsLog.h>
#endif

#include <fcfClass.hh>
#include <rts.h>
#include <TPC/padfinder.h>
#include <fcfAfterburner.hh>


// Version: 1.0; 17Sep2003; Tonko
//

void fcfAfterburner::print_hit(char *str, struct fcfHit *h)
{
        if(str) {
                fprintf(stdout,"%s: row %d: %f %f %d %d %d %d %d %d\n",
                        str,row,(double)h->pad/64.0+0.5,(double)h->tm/64.0+0.5!
                        h->p1,h->p2,h->t1,h->t2) ;
        }
        else {
                fprintf(stdout,"%d %f %f %d %d %d %d %d %d\n",
                        row,(double)h->pad/64.0+0.5,(double)h->tm/64.0+0.5,h->!
                        h->p1,h->p2,h->t1,h->t2) ;

        }
	return  ;
}

int fcfAfterburner::output(struct fcfHit *hit, char *anystruct)
{	
	int ret ;

	// CUTS!
	if(do_cuts) {
		if(hit->f & (FCF_ONEPAD | FCF_ROW_EDGE | FCF_DEAD_EDGE)) ret = 0 ;
		else if(hit->c < 50) ret = 0 ;
		else ret = 1 ;
	}
	else {
		ret = 1 ;
	}

	if(ret) {
		//print_hit("SAVED",hit) ;
	}
	else {
		//print_hit("REJEC",hit) ;
	}

	return ret;

}

void fcfAfterburner::decode(u_int *ptr, struct fcfHit *hit)
{
	u_int pt, cf ;
	u_short flags, fl ;
	u_short tm, pad ;
	u_short p1,p2,t1,t2 ;
	u_short charge ;


	pt = *ptr++ ;
	cf = *ptr++ ;

	if(do_swap) {	// bytes swapping...
		pt = swap32(pt) ;
		cf = swap32(pt) ;
	}

	tm = (pt >> 16) & 0x7FFF ;
	pad = pt & 0x3FFF ;
	charge = cf >> 16 ;
	fl = cf & 0xFFFF ;

	flags = 0 ;
	if(pt & 0x8000) flags |= FCF_DOUBLE_PAD ;
	if(pt & 0x4000) flags |= FCF_DEAD_EDGE ;
	if(pt & (0x8000 << 16)) flags |= FCF_ONEPAD ;
	if(fl & 0x8000) flags |= FCF_ROW_EDGE ;
	if(fl & 0x4000) flags |= FCF_BROKEN_EDGE ;

	t2 = (fl & 0x00F0) >> 4 ;
	t1 = fl & 0x000F ;

	t2 = (tm >> 6) + t2 ;
	t1 = (tm >> 6) - t1 ;

	p2 = (fl & 0x3800) >> 11 ;
	p1 = (fl & 0x0700) >> 8 ;

	p2 = (pad >> 6) + p2 ;
	p1 = (pad >> 6) - p1 ;

	// NOTE: pad,p1,p2 count from "1" whereas tm,t1,t2 count from 0!
	hit->pad = pad ;
	hit->tm = tm ;
	hit->c = charge ;
	hit->f = flags ;
	hit->p1 = p1 ;
	hit->p2 = p2 ;
	hit->t1 = t1 ;
	hit->t2 = t2 ;

	return ;
}


int fcfAfterburner::check_merge(struct fcfHit *hit_l, struct fcfHit *hit_r) 
{
	u_int charge ;
	double tm[2] ;

	tm[0] = (double)hit_l->tm  ;
	tm[1] = (double)hit_r->tm  ;

	if(fabs(tm[0]/64.0-tm[1]/64.0)<2.0) { // merge!
		//print_hit("Left",hit_l) ;
		//print_hit("Right",hit_r) ;

		charge = hit_r->c + hit_l->c ;
		if(charge & 0xFFFF0000) { // tooooo big! kill!
			LOG(ERR,"Merge: charge too big %d %d",hit_r->c, hit_l->c,0,0,0) ;
			hit_l->c = hit_r->c = 0 ;
			return 0 ;
		}

		hit_l->tm = (short)((tm[0]*(double)hit_l->c + tm[1]*(double)hit_r->c)/(double)charge) ;

		tm[0] = (double)hit_l->pad ;
		tm[1] = (double)hit_r->pad ;
		hit_l->pad = (short)((tm[0]*(double)hit_l->c + tm[1]*(double)hit_r->c)/(double)charge) ;
		
		hit_l->c = charge ;
		hit_r->c = 0 ;	// kill...

		hit_l->f |= hit_r->f ;
		hit_l->f &= ~FCF_ONEPAD ;	// remove ONEPAD, if any...

		// adjust maxpad of the left one...
		hit_l->p2 = hit_r->p2 ;

		// adjust time extents...
		if(hit_r->t2 > hit_l->t2) hit_l->t2 = hit_r->t2 ;
		if(hit_r->t1 < hit_l->t1) hit_l->t1 = hit_r->t1 ;

		//print_hit("Merged",hit_l) ;
		return 1 ;	// done
	}


	return 0 ;
}


int fcfAfterburner::burn(u_int *ptr_res[3])
{
	u_int i ;

	last_i = last_n = last_count = last_stage = 0 ;	// wrap to the beginning!
	
	ptr = ptr_res ;	// store arg in member...

	if(ptr_res[0]) {
		row = *ptr_res[0] ;	// this should point to "row"

		if(row < 123) do_swap = 0 ;	// same endianes == LOCAL
		else do_swap = 1 ;		// different endianess == SWAP
	}
	else {
		row = 0 ;	// mark unknown...
		return -1 ;	// data doesn't exist for this row...
	}


	// get the row + sanity checks
	row = 123 ;
	for(i=0;i<3;i++) {
		if(ptr[i]) {
			if(row == 123) row = do_swap ? swap32(*ptr[i]) : (*ptr[i]) ;
			else if(row != (do_swap ? swap32(*ptr[i]) : (*ptr[i])))  {
				row = 0 ;
				LOG(ERR,"Corrupted row data pointer %d: is %d expect %d",i,*ptr[i],row,0,0) ;
				return -1 ;
			}
		}
	}


	// set the broken edges
	// 0 signifies none exists...
	edge[0] = padfinder[row-1][0].maxpad ;
	edge[1] = padfinder[row-1][1].minpad ;
	edge[2] = padfinder[row-1][1].maxpad ;	
	edge[3] = padfinder[row-1][2].minpad ;

	memset(cou_broken,0,sizeof(cou_broken)) ;

	return 0 ;
}

int fcfAfterburner::next(fcfHit *h)
{
	// move through (up to) 3 components
	// and find and tag broken clusters
	// and output the others at the same time...
	u_int *res ;

	if(row == 0) {	// no data to begin with in "burn"
		return 0 ;
	}

	//fprintf(stderr,"stage %d, last_n %d, last_i %d, last_count %d\n",last_stage,last_n,last_i,last_count) ;

	// where are we?
	switch(last_stage) {
	case 0 :
		goto stage1 ;
		break ;
	case 1 :
		goto stage2 ;
		break ;
	case 2 :
		goto stage3 ;
		break ;
	}

	
	stage1: ;	// the normal clusters + marking of merge candidates...
	if(last_n >= 3) {	// done with stage1
		goto stage2 ;
	}

	res = ptr[last_n] ;
	if(res == 0) {
		last_n++ ;
		last_i = 0 ;
		goto stage1;
	}

	if(last_i == 0) {	// need new counters
		u_int irow = *res++ ;
		last_count = *res++ ;

		if(do_swap) {	
			irow = swap32(irow) ;
			last_count = swap32(last_count) ;
		}

		if(irow != row) LOG(ERR,"Row %d and row %d in data don't match!!!",row,irow,0,0,0) ;
	}
	else {
		res += 2 + 2*last_i ;
	}

	while(last_i < last_count) {

		fcfHit hit ;
		decode(res,&hit) ;
		res += 2 ;
		last_i++ ;

		if(do_merge && (hit.f & FCF_BROKEN_EDGE)) {
			int list ;

			list = -1 ;

			if(hit.p2 == edge[0]) list = 0 ;
			else if(hit.p1 == edge[1]) list = 1 ;
			else if(hit.p2 == edge[2]) list = 2 ;
			else if(hit.p1 == edge[3]) list = 3 ;
				
			if(list >= 0) {
				memcpy(&(broken[list][cou_broken[list]]),&hit,sizeof(hit)) ;
				cou_broken[list]++ ;
				//print_hit("Broken",&hit) ;
				//LOG(NOTE,"    list %d: %d %d %d %d",list,edge[0],edge[1],edge[2],edge[3]) ;
			}
			else {	// this happens when the pad width of hit is more than 7 in one direction...
				LOG(NOTE,"Row %d: incorrect edges are %d:%d, %f???",row,hit.p1,hit.p2,(double)hit.pad/64.0+0.5,0) ;
				LOG(NOTE,"    %d %d %d %d",edge[0],edge[1],edge[2],edge[3],0) ;
			}
		}
		else {
			if(output(&hit,NULL)) {
				memcpy(h,&hit,sizeof(hit)) ;
				return 1 ;
			}
		}
	}

	last_i = 0 ;
	last_n++ ;
	goto stage1 ;


	stage2: ;
	last_stage = 1 ;

	if(edge[0] == 0) return 0 ;	// non-broken row, no merged found...
	
	// If I'm here this means that I'm on a "breakable" row
	// so let's merge lists 0-1 and 2-3
	int right ;
	for(right=0;right<4;right+=2) {
		u_int i, j ;
		for(i=0;i<cou_broken[right];i++) {
			int merged = 0 ;
			for(j=0;j<cou_broken[right+1];j++) {
				if(broken[right+1][j].c == 0) continue ; // already merged

				// check_merge will merge into the first argument and zero out the second...
				merged = check_merge(&broken[right][i],&broken[right+1][j]) ;
				if(merged) break ;
			}
		}
	}


	// output the final resuts
	last_i = 0 ;
	last_n = 0 ;

	stage3: ;
	last_stage = 2 ;
 
	if(last_n >= 4) return 0 ;	// done
	while(last_i<cou_broken[last_n]) {
		if(output(&broken[last_n][last_i],NULL)) {
			memcpy(h,&broken[last_n][last_i],sizeof(fcfHit)) ;
			last_i++ ;
			return 1 ;
		}
		last_i++ ;
	}
	last_n++ ;
	last_i = 0 ;
	goto stage3 ;

}

/*
	This function has resursive use of the object.
	The "p1" data will use the current object while
	the "p2" data will be placed into a new object...
*/
int fcfAfterburner::compare(u_int *p1[3], u_int *p2[3])
{
	u_int matched1, count1, count2 ;
	u_int match ;
	fcfAfterburner after ;
	fcfHit h1, h2 ;
	static u_char marray[2][10000] ;
	int i ;
	int ret = 1 ;	// assume match...

	memset(marray,0,sizeof(marray)) ;

	burn(p1) ;
	matched1 = count1 = 0 ;

	do_merge = do_cuts = 0 ;
	while(next(&h1)) {
		
		match = 0 ;
		after.burn(p2) ;	// reset!
		after.do_merge = after.do_cuts = 0 ;
		count2 = 0 ;
		while(after.next(&h2)) {
			if(marray[1][count2]) {
				count2++ ;
				continue ;
			}

			if(memcmp(&h1,&h2,sizeof(h1))==0) {
				marray[1][count2] = 1 ;
				match = 1 ;
				break ;
			}
			count2++ ;
		}

		if(match) {
			marray[0][count1] = 1 ;
			matched1++ ;
		}

		count1++ ;
	}

	burn(p1) ;
	i = 0 ;
	while(next(&h1)) {
		if(marray[0][i] == 0) {	// unmatched
			if((h1.f & FCF_BROKEN_EDGE) && h1.c <= 10) {
				;
			}
			else {
				print_hit("UNMATCHED 1",&h1) ;
				ret = 0 ;
			}
		}
		i++ ;
	}

	after.burn(p2) ;
	i = 0 ;
	while(after.next(&h1)){
		if(marray[1][i] == 0) {	// unmatched
			if(h1.f & (FCF_DEAD_EDGE | FCF_ROW_EDGE)) {
				;
			}
			else {
				print_hit("UNMATCHED 2",&h1) ;
				ret = 0 ;
			}
		}
		i++ ;
	}

	return ret ;
}
