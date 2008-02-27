#include <stdio.h>
#include <sys/types.h>
#include <math.h>
#include <string.h>
#include <assert.h>

#ifdef __ROOT__

#define LOG(x,a,v1,v2,v3,v4,v5)

#else
#include <rtsLog.h>
#endif

#include <fcfClass.hh>
#include <rts.h>
#include <TPC/padfinder.h>
//#include <fcfAfterburner.hh>


// Version: 1.0; 17Sep2003; Tonko
//

// Tonko: what is this? You can't make these global - someone can step on them
// by using common names in their code!

//void *lastAfter=0;
//int myNext = 0;



fcfAfterburner::fcfAfterburner() 
{ 
	last_n = last_count = last_i = last_stage = 0; 
	
	do_merge = do_cuts = 1 ; 
	
	verbose = true; 

//    	lastAfter=this;
}

/*
	This is a small helper function which prints the
	hit data in "h" to stdout.
	If "str" is non-NULL it prepends it to the line
	as a label.
*/
void fcfAfterburner::print_hit(char *str, struct fcfHit *h)
{
        if(str) {
                fprintf(stdout,"%s: ",str) ; 
        }

	fprintf(stdout,"%d %f %f %d %d %d %d %d %d %d %d %d\n",
		row,(double)h->pad/64.0+0.5,(double)h->tm/64.0+0.5,h->c,h->f,
		h->p1,h->p2,h->t1,h->t2,h->cl_id,h->id_simtrk,h->id_quality) ;

	return  ;
}


/*
	The function returns TRUE of the hit in "hit"
	satisfies the post-burner cuts.
*/
int fcfAfterburner::output(struct fcfHit *hit)
{	
	int ret ;

	// CUTS - NEVER TOUCH THESE VALUES!
	if(do_cuts) {
		if(hit->f & (FCF_ONEPAD | FCF_ROW_EDGE | FCF_DEAD_EDGE)) ret = 0 ;	// bad flag
		else if((hit->t2 - hit->t1) <= 3) ret = 0 ;	// too short in timebin
		else if(hit->t1 == 0) ret = 0 ;			// touches timebin 0
		else if(hit->c < 40) ret = 0 ;			// small charge
		else ret = 1 ;					// OK!
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

/*
	The function decodes the FCF-packed data in "ptr" into
	the local structure "hit".
	It also uses the FCF-packed simulation data from "sim" (if it
	exists) and adds it to "hit". "sim" defaults to "NULL" in
	the function declaration.
*/
void fcfAfterburner::decode(u_int *ptr, struct fcfHit *hit, u_int *sim)
{
	u_int pt, cf ;
	u_short flags, fl ;
	u_short tm, pad ;
	u_short p1,p2,t1,t2 ;
	u_short charge ;



	if(sim) {
		struct FcfSimOutput *s = (struct FcfSimOutput *) sim ;

		// note that sim is always done on the local machine so
		// there is no need to check for endianess...
		hit->id_simtrk = s->id_simtrk ;
		hit->id_quality = s->id_quality ;
		hit->cl_id = s->cl_id ;
	}
	else {
		// these are the defaults which FCF will also use - don't change!
		hit->id_simtrk = 0 ;
		hit->id_quality = 100 ;
		hit->cl_id = -1 ;
	}


	pt = *ptr++ ;
	cf = *ptr++ ;

	
	if(do_swap) {	// bytes swapping...
		pt = swap32(pt) ;
		cf = swap32(cf) ;
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

/*
	Checks for mergability between the 2 hits and makes
	a merge by merging into "hit_l" while marking the
	charge of "hit_r" as 0.
*/
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

		// adjust track ids 
		if(hit_r->id_simtrk != hit_l->id_simtrk) {
			// choose the ID with the larger charge
			if(hit_r->c > hit_l->c) {
				hit_l->id_simtrk = hit_r->id_simtrk ;
			}

		}

		// quality is the weighted mean...
		hit_l->id_quality = (hit_l->id_quality * hit_l->c + hit_r->id_quality * hit_r->c) / charge ;


		// adjust cluster ids
		if(hit_r->cl_id != hit_l->cl_id) {
			hit_l->cl_id = 0xFFFF ;	// mark as unknown...
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


int fcfAfterburner::burn(u_int *ptr_res[3], u_int *ptr_simu_res[3])
{
	u_int i ;

	last_i = last_n = last_count = last_stage = 0 ;	// wrap to the beginning!
	
	ptr = ptr_res ;	// store arg in member...
	ptr_simu = ptr_simu_res ;

	for(i=0;i<3;i++) {
		if(ptr_res[i]) {
			row = *ptr_res[i] ;	// this should point to "row"

			if(row < 123) do_swap = 0 ;	// same endianes == LOCAL
			else do_swap = 1 ;		// different endianess == SWAP
			
			break ;	// at least one row
		}
	}
	
	if(i==3) { // no content
		row = 0 ;
		return -1 ;
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
	u_int *res_sim ;

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
	if(ptr_simu) {
		res_sim = ptr_simu[last_n] ;
	}
	else {
		res_sim = 0 ;
	}

	if(res == 0) {
		last_n++ ;
		last_i = 0 ;
		goto stage1;
	}

	if(last_i == 0) {	// need new counters
		u_int irow = *res++ ;
		last_count = *res++ ;

		// need to advance the sim guy as well...
		if(res_sim) res_sim += 2 ;

		if(do_swap) {	
			irow = swap32(irow) ;
			last_count = swap32(last_count) ;
		}

		if(irow != row) LOG(ERR,"Row %d and row %d in data don't match!!!",row,irow,0,0,0) ;
	}
	else {
		res += 2 + 2*last_i ;
		if(res_sim) res_sim += 2 + (sizeof(struct FcfSimOutput)/4)*last_i ;
	}

	while(last_i < last_count) {

		fcfHit hit ;
		decode(res,&hit,res_sim) ;

		res += 2 ;	// skip to next
		if(res_sim) {
			res_sim += (sizeof(struct FcfSimOutput)/4) ;
		}

		last_i++ ;

		if(do_merge && (hit.f & FCF_BROKEN_EDGE)) {
			int list ;

			list = -1 ;

			if(hit.p2 == edge[0]) list = 0 ;
			else if(hit.p1 == edge[1]) list = 1 ;
			else if(hit.p2 == edge[2]) list = 2 ;
			else if(hit.p1 == edge[3]) list = 3 ;
				
			if(list >= 0 && cou_broken[list]<kMax_fcfHit) {
				memcpy(&(broken[list][cou_broken[list]]),&hit,sizeof(hit)) ;
				cou_broken[list]++ ;
				//print_hit("Broken",&hit) ;
				//LOG(NOTE,"    list %d: %d %d %d %d",list,edge[0],edge[1],edge[2],edge[3]) ;
			}
			else {	// this happens when the pad width of hit is more than 7 in one direction...
				LOG(DBG,"Row %d: incorrect edges are %d:%d, %f???",row,hit.p1,hit.p2,(double)hit.pad/64.0+0.5,0) ;
				LOG(DBG,"    %d %d %d %d",edge[0],edge[1],edge[2],edge[3],0) ;
			}
		}
		else {
			if(output(&hit)) {
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
		if(output(&broken[last_n][last_i])) {
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

	returns # of mismatches
	0 means match....
*/
int fcfAfterburner::compare(u_int *p1[3], u_int *p2[3])
{
	u_int matched1, count1, count2 ;
	u_int match ;
	fcfAfterburner after ;
	fcfHit h1, h2 ;
	static u_char marray[2][10000] ;	// the size could be a problem!
	int i ;
	int ret = 0 ;	// return number of mismatches
	int save_merge, save_cuts ;

	// save the original steering variables...
	save_merge = do_merge ;
	save_cuts = do_cuts ;

	// ...and reset the merging and cutting to 0 so that we
	// can compare "raw" data

	do_merge = do_cuts = 0 ;
	after.do_merge = after.do_cuts = 0 ;

	memset(marray,0,sizeof(marray)) ;

	burn(p1) ;
	matched1 = count1 = 0 ;


	while(next(&h1)) {
		
		match = 0 ;
		after.burn(p2) ;	// reset!

		count2 = 0 ;
		//print_hit("Checking 1",&h1) ;
		while(after.next(&h2)) {
			//print_hit(" with",&h2) ;
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
			//printf("*MATCH\n") ;
			marray[0][count1] = 1 ;
			matched1++ ;
		}
		count1++ ;
	}

	burn(p1) ;
	i = 0 ;
	while(next(&h1)) {
		if(marray[0][i] == 0) {	// unmatched
// Tonko: following taken out but left as comments in case we need it
//			if((h1.f & FCF_BROKEN_EDGE) && h1.c <= 10) {
//				;
//			}
//			else {
			{
			        if(verbose) print_hit("UNMATCHED 1",&h1) ;
				ret++;
			}
		}
		i++ ;
	}

	after.burn(p2) ;
	i = 0 ;
	while(after.next(&h1)){
		if(marray[1][i] == 0) {	// unmatched
// Tonko: following taken out but left as comments in case we need it
//			if(h1.f & (FCF_DEAD_EDGE | FCF_ROW_EDGE)) {
//				;
//			}
//			else {
			{
			        if(verbose) print_hit("UNMATCHED 2",&h1) ;
				ret++;
			}
		}
		i++ ;
	}

	// save the steering before exit!
	do_cuts = save_cuts ;
	do_merge = save_merge ;
	return ret ;
}

