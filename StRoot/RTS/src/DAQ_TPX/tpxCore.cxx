#include <stdio.h>
#include <math.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <rtsLog.h>

#include <TPX/tpx_altro_to_pad.h>
#include <DAQ1000/ddl_struct.h>
#include <TPC/fee_readout.h>



#include <DAQ_TPX/tpxCore.h>

// globally visible
struct tpx_rdo tpx_rdo[6] ;

struct tpx_rdo_dbg tpx_rdo_dbg[6] ;


// statics...
static inline u_int get10(u_int *l, u_int p) ;
static u_int *data_test(u_int *h, struct tpx_altro_struct *a, int log)  ;
static int check_emul(u_int *a) ;

static struct {
	u_char altro ;
	u_char ch ;
	u_char rdo ;	// from 1 !!!
} tpx_pad_to_altro[46][183] ;



/*
	sectors count 1..24
	rows count 1..45 _but_ row 0 is reserved for non-connected channels
	pads count 1..182 ; pad 0 is never present

*/

void tpx_to_altro(int row, int pad, int &rdo, int &a, int &ch)
{
	static int first ;
	int ro, pa ;

//	LOG(DBG,"row %d, pad %d",row,pad) ;

	if(first == 0) {
		for(int r=0;r<6;r++) {
		for(int a=0;a<256;a++) {
		for(int ch=0;ch<16;ch++) {
			tpx_from_altro(r,a,ch,ro,pa) ;
			if(ro > 45) continue ;

			tpx_pad_to_altro[ro][pa].altro = a ;
			tpx_pad_to_altro[ro][pa].ch = ch ;
			tpx_pad_to_altro[ro][pa].rdo = r+1 ;	// rdo from 1
		}
		}
		}
		first = 1 ;
	}

//	LOG(DBG,"row %d, pad %d",row,pad) ;

	rdo = tpx_pad_to_altro[row][pad].rdo ;
	a = tpx_pad_to_altro[row][pad].altro ;
	ch = tpx_pad_to_altro[row][pad].ch ;

}


void tpx_from_altro(int rdo, int a, int ch, int &row, int &pad)
{
	row = tpx_altro_to_pad[rdo][a][ch].row ;
	pad = tpx_altro_to_pad[rdo][a][ch].pad ;

#ifdef TEST_RDO
	if(rdo==5) {	// only 6th RDO can be a test!
		if((a>=50) && (a<=55)) {
			row = 255 ;
			pad = 255 ;
			return ;
		}
		if((a>=250) && (a<=255)) {
			a -= 200 ;

			row = tpx_altro_to_pad[rdo][a][ch].row ;
			pad = tpx_altro_to_pad[rdo][a][ch].pad ;
			return ;
		}
	}
#endif

	return ;
}

int tpx_altro_to_fee(int rdo, int a)
{
	
	rdo-- ;	// to start from 0

	for(int i=0;i<36;i++) {
		int fee, altro ;

		fee = fee_position[rdo][i] ;
		if(fee == 255) continue ;

		altro = (fee << 1) & 0xFF ;

		if(altro == a) return fee ;
	}

	return -1 ;
}

u_char tpx_rdo_fees(int rdo, int cou)
{
	if(cou >= 36) return 255 ;

	return fee_position[rdo-1][cou] ;
}

u_char tpx_altro_ch_to_fee(int a, int ch)
{
	if(a & 1) ch += 16 ;	// for odd altros, add 16

	for(int i=0;i<32;i++) {
		if(tpx_old_to_new_ch[i] == ch) return i ;
	}

	return 255 ;

}



/*
	Go through the event, check what you can
	and return the token.
	If we are called with do_log=1  _JUST_ do some
	checks and return the token with logging. Made to work as "get_token"
*/
int tpx_get_start(char *buff, u_int words, struct tpx_rdo_event *rdo, int do_log) 
{
	struct ddl_header *hdr ;
	struct ddl_trailer *trl ;

	u_int *l ;	// temporary pointer...

//	buff +=8 ;	// suuuuuuuuper hack!
	
	hdr = (struct ddl_header *) buff ;

	rdo->data_err = 0 ;
	rdo->token = -ENOTSUP ;	// start as if error
	rdo->trg_cou = 0 ;

	// get stuff from the header...
	rdo->type = hdr->type & 0xF ; ;
	rdo->subtype = (hdr->type >> 4) & 0xF ;
	rdo->sector = (hdr->type >> 12) & 0x7F ;	// last bit might indicate an error!
	rdo->rdo = (hdr->type >> 8) & 0xF ;

	rdo->data_end = 0 ;
	rdo->data_start = (u_int *)(buff + sizeof(struct ddl_header)) ;	// skip the header...

	rdo->l2_cmd = 0 ;

	// now lets move to the end...
	trl = (struct ddl_trailer *) (buff + 4*words - sizeof(struct ddl_trailer)) ;

	LOG(DBG,"Header 0x%08X 0x%08X",hdr->type,hdr->ev_cou) ;

//	for(u_int i=0;i<words+8;i++) {
//		LOG(DBG,"%2d: 0x%08X",i,*((u_int *)buff + i)) ;
//	}



	if(trl->type != hdr->type) {
		if(hdr->type == 0xFEED0301) {	// HACK! Old factory type!
			if(do_log) LOG(WARN,"RDO %d: Old factory log?",rdo->rdo) ;
			rdo->data_start = (u_int *)(buff + 12) ;
			rdo->type = DDL_TYPE_LOG ;
		}
		else {
			if(do_log) LOG(ERR,"RDO %d:%d: Header type 0x%08X and trailer type 0x%08X mismatch",rdo->sector,rdo->rdo,hdr->type,trl->type) ;
			rdo->token = -EBADF ;
			return rdo->token ;
		}
	}



	switch(trl->fl_wc >> 28) {
	case 0 :		// all OK
		rdo->data_err = 0 ;
		break ;
	default :

		if(do_log) rdo->data_err = 1 ;	// HACK!
		break ;	// go on...
	}


	switch(rdo->type) {
	case DDL_TYPE_LOG :
	case DDL_TYPE_MSC :
		rdo->token = 4096 ;	// valid
		return rdo->token ;
	case DDL_TYPE_DTA :
		break ;	// later...
	default :
		if(do_log) {
			LOG(WARN,"Hm,I haven't coded this type %d", rdo->type) ;
			u_int *val = rdo->data_start ;
			for(int i=0;i<10;i++) {
				LOG(WARN,"\t%2d: 0x%08X [%u dec]",i,val[i],val[i]) ;
			}
		}
		rdo->token = -ENOTSUP ;
		return rdo->token ;

		break ;
	}



	l = (u_int *) trl - 1 ;		// move before the trailer...

	rdo->trg_cou = *l ;		// get the trigger count

	if(rdo->trg_cou > 120) {
		LOG(ERR,"Bad trg count %d",rdo->trg_cou) ;
		rdo->trg_cou = 0 ;
		rdo->token = -ENOTSUP ;
		return rdo->token ;
	}
	
	// move to the start of trigger data
	l -= rdo->trg_cou * (sizeof(struct trg_data)/4) ;

	if(l < rdo->data_start) {
		LOG(ERR,"Bad trigger data!") ;
		rdo->data_err = 1 ;
		rdo->token =  -EBADF ;
		return rdo->token ;
	}

	rdo->trg = (struct trg_data *) l ;


	u_int rh  ;
	u_int rh_prompt = 0 ;
	rdo->token = 4097 ;	// in case I don't find a real token!

	for(u_int i=0;i<rdo->trg_cou;i++) {
		int rh_delta ;

		// fish out the token...
		switch(rdo->trg[i].csr & 0xFF000000) {
		case 0xFF000000 :	// fifo,skip
		case 0xDD000000 :	// FIFO, but selftrigger
			rh = rdo->trg[i].rhic_counter ;
			rh_delta = rh - rh_prompt ;
			break ;	
		case 0xEE000000 :	// emulated!
		default :		// real TCD prompt trigger
			rh_prompt = rdo->trg[i].rhic_counter ;
			rh_delta = rh_prompt - tpx_rdo_dbg[rdo->rdo-1].old_rhic ;
			rdo->token = rdo->trg[i].data & 0xFFF ;
			break ;

		}

		if(do_log) {
		    if(rdo->rdo == 6) {
			 LOG(TERR,"\tRDO %d: evt %d: trg %d: RHIC %u, CSR 0x%08X, data 0x%08X [t %d], bytes %u, delta %u",rdo->rdo,hdr->ev_cou,i,
		    		rdo->trg[i].rhic_counter,
		    		rdo->trg[i].csr,
		    		rdo->trg[i].data,rdo->trg[i].data&0xFFF,
		    		words*4,
		    		rh_delta) ;
		    }
		}

	}

	if(do_log) {
		if(rh_prompt) {
			tpx_rdo_dbg[rdo->rdo-1].delta = rh_prompt - tpx_rdo_dbg[rdo->rdo-1].old_rhic ;
			tpx_rdo_dbg[rdo->rdo-1].old_rhic = rh_prompt ;
		}
		else {
			tpx_rdo_dbg[rdo->rdo-1].delta = 0 ;
			// leave "old rhic" intact!

		}
		
	}

	if(do_log) LOG(DBG,"Event %d: sector %2d, rdo %d, type %d, subtype %d, token %d",
	    hdr->ev_cou,rdo->sector,rdo->rdo,rdo->type,rdo->subtype,rdo->token) ;

	l -= 2 ;	// points to FEE mask, will go away eventually since I never use it...

	//fee_mask[0] = l[0] ;
	//fee_mask[1] = l[1] ;
	
	if(do_log && (rdo->rdo==6)) {
		LOG(TERR,"\t evt %d: %u %u",hdr->ev_cou,l[0],l[1]) ;
	}

	// 
	//if(rdo->subtype==DDL_DTA_EMUL) {
	//	check_emul(rdo->data_start) ;
	//	rdo->data_start = 0 ;	// mark as non-physics...
	//}

	if(rdo->data_err) LOG(ERR,"RDO %d: token %d, event %10u marked as in error [0x%X]",rdo->rdo,rdo->token,hdr->ev_cou,(trl->fl_wc >> 28)) ;

	if(do_log) LOG(DBG,"RDO %d: real data...",rdo->rdo) ;

	l-- ;	// and now points to the last datum

	rdo->data_end = l ;

	return rdo->token ;
}

	

/*
	This runs through the data pointer "now"
	backwards (in the altro data manner), checks
	for correctness.

	If the data is correct, unpacks it into 
	an altro_struct and returns the pointer 
	to the next altro in the data block.

	If the data is NOT correct, it scans
	bacwards until it finds a correct bank
	and repeats.

	If the "first" pointer is reached, it
	returns NULL.

*/
u_int *tpx_scan_to_next(u_int *now, u_int *first, struct tpx_altro_struct *a_struct)
{
	u_int *next_altro ;
	u_int *store = now ;

	int log_yes = 1 ;


	do {
		next_altro = data_test(now,a_struct,log_yes) ;	// returns pointer to next altro!


		// logic to print out on first error only....
		if(next_altro==0) {	// error in the bank!
			log_yes = 0 ;	// switch of further logging
			if(now > first) {
				now-- ;		// decrement data buffer and try again...
			}
			else {
				return 0 ;
			}
	
		}
		else {		// data is OK
	
			if(log_yes==0) {	// we lost something before, turn on WARN
				LOG(WARN,"    ...but found A%03d:%02d %d words earlier",a_struct->id,a_struct->ch,store-now) ;
	
			}
			else {
				//LOG(DBG,"Found %03d:%02d : %3d adcs",a_struct->id,a_struct->ch,a_struct->count) ;
			}

		
			//if(log_yes==0) {	// had error
			//	LOG(WARN,"\t... now 0x%08X, next_altro 0x%08X, now-first 0x%08X, next-first 0x%08X",
			//	    now,next_altro,now-first,next_altro-first) ;
			//}

			if(((next_altro-first)<-1) || ((next_altro-first)>1000000)) {
				// I wan't to see this...
				LOG(ERR,"next_altro-first %d",next_altro-first) ;
			}

			return next_altro ;	// pointer to next altro

		}


	} while((now-first)>=1) ;	// there must be at least 2 words!

	// can't be -- I wan't to see this
	//LOG(ERR,"At end: now 0x%08X, next_altro 0x%08X, now-first 0x%08X, next-first 0x%08X",
	//    now,next_altro,now-first,next_altro-first) ;

	return 0 ;	// nothing found!

}

int tpx_use_rdo(char *rdobuff, int bytes, int (userfunc)(struct tpx_altro_struct *a, void *arg), void *arg)
{
	int t ;
	u_int *data_end ;
	tpx_rdo_event rdo ;
	tpx_altro_struct a ;
	int ret ;

	ret = 0 ;

	t = tpx_get_start(rdobuff, bytes/4, &rdo, 0) ;
	if(t<=0) {
		LOG(NOTE,"token %d, rdo %d: not an altro event",t,rdo.rdo) ;
		return ret ;
	}

	if(rdo.data_err) {
		LOG(ERR,"token %d, rdo %d: altro data error!",t,rdo.rdo) ;
		return -1 ;
	}

	a.what = TPX_ALTRO_DO_ADC ;
	a.rdo = rdo.rdo - 1;
	a.t = t ;

	LOG(DBG,"token %d, rdo %d: running through %d bytes...",t,rdo.rdo,bytes) ;

	data_end = rdo.data_end ;
	do {
		data_end = tpx_scan_to_next(data_end, rdo.data_start, &a) ;

		ret |= userfunc(&a, arg) ;
	} while((data_end > rdo.data_start) && data_end) ;

	return ret ;
}


/*
	Scans and uses one altro channels worth of data...

	h points to where the data is
	a->rb points to the input RB!

*/
static u_int *data_test(u_int *h, struct tpx_altro_struct *a, int log) 
{
  u_int hi, lo ;
  int wc ;
  int ret ;


  ret = 0 ;

  a->count = 0 ;
  a->wc = 0 ;
  a->row = 100 ;	// unknown...

  lo = *h-- ;
  hi = *h-- ;

  // "h" now points to the data part...


  if((lo & 0xCFF00000) || (hi & 0xCFF00000)) {
    // this should _not_ happen, so I wan't to see it
    if(log) LOG(WARN,"  Header words have junk: HI 0x%08X, LO 0x%08X",hi,lo) ;
    ret = -1 ;
  }


  // standard tests of the last ALTRO word...
  if((hi & 0xFFFC0) != 0xAAA80) {
    if(log) LOG(WARN,"  Error HI in last ALTRO word: 0x%08X 0x%08X",hi,lo) ;
    ret = -1 ;

  }
  if((lo & 0x0F000) != 0x0A000) {
    if(log) LOG(WARN,"  Error LO in last ALTRO word: 0x%08X 0x%08X",hi,lo) ;
    ret = -1 ;
  }


  wc = ((hi&0x3F)<<4) | ((lo&0xF0000)>>16) ;	// altro's word count


  a->id = (lo & 0xFF0)>>4 ;	// altro ID 0..255
  a->ch = lo & 0xF ;		// channel 0..15


  // get the row and pad; this is why we needed the rdo...
  tpx_from_altro(a->rdo, a->id, a->ch, (int &) a->row, (int &) a->pad) ;

  a->where = h ;

  if((wc > 529) || (wc<0)) {	// for 512 tb + 15 pre + 2
    if(log) LOG(WARN,"  Error in last ALTRO word: 0x%08X 0x%08X; bad WC %d",hi,lo,wc) ;
    ret = -1 ;
  }

  // we bomb out here if there was any error
  if(ret) {
	if(log) LOG(ERR,"RDO %d: token %d: Altro %03d:%02d (?)  bad header [1]",a->rdo+1,a->t,a->id,a->ch) ;
	return 0 ;	// already error...
  }
  else {
	//LOG(DBG,"Token %d (rdo %d): Altro %03d:%02d wc %d",a->t,a->rdo,a->id,a->ch,wc) ;
  }



  if(wc == 0) return h ;	// empty channel...


  a->wc = wc ;


  int p10 = 0 ;		// backward counter of the 10bit contributions

  // now we move through the padding, depending on the wc's last 2 bits...
  switch(wc&3) {
  case 0 :
    break ;

  case 1 :
    if(get10(h,p10) != 0x2AA) {
      if(log) LOG(WARN,"  Bad 0x2AA 1:1") ;
      ret = -1 ;
    }
    p10++ ;

    if(get10(h,p10) != 0x2AA) {
      if(log) LOG(WARN,"  Bad 0x2AA 1:2") ;
      ret = -1 ;
    }
    p10++ ;

    if(get10(h,p10) != 0x2AA) {
      if(log) LOG(WARN,"  Bad 0x2AA 1:3") ;
      ret = -1 ;
    }
    p10++ ;

    break ;

  case 2 :
    if(get10(h,p10) != 0x2AA) {
      if(log) LOG(WARN,"  Bad 0x2AA 2:1") ;
      ret = - 1 ;
    }
    p10++ ;

    if(get10(h,p10) != 0x2AA) {
      if(log) LOG(WARN,"  Bad 0x2AA 2:2") ;
      ret = -1 ;
    }
    p10++ ;

    break ;

  case 3 :
    if(get10(h,p10) != 0x2AA) {
      if(log) LOG(WARN,"  Bad 0x2AA 3:1") ;
      ret = -1 ;
    }
    p10++ ;

    break ;
  }

  int l10 = wc ;

  while(l10 % 4) l10++ ;	// move until divisible by 4

  // l10 is minimally 4 for right now


#ifdef VEERY_PARANOID
  // check general form of _all_ the data -- nothing should be in the upper 12 bits
  for(int i=0;i<l10/4;i++) {
    if(h[-i] & 0xCFF00000) {
      // should not happen -- I want to see this!
      if(log) LOG(ERR,"  Bad form 0x%08X at %d",h[-i],i) ;	
      ret = -1 ;
      break ;
    }
  }
#endif

  if(ret) {
	if(log) LOG(ERR,"RDO %d: token %d: Altro %03d:%02d (?)  bad header [2]",a->rdo+1,a->t,a->id,a->ch) ;
	return 0 ;	// already error...
  }




  // data check; we are in the data mode now...
  int tb_prev = 512 ;

  u_short *p_adc = a->adc ;
  u_short *p_tb = a->tb ;

  while(p10 < l10) {
	int tb_cou = get10(h,p10++) ;
	int tb_last = get10(h,p10++) ;


	tb_cou -= 2 ;	// tb_cou & tb_last are included in the count, get rid of them...

	/*
	u_int tb_last 
		can't be greater than 511
		can't be less than 0
		can't be greater or equal to the end of the previous sequence

	
	int tb_cou
		can't be less than 0 
		can't be greater than 512
		can't be greater than the total word count!

	*/

	// do some sanity checks...
	if((tb_cou > wc) || (tb_cou <= 0) || (tb_cou > 512)) {
		if(log) LOG(WARN,"  A%03d:%02d: Bad tb_cou %d > wc %d [tb_last %d]?",a->id,a->ch,tb_cou,wc,tb_last) ;
		ret = -1 ;
	}

	if((tb_last < 0) || (tb_last >= tb_prev)) {
		if(log) LOG(WARN,"  A%03d:%02d: Bad tb_last %d => tb_prev %d?",a->id,a->ch,tb_last,tb_prev) ;
		ret = -1 ;
	}

	if(ret) {
		if(log) LOG(ERR,"RDO %d: token %d: Altro %03d:%02d (?)  bad data [1]",a->rdo+1,a->t,a->id,a->ch) ;
		return 0 ;
	}


	tb_prev = tb_last - tb_cou ;

	if(a->what & TPX_ALTRO_DO_ADC) {
		for(;tb_last > tb_prev; tb_last--) {
			*p_adc++ = get10(h, p10++) ;
			*p_tb++ = tb_last ;
		}
	}
	else {
		p10 += tb_cou ;
	}


	
  }





  if(ret) {
	if(log) LOG(ERR,"RDO %d: token %d: Altro %03d:%02d (?)  bad data [2]",a->rdo+1,a->t,a->id,a->ch) ;
	return 0 ;	// already error...
  }
  else {
	a->count = p_adc - a->adc ;	// put count only if all OK
  }

  l10 /= 2 ;	// how many more words in this event...
			
  h -= l10 ;	// point now to the start of next altro...


  //LOG(DBG,"AID %d:%d, %d ADCs...",a->id,a->ch,a->count) ;
  return h ;	// return pointer to the start of the next altro!
}




static int check_emul(u_int *a)
{
  int i ;
  u_int dta, should ;
  u_int errors ;

  errors = 0 ;
  for(i=0;i<200000;i++) {
    dta = *a++ ;

    should = ((i&0xFFFF)<<16)|(i&0xFFFF) ;

    if(dta != should) {
      errors++ ;
      LOG(ERR,"Mismatch %d: at %d: should 0x%08X, is 0x%08X",errors,i,should,dta) ;
      if(errors > 3) break ;
    }
  }

  if(errors) {
    return -1 ;
  }
	
  LOG(NOTE,"Emul evt checks OK...") ;
  return 0 ;

}

static inline u_int get10(u_int *l, u_int p)
{
  u_int ret ;

  l -= 2*(p/4) ;

  switch(p&3) {
  case 0 :
    ret = (l[-1] & 0xFFC00) >> 10 ;
    break ;
  case 1 :
    ret = (l[-1] & 0x3FF) ;
    break ;
  case 2 :
    ret = (l[0] & 0xFFC00) >> 10 ;
    break ;
  case 3 :
  default: 
    ret = l[0] & 0x3FF ;
    break ;
  }

  //printf("P %d, data 0x%X\n",p,ret) ;

  return ret ;
}


void tpx_analyze_log(int sector,int rdo, char *buff)
{
	int i ;
	char *start_ptr ;
	int len ;


#if 0
	// super-special handling!
	if(strstr(buff,"RDO Xilinx check")) {
		char fname[128] ;

		sprintf(fname,"/RTS/tpx/tpx_log_sec%02d_rb%d.txt",sector,rdo) ;
		FILE *f = fopen(fname,"w") ;

		if(f==0) {
			LOG(ERR,"Can't open TPX check file \"%s\" [%s]",fname,strerror(errno)) ;
		}
		else {
			fprintf(f,"%s",buff) ;
			fclose(f) ;
			LOG(INFO,"Status file \"%s\" created...",fname) ;
		}
	}
#endif

	len = strlen(buff) ;
	start_ptr = buff ;



	while(len) {
		for(i=0;i<len;i++) {
			if(start_ptr[i] == '\n') {
				start_ptr[i] = 0 ;
				if(strlen(start_ptr)) {

					LOG(INFO,"[RB_%d] %s",rdo,start_ptr) ;
				}
				start_ptr = start_ptr + i + 1 ;
				len = strlen(start_ptr) ;
				break ;
			}
		}
	}

	return ;

}

void tpx_analyze_msc(int sector,int rb, char *buff)
{
	struct tpx_rdo *rdo ;

	rdo = (struct tpx_rdo *) buff ;


	memcpy(&(tpx_rdo[rb]),buff,sizeof(struct tpx_rdo)) ;

	LOG(NOTE,"Config for RB %d: sector %d, rdo %d",rb,rdo->sector,rdo->rdo) ;
	LOG(NOTE,"remote %d, temp rdo %d, temp stratix %d",rdo->remote,rdo->temp_rdo,rdo->temp_stratix) ;
//	LOG(NOTE,"altro.c %s, trigger.c %s",rdo->altro_date,rdo->trigger_date) ;

	LOG(NOTE,"\t FPGAs: 0x%08X 0x%08X 0x%08X 0x%08X 0x%08X",
	    rdo->fpga_usercode[0],
	    rdo->fpga_usercode[1],
	    rdo->fpga_usercode[2],
	    rdo->fpga_usercode[3],
	    rdo->fpga_usercode[4]
	    ) ;

	if(rdo->status_xilinx) {
		LOG(ERR,"RDO %d: xilinx status: 0x%02X",rdo->rdo,rdo->status_xilinx) ;
	}
	else {
		LOG(NOTE,"RDO %d: xilinx status: 0x%02X",rdo->rdo,rdo->status_xilinx) ;
	}



	int fcou = 1 ;
	for(int b=0;b<3;b++) {
		for(int c=0;c<12;c++) {

			if(rdo->fee[b][c].fee_status) {
				if(rdo->fee[b][c].fee_status == 1) {
					int expect = ((rdo->fee[b][c].pad_id & 0x7F) << 1) ;

					if((rdo->fee[b][c].jumpers != 3) || (expect != rdo->fee[b][c].id)) {

						LOG(WARN,"RDO %d: %2d: FEE %3d (%3d,%d) [%d:%d:%d] = 0x%X",rdo->rdo,fcou,
						    rdo->fee[b][c].id,
						    rdo->fee[b][c].pad_id,
						    rdo->fee[b][c].jumpers,
						    b,
						    rdo->fee[b][c].x_s>>4,
						    rdo->fee[b][c].x_s&1,
						    rdo->fee[b][c].fee_status
						   ) ;
					}
					else {
						LOG(NOTE,"RDO %d: %2d: FEE %3d (%3d,%d) [%d:%d:%d] = 0x%X",rdo->rdo,fcou,
						    rdo->fee[b][c].id,
						    rdo->fee[b][c].pad_id,
						    rdo->fee[b][c].jumpers,
						    b,
						    rdo->fee[b][c].x_s>>4,
						    rdo->fee[b][c].x_s&1,
						    rdo->fee[b][c].fee_status
						   ) ;



					}
				}
				else {
						LOG(ERR,"RDO %d: %2d: FEE %3d (%3d,%d) [%d:%d:%d] = 0x%X",rdo->rdo,fcou,
						    rdo->fee[b][c].id,
						    rdo->fee[b][c].pad_id,
						    rdo->fee[b][c].jumpers,
						    b,
						    rdo->fee[b][c].x_s>>4,
						    rdo->fee[b][c].x_s&1,
						    rdo->fee[b][c].fee_status
						   ) ;





				}
				fcou++ ;
			}
		}
	}
	

	return ;
}
