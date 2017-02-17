#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <rtsLog.h>

#include "itpc_maps.h"
#include "itpc_rowlen.h"
#include "itpc_padplane.h"

#include "itpcCore.h"

// Copied over from tpx_altro_to_pad.h 
static const u_char tpx_altro_to_j1[2][16] = {
    {21,22,19,20,17,18,15,16,     8, 7,10, 9,12,11,14,13 },
    {38,37,36,35,34,33,32,31,    23,24,25,26,27,28,29,30 }
} ;


static struct itpc_rowpad_to_id_t {
	u_char id ;
	u_char pin ;
} itpc_rowpad_to_id_s[41][121] ;

void itpc_sampa_to_rowpad(int id, int sampa, int ch, int &row, int &pad)
{
	row = pad = 0 ;

	if((id<1)||(id>55)) return ;
	if((ch<0)||(ch>31)) return ;

	sampa &= 1 ;	
	
	int pin = itpc_sampa_to_pin[sampa][ch] ;

	row = itpc_padplane[id][pin].row ;
	pad = itpc_padplane[id][pin].pad ;

	return ;
}

void itpc_rowpad_to_id(int row, int pad, int &id, int &pin)
{
	static int first ;

	id = pin = 0 ;

	if((row<0)||(row>40)) return ;
	if((pad<1)||(pad>120)) return ;


	if(first==0) {
		for(int id=1;id<=55;id++) {
		for(int pin=1;pin<=78;pin++) {
			int r, p ;

			int sampa = -1 ;
			int ch = -1 ;

			for(int s=0;s<2;s++) {	
				for(int c=0;c<32;c++) {
					if(pin==itpc_sampa_to_pin[s][c]) {
						sampa = s ;
						ch = c ;
						goto found ;
					}
				}
			}

			found:;
			if(sampa<0) {	
				continue ;
			}

			itpc_sampa_to_rowpad(id,sampa,ch,r,p) ;

			itpc_rowpad_to_id_s[r][p].id = id ;
			itpc_rowpad_to_id_s[r][p].pin = pin ;
			

		}
		}


		first = 1 ;
	}

	id = itpc_rowpad_to_id_s[row][pad].id ;
	pin = itpc_rowpad_to_id_s[row][pad].pin ;

}


int itpc_altro_to_ifee(int altro)
{
	int fee = altro/2 ;

	int ifee = 0 ;

	if(fee&(1<<0)) ifee |= (1<<0) ;
	if(fee&(1<<1)) ifee |= (1<<2) ;
	if(fee&(1<<2)) ifee |= (1<<1) ;
	if(fee&(1<<3)) ifee |= (1<<3) ;
	if(fee&(1<<4)) ifee |= (1<<4) ;

	if(fee&(1<<6)) ifee |= (1<<5) ;

	return ifee ;
}

void itpc_altro_to_rowpad(int altro, int ch, int odd, int &row, int &pad)
{

	int j1 ;

	int ifee = itpc_altro_to_ifee(altro) ;

	altro &= 1 ;

	j1 = tpx_altro_to_j1[altro][ch] ;

	int pin = itpc_adapter_jx_to_pin[odd][j1] ;


	row = itpc_padplane[ifee][pin].row ;
	pad = itpc_padplane[ifee][pin].pad ;
}


itpc_ped_t *itpc_data_c::ped_p = 0 ;
int itpc_data_c::ped_run = 0 ;

void itpc_data_c::data_accum(int fee, int ch, int tb, int adc)
{
	at[tb_cou].tb = tb ;
	at[tb_cou].adc = adc ;
	tb_cou++ ;

	if(!ped_p || !ped_run) return ;
	
	ped_p->mean[fee][ch][tb] += adc ;
	ped_p->rms[fee][ch][tb] += adc*adc ;
	ped_p->cou[fee][ch][tb]++ ;
	
}

void itpc_data_c::ped_start()
{
	if(ped_p==0) {
		ped_p = (itpc_ped_t *)malloc(sizeof(itpc_ped_t)) ;
	}

	memset(ped_p,0,sizeof(itpc_ped_t)) ;
}

void itpc_data_c::ped_stop()
{
	if(ped_p==0) return ;
	if(!ped_run) return ;

	for(int f=0;f<64;f++) {
		for(int c=0;c<64;c++) {
			int any_cou = 0 ;
			for(int t=0;t<512;t++) {
				int cou = ped_p->cou[f][c][t] ;
				
				if(cou==0) continue ;
				any_cou++ ;

				double mean = ped_p->mean[f][c][t] / cou ;
				double rms = ped_p->rms[f][c][t] / cou ;

				rms = sqrt(rms-mean*mean) ;

				ped_p->mean[f][c][t] = mean ;
				ped_p->rms[f][c][t] = rms ;
			}
			ped_p->cou[f][c][0] = any_cou ; //marker for this channel!
		}
	}


	LOG(TERR,"Writing out pedestals") ;
	FILE *of = fopen("/RTScache/pedestals.txt","w") ;
	for(int f=0;f<64;f++) {
	for(int c=0;c<64;c++) {
		if(ped_p->cou[f][c][0]) ;
		else continue ;

		for(int t=0;t<512;t++) {
			fprintf(of,"%d %d %d %.3f %.3f\n",f,c,t,ped_p->mean[f][c][t],ped_p->rms[f][c][t]) ;
		}
	}}
	fclose(of) ;

}

//start of channel data!
void itpc_data_c::start(u_short *d16)
{
	fee_ch_t *fee_p = rdo_p->fee_ch[rdo_p->fee_ch_cou] = (fee_ch_t *)malloc(sizeof(fee_ch_t)) ;
	rdo_p->fee_ch_cou++ ;

	fee_p->fee = fee_id ;
	fee_p->ch = fee_ch ;
	fee_p->words = words ;
	fee_p->err = 0 ;

	u_short *d16_stop = d16 + words ;
		
	int tb_stop_last = -1 ;

//	for(int i=0;i<words;i++) {
//		LOG(TERR,"%d: 0x%X [%d dec]",i,d16[i]&0x3FF,d16[i]&0x3FF) ;
//	}


	tb_cou = 0 ;

	while(d16<d16_stop) {
		
		int tb_cou = *d16++ & 0x3FF ;
		int tb_start = *d16++ & 0x3FF ;

		int tb_stop = tb_start + tb_cou - 1 ;

//		LOG(TERR,"%d:%d - tb_start %d, tb_stop %d, tb_stop_last %d",fee_p->fee,fee_p->ch,
//		    tb_start,tb_stop,tb_stop_last) ;

		if(tb_start <= tb_stop_last) {
			fee_p->err |= 1 ;
			break ;
		}
		if(tb_stop > 512) {	//NOTE: hardcoded max tb!
			fee_p->err |= 2 ;
			break ;
		}

		//do something with the data at timebin "t"
		for(int t=tb_start;t<=tb_stop;t++) {
			data_accum(fee_p->fee, fee_p->ch, t, *d16 & 0x3FF) ;
			d16++ ;
		}
			
		tb_stop_last = tb_stop ;
			
	}

	if(fee_p->err) {
		LOG(ERR,"FEE %d:%d -- error 0x%X",fee_p->fee,fee_p->ch,fee_p->err) ;
	}

//	return fee_p->err ;
}




void itpc_data_c::rdo_zap(void *rdo_p)
{
	rdo_t *rdo = (rdo_t *)rdo_p ;

	for(int i=0;i<rdo->fee_ch_cou;i++) {
		free(rdo->fee_ch[i]) ;
	}

	free(rdo) ;
}


int itpc_data_c::fee_scan(u_short *d16, int shorts)
{


	if(next_word==0) {	//start of scan!
		//event header check
		if(d16[0] != 0xFD04) fee_err |= 1 ;	//start of FEE marker
//		if(d16[1]!=0xFF8C || d16[1]!=0xFF16) fee_err |= 1 ;
		if(d16[2] != 0xFFFF) fee_err |= 1 ;
		if(d16[3] != 0xFD05) fee_err |= 1 ;	//end of FEE marker


//		for(int i=0;i<4;i++) LOG(TERR,"HDR %d: 0x%04X",i,d16[i]) ;

		sector = 1 ;	//we assume we get it in the data...

		fee_err = 0 ;

		sampa_type = -1 ;

		words = 0 ;

		sampa_id = -1 ;

		sampa_ch = -1 ;

		sampa_bx = -1 ;

		fee_id = -1 ;

		memset(hdr_cou,0,sizeof(hdr_cou)) ;

		next_word = 4 ;
	}

	sampa_ch++ ;

	int type = sampa_type ;
	int id = -1 ;
	int ch = -1 ;
	int bx = -1 ;

	int lane_expect = 0 ;

	int fee = -1 ;

	//LOG(TERR,"next_word %d/%d",next_word,shorts) ;
	
	for(int i=next_word;i<shorts;i++) {
		int l = d16[i] >> 13 ;
		int h = (d16[i] >> 10) & 0x7 ;
		int d = d16[i] & 0x3FF ;
		
		//LOG(TERR,"Lane %d, hdr %d, d 0x%03X - 0x%04X",l,h,d,d16[i]) ;

		hdr_cou[h]++ ;	//MUST come last to avoid the spurious, last 0xFFFF datum
		
		switch(h) {
		case 0 :
			type = d>>7 ;

			if(sampa_type < 0) sampa_type = type ;
			else if(sampa_type != type) fee_err |= 0x2 ;

			
			if(type==0) lane_expect = 2 ;	//hearbeat!

			break ;
		case 1 :
			if(words != 0) {
				fee_err |= 0x4000 ;
			}

			words = d ;

			break ;
		case 2 :
			id = d & 0xF ;
			ch = (d>>4)&0x1F ;

			//LOG(TERR,"Id %d, expect %d; ch %d, expect %d",id,sampa_id,ch,sampa_ch) ;

			if(id != sampa_id) fee_err |= 0x4 ;

			if(type == 0) {
				if(ch != 21) fee_err |= 0x8 ;
			}
			else {
				if(ch != sampa_ch) {
					LOG(ERR,"SAMPA ch %d, expect %d",ch,sampa_ch) ;
					fee_err |= 0x8 ;
				}
			}

			sampa_id = id ;
			sampa_ch = ch  ;

			if(id & 1) fee_ch = 32 + ch ;
			else fee_ch = ch ;

			break ;
		case 3 :
			bx = d ;

			break ;
		case 4 :
			bx = (bx<<10)|(d&0x1FF) ;

			if(sampa_bx < 0) sampa_bx = bx ;
			else if(bx != sampa_bx) {
				fee_err |= 0x10 ;
			}	

			switch(type) {
			case 0:	//heartbeat
				if(words!=0 || ch!=21) fee_err |= 0x20 ;
				break ;
			case 4 :	//data
				break ;
			default :
				LOG(ERR,"type %d",type) ;
				fee_err |= 0x40 ;
				break ;
			}

			
			start(d16+i+1) ;
			i += words ;
			next_word = i + 1 ;
			if(fee_err) LOG(ERR,"0x%X",fee_err) ;
			//LOG(INFO,"Done: FEE %d, SAMPA %d, ch %d",fee_id,sampa_id,sampa_ch) ;
			//LOG(TERR,"Next word: %d/%d, words %d",next_word,shorts,words) ;
			words = 0 ;
			return 1 ;

			break ;
		case 5 :	//data
			LOG(ERR,"Can't have data!") ;
			break ;
		case 7 :	//lane header 1st word
			//LOG(TERR,"lane %d, lane_expect %d",l,lane_expect) ;

			if(d16[i]==0xFD03) {
				if(i==(shorts-1) || i==(shorts-2)) {
					hdr_cou[7]-- ;
					goto stop ;
				}
			}
//			if(d16[i]==0xFFFF && (i==(shorts-1))) {
//				hdr_cou[7]-- ;	// negate the count
//				goto stop;
//			}

			lane_expect = l+1 ;	//next lane

			fee = d & 0x3F ;

			if(fee_id < 0) fee_id = fee ;
			else if(fee != fee_id) fee_err |= 0x100 ;

			switch(l) {
			case 0 :	
				sampa_ch = 0 ;
				sampa_id = (fee << 1)|0 ;
				break ;
			case 1 :
				sampa_ch = 16 ;
				sampa_id = (fee << 1)|0 ;
				break ;
			case 2 :
				sampa_ch = 0 ;
				sampa_id = (fee << 1)|1 ;
				break ;
			case 3 :
				sampa_ch = 16 ;
				sampa_id = (fee << 1)|1 ;
				break ;
			}

			sampa_id &= 0xF ;

			break ;
		case 6 :	//lane header 2nd & 3rd word
			break ;
		}



	}

	stop:; 

	if(words != 0) fee_err |= 0x4000 ;
	
	if(sampa_type == 4) {	//normal

		//there is a bug in this version of the FEE firmware which 
		//eats header 0 for all cases apart from the start of lane
		//if(hdr_cou[0] != 64) fee_err |= 0x1000 ;
		if(hdr_cou[0] != 4) fee_err |= 0x1000 ;

		if(hdr_cou[1] != 64) fee_err |= 0x1000 ;
		if(hdr_cou[2] != 64) fee_err |= 0x1000 ;
		if(hdr_cou[3] != 64) fee_err |= 0x1000 ;
		if(hdr_cou[4] != 64) fee_err |= 0x1000 ;
		if(hdr_cou[5] != 0) fee_err |= 0x2000 ;
		if(hdr_cou[6] != 8) fee_err |= 0x1000 ;
		if(hdr_cou[7] != 4) fee_err |= 0x1000 ;

	}
	else if(sampa_type == 0) {
		if(hdr_cou[0] != 2) fee_err |= 0x2000 ;
		if(hdr_cou[1] != 2) fee_err |= 0x2000 ;
		if(hdr_cou[2] != 2) fee_err |= 0x2000 ;
		if(hdr_cou[3] != 2) fee_err |= 0x2000 ;
		if(hdr_cou[4] != 2) fee_err |= 0x2000 ;
		if(hdr_cou[5] != 0) fee_err |= 0x2000 ;

		if(hdr_cou[6] != 4) fee_err |= 0x2000 ;
		if(hdr_cou[7] != 2) fee_err |= 0x2000 ;		
	}

	if(fee_err) {
		LOG(ERR,"Error 0x%08X",fee_err) ;
		if(fee_err & 0x3000) {
			for(int i=0;i<8;i++) {
				LOG(TERR,"Hdr %d: %d",i,hdr_cou[i]) ;
			}
		}
	}

	return 0 ;
//	return err ;


}

#if 0
int itpc_data_c::fee_sanity(u_short *d16, int shorts)
{
	int err = 0 ;


	//event header check
	if(d16[0] != 0xFD04) err |= 1 ;	//start of FEE marker
//	if(d16[1]!=0xFF8C || d16[1]!=0xFF16) err |= 1 ;
	if(d16[2] != 0xFFFF) err |= 1 ;
	if(d16[3] != 0xFD05) err |= 1 ;	//end of FEE marker


	for(int i=0;i<4;i++) LOG(DBG,"HDR %d: 0x%04X",i,d16[i]) ;

	int type = -1 ;
	sampa_type = -1 ;

	words = 0 ;

	int id = -1 ;
	sampa_id = -1 ;

	int ch = -1 ;
	sampa_ch = -1 ;

	int bx = -1 ;
	sampa_bx = -1 ;


	int lane_expect = 0 ;

	int fee = -1 ;
	fee_id = -1 ;


	memset(hdr_cou,0,sizeof(hdr_cou)) ;


	for(int i=4;i<shorts;i++) {
		int l = d16[i] >> 13 ;
		int h = (d16[i] >> 10) & 0x7 ;
		int d = d16[i] & 0x3FF ;
		
		LOG(DBG,"Lane %d, hdr %d, d 0x%03X - 0x%04X",l,h,d,d16[i]) ;

		

		switch(h) {
		case 0 :
			type = d>>7 ;

			if(sampa_type < 0) sampa_type = type ;
			else if(sampa_type != type) err |= 0x2 ;

			
			if(type==0) lane_expect = 2 ;	//hearbeat!

			break ;
		case 1 :
			if(words != 0) {
				err |= 0x4000 ;
			}

			words = d ;

			break ;
		case 2 :
			id = d & 0xF ;
			ch = (d>>4)&0x1F ;

			LOG(DBG,"Id %d, expect %d; ch %d, expect %d",id,sampa_id,ch,sampa_ch) ;

			if(id != sampa_id) err |= 0x4 ;

			if(type == 0) {
				if(ch != 21) err |= 0x8 ;
			}
			else {
				if(ch != sampa_ch) err |= 0x8 ;
			}

			sampa_ch = ch  ;

			break ;
		case 3 :
			bx = d ;

			break ;
		case 4 :
			bx = (bx<<10)|(d&0x1FF) ;

			if(sampa_bx < 0) sampa_bx = bx ;
			else if(bx != sampa_bx) {
				err |= 0x10 ;
			}	

			switch(type) {
			case 0:	//heartbeat
				if(words!=0 || ch!=21) err |= 0x20 ;
				break ;
			case 4 :	//data
				break ;
			default :
				err |= 0x40 ;
				break ;
			}

			start(d16+i+1) ;
			i += words ;
			words = 0 ;

			sampa_ch = ch + 1;

			break ;
		case 5 :	//data
			LOG(ERR,"Can't have data!") ;
			break ;
		case 7 :	//lane header 1st word
			LOG(DBG,"lane %d, lane_expect %d",l,lane_expect) ;

			if(d16[i]==0xFFFF && (i==(shorts-1))) goto stop;

			lane_expect = l+1 ;	//next lane

			fee = d & 0x3F ;

			if(fee_id < 0) fee_id = fee ;
			else if(fee != fee_id) err |= 0x100 ;

			switch(l) {
			case 0 :	
				sampa_ch = 0 ;
				sampa_id = (fee << 1)|0 ;
				break ;
			case 1 :
				sampa_ch = 16 ;
				sampa_id = (fee << 1)|0 ;
				break ;
			case 2 :
				sampa_ch = 0 ;
				sampa_id = (fee << 1)|1 ;
				break ;
			case 3 :
				sampa_ch = 16 ;
				sampa_id = (fee << 1)|1 ;
				break ;
			}

			sampa_id &= 0xF ;

			break ;
		case 6 :	//lane header 2nd & 3rd word
			break ;
		}

		hdr_cou[h]++ ;
	}

	stop:; 

	if(words != 0) err |= 0x4000 ;
	
	for(int i=0;i<8;i++) {
		LOG(DBG,"hdr %d = %d",i,hdr_cou[i]) ;
	}

	if(sampa_type == 4) {	//normal

		//there is a bug in this version of the FEE firmware which 
		//eats header 0 for all cases apart from the start of lane
		//if(hdr_cou[0] != 64) err |= 0x1000 ;
		if(hdr_cou[0] != 4) err |= 0x1000 ;

		if(hdr_cou[1] != 64) err |= 0x1000 ;
		if(hdr_cou[2] != 64) err |= 0x1000 ;
		if(hdr_cou[3] != 64) err |= 0x1000 ;
		if(hdr_cou[4] != 64) err |= 0x1000 ;
		if(hdr_cou[5] != 0) err |= 0x2000 ;
		if(hdr_cou[6] != 8) err |= 0x1000 ;
		if(hdr_cou[7] != 4) err |= 0x1000 ;

	}
	else if(sampa_type == 0) {
		if(hdr_cou[0] != 2) err |= 0x2000 ;
		if(hdr_cou[1] != 2) err |= 0x2000 ;
		if(hdr_cou[2] != 2) err |= 0x2000 ;
		if(hdr_cou[3] != 2) err |= 0x2000 ;
		if(hdr_cou[4] != 2) err |= 0x2000 ;
		if(hdr_cou[5] != 0) err |= 0x2000 ;

		if(hdr_cou[6] != 4) err |= 0x2000 ;
		if(hdr_cou[7] != 2) err |= 0x2000 ;		
	}


	if(err) {
		LOG(ERR,"Error 0x%08X",err) ;
	}
	return err ;

}
#endif



#ifdef MAIN
int main()
{
	for(int id=1;id<=55;id++) {
	for(int sampa=0;sampa<2;sampa++) {
	for(int ch=0;ch<32;ch++) {
		int row, pad ;
		int iid,pin ;

		itpc_sampa_to_rowpad(id,sampa,ch,row,pad) ;

		itpc_rowpad_to_id(row,pad,iid,pin) ;


		printf("Id %2d[==%2d], SAMPA %d, Ch %2d == row %2d, pad %3d, pin %2d\n",id,iid,sampa,ch,row,pad,pin) ;

	}}}




	int altro = 160 ;
	int adapter = 0 ;

	printf("ALTRO %d to iFEE %d %d\n",altro,itpc_altro_to_ifee(altro),itpc_altro_to_ifee(altro|1)) ;

	for(int i=0;i<16;i++) {
		int row, pad ;

		itpc_altro_to_rowpad(altro,i,adapter,row,pad) ;

		printf("Altro %d, adapter %d: channel %d: row %d, pad %d\n",altro,adapter,i,row,pad) ;
	}
	for(int i=0;i<16;i++) {
		int row, pad ;

		itpc_altro_to_rowpad(altro|1,i,adapter,row,pad) ;

		printf("Altro %d, adapter %d: channel %d: row %d, pad %d\n",altro|1,adapter,i,row,pad) ;

	}

	adapter = 1 ;

	for(int i=0;i<16;i++) {
		int row, pad ;

		itpc_altro_to_rowpad(altro,i,adapter,row,pad) ;

		printf("Altro %d, adapter %d: channel %d: row %d, pad %d\n",altro,adapter,i,row,pad) ;
	}
	for(int i=0;i<16;i++) {
		int row, pad ;

		itpc_altro_to_rowpad(altro|1,i,adapter,row,pad) ;

		printf("Altro %d, adapter %d: channel %d: row %d, pad %d\n",altro|1,adapter,i,row,pad) ;

	}


	return 0 ;
}

#endif
