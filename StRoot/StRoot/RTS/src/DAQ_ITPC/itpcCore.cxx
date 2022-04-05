#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#include <rtsLog.h>

#define IFEE_NO_LANE_HDRS	1

#include "itpc_maps.h"
#include "itpc_rowlen.h"
#include "itpc_padplane.h"

#include "itpcCore.h"

// Copied over from tpx_altro_to_pad.h 
static const u_char tpx_altro_to_j1[2][16] = {
    {21,22,19,20,17,18,15,16,     8, 7,10, 9,12,11,14,13 },
    {38,37,36,35,34,33,32,31,    23,24,25,26,27,28,29,30 }
} ;


static struct itpc_to_ifee_table_t {
	u_char fee_id ;
	u_char fee_ch ;
} itpc_to_ifee_table[72+1][182+1] ;

static struct itpc_rowpad_to_id_t {
	u_char id ;
	u_char pin ;
} itpc_rowpad_to_id_s[41][121] ;


void itpc_ifee_to_rowpad(int fee_id, int ch, int &row, int &pad)
{
	int pin ;

	row = pad = 0 ;

	if((fee_id<1)||(fee_id>55)) return ;
	if((ch<0)||(ch>63)) return ;

	//first: get from the channel# to the pin of the padplane connector
	if(ch > 31) pin = itpc_sampa_to_pin[1][ch-32] ;
	else pin = itpc_sampa_to_pin[0][ch] ;

	row = itpc_padplane[fee_id][pin].row ;
	pad = itpc_padplane[fee_id][pin].pad ;

	return ;
}

// NOT FINISHED YET
#if 0
void itpc_rowpad_to_rdo(int row, int pad, int &rdo)
{
	int fee_id, fee_ch ;

	itpc_rowpad_to_ifee(row,pad,fee_id,fee_ch) ;

	if(fee_id<1 || fee_id>55) {
		rdo = 0 ;	// what?
	}
	else return ifee_to_irdo_map[fee_id] ;
}
#endif ;

void itpc_rowpad_to_ifee(int row, int pad, int &fee_id, int &fee_ch)
{
	static int first ;

	if(first == 0) {
		first = 1 ;

		for(int f=1;f<=55;f++) {
			for(int c=0;c<64;c++) {
				int row, pad ;

				itpc_ifee_to_rowpad(f,c,row,pad) ;

				itpc_to_ifee_table[row][pad].fee_id = f ;
				itpc_to_ifee_table[row][pad].fee_ch = c ;
			}
		}

	}

	fee_id = 0  ;
	fee_ch = 0 ;

	if(row < 1) return ;
	if(pad < 1) return ;

	fee_id = itpc_to_ifee_table[row][pad].fee_id ;
	fee_ch = itpc_to_ifee_table[row][pad].fee_ch ;

}

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

void itpc_data_c::data_accum(fee_ch_t *fee_p, int tb, int adc)
{
	int fee = fee_p->fee ;
	int ch = fee_p->ch ;

	at[tb_cou].tb = tb ;
	at[tb_cou].adc = adc ;
	tb_cou++ ;

	if(!ped_p || !ped_run) return ;
	
	ped_p->g_mean[fee][ch] += adc ;
	ped_p->g_rms[fee][ch] += adc*adc ;
	ped_p->g_cou[fee][ch]++ ;

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

			int cou = ped_p->g_cou[f][c] ;
			if(cou==0) continue ;

			double mean = ped_p->g_mean[f][c]/cou ;
			double rms = ped_p->g_rms[f][c]/cou ;

			rms = sqrt(rms-mean*mean) ;


			int row, pad ;

			itpc_ifee_to_rowpad(f,c,row,pad) ;

			printf("%2d %2d %2d %3d %f %f\n",f,c,row,pad,mean,rms) ;
		}
	}




	time_t now = time(0) ;
	struct tm *tm = localtime(&now) ;

	char fname[128] ;

	sprintf(fname,"/RTScache/itpc_pedestals_%d_%d_%d_%d_%d.txt",
		tm->tm_year+1900,
		tm->tm_mon+1,
		tm->tm_mday,
		tm->tm_hour,
		tm->tm_min) ;


	FILE *of = fopen(fname,"w") ;
	LOG(TERR,"Writing pedestals to %s",fname) ;


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
int itpc_data_c::start(u_short *d16)
{
	fee_ch_t *fee_p = rdo_p->fee_ch[rdo_p->fee_ch_cou] = (fee_ch_t *)malloc(sizeof(fee_ch_t)) ;
	rdo_p->fee_ch_cou++ ;

	fee_p->port = port_id ;
	fee_p->fee = fee_id ;
	fee_p->ch = fee_ch ;
	fee_p->err = 0 ;

	fee_p->words = words ;


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
			data_accum(fee_p, t, *d16 & 0x3FF) ;
			d16++ ;
		}
			
		tb_stop_last = tb_stop ;
			
	}

	if(fee_p->err) {
		LOG(ERR,"FEE %d:%d -- error 0x%X",fee_p->fee,fee_p->ch,fee_p->err) ;
		return -1 ;
	}

	return 0 ;
}




void itpc_data_c::rdo_zap(void *rdo_p)
{
	if(rdo_p==0) {
		LOG(ERR,"rdo_p NULL") ;
		return ;
	}

	rdo_t *rdo = (rdo_t *)rdo_p ;

	for(int i=0;i<rdo->fee_ch_cou;i++) {
		if(rdo->fee_ch[i]) free(rdo->fee_ch[i]) ;
	}

	free(rdo) ;
}


int itpc_data_c::fee_scan(u_short *d16, int shorts)
{

//	for(int i=0;i<10;i++) {
//		LOG(TERR,"%d = 0x%04X",i,d16[i]) ;
//	}

	if(next_word==0) {	//start of FEE scan!
		fee_err = 0 ;

		//event header check
//		if(d16[0] != 0xFD04) fee_err |= 1 ;	//start of FEE marker
//		if(d16[1] != 0xFF8C || d16[1]!=0xFF16) fee_err |= 1 ;
//		if(d16[2] != 0xFFFF) fee_err |= 1 ;
//		if(d16[3] != 0xFD05) fee_err |= 1 ;	//end of FEE marker


//		for(int i=0;i<4;i++) LOG(TERR,"HDR %d: 0x%04X",i,d16[i]) ;

		sector = 1 ;	//we assume we get it in the data...


		sampa_type = -1 ;

		words = 0 ;

		sampa_id = -1 ;

		sampa_ch = -1 ;

		sampa_bx = -1 ;

//		fee_id = -1 ;

		port_id = d16[0] ;
		fee_id = d16[1] ;	// actually FEE port, from RDO!

		memset(hdr_cou,0,sizeof(hdr_cou)) ;

//		next_word = 4 ;
		next_word = 2 ;
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
		
		LOG(TERR,"Lane %d, hdr %d, d 0x%03X - 0x%04X [%d]",l,h,d,d16[i],i) ;

		hdr_cou[h]++ ;	//MUST come last to avoid the spurious, last 0xFFFF datum
		
		switch(h) {
		case 0 :
			type = d>>7 ;

			if(sampa_type < 0) sampa_type = type ;
			else if(sampa_type != type) {
				fee_err |= 0x2 ;
				LOG(ERR,"Different event type %d, expect %d",type,sampa_type) ;
			}
			
			if(type==0) lane_expect = 2 ;	//hearbeat!

			break ;
		case 1 :
			if(words != 0) {
				fee_err |= 0x4000 ;
				LOG(ERR,"Words are not 0") ;
			}

			words = d ;

			break ;
		case 2 :
			id = d & 0xF ;
			ch = (d>>4)&0x1F ;

			//LOG(TERR,"Id %d, expect %d; ch %d, expect %d",id,sampa_id,ch,sampa_ch) ;

#ifdef IFEE_NO_LANE_HDRS
//			if(sampa_id >= 0) {
//				if(id != sampa_id) fee_err |= 0x4 ;
//			}

//			fee_id = id & 0xFE ;
#else
			if(id != sampa_id) fee_err |= 0x4 ;
#endif

			if(type == 0) {
				if(ch != 21) {
					fee_err |= 0x8 ;
					LOG(ERR,"Wrong channel") ;
				}
			}
			else {
#ifdef IFEE_NO_LANE_HDRS
#else
				if(ch != sampa_ch) {
					LOG(ERR,"SAMPA ch %d, expect %d",ch,sampa_ch) ;
					fee_err |= 0x8 ;
				}
#endif
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
				LOG(ERR,"Different BX") ;
			}	

			switch(type) {
			case 0:	//heartbeat
				if(words!=0 || ch!=21) {
					fee_err |= 0x20 ;
					LOG(ERR,"Wrong words in Hearbeat") ;
				}
				break ;
			case 4 :	//data
				break ;
			default :
				LOG(ERR,"Bad event type %d",type) ;
				fee_err |= 0x40 ;
				break ;
			}

			
			start(d16+i+1) ;	//THIS is where I fill the data!!!

			LOG(INFO,"Starting: FEE %d, SAMPA %d, ch %d, words %d",fee_id,sampa_id,sampa_ch,words) ;

			i += words ;
			next_word = i + 1 ;
			if(fee_err) LOG(ERR,"0x%X",fee_err) ;

			LOG(INFO,"Done: FEE %d, SAMPA %d, ch %d, words %d",fee_id,sampa_id,sampa_ch,words) ;

			//LOG(TERR,"Next word: %d/%d, words %d",next_word,shorts,words) ;
			words = 0 ;
			return 1 ;

			break ;
		default :
		case 5 :	//data
			LOG(ERR,"Can't have data 0x%04X!",d16[i]) ;
			break ;

		case 7 :	// end of lane data
			//fee_id = d & 0x3F ;
			{
				LOG(TERR,"Lane %d: end of lane data 0x%04X, token %d",l,d16[i],d) ;

			}
			break ;
		case 6 :
			{
				int fee = d & 0x3F ;
				int ev = (d >> 6)&3 ;
				int tkn = (d>>8)&3 ;
				LOG(TERR,"Lane %d: start of lane data 0x%X [0x%04X] - fee %d, ev %d, tkn %d",
					l,d,d16[i],
					fee,ev,tkn) ;
			}
			break ;
		}
	}

	stop:; 

	if(words != 0) fee_err |= 0x4000 ;
	
	if(sampa_type == 4) {	//normal

		//there is a bug in this version of the FEE firmware which 
		//eats header 0 for all cases apart from the start of lane
		if(hdr_cou[0] != 64) fee_err |= 0x1000 ;
//		if(hdr_cou[0] != 4) fee_err |= 0x1000 ;

		if(hdr_cou[1] != 64) fee_err |= 0x1000 ;
		if(hdr_cou[2] != 64) fee_err |= 0x1000 ;
		if(hdr_cou[3] != 64) fee_err |= 0x1000 ;
		if(hdr_cou[4] != 64) fee_err |= 0x1000 ;
		if(hdr_cou[5] != 0) fee_err |= 0x2000 ;

#ifdef IFEE_NO_LANE_HDRS
#else
		if(hdr_cou[6] != 8) fee_err |= 0x1000 ;
		if(hdr_cou[7] != 4) fee_err |= 0x1000 ;
#endif

	}
	else if(sampa_type == 0) {
		if(hdr_cou[0] != 2) fee_err |= 0x2000 ;
		if(hdr_cou[1] != 2) fee_err |= 0x2000 ;
		if(hdr_cou[2] != 2) fee_err |= 0x2000 ;
		if(hdr_cou[3] != 2) fee_err |= 0x2000 ;
		if(hdr_cou[4] != 2) fee_err |= 0x2000 ;
		if(hdr_cou[5] != 0) fee_err |= 0x2000 ;
#ifdef IFEE_NO_LANE_HDRS
#else
		if(hdr_cou[6] != 4) fee_err |= 0x2000 ;
		if(hdr_cou[7] != 2) fee_err |= 0x2000 ;	
#endif	
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
