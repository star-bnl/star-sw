#include <stdio.h>
#include <sys/types.h>

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
