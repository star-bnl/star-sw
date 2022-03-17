/*
  origin online/RTS/src/TPX_SUPPORT/tpx_pedestals.C
  cd ~/work/Tpc/ped/tpx_23074031
  root.exe lRTS.C tpx_pedestals.C+
 */
#include <stdio.h>
#include <sys/types.h>
#include <math.h>
#include <string.h>
typedef unsigned short u_short;
typedef unsigned char u_char;
typedef unsigned int u_int;

#include <DAQ_TPX/tpxCore.h>

static int bad_ped[25][46][183] = {0};
static int bad_rms[25][46][183] = {0};

static int bad_delta[25][7] = {0};


static int pr_all = 0 ;

#include "TString.h"
#include "TFile.h"
#include "TNtuple.h"
struct BPoint_t {
  Float_t sec, row, pad, tb, ped, rms, x;
};
BPoint_t P;
int tpx_pedestals()
{
  Int_t nPadsOuter[32]  = {    98 ,    100,    102,    104,    106,    106,    108,    110,    112,    112,
			       114,    116,    118,    120,    122,    122,    124,    126,    128,    128,
			       130,    132,    134,    136,    138,    138,    140,    142,    144,    144,    
			       144,    144};
  TFile *fOut = new TFile("tpc_pedestals.root","recreate");
  TNtuple *FitP = new TNtuple("FitP","Pedestals","sec:row:pad:tb:ped:rms:x");

  //for(int n=1;n<=36;n++) {
for(int s=1;s<=24;s++) {
for(int r=3;r<=6;r++) {


	char fname[64] ;

	//	sprintf(fname,"/net/tpx%02d/RTScache/pedestals_s%02d_r%d.txt",n,s,r) ;
	//	sprintf(fname,"./pedestals_s%02d_r%d.txt",n,s,r) ;
	sprintf(fname,"./pedestals_s%02d_r%d.txt",s,r) ;

	FILE *f = fopen(fname,"r") ;

	if(f==0) {
		//perror(fname) ;
		continue ;
	}

	printf("*****  sector %2d, rdo %d\n",s,r) ;

	int old_row, old_pad ;
	float ped_start = 0.0 ;

	old_row = old_pad = -1 ;
	
	while(!feof(f)) {
		int row, pad, tb ;
		float ped, rms ;
		char buff[128] ;
		int ok ;

		if(fgets(buff,sizeof(buff),f)==0) continue ;

		if(buff[0]=='#' || buff[0]=='/') continue ;

		int ret =sscanf(buff,"%d %d %d %f %f",&row,&pad,&tb,&ped,&rms) ;
		
		if(ret != 5) continue ;

		if(ped > 1022.9) continue ;

		if((row != old_row) || (pad != old_pad)) {
			ped_start = ped ;
		}

		old_row = row ;
		old_pad = pad ;

		// skip over the GG pickup; especially for the new GG
		if((tb>=19)&&(tb<=25)) continue ;
		//if((tb>=420)&&(tb<=425)) continue ;
		if((tb>=400)) continue ;

		ok = 1 ;
		if(ped<10) {
			if(pr_all) printf("SMALL PED sec %d: row %d, pad %d, tb %d, ped %f, rms %f\n",s,row,pad,tb,ped,rms) ;
			ok = 0 ;
		}
		else if(ped>100) {
			if(pr_all) printf("LARGE PED sec %d: row %d, pad %d, tb %d, ped %f, rms %f\n",s,row,pad,tb,ped,rms) ;
			ok = 0 ;
		}

		if(!ok) bad_ped[s][row][pad]++ ;

		ok = 1 ;
		if(rms > 1.5) {
			if(pr_all) printf("LARGE RMS sec %d: row %d, pad %d, tb %d, ped %f, rms %f\n",s,row,pad,tb,ped,rms) ;
			ok = 0 ;
			
		}
		else if(rms < 0.1) {
			if(pr_all) printf("SMALL RMS %d: %d %d %d %f %f\n",s,row,pad,tb,ped,rms) ;
			ok = 0 ;
		}


		if(!ok) bad_rms[s][row][pad]++ ;



		double delta_ped = fabs(ped - ped_start) ;

		if(delta_ped>20.0 && tb<30 && tb>18) {	//this is quuestionable
			bad_delta[s][r]++ ;
			if(pr_all) printf("DELTA PED sector %d:%d = %f: %d %d %d %f %f\n",s,r,delta_ped,row,pad,tb,ped,rms) ;
		}
		if (bad_rms[s][row][pad ] || bad_ped[s][row][pad]) continue;

		P.sec = s;
		P.row = row;
		P.pad = pad;
		P.tb  = tb;
		P.ped = ped;
		P.rms = rms;
		P.x   = -1;
		if (row > 13 && row <= 45 && pad > 0 && pad < nPadsOuter[row-14]) 
		  P.x   = P.pad/nPadsOuter[row-14];
		FitP->Fill(&P.sec);
	}

	fclose(f) ;
}
}
//}

	for(int s=1;s<=24;s++) {
		bad_delta[s][1] += bad_delta[s][2] ;
		bad_delta[s][3] += bad_delta[s][4] + bad_delta[s][5] + bad_delta[s][6] ;
		

		printf("BAD DELTA: Sector %d : %d inner, %d outer\n",s,bad_delta[s][1],bad_delta[s][3]) ;
	}

	for(int s=1;s<=24;s++) {
	for(int row=1;row<=45;row++) {
	for(int pad=1;pad<=182;pad++) {
		char ctmp[32] ;
		ctmp[0] = 0 ;

		int ok = 1 ;
		if(bad_ped[s][row][pad]>2) {
			ok = 0 ;
			strcat(ctmp,"+ped") ;
		}
		if(bad_rms[s][row][pad]>2) {
			ok = 0 ;
			strcat(ctmp,"+rms") ;
		}
		
		if(!ok) {
			// get rdo, ALTRO, ch
			int rdo, a, ch ;
			tpx_to_altro(row,pad,rdo,a,ch) ;
			int fee = tpx_altro_to_fee(rdo,a) ;

			//printf("BAD: S%02d:%d, row %2d, pad %3d; ALTRO %3d:%02d == %d\n",s,rdo,row,pad,a,ch,bad[s][row][pad]) ;

			printf("%2d %2d %2d %3d 0.000 0.000       # bad %s: S%d:%d, FEE %3d, AID %3d:%02d [%d,%d]\n",
			       s,rdo,row,pad,ctmp,s,rdo,fee,a,ch,bad_ped[s][row][pad],bad_rms[s][row][pad]) ;
		}
	}
	}
	}
        fOut->Write();
	return 0 ;
}
