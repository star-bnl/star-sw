/*
  origin online/RTS/src/ITPC_SUPPORT/itpc_pedestals.C
  cd ~/work/Tpc/ped/itpc_20106028
  root.exe lRTS.C 'itpc_pedestals.C+("pedestals_s01_20106028.txt")'
 */
#include <stdio.h>
#include <bits/types.h>
#include <sys/types.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
typedef unsigned short u_short;
typedef unsigned char u_char;
typedef unsigned int u_int;

#include <rtsLog.h>

#include "RTS/src/DAQ_ITPC/itpcCore.h"
#include "RTS/src/DAQ_ITPC/itpcPed.h"
#include "RTS/src/DAQ_ITPC/itpcFCF.h"
//static int bad[25][40][183] ;

//static int bad_delta[25][7] ;

static itpc_fcf_c fcf ;
#include "TString.h"
#include "TFile.h"
#include "TNtuple.h"
struct BPoint_t {
  Float_t sec, row, pad, ped, rms, x;
};
BPoint_t P;
//int itpc_pedestals(const char *pattern = "pedestals_s%02i_20106028.txt")
int itpc_pedestals(const char *pattern = "pedestals_s%02i_23074031.txt")
{

  Int_t NinnerRows = 40;
  Int_t nPadsInner[40] = { //J.Thomas, 05/31/2016
    52, 54, 56, 58, 60, 62, 62, 64, 66, 68,
    70, 72, 74, 74, 76, 78, 80, 82, 84, 86,
    86, 88, 90, 92, 94, 96, 98, 98,100,102,
   104,106,108,110,110,112,114,116,118,120};
 TString fName(pattern);
  fName.ReplaceAll("%02i","");
  fName.ReplaceAll(".txt",".root");
  TFile *fOut = new TFile(fName,"recreate");
  TNtuple *FitP = new TNtuple("FitP","Pedestals","sec:row:pad:ped:rms:x");
	rtsLogOutput(RTS_LOG_STDERR) ;

	itpcPed ped_c ;
	
	fcf.init(0,"/RTS/conf/itpc/itpc_gains.txt") ;
#if 0
	for(int r=1;r<=40;r++) {	
	for(int p=1;p<=120;p++) {
		if(fcf.get_bad(20,r,p)) {
			printf("%d %d BAD\n",r,p) ;
		}
	}
	}
#endif
	//init ALL: counts from 1!
	for(int s=1;s<=24;s++) {
	for(int r=1;r<=4;r++) {
		ped_c.init(s,r,0xFFFF) ;
	}
	}

	ped_c.clear() ;

	for(int s=1;s<=24;s++) {
		char fname[128] ;
#if 0

		if(argc==2) {
			strcpy(fname,argv[1]) ;
		}
		else {
			sprintf(fname,"/net/itpc%02d/RTScache/itpc_pedestals_s%02d_r%d.txt",s,s,r) ;
		}
#else
		sprintf(fname,pattern,s);
		printf("Open %s\n",fname);
#endif
		if(ped_c.from_cache(fname,-1,-1)>=0) {
			LOG(INFO,"Success for sector %d, file \"%s\"",s,fname) ;
		}
	}

	// now we count from 0!
	for(int s=0;s<24;s++) {
	for(int r=0;r<4;r++) {
	for(int p=0;p<16;p++) {
	for(int c=0;c<64;c++) {
		itpcPed::ped_t *pt = ped_c.ped_p[s][r][p][c] ;

		int bad = 0 ;

		// I need FEE ID
		int row, pad, fee_id ;

		fee_id = ped_c.padplane_id[s][r][p] ;

		itpc_ifee_to_rowpad(fee_id,c,row,pad) ;

		int gain_bad = fcf.get_bad(s,row,pad) ;

		//if row==0 but pad is not -- channel exists but is not connected to the padplane
		//if row==0 AND pad==0 -- no FEE present

		if((row==0)&&(pad==0)) continue	; // no FEE present

		gain_bad = fcf.get_bad(s+1,row,pad) ;

		//LOG(TERR,"%d %d %d %d = %f %f",s,r,p,c,pt->c_ped,pt->c_rms) ;

		//LOG(TERR,"%d %d %d %d: %d %d",s,r,p,c,row,pad) ;

		int lo_rms = 0 ;
		int hi_rms = 0 ;
		int lo_ped = 0 ;
		int hi_ped = 0 ;

		for(int t=0;t<512;t++) {
			float ped = ped_c.ped_p[s][r][p][c]->ped[t] ;
			float rms = ped_c.ped_p[s][r][p][c]->rms[t] ;

			//LOG(TERR,"%d %f %f",t,ped,rms) ;

			if(rms>1021.9) continue ;

			// need to skip the GG
			if((t>=26)&&(t<=76)) continue ;

			if(rms < 0.4) {
				lo_rms++ ;
			}
			else if(rms > 2.0) {
				hi_rms++ ;
			}

			if(ped<20) {
				lo_ped++ ;
			}
			else if(ped>150) {
				hi_ped++ ;
			}
		}

		if(row==0) {
			//LOG(ERR,"ROW0 %d %d %d %d (rp %d:%d) = %f %f: %d %d",s+1,r+1,p+1,c,row,pad,pt->c_ped,pt->c_rms,lo_rms,hi_rms) ;
		}

		// I need to put some higher counts here because of the gating grid
		if((lo_rms>10) || (hi_rms>30)) {
			bad |= 1 ;
			//LOG(ERR,"RMS %d %d %d %d (rp %d:%d) = %f %f: %d %d",s+1,r+1,p+1,c,row,pad,pt->c_ped,pt->c_rms,lo_rms,hi_rms) ;
		}

		if((lo_ped>10) || (hi_ped>10)) {
			bad |= 2 ;
			//LOG(ERR,"PED %d %d %d %d (rp %d:%d) = %f %f: %d %d",s+1,r+1,p+1,c,row,pad,pt->c_ped,pt->c_rms,lo_ped,hi_ped) ;
		}


		if(row==0) {
			bad |= 4 ;
			continue ;
			//LOG(ERR,"ROW0 %d %d %d %d (rp %d:%d) = %f %f: %d %d",s+1,r+1,p+1,c,row,pad,pt->c_ped,pt->c_rms,lo_rms,hi_rms) ;
		}

		//BY HAND!!!
//		if(((s+1)==20)&&((r+1)==2)&&((p+1)==2)&&(c==28)) bad |= 8 ;

		if(bad) {
			//printf("%d %d %d %d 0.0 0.0 # pedestal %.1f %.1f %d %d %d %d %d\n",s+1,r+1,p+1,c,
			//       pt->c_ped,pt->c_rms,lo_rms,hi_rms,lo_ped,hi_ped,row==0?1:0) ;
		//s 24 r 4 p 13 c 51 row 31 pad 91 ped 0.0 rms 0.0 # pedestal 100.6 0.8 0 0 0 0 0
			printf("%d %d %d %d %d %d 0.0 0.0 # pedestal %.2f %.2f %d %d %d %d %d\n",s+1,r+1,p+1,c,row,pad,
			       pt->c_ped,pt->c_rms,lo_rms,hi_rms,lo_ped,hi_ped,gain_bad) ;
		}
		P.sec = s+1;
		P.row = row;
		P.pad = pad;
		P.ped = pt->c_ped;
		P.rms = pt->c_rms;
		P.x   = -1;
		if (row > 0 && row <= 40 && pad > 0 && pad < nPadsInner[row-1]) 
		  P.x   = P.pad/nPadsInner[row-1];
		FitP->Fill(&P.sec);
	}}}}

	fOut->Write();
	return 0 ;
}
