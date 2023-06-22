#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg_base.h"
#include "fcs_ecal_epd_mask.h"

// Modifed from stage_2_JP5_202206.cxx

namespace {
  //version2 with top2 & bottom2 rows in trigger, missing far side column
  static const int EtoHmap[15][9][2] = {
    { { 0, 0},{ 0, 1},{ 0, 1},{ 0, 2},{ 0, 2},{ 0, 3},{ 0, 4},{ 0, 4},{ 0, 4}},
    { { 0, 0},{ 0, 1},{ 0, 1},{ 0, 2},{ 0, 2},{ 0, 3},{ 0, 4},{ 0, 4},{ 0, 4}},
    { { 1, 0},{ 1, 1},{ 1, 1},{ 1, 2},{ 1, 2},{ 1, 3},{ 1, 4},{ 1, 4},{ 1, 4}},
    { { 2, 0},{ 2, 1},{ 2, 1},{ 2, 2},{ 2, 2},{ 2, 3},{ 2, 4},{ 2, 4},{ 2, 4}},
    { { 2, 0},{ 2, 1},{ 2, 1},{ 2, 2},{ 2, 2},{ 2, 3},{ 2, 4},{ 2, 4},{ 2, 4}},
    { { 3, 0},{ 3, 1},{ 3, 1},{ 3, 2},{ 3, 2},{ 3, 3},{ 3, 4},{ 3, 4},{ 3, 4}},
    { { 3, 0},{ 3, 1},{ 3, 1},{ 3, 2},{ 3, 2},{ 3, 3},{ 3, 4},{ 3, 4},{ 3, 4}},
    { { 4, 0},{ 4, 1},{ 4, 1},{ 4, 2},{ 4, 2},{ 4, 3},{ 4, 4},{ 4, 4},{ 4, 4}},
    { { 5, 0},{ 5, 1},{ 5, 1},{ 5, 2},{ 5, 2},{ 5, 3},{ 5, 4},{ 5, 4},{ 5, 4}},
    { { 5, 0},{ 5, 1},{ 5, 1},{ 5, 2},{ 5, 2},{ 5, 3},{ 5, 4},{ 5, 4},{ 5, 4}},
    { { 6, 0},{ 6, 1},{ 6, 1},{ 6, 2},{ 6, 2},{ 6, 3},{ 6, 4},{ 6, 4},{ 6, 4}},
    { { 6, 0},{ 6, 1},{ 6, 1},{ 6, 2},{ 6, 2},{ 6, 3},{ 6, 4},{ 6, 4},{ 6, 4}},
    { { 7, 0},{ 7, 1},{ 7, 1},{ 7, 2},{ 7, 2},{ 7, 3},{ 7, 4},{ 7, 4},{ 7, 4}},
    { { 8, 0},{ 8, 1},{ 8, 1},{ 8, 2},{ 8, 2},{ 8, 3},{ 8, 4},{ 8, 4},{ 8, 4}},
    { { 8, 0},{ 8, 1},{ 8, 1},{ 8, 2},{ 8, 2},{ 8, 3},{ 8, 4},{ 8, 4},{ 8, 4}}
  } ;
  
  static const int EtoH3map[15][9][4] = {
    {{-1,-1,-1, 0},{-1,-1, 0, 1},{-1,-1, 1, 2},{-1,-1, 1, 2},{-1,-1, 2, 3},{-1,-1, 2, 3},{-1,-1, 3, 4},{-1,-1, 4,-1},{-1,-1, 4,-1}},
    {{-1, 0,-1, 5},{ 0, 1, 5, 6},{ 1, 2, 6, 7},{ 1, 2, 6, 7},{ 2, 3, 7, 8},{ 2, 3, 7, 8},{ 3, 4, 8, 9},{ 4,-1, 9,-1},{ 4,-1, 9,-1}},
    {{-1, 0,-1, 5},{ 0, 1, 5, 6},{ 1, 2, 6, 7},{ 1, 2, 6, 7},{ 2, 3, 7, 8},{ 2, 3, 7, 8},{ 3, 4, 8, 9},{ 4,-1, 9,-1},{ 4,-1, 9,-1}},
    {{-1, 5,-1,10},{ 5, 6,10,11},{ 6, 7,11,12},{ 6, 7,11,12},{ 7, 8,12,13},{ 7, 8,12,13},{ 8, 9,13,14},{ 9,-1,14,-1},{ 9,-1,14,-1}},
    {{-1,10,-1,15},{10,11,15,16},{11,12,16,17},{11,12,16,17},{12,13,17,18},{12,13,17,18},{13,14,18,19},{14,-1,19,-1},{14,-1,19,-1}},
    {{-1,10,-1,15},{10,11,15,16},{11,12,16,17},{11,12,16,17},{12,13,17,18},{12,13,17,18},{13,14,18,19},{14,-1,19,-1},{14,-1,19,-1}},
    {{-1,15,-1,20},{15,16,20,21},{16,17,21,22},{16,17,21,22},{17,18,22,23},{17,18,22,23},{18,19,23,24},{19,-1,24,-1},{19,-1,24,-1}},
    {{-1,20,-1,25},{20,21,25,26},{21,22,26,27},{21,22,26,27},{22,23,27,28},{22,23,27,28},{23,24,28,29},{24,-1,29,-1},{24,-1,29,-1}},
    {{-1,20,-1,25},{20,21,25,26},{21,22,26,27},{21,22,26,27},{22,23,27,28},{22,23,27,28},{23,24,28,29},{24,-1,29,-1},{24,-1,29,-1}},
    {{-1,25,-1,30},{25,26,30,31},{26,27,31,32},{26,27,31,32},{27,28,32,33},{27,28,32,33},{28,29,33,34},{29,-1,34,-1},{29,-1,34,-1}},
    {{-1,25,-1,30},{25,26,30,31},{26,27,31,32},{26,27,31,32},{27,28,32,33},{27,28,32,33},{28,29,33,34},{29,-1,34,-1},{29,-1,34,-1}},
    {{-1,30,-1,35},{30,31,35,36},{31,32,36,37},{31,32,36,37},{32,33,37,38},{32,33,37,38},{33,34,38,39},{34,-1,39,-1},{34,-1,39,-1}},
    {{-1,35,-1,40},{35,36,40,41},{36,37,41,42},{36,37,41,42},{37,38,42,43},{37,38,42,43},{38,39,43,44},{39,-1,44,-1},{39,-1,44,-1}},
    {{-1,35,-1,40},{35,36,40,41},{36,37,41,42},{36,37,41,42},{37,38,42,43},{37,38,42,43},{38,39,43,44},{39,-1,44,-1},{39,-1,44,-1}},
    {{-1,40,-1,-1},{40,41,-1,-1},{41,42,-1,-1},{41,42,-1,-1},{42,43,-1,-1},{42,43,-1,-1},{43,44,-1,-1},{44,-1,-1,-1},{44,-1,-1,-1}}
  };
}

void  fcs_trg_base::stage_2_202207(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[], u_short* s2_to_dsm)
{    
    int ns=geo.ns;
    if(fcs_trgDebug>=2) printf("Stage2v1 ns=%d\n",ns);
    static int first=0;
    // creating 2x2 row/column address map when called first time
    static u_int ETbTdep[16][10]; //DEP#
    static u_int ETbTadr[16][10]; //Input Link data address
    static u_int HTbTdep[10][6];  //DEP#
    static u_int HTbTadr[10][6];  //Input Link data address          
    if(first==0){
	first=1;
	//making map of 2x2 Ecal Sums of [4][4]
        for(int r=0; r<16; r++){
            printf("Ecal r=%2d : ",r);
            for(int c=0; c<10; c++){
                ETbTdep[r][c]= c/2 + (r/4)*5;
                ETbTadr[r][c]= c%2 + (r%4)*2;
                printf("%2d-%1d ",ETbTdep[r][c],ETbTadr[r][c]);
            }
            printf("\n");
        }
        //making map of 2x2 Hcal sums of [10][6]
        for(int r=0; r<10; r++){
            printf("HCal r=%2d : ",r);
            for(int c=0; c<6; c++){
                if      (r==0){
                    HTbTdep[r][c]= 6;
                    HTbTadr[r][c]= c;
                }else if(r==9){
                    HTbTdep[r][c]= 7;
                    HTbTadr[r][c]= c;
                }else{
                    HTbTdep[r][c]= c/2 + ((r-1)/4)*3;
                    HTbTadr[r][c]= c%2 + ((r-1)%4)*2;
                }
                printf("%2d-%1d ",HTbTdep[r][c],HTbTadr[r][c]);
            }
            printf("\n");
	}
    }
    
    // Ecal 2x2 "HT" trigger                                                                            
    int ecal_ht = 0 ;
    for(int i=0;i<DEP_ECAL_TRG_COU;i++) {// 0..19                                                       
      for(int j=0;j<8;j++) {
	if(s2_ch_mask[geo.ns] & (1ll<<i)) {
	  ecal[i].d[j] = 0 ;
	  continue ;
	}
	if(ecal[i].d[j] > EHTTHR) ecal_ht |= 1 ;
      }
    }

    // Hcal 2x2 "HT" trigger                                                                            
    int hcal_ht = 0 ;
    for(int i=0;i<DEP_HCAL_TRG_COU;i++) {// 20..27                                                      
      for(int j=0;j<8;j++) {
	if(s2_ch_mask[geo.ns] & (1ll<<(20+i))) {
	  hcal[i].d[j] = 0 ;
	  continue ;
	}
	if(hcal[i].d[j] > HHTTHR) hcal_ht |= 1 ;
      }
    }

    // Pres OR trigger                                                                                  
    int fpre_or = 0 ;
    for(int i=0;i<DEP_PRE_TRG_COU;i++) {// 28..32                                                       
      for(int j=0;j<8;j++) {
	if(s2_ch_mask[geo.ns] & (1ll<<(28+i))) {
	  pres[i].d[j] = 0 ;
	  continue ;
	}
	if(pres[i].d[j]) fpre_or |= 1 ;
      }
    }

    //mapped Ecal 2x2                                                                                   
    for(int r=0; r<16; r++){
      for(int c=0; c<10; c++){
        e2x2[ns][r][c]=ecal[ETbTdep[r][c]].d[ETbTadr[r][c]];
      }
    }
    //mapped Hcal 2x2                                                                                   
    for(int r=0; r<10; r++){
      for(int c=0; c<6; c++){
        h2x2[ns][r][c]=hcal[HTbTdep[r][c]].d[HTbTadr[r][c]];
      }
    }

    //compute overlapping Hcal 4x4 sum of [9][5]
    //u_int hsum[9][5];
    for(int r=0; r<9; r++){
        if(fcs_trgDebug>=2) printf("H4x4 ");
        for(int c=0; c<5; c++){
            hsum[ns][r][c]
                = hcal[HTbTdep[r  ][c  ]].d[HTbTadr[r  ][c  ]]
                + hcal[HTbTdep[r  ][c+1]].d[HTbTadr[r  ][c+1]]
                + hcal[HTbTdep[r+1][c  ]].d[HTbTadr[r+1][c  ]]
                + hcal[HTbTdep[r+1][c+1]].d[HTbTadr[r+1][c+1]];
            //if(hsum[r][c] > 0xff) hsum[r][c]=0xff;  //Tonko says no point to saturate at 8bit here
            if(fcs_trgDebug>=2) printf("%5d ",hsum[ns][r][c]);
        }
        if(fcs_trgDebug>=2) printf("\n");
    }
    
    //PRES for QA
    if(fcs_trgDebug>0){
	for(int dep=0; dep<6; dep++) {
	    for(int j=0; j<4; j++) {
		for(int k=0; k<8; k++){
		    phit[ns][dep][j*8+k] = (pres[dep].d[j] >> k) & 0x1;
		}
	    }
	}
    }
    if(fcs_trgDebug>=2){
	for(int dep=0; dep<6; dep++) {
	    printf("PRES NS%1d DEP%1d : ",ns,dep);
	    for(int j=0; j<4; j++) {
		for(int k=0; k<8; k++){
		    phit[ns][dep][j*8+k] = (pres[dep].d[j] >> k) & 0x1;
		    printf("%1d", (pres[dep].d[j]>>k)&0x1);
		}
		printf(" ");
	    }
	    printf("\n");
	}
    }
    
    //compute overlapping Ecal 4x4 sums of [15][9]
    //take ratio with the closest hcal 4x4
    //u_int esum[15][9];
    //u_int sum[15][9];
    //float ratio[15][9];
    u_int EM2 =0, EM1 =0, EM0=0, EM3=0;
    u_int ELE2=0, ELE1=0, ELE0=0;
    u_int HAD2=0, HAD1=0, HAD0=0;
    u_int ETOT=0, HTOT=0;
    for(int r=0; r<15; r++){
        if(fcs_trgDebug>=2) printf("EM4x4 ");
        for(int c=0; c<9; c++){
            esum[ns][r][c]
                = ecal[ETbTdep[r  ][c  ]].d[ETbTadr[r  ][c  ]]
                + ecal[ETbTdep[r  ][c+1]].d[ETbTadr[r  ][c+1]]
                + ecal[ETbTdep[r+1][c  ]].d[ETbTadr[r+1][c  ]]
                + ecal[ETbTdep[r+1][c+1]].d[ETbTadr[r+1][c+1]];
            //if(esum[r][c] > 0xff) esum[r][c]=0xff; //Tonko says no point to saturate at 8bit here

	    // locate the closest hcal
            u_int h=hsum[ns][EtoHmap[r][c][0]][EtoHmap[r][c][1]];

            // locate the max 2x2 hcal
            u_int hmax=0;
            for(int iz=0; iz<4; iz++){
              int iHCalID = EtoH3map[r][c][iz];
              if(iHCalID < 0) continue;
              int irow = iHCalID/5;
              int icol = iHCalID%5;
              if(hmax < hsum[ns][irow][icol]) hmax = hsum[ns][irow][icol];
            }

            //if(h > 0 || hmax > 0) printf("Checking: %d, %d\n", h, hmax);
 
	    // E+H sum
            sum[ns][r][c] = esum[ns][r][c] + h;
	    summax[ns][r][c] = esum[ns][r][c] + hmax;

            //in VHDL we will do esum>hsum*threshold. Ratio is for human only
            if(sum[ns][r][c]==0) {
                ratio[ns][r][c]=0.0;
            }else{
                ratio[ns][r][c] = float(esum[ns][r][c]) / float(sum[ns][r][c]);
            }
	    if(esum[ns][r][c]+hmax==0){
                ratiomax[ns][r][c]=0.0;
	    }else{
	        ratiomax[ns][r][c]=float(esum[ns][r][c]) / float(summax[ns][r][c]);
	    }

	    //check EPD hits using the mask
	    epdcoin[ns][r][c]=0;
	    for(int dep=0; dep<6; dep++){
		int mask;
		if(fcs_readPresMaskFromText==0){
		    mask = fcs_ecal_epd_mask[r][c][dep]; //from include file
		}else{
		    mask = PRES_MASK[r][c][dep]; //from static which was from text file 
		}
		for(int j=0; j<4; j++) {
		    for(int k=0; k<8; k++){
			if( (mask >> (j*8 + k)) & 0x1) { //if this is 0, don't even put the logic in VHDL
			    epdcoin[ns][r][c] |= (pres[dep].d[j] >> k) & 0x1;
			}
		    }
		}
	    }

	    // integer multiplication as in VHDL!
	    // ratio thresholds are in fixed point integer where 1.0==128
	    u_int h128 = h*128 ;
            u_int hmax128 = hmax*128 ;

	    if(hmax128 <= esum[ns][r][c] * EM_HERATIO_THR){
		if(esum[ns][r][c] > EMTHR2)  EM2 = 1;
		if(esum[ns][r][c] > EMTHR1)  EM1 = 1;
		if(esum[ns][r][c] > EMTHR0)  EM0 = 1;
		if(esum[ns][r][c] > ELETHR2) EM3 = 1; //Using ELE Thr2 
		if(epdcoin[ns][r][c]==1){
		    if(esum[ns][r][c] > ELETHR2) ELE2 = 1;
		    if(esum[ns][r][c] > ELETHR1) ELE1 = 1;
		    if(esum[ns][r][c] > ELETHR0) ELE0 = 1;
		}
	    }

	    if(h128 > esum[ns][r][c] * HAD_HERATIO_THR){
		if(sum[ns][r][c] > HADTHR0) HAD0 = 1;
		if(sum[ns][r][c] > HADTHR1) HAD1 = 1;
		if(sum[ns][r][c] > HADTHR2) HAD2 = 1;
	    }
	    if(fcs_trgDebug>=2) printf("%5d %1d %3.2f ",esum[ns][r][c],epdcoin[ns][r][c],ratiomax[ns][r][c]);
	}
	if(fcs_trgDebug>=2) printf("\n");
    }
    if(fcs_trgDebug>=2){
      for(int r=0; r<15; r++){
	printf("HAD4x4 ");
	for(int c=0; c<9; c++){
	  printf("%5d %3.2f ",sum[ns][r][c],ratio[ns][r][c]);
	}
	printf("\n");
      }
    }

    //5 square JP
    int e_col_start[5] = { 0, 0, 0, 3, 3};  //these are 2x2 row/col
    int e_col_stop[5]  = { 3, 6, 6, 9, 9};
    int e_row_start[5] = { 3, 0, 5, 0, 5};
    int e_row_stop[5]  = {12,10,15,10,15};
    int h_col_start[5] = { 0, 0, 0, 2, 2};
    int h_col_stop[5]  = { 2, 3, 3, 5, 5};
    int h_row_start[5] = { 2, 0, 3, 0, 3};
    int h_row_stop[5]  = { 7, 6, 9, 6, 9};			  
    int JP2[5] = {0,0,0,0,0};
    int JP1[5] = {0,0,0,0,0};    
    int JP0[5] = {0,0,0,0,0};    
    int JPd[5] = {0,0,0,0,0};    
    for(int i=0; i<5; i++){
      int ejet=0, hjet=0;
      for(int c=e_col_start[i]; c<=e_col_stop[i]; c++){ 
	for(int r=e_row_start[i]; r<=e_row_stop[i]; r++){ 
	  if((i==1 || i==2) && (c==0 || c==1) && (r>=5 && r<=10)) continue; //cutout
	  ejet += ecal[ETbTdep[r][c]].d[ETbTadr[r][c]]; 
	} 
      }
      for(int c=h_col_start[i]; c<=h_col_stop[i]; c++){ 
	for(int r=h_row_start[i]; r<=h_row_stop[i]; r++){ 
	  if((i==1 || i==2) && (c==0) && (r>=3 && r<=6)) continue; //cutout
	  hjet += hcal[HTbTdep[r][c]].d[HTbTadr[r][c]]; 
	}
      }
      jet[ns][i] = ejet + hjet;
      //if(jet[ns][i]>=0xff) jet[ns][i]=0xff;
      //int jet2 = ((ejet & 0x3ff) + (hjet & 0x3ff)) & 0x3ff;
      //if(jet2>=0xff) jet2=0xff;
      if(i==0){ //JPA	  
	  if(jet[ns][i]>JPATHR2) JP2[i] = 1;
	  if(jet[ns][i]>JPATHR1) JP1[i] = 1;
	  if(jet[ns][i]>JPATHR0) JP0[i] = 1;
      }else if(i==1 || i==2){ //JPB and JPC
	  if(jet[ns][i]>JPBCTHR2) JP2[i] = 1;
	  if(jet[ns][i]>JPBCTHR1) JP1[i] = 1;
	  if(jet[ns][i]>JPBCTHR0) JP0[i] = 1;
	  if(jet[ns][i]>JPBCTHRD) JPd[i] = 1;
      }else{ //JPD and JPE
	  if(jet[ns][i]>JPDETHR2) JP2[i] = 1;
	  if(jet[ns][i]>JPDETHR1) JP1[i] = 1;
	  if(jet[ns][i]>JPDETHR0) JP0[i] = 1;
	  if(jet[ns][i]>JPDETHRD) JPd[i] = 1;
      }
    }
    if(fcs_trgDebug>=2) printf("JP5 (ns=%1d) = %3d %3d %3d %3d %3d\n",ns,jet[ns][0],jet[ns][1],jet[ns][2],jet[ns][3],jet[ns][4]);
    if(fcs_trgDebug>=2) printf("JP5x(ns=%1d) = %3x %3x %3x %3x %3x\n",ns,jet[ns][0],jet[ns][1],jet[ns][2],jet[ns][3],jet[ns][4]);
    
    //Ecal sub-crate sum
    u_int esub[4];
    esub[0] = esum[ns][ 0][0]+esum[ns][ 0][2]+esum[ns][ 0][4]+esum[ns][ 0][6]+esum[ns][ 0][8]
	    + esum[ns][ 2][0]+esum[ns][ 2][2]+esum[ns][ 2][4]+esum[ns][ 2][6]+esum[ns][ 2][8];
    esub[1] = esum[ns][ 4][0]+esum[ns][ 4][2]+esum[ns][ 4][4]+esum[ns][ 4][6]+esum[ns][ 4][8]
	    + esum[ns][ 6][0]+esum[ns][ 6][2]+esum[ns][ 6][4]+esum[ns][ 6][6]+esum[ns][ 6][8];
    esub[2] = esum[ns][ 8][0]+esum[ns][ 8][2]+esum[ns][ 8][4]+esum[ns][ 8][6]+esum[ns][ 8][8]
	    + esum[ns][10][0]+esum[ns][10][2]+esum[ns][10][4]+esum[ns][10][6]+esum[ns][10][8];
    esub[3] = esum[ns][12][0]+esum[ns][12][2]+esum[ns][12][4]+esum[ns][12][6]+esum[ns][12][8]
	    + esum[ns][14][0]+esum[ns][14][2]+esum[ns][14][4]+esum[ns][14][6]+esum[ns][14][8];
    for(int i=0; i<4; i++) if(esub[i]>0xff) esub[i]=0xff;
    
    //Hcal sub-crate sum
    u_int hsub[4];
    hsub[0] = hsum[ns][ 1][0]+hsum[ns][ 1][2]+hsum[ns][ 1][4];
    hsub[1] = hsum[ns][ 3][0]+hsum[ns][ 3][2]+hsum[ns][ 3][4];
    hsub[2] = hsum[ns][ 5][0]+hsum[ns][ 5][2]+hsum[ns][ 5][4];
    hsub[3] = hsum[ns][ 7][0]+hsum[ns][ 7][2]+hsum[ns][ 7][4];
    for(int i=0; i<4; i++) if(hsub[i]>0xff) hsub[i]=0xff;

    //total ET
    etot[ns] = esub[0] + esub[1] + esub[2] + esub[3];
    htot[ns] = hsub[0] + hsub[1] + hsub[2] + hsub[3];
    if(etot[ns]>ETOTTHR) ETOT=1;
    if(htot[ns]>HTOTTHR) HTOT=1;
    if(fcs_trgDebug>=2) printf("E/H Tot = %3d %3d\n",etot[ns],htot[ns]);

    //sending output bits
    output[0].d[0] = (EM0<<0)  + (EM1<<1)  + (EM2<<2)  + (EM3<<3) + (ELE0<<4) + (ELE1<<5) + (ELE2<<6) + (fpre_or<<7);
    output[1].d[0] = (HAD0<<0) + (HAD1<<1) + (HAD2<<2) + (0<<3)   + (ecal_ht<<4) + (hcal_ht<<5) + (ETOT<<6) + (HTOT<<7);
    output[0].d[1] = (JP2[0]<<0) + (JP2[1]<<1) + (JP2[2]<<2) + (JP2[3]<<3) + (JP2[4]<<4);
    output[1].d[1] = (JP1[0]<<0) + (JP1[1]<<1) + (JP1[2]<<2) + (JP1[3]<<3) + (JP1[4]<<4);
    output[0].d[2] = (JP0[0]<<0) + (JP0[1]<<1) + (JP0[2]<<2) + (JP0[3]<<3) + (JP0[4]<<4);
    output[1].d[2] = (JPd[0]<<0) + (JPd[1]<<1) + (JPd[2]<<2) + (JPd[3]<<3) + (JPd[4]<<4); //JPd[0]==0
    output[0].d[3] = 0;
    output[1].d[3] = 0;
    output[1].d[4] = 0;
    output[0].d[4] = 0;
    output[0].d[5] = 0;
    output[1].d[5] = 0;
    output[0].d[6] = 0;
    output[1].d[6] = 0;
    output[0].d[7] = 0;
    output[1].d[7] = 0;

    *s2_to_dsm = (ELE0<<0) + (ELE1<<1) + (ELE2<<2) + (EM3<<3)
   	       + (ecal_ht<<4) + (hcal_ht<<5) + (ETOT<<6) + (HTOT<<7);

    if(fcs_trgDebug>=1){    
        printf("FCS STG2 NS=%1d output = %02x %02x %02x %02x %02x %02x TCU=%04x\n",
	       ns,
	       output[0].d[0],output[1].d[0],output[0].d[1],output[1].d[1],
	       output[0].d[2],output[1].d[2],*s2_to_dsm);
        printf("emuout   = %02x%02x %02x%02x %02x%02x %02x%02x %02x%02x %02x%02x %02x%02x %02x%02x TCU=%04x\n",
               output[1].d[0],output[0].d[0],
               output[1].d[1],output[0].d[1],
               output[1].d[2],output[0].d[2],
               output[1].d[3],output[0].d[3],
	       output[1].d[4],output[0].d[4],
               output[1].d[5],output[0].d[5],
               output[1].d[6],output[0].d[6],
               output[1].d[7],output[0].d[7],
               *s2_to_dsm);
    }
    
    return ;
}
