#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg_base.h"

//#include "fcs_trg_201901.h"

// Processing on the North or South DEP/IO flavoured board. 
// Inputs are up to 32 links but I already organized them according to strawman.
// output is 1 link over the external OUT connector.
// Right now we assume we have 20 inputs from ECAL, 6 from HCAL and 4 from PRE.
// We also assume there is no need to know if this is North or South as
// the processing is exactly the same. Right??

void  fcs_trg_base::stage_2_201900(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[])
{    
    static int first=0;
    static u_int ETbTdep[4][4]; //DEP#
    static u_int ETbTadr[4][4]; //Input Link data address
    if(first==0){
	first=1;
	//making map of 2x2 Ecal Sums of [4][4]
	for(int r=0; r<4; r++){
	    printf("Ecal r=%2d : ",r);
	    for(int c=0; c<4; c++){
		ETbTdep[r][c]= c/2 + (r/4);
		ETbTadr[r][c]= c%2 + (r%4)*2;
		printf("%2d-%1d ",ETbTdep[r][c],ETbTadr[r][c]);
	    }
	    printf("\n");
	}
    }
    
    //compute hcal 4x4 sum -- note d[1,3,5,7] are not used at the stage-1 DEP
    u_int hsum=hcal[0].d[0]+hcal[0].d[2]+hcal[0].d[4]+hcal[0].d[6];
    if(fcs_trgDebug>=2) printf("Hcal sum=%5d\n",hsum);
    
    //compute ecal 4x4 sums of [3][3]
    u_int esum[3][3];
    float ratio[3][3];
    u_int EM1 =0, EM2 =0;
    u_int GAM1=0, GAM2=0;
    u_int ELE1=0, ELE2=0;
    u_int HAD1=0, HAD2=0;
    u_int JP1 =0, JP2 =0;
    for(int r=0; r<3; r++){
	if(fcs_trgDebug>=2) printf("E4x4 ");
	for(int c=0; c<3; c++){
	    esum[r][c]
		= ecal[ETbTdep[r  ][c  ]].d[ETbTadr[r  ][c  ]]
		+ ecal[ETbTdep[r  ][c+1]].d[ETbTadr[r  ][c+1]]
		+ ecal[ETbTdep[r+1][c  ]].d[ETbTadr[r+1][c  ]]
		+ ecal[ETbTdep[r+1][c+1]].d[ETbTadr[r+1][c+1]];

//	Tonko: removed, no point in pegging at 0xFF since it's only 0x3FF
//	    if(esum[r][c] > 0xff) esum[r][c]=0xff;
	    
	    //in VHDL we will do hsum > esum * threshold. Ratio is for human only
	    u_int h=hsum; //find closest hcal but 4x4... but we have only 1 in run19
	    u_int sum = esum[r][c] + h;
	    if(sum==0) {
		ratio[r][c]=0.0;
	    }else{
		ratio[r][c] = float(esum[r][c]) / float(sum); //doing with float in C;
	    }


	    // integer multiplication as in VHDL!
	    // ratio thresholds are in fixed point integer where 1.0==128
	    u_int h128 = h*128 ;

	    if(h128 < esum[r][c] * EM_HERATIO_THR){

		if(sum > EMTHR1){
		    EM1 |= (1<<r);
		    if((pres[0].d[0] & (1<<r))==0) {GAM1 |= (1<<r);}
		    else                           {ELE1 |= (1<<r);}
		}

		if(sum > EMTHR2){
		    EM2 |= (1<<r);		
		    if((pres[0].d[0] & (1<<r))==0) {GAM2 |= (1<<r);}
		    else                           {ELE2 |= (1<<r);}
		}
	    }

	    if(h128 > esum[r][c] * HAD_HERATIO_THR){
		if(sum > HADTHR1) HAD1 |= (1<<r);
		if(sum > HADTHR2) HAD2 |= (1<<r);
	    }

	    if(fcs_trgDebug>=2) printf("%5d %3.2f  ",esum[r][c],ratio[r][c]);
	}
	if(fcs_trgDebug>=2) printf("\n");
    }
    
    //Ecal total sum    
    u_int etot;
    etot= esum[ 0][0]+esum[ 0][2]
	+ esum[ 2][0]+esum[ 2][2];
    
    //Jet sum
    u_int jet = hsum + etot;
    if(etot > JETTHR1) JP1 = 1; //ecal only (pi0 trigger)
    if(jet  > JETTHR2) JP2 = 1; //ecal+hcal
    
    if(fcs_trgDebug>=2) printf("Jet = %4d + %4d = %3d\n",etot,hsum,jet);

    //sending output bits
    output[0].d[0] = 0x80 | (EM1  + (EM2 <<3));	// Tonko: added last bit
    output[0].d[1] = GAM1 + (GAM2<<3);
    output[0].d[2] = ELE1 + (ELE2<<3);
    output[0].d[3] = HAD1 + (HAD2<<3);
    output[0].d[4] = JP1  + (JP2 <<1);
    output[0].d[5] = pres[0].d[0] ;	// Tonko: added
    output[0].d[6] = pres[0].d[1] ;	// Tonko: added
    output[0].d[7] = pres[0].d[7] ;	// Tonko: added

#if 0
    for(int i=0;i<4;i++) {
    for(int j=0;j<8;j++) {
	printf("PRES %d %d = 0x%X\n",i,j,pres[i].d[j]) ;
    }}
#endif

    if(fcs_trgDebug>=1){    
        printf("FCS STG2 NS=%1d output = %02x %02x %02x %02x %02x\n",
	       geo.ns,output[0].d[0],output[0].d[1],output[0].d[2],output[0].d[3],output[0].d[4]);
    }

    return ;
}

