//
// ZDC_SMD Constants from Gang Wang 02/25/2004
//
// Included in eventLoop.cxx
//

float smd_temp =0.01;

static  float zdc_smd_gain[32];

static  int zdc_smd_w_v[8];
static  int zdc_smd_w_h[8];
static  int zdc_smd_e_v[8];
static  int zdc_smd_e_h[8];

static float zdc_smd_ped[32]={4.942,2.403,4.044,3.079,2.069,3.241,5.087,1.665,4.04,4.07,2.883,4.109,2.729,4.174,2.402,3.032,8.685,4.051,3.988,8.113,0.113,6.3,5.527,5.065,9.027,7.963,9.982,7.072,7.937,7.117,6.629,8.389}; // run6041041

zdc_smd_gain[0]=1.;
zdc_smd_gain[1]=1.579;
zdc_smd_gain[2]=1.22;
zdc_smd_gain[3]=1.261;
zdc_smd_gain[4]=1.183;
zdc_smd_gain[5]=1.491;
zdc_smd_gain[6]=1.148;
zdc_smd_gain[7]=0.989;
zdc_smd_gain[8]=1.085;
zdc_smd_gain[9]=1.179;
zdc_smd_gain[10]=1.463;
zdc_smd_gain[11]=2.;
zdc_smd_gain[12]=0.947;
zdc_smd_gain[13]=1.292;
zdc_smd_gain[14]=1.12;
zdc_smd_gain[15]=1.169;
zdc_smd_gain[16]=1.296;
zdc_smd_gain[17]=1.843;
zdc_smd_gain[18]=1.357;
zdc_smd_gain[19]=1.276;
zdc_smd_gain[20]=1.168;
zdc_smd_gain[21]=1.415;
zdc_smd_gain[22]=1.108;
zdc_smd_gain[23]=1.019;
zdc_smd_gain[24]=2.;
zdc_smd_gain[25]=1.739;
zdc_smd_gain[26]=2.357;
zdc_smd_gain[27]=1.972;
zdc_smd_gain[28]=1.409;
zdc_smd_gain[29]=1.532;
zdc_smd_gain[30]=1.469;
zdc_smd_gain[31]=1.393;


zdc_smd_e_v[1-1] = 7 ;
zdc_smd_e_v[2-1] = 6 ;
zdc_smd_e_v[3-1] = 5 ;
zdc_smd_e_v[4-1] = 4 ;
zdc_smd_e_v[5-1] = 3 ;
zdc_smd_e_v[6-1] = 2 ;
zdc_smd_e_v[7-1] = 1 ;
zdc_smd_e_v[8-1] = 11;       //     e_led
zdc_smd_e_h[1-1] = 0 ;
zdc_smd_e_h[2-1] = 15;
zdc_smd_e_h[3-1] = 14;
zdc_smd_e_h[4-1] = 13;
zdc_smd_e_h[5-1] = 12;
zdc_smd_e_h[6-1] = 8 ;
zdc_smd_e_h[7-1] = 10;
zdc_smd_e_h[8-1] = 9 ;
zdc_smd_w_v[1-1] = 23;
zdc_smd_w_v[2-1] = 22;
zdc_smd_w_v[3-1] = 21;
zdc_smd_w_v[4-1] = 20;
zdc_smd_w_v[5-1] = 19;
zdc_smd_w_v[6-1] = 18;
zdc_smd_w_v[7-1] = 17;
zdc_smd_w_v[8-1] = 24;    // w_led
zdc_smd_w_h[1-1] = 16;
zdc_smd_w_h[2-1] = 31;
zdc_smd_w_h[3-1] = 30;
zdc_smd_w_h[4-1] = 29;
zdc_smd_w_h[5-1] = 28;
zdc_smd_w_h[6-1] = 27;
zdc_smd_w_h[7-1] = 26;
zdc_smd_w_h[8-1] = 25;

