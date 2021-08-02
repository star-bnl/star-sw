//
// ZDC_SMD Constants from Gang Wang 03/26/2007
// Pedestals updated by Gang Wang 12/31/2009
// Included in eventLoop.cxx
//

//float smd_temp =0.01;

int zdc_smd_w_v[8];
int zdc_smd_w_h[8];
int zdc_smd_e_v[8];
int zdc_smd_e_h[8];

float zdc_smd_ped[2][2][8]={{{85.58,83.2125,80.8655,87.2975,67.171,85.1085,80.6875,78.2435},
				    {80.57,78.463,85.4785,91.8345,85.742,82.1725,80.7425,77.463}},
				   {{87.932,89.557,90.718,93.4085,92.1285,87.849,74.11,77.062},
				    {82.569,75.6175,74.23,75.9275,73.002,80.4215,73.9905,75.7385}}}; // run10363017


inline void init_zdc_smd() {
    zdc_smd_e_v[1-1] = 7 ;
    zdc_smd_e_v[2-1] = 6 ;
    zdc_smd_e_v[3-1] = 5 ;
    zdc_smd_e_v[4-1] = 4 ;
    zdc_smd_e_v[5-1] = 3 ;
    zdc_smd_e_v[6-1] = 2 ;
    zdc_smd_e_v[7-1] = 1 ;
    zdc_smd_e_v[8-1] = 11;       // e_led
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
    zdc_smd_w_v[8-1] = 26;    // w_led
    zdc_smd_w_h[1-1] = 16;
    zdc_smd_w_h[2-1] = 31;
    zdc_smd_w_h[3-1] = 30;
    zdc_smd_w_h[4-1] = 29;
    zdc_smd_w_h[5-1] = 28;
    zdc_smd_w_h[6-1] = 27;
    zdc_smd_w_h[7-1] = 24;
    zdc_smd_w_h[8-1] = 25;
}
    
    

