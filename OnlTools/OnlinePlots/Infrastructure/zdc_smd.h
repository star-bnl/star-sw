//
// ZDC_SMD Constants from Gang Wang 03/26/2007
//
// Included in eventLoop.cxx
//

//float smd_temp =0.01;

static  int zdc_smd_w_v[8];
static  int zdc_smd_w_h[8];
static  int zdc_smd_e_v[8];
static  int zdc_smd_e_h[8];

/* run8084073
static float zdc_smd_ped[32]=
{5.028,3.1,4.039,3.461,2.518,3.579,5.478,1.752,4.033,4.499,2.58,4.03,2.799,4.562,2.527,2.853,
8.88,4.019,3.98,8.115,0.072,6.693,5.941,4.521,9.018,7.96,137.942,7.181,8.026,7.412,6.642,8.274};

static float zdc_smd_gain[32]=
{1.,1.274,1.110,0.926,1.116,1.150,1.376,1.,1.268,1.202,1.334,1.,1.021,1.144,1.194,1.075,
1.,0.931,0.957,0.841,0.963,1.292,1.365,1.,1.169,0.949,1.,1.256,0.923,0.833,0.890,0.907};
*/

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






/***************************************************************************
*
* $Id: zdc_smd.h,v 1.2 2009/03/05 00:03:16 dkettler Exp $
*
* Author: Frank Laue, laue@bnl.gov
***************************************************************************
*
* Description:
*
***************************************************************************
*
* $Log: zdc_smd.h,v $
* Revision 1.2  2009/03/05 00:03:16  dkettler
* EMC Updates
*
* Revision 1.1  2009/01/23 16:11:00  jeromel
* Import from online/RTS/src/
*
* Revision 1.2  2007/03/26 17:38:16  laue
* Updated zdc mapping for run 2007
*
* Revision 1.1  2007/02/27 15:23:42  laue
* Initial version
*
* Revision 1.1  2006/10/04 20:31:34  laue
* Initial Version
*
*
***************************************************************************/

