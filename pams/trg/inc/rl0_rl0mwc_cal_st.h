/*   rl0_rl0mwc_cal_st.h
 *    (onetasth.ace)
 */
/*****************************************************************************
*   Table: rl0mwc_cal
*descrip struct name: Rl0mwc_cal_desc_st                                          
*    row struct name: Rl0mwc_cal_row_st                                           
*        description: Clibration of MWC in RL0                
*             more..:                                         
*/

   /*******************  Row struct *******************/
   typedef struct {          /* Rl0mwc_cal_row_st */ 
       float     fit[2];     /* slope and interc. adc.vs.hits mwc eta+ */
   } Rl0mwc_cal_row_st;

   /********************  Table struct *****************/
   typedef struct {                               /* Rl0mwc_cal_desc_st */
       Table_head_st         h;                   /* table header */
       Table_vinfo_st        vinfo[RL0MWC_CAL_NV]; /* variable info */
   } Rl0mwc_cal_desc_st;
/*  Last mod. for rl0mwc_cal:   758587281 Fri Jan 14 17:41:21 1994 */

