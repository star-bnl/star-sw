/*   rl0_rl0ctb_cal_st.h
 *    (onetasth.ace)
 */
/*****************************************************************************
*   Table: rl0ctb_cal
*descrip struct name: Rl0ctb_cal_desc_st                                          
*    row struct name: Rl0ctb_cal_row_st                                           
*        description: Level 0 Calibration Table for CTB       
*             more..:                                         
*/

   /*******************  Row struct *******************/
   typedef struct {          /* Rl0ctb_cal_row_st */ 
       float     fit[2];     /* Fit for ADC to hit calibration */
   } Rl0ctb_cal_row_st;

   /********************  Table struct *****************/
   typedef struct {                               /* Rl0ctb_cal_desc_st */
       Table_head_st         h;                   /* table header */
       Table_vinfo_st        vinfo[RL0CTB_CAL_NV]; /* variable info */
   } Rl0ctb_cal_desc_st;
/*  Last mod. for rl0ctb_cal:   758587065 Fri Jan 14 17:37:45 1994 */

