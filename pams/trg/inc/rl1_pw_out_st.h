/*   rl1_pw_out_st.h
 *    (onetasth.ace)
 */
/*****************************************************************************
*   Table: pw_out
*descrip struct name: Pw_out_desc_st                                              
*    row struct name: Pw_out_row_st                                               
*        description: Level 1 output table for autocorrelation
*             more..: analysis                                
*/

   /*******************  Row struct *******************/
   typedef struct {          /* Pw_out_row_st */ 
       float     aut_cpu;    /* CPU time to do autocorrelation */
       float     autocorr[100];/* autocorrelation output */
       float     autonull[100];/* autocorrelation minus null hyp. */
       float     c1[100];    /* cos coefficient from null hyp. */
       float     phase;      /* phase value */
       float     pow_cpu;    /* CPU time to do power spectra */
       float     power;      /* power value */
       float     s1[100];    /* sin coefficient from null hyp. */
   } Pw_out_row_st;

   /********************  Table struct *****************/
   typedef struct {                               /* Pw_out_desc_st */
       Table_head_st         h;                   /* table header */
       Table_vinfo_st        vinfo[PW_OUT_NV];    /* variable info */
   } Pw_out_desc_st;
/*  Last mod. for pw_out:   783377928 Fri Oct 28 16:58:48 1994 */

