/*   rl1_ks_ctrl_st.h
 *    (onetasth.ace)
 */
/*****************************************************************************
*   Table: ks_ctrl
*descrip struct name: Ks_ctrl_desc_st                                             
*    row struct name: Ks_ctrl_row_st                                              
*        description: K-S test control parameters for level 1.
*             more..:                                         
*/

   /*******************  Row struct *******************/
   typedef struct {          /* Ks_ctrl_row_st */ 
       int       ctb_eta;    /* Number of eta bins in CTB */
       int       ctb_phi;    /* Number of phi bins in CTB */
       int       mwc_eta;    /* Number of eta bins per endcap */
       int       mwc_phi;    /* Number of phi bins per endcap */
       int       n_bins;     /* Number of compressed 1-D bins. */
       int       switch;     /* switch to turn module on */
       int       ref_data[100];/* K-S test reference. */
       int       ref_data_flag;/* reference data flag */
       float     threshold;  /* K-S test selectivity threshold. */
   } Ks_ctrl_row_st;

   /********************  Table struct *****************/
   typedef struct {                               /* Ks_ctrl_desc_st */
       Table_head_st         h;                   /* table header */
       Table_vinfo_st        vinfo[KS_CTRL_NV];   /* variable info */
   } Ks_ctrl_desc_st;
/*  Last mod. for ks_ctrl:   759596937 Wed Jan 26 10:08:57 1994 */

