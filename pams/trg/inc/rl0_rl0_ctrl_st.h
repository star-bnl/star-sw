/*   rl0_rl0_ctrl_st.h
 *    (onetasth.ace)
 */
/*****************************************************************************
*   Table: rl0_ctrl
*descrip struct name: Rl0_ctrl_desc_st                                            
*    row struct name: Rl0_ctrl_row_st                                             
*        description: Controls the level 0 emulation          
*             more..:                                         
*/

   /*******************  Row struct *******************/
   typedef struct {          /* Rl0_ctrl_row_st */ 
       int       ctb_pixel_out;/* Number of pixels output for CTB */
       int       det_set;    /* Detector Set ID number */
       int       mwc_pixel_out;/* Number of pixels output for MWC */
       int       nthres;     /* Multiplicity threshold */
       int       segment[2]; /* Determines how segments are summed in RL */
       float     evc_cut;    /* VC energy cut */
       float     fpga_gain[9];/* Gain for FPGA trees */
       float     z_cut;      /* Maximum allowed dz */
   } Rl0_ctrl_row_st;

   /********************  Table struct *****************/
   typedef struct {                               /* Rl0_ctrl_desc_st */
       Table_head_st         h;                   /* table header */
       Table_vinfo_st        vinfo[RL0_CTRL_NV];  /* variable info */
   } Rl0_ctrl_desc_st;
/*  Last mod. for rl0_ctrl:   761409715 Wed Feb 16 09:41:55 1994 */

