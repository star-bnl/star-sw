/*   rl1_pw_ctrl_st.h
 *    (onetasth.ace)
 */
/*****************************************************************************
*   Table: pw_ctrl
*descrip struct name: Pw_ctrl_desc_st                                             
*    row struct name: Pw_ctrl_row_st                                              
*        description: Controll table for level 1 1 dimensional
*             more..: power spectrum code.                    
*/

   /*******************  Row struct *******************/
   typedef struct {          /* Pw_ctrl_row_st */ 
       int       bnd_width;  /* power spectrum band width */
       int       ctb_eta;    /* Number of eta bins in CTB */
       int       ctb_phi;    /* Number of phi bins in CTB */
       int       mwc_eta;    /* Number of eta bins in MWC */
       int       mwc_phi;    /* Number of phi bins in MWC */
       int       n_bins;     /* Number of level 1 1-D bins */
       int       nul_hyp;    /* switch for null hypothesis */
       int       switch;    /* switch to turn module on */
       int       time_loop;  /* number of do loop iterations */
       float     c_coef[100];/* cos coefficient */
       float     ctb_fact;   /* factor for CTB input data */
       float     mwc_fact;   /* factor for MWC input data */
       float     s_coef[100];/* sin coefficient */
   } Pw_ctrl_row_st;

   /********************  Table struct *****************/
   typedef struct {                               /* Pw_ctrl_desc_st */
       Table_head_st         h;                   /* table header */
       Table_vinfo_st        vinfo[PW_CTRL_NV];   /* variable info */
   } Pw_ctrl_desc_st;
/*  Last mod. for pw_ctrl:   787946742 Tue Dec 20 13:05:42 1994 */

