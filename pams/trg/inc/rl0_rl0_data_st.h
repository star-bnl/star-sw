/*   rl0_rl0_data_st.h
 *    (onetasth.ace)
 */
/*****************************************************************************
*   Table: rl0_data
*descrip struct name: Rl0_data_desc_st                                            
*    row struct name: Rl0_data_row_st                                             
*        description: Level 0 trigger data                    
*             more..:                                         
*/

   /*******************  Row struct *******************/
   typedef struct {          /* Rl0_data_row_st */ 
       int       bits;       /* Bit word */
       int       fpga1_ctb[64];/* Fpga sum for 1st tree CTB */
       int       fpga1_mwc[64];/* Fpga sum for 1st tree MWC */
       int       fpga2_ctb[32];/* Fpga sum for 2nd tree CTB */
       int       fpga2_mwc[32];/* Fpga sum for 2nd tree MWC */
       int       fpga3_ctb[4];/* Fpga sum for 3rd tree CTB */
       int       fpga3_mwc[4];/* Fpga sum for 3rd tree MWC */
       int       fpga4_ctb;  /* 4th fpga tree tot ctb */
       int       fpga4_mwc;  /* 4th Fpga tree for MWC */
       int       fpga5_tot;  /* Fpga sum for 5th tree Total Multiplicity */
       int       info[64];   /* Level zero information */
   } Rl0_data_row_st;

   /********************  Table struct *****************/
   typedef struct {                               /* Rl0_data_desc_st */
       Table_head_st         h;                   /* table header */
       Table_vinfo_st        vinfo[RL0_DATA_NV];  /* variable info */
   } Rl0_data_desc_st;
/*  Last mod. for rl0_data:   762211102 Fri Feb 25 16:18:22 1994 */

