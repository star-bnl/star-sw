/*   rl1_ks_out_st.h
 *    (onetasth.ace)
 */
/*****************************************************************************
*   Table: ks_out
*descrip struct name: Ks_out_desc_st                                              
*    row struct name: Ks_out_row_st                                               
*        description: K-S test level 1 output table.          
*             more..:                                         
*/

   /*******************  Row struct *******************/
   typedef struct {          /* Ks_out_row_st */ 
       int       accept;     /* Accept - 1, or reject -0 event. */
       float     cpu;        /* CPU level 1 processing time. */
       float     ks_d;       /* K-S test d parameter. */
       float     ks_sig;     /* K-S test significance value. */
   } Ks_out_row_st;

   /********************  Table struct *****************/
   typedef struct {                               /* Ks_out_desc_st */
       Table_head_st         h;                   /* table header */
       Table_vinfo_st        vinfo[KS_OUT_NV];    /* variable info */
   } Ks_out_desc_st;
/*  Last mod. for ks_out:   758570317 Fri Jan 14 12:58:37 1994 */

