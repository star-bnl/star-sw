/*   vpm_vpd_time_st.h
 *    (onetasth.ace)
 */
/*****************************************************************************
*   Table: vpd_time
*descrip struct name: Vpd_time_desc_st                                            
*    row struct name: Vpd_time_row_st                                             
*        description: Determines time minimum from each       
*             more..: segment of VPD                          
*/

   /*******************  Row struct *******************/
   typedef struct {          /* Vpd_time_row_st */ 
       int       id;         /* primary key */
       float     neg_time;   /* Min time for VPD eta lt 0 */
       float     pos_time;   /* Min time for VPD eta gt 0 */
   } Vpd_time_row_st;

   /********************  Table struct *****************/
   typedef struct {                               /* Vpd_time_desc_st */
       Table_head_st         h;                   /* table header */
       Table_vinfo_st        vinfo[VPD_TIME_NV];  /* variable info */
   } Vpd_time_desc_st;
/*  Last mod. for vpd_time:   749769837 Mon Oct  4 17:23:57 1993 */

