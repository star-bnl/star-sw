/*  scv_messages_c.c
 *   created by ecvmsgc.ace 1.2  on  21-Jun-1994    at 14:17:53
 *
 */

#include <stddef.h>
#define MESSLEN 120

char *scv_messages_c( ivalue )
int *ivalue;  /* condition value to be translated */
{
/*  scv_messages_c returns a pointer to the string which is the
 *  text translation of the STAR condition ivalue 
 *  If a matching string for ivalue is not found a NULL pointer is returned.
 */
   static char mess[MESSLEN+1];
   char *result;

   switch( *ivalue )
   {
       case 257 :
           strcpy( mess,
               "TAS_OK_CV: general TAS success value");
           strcat( mess, " ");
           result = mess;
           break;
       case 514 :
           strcpy( mess,
               "TAS_TBCODE_NOMATCH_CV: did not match table descriptor parameter code");
           strcat( mess, " ");
           result = mess;
           break;
       case 1540 :
           strcpy( mess,
               "TAS_TABNOTINIT_CV: the table is not initialized");
           strcat( mess, " ");
           result = mess;
           break;
       case 1796 :
           strcpy( mess,
               "TAS_EVCOMNOSPACE_CV: no more space available in tas_event_com to register more");
           strcat( mess, " tables");
           result = mess;
           break;
       case 2052 :
           strcpy( mess,
               "TAS_BADMODNUM_CV: illegal value for analysis module handle");
           strcat( mess, " ");
           result = mess;
           break;
       case 2308 :
           strcpy( mess,
               "TAS_AMTABCONFUSED_CV: list of tables for analysis module is confused in tam_com");
           strcat( mess, " ");
           result = mess;
           break;
       case 2564 :
           strcpy( mess,
               "TAS_TABREGEXIST_CV: table name is already registered with TAS, can not register");
           strcat( mess, " again");
           result = mess;
           break;
       case 2820 :
           strcpy( mess,
               "TAS_AMREGEXIST_CV: module name already exists");
           strcat( mess, " ");
           result = mess;
           break;
       case 3074 :
           strcpy( mess,
               "TAS_NOMOREDTS_CV: the DTS routines from EOS are no longer suported");
           strcat( mess, " ");
           result = mess;
           break;
       case 3332 :
           strcpy( mess,
               "TAS_MACHDUMMY_CV: this is a dummy routine on this type of machine.");
           strcat( mess, " ");
           result = mess;
           break;
       case 4100 :
           strcpy( mess,
               "TAS_BADTFTYPE_CV: bad format type for table file");
           strcat( mess, " ");
           result = mess;
           break;
       case 4356 :
           strcpy( mess,
               "TAS_NOLUN_CV: no logical unit available");
           strcat( mess, " ");
           result = mess;
           break;
       case 4612 :
           strcpy( mess,
               "TAS_NOZTABLN_CV: no ztablinks avaiable");
           strcat( mess, " ");
           result = mess;
           break;
       case 4868 :
           strcpy( mess,
               "TAS_FNOTABWR_CV: no tables found to write out");
           strcat( mess, " ");
           result = mess;
           break;
       case 5124 :
           strcpy( mess,
               "TAS_BADSTNUM_CV: bad stream number");
           strcat( mess, " ");
           result = mess;
           break;
       case 5380 :
           strcpy( mess,
               "TAS_BADSTPAR_CV: bad value for which parameter in the tas_tio_spar_* paramete");
           strcat( mess, " r set (in tas_tio.inc)");
           result = mess;
           break;
       case 5636 :
           strcpy( mess,
               "TAS_ERRDTSOPEN_CV: error on dts open");
           strcat( mess, " ");
           result = mess;
           break;
       case 5892 :
           strcpy( mess,
               "TAS_STRNOTINIT_CV: stream is not initialized");
           strcat( mess, " ");
           result = mess;
           break;
       case 6148 :
           strcpy( mess,
               "TAS_UNKSTMODE_CV: unknown I/O mode for stream (not input or output)");
           strcat( mess, " ");
           result = mess;
           break;
       case 6404 :
           strcpy( mess,
               "TAS_FZOPENFAIL_CV: fzopen failed");
           strcat( mess, " ");
           result = mess;
           break;
       case 6658 :
           strcpy( mess,
               "TAS_CLSTNOP_CV: trying to close a stream which is not open");
           strcat( mess, " ");
           result = mess;
           break;
       case 6916 :
           strcpy( mess,
               "TAS_BADSFTPO_CV: bad format type of previously open stream, this can not happ");
           strcat( mess, " en, tell someone knowledgeable.");
           result = mess;
           break;
       case 7172 :
           strcpy( mess,
               "TAS_SCLOSERR_CV: error closing stream");
           strcat( mess, " ");
           result = mess;
           break;
       case 7428 :
           strcpy( mess,
               "TAS_UNKSIOTYP_CV: unknown stream I/O type, not f77, C fd or Zebra CF");
           strcat( mess, " ");
           result = mess;
           break;
       case 7684 :
           strcpy( mess,
               "TAS_UNKTLIST_CV: unknown table list type");
           strcat( mess, " ");
           result = mess;
           break;
       case 7940 :
           strcpy( mess,
               "TAS_TLISTINVALID_CV: invalid table list type for operation");
           strcat( mess, " ");
           result = mess;
           break;
       case 8196 :
           strcpy( mess,
               "TAS_INVSTMODE_CV: invalid I/O mode for stream operation");
           strcat( mess, " ");
           result = mess;
           break;
       case 8452 :
           strcpy( mess,
               "TAS_SETTLERR_CV: error setting table list, probably did not match a table nam");
           strcat( mess, " e");
           result = mess;
           break;
       case 8708 :
           strcpy( mess,
               "TAS_STNOP_CV: stream is not open");
           strcat( mess, " ");
           result = mess;
           break;
       case 8964 :
           strcpy( mess,
               "TAS_INVSTFORMAT_CV: invalid stream format for operation");
           strcat( mess, " ");
           result = mess;
           break;
       case 9220 :
           strcpy( mess,
               "TAS_NOTFZSTF_CV: Not an FZ format stream");
           strcat( mess, " ");
           result = mess;
           break;
       case 9476 :
           strcpy( mess,
               "TAS_FZSGERR_CV: error getting event in FZ stream");
           strcat( mess, " ");
           result = mess;
           break;
       case 9732 :
           strcpy( mess,
               "TAS_FZSZPERR_CV: error writing FZ event");
           strcat( mess, " ");
           result = mess;
           break;
       case 9988 :
           strcpy( mess,
               "TAS_STRWLOV_CV: requested more tables in read/write list than there is space");
           strcat( mess, " ");
           result = mess;
           break;
       case 10244 :
           strcpy( mess,
               "TAS_STNAMUSE_CV: stream name already in use");
           strcat( mess, " ");
           result = mess;
           break;
       case 12548 :
           strcpy( mess,
               "TAS_TIO_STNOAVAIL_CV: no stream available");
           strcat( mess, " ");
           result = mess;
           break;
       case 12804 :
           strcpy( mess,
               "TAS_TIO_STINNOAVAIL_CV: no more input streams available");
           strcat( mess, " ");
           result = mess;
           break;
       case 13060 :
           strcpy( mess,
               "TAS_TIO_STOUTNOAVAIL_CV: no output streams available");
           strcat( mess, " ");
           result = mess;
           break;
       case 13316 :
           strcpy( mess,
               "TAS_BADSTNAME_CV: stream name does not exist or is not initialized");
           strcat( mess, " ");
           result = mess;
           break;
       case 13572 :
           strcpy( mess,
               "TAS_GETTLERR_CV: error getting save table list");
           strcat( mess, " ");
           result = mess;
           break;
       case 13828 :
           strcpy( mess,
               "TAS_TAP_NOTNAME_CV: table name not found");
           strcat( mess, " ");
           result = mess;
           break;
       case 3585 :
           strcpy( mess,
               "TLS_SORT_NORMAL_CV: Sort or Search routine successful");
           strcat( mess, " ");
           result = mess;
           break;
       case 3844 :
           strcpy( mess,
               "TLS_SORT_ID_SHORT_CV: Array passed to Id_Offsets for indices is too small.");
           strcat( mess, " Some rows not included in index table.");
           result = mess;
           break;
       case 10498 :
           strcpy( mess,
               "TLS_SORT_ID_MULTIPLE_CV: Table has two or more keys with same value. (Id_Offsets)");
           strcat( mess, " ");
           result = mess;
           break;
       case 10756 :
           strcpy( mess,
               "TLS_SORT_IS_SHORT_CV: Array for sorted indices too short (Index_Sort)");
           strcat( mess, " ");
           result = mess;
           break;
       case 11012 :
           strcpy( mess,
               "TLS_SORT_QS_LONGROW_CV: Temporary storage space within quicksort too short.");
           strcat( mess, " (Quick_Sort)");
           result = mess;
           break;
       case 11268 :
           strcpy( mess,
               "TLS_SORT_QS_STACKSHORT_CV: Stack too short within quicksort (need recompile).");
           strcat( mess, " (Quick_Sort)");
           result = mess;
           break;
       case 11522 :
           strcpy( mess,
               "TLS_SORT_S_LISTSHORT_CV: Array to hold indices of matched keys too short. (Search)");
           strcat( mess, " ");
           result = mess;
           break;
       case 11779 :
           strcpy( mess,
               "TLS_SORT_S_NOMATCH_CV: Key not found in table. (Search)");
           strcat( mess, " ");
           result = mess;
           break;
       case 12034 :
           strcpy( mess,
               "TLS_SORT_SN_LISTSHORT_CV: List to hold indices of keys too short. (Search_Near)");
           strcat( mess, " ");
           result = mess;
           break;
       case 12291 :
           strcpy( mess,
               "TLS_SORT_SN_NOMATCH_CV: Matching key not found in table. (Search_Near)");
           strcat( mess, " ");
           result = mess;
           break;
       case 14082 :
           strcpy( mess,
               "TLS_SORT_SI_LISTSHORT_CV: Index array shorter than array to be sorted (Index_Sort)");
           strcat( mess, " ");
           result = mess;
           break;
       case 14339 :
           strcpy( mess,
               "TLS_SORT_SNI_NOMATCH_CV: Key not found (Search_Near_Index)");
           strcat( mess, " ");
           result = mess;
           break;
       case 14594 :
           strcpy( mess,
               "TLS_SORT_SNI_LISTSHORT_CV: Output array too small to hold all rows found");
           strcat( mess, " (Search_Near_Index)");
           result = mess;
           break;
       case 14851 :
           strcpy( mess,
               "TLS_SORT_SI_NOMATCH_CV: Key not found in specified column of table (Search_Index)");
           strcat( mess, " ");
           result = mess;
           break;
       default :
           result = NULL;
   }
   return result;
}
