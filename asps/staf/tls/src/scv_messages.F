      SUBROUTINE SCV_MESSAGES( ivalue, message )
      IMPLICIT NONE
      INTEGER ivalue         ! input: condition value to be translated
      CHARACTER*(*) message  ! output: condition value message text
C
C  created by  ecvmsgf.ace on  08-Dec-1993
C
C   SCV_MESSAGES translates a condition value (ivalue) into its
C   associated text value (message).
C
      IF( ivalue .le. 0 ) THEN
          message = 'SCV- invalid condition value'
      ELSEIF( ivalue .eq. 257 ) THEN
          message = 'TAS_OK_CV: '
     +     //' general TAS success value'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 514 ) THEN
          message = 'TAS_TBCODE_NOMATCH_CV: '
     +     //' did not match table descriptor parameter'
     +     //'  code'
     +     //' '
      ELSEIF( ivalue .eq. 1540 ) THEN
          message = 'TAS_TABNOTINIT_CV: '
     +     //' the table is not initialized'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 1796 ) THEN
          message = 'TAS_EVCOMNOSPACE_CV: '
     +     //' no more space available in tas_event_com'
     +     //'  to register more   tables'
     +     //' '
      ELSEIF( ivalue .eq. 2052 ) THEN
          message = 'TAS_BADMODNUM_CV: '
     +     //' illegal value for analysis module handle'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 2308 ) THEN
          message = 'TAS_AMTABCONFUSED_CV: '
     +     //' list of tables for analysis module is co'
     +     //' nfused in tam_com'
     +     //' '
      ELSEIF( ivalue .eq. 2564 ) THEN
          message = 'TAS_TABREGEXIST_CV: '
     +     //' table name is already registered with TA'
     +     //' S, can not register again'
     +     //' '
      ELSEIF( ivalue .eq. 2820 ) THEN
          message = 'TAS_AMREGEXIST_CV: '
     +     //' module name already exists'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 3074 ) THEN
          message = 'TAS_NOMOREDTS_CV: '
     +     //' the DTS routines from EOS are no longer'
     +     //' suported'
     +     //' '
      ELSEIF( ivalue .eq. 3332 ) THEN
          message = 'TAS_MACHDUMMY_CV: '
     +     //' this is a dummy routine on this type of'
     +     //' machine.'
     +     //' '
      ELSEIF( ivalue .eq. 4100 ) THEN
          message = 'TAS_BADTFTYPE_CV: '
     +     //' bad format type for table file'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 4356 ) THEN
          message = 'TAS_NOLUN_CV: '
     +     //' no logical unit available'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 4612 ) THEN
          message = 'TAS_NOZTABLN_CV: '
     +     //' no ztablinks avaiable'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 4868 ) THEN
          message = 'TAS_FNOTABWR_CV: '
     +     //' no tables found to write out'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 5124 ) THEN
          message = 'TAS_BADSTNUM_CV: '
     +     //' bad stream number'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 5380 ) THEN
          message = 'TAS_BADSTPAR_CV: '
     +     //' bad value for which parameter in the tas'
     +     //' _tio_spar_* parameter set (in tas_tio.in'
     +     //' c)'
      ELSEIF( ivalue .eq. 5636 ) THEN
          message = 'TAS_ERRDTSOPEN_CV: '
     +     //' error on dts open'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 5892 ) THEN
          message = 'TAS_STRNOTINIT_CV: '
     +     //' stream is not initialized'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 6148 ) THEN
          message = 'TAS_UNKSTMODE_CV: '
     +     //' unknown I/O mode for stream (not input o'
     +     //' r output)'
     +     //' '
      ELSEIF( ivalue .eq. 6404 ) THEN
          message = 'TAS_FZOPENFAIL_CV: '
     +     //' fzopen failed'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 6658 ) THEN
          message = 'TAS_CLSTNOP_CV: '
     +     //' trying to close a stream which is not op'
     +     //' en'
     +     //' '
      ELSEIF( ivalue .eq. 6916 ) THEN
          message = 'TAS_BADSFTPO_CV: '
     +     //' bad format type of previously open strea'
     +     //' m, this can not happen, tell someone kno'
     +     //' wledgeable.'
      ELSEIF( ivalue .eq. 7172 ) THEN
          message = 'TAS_SCLOSERR_CV: '
     +     //' error closing stream'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 7428 ) THEN
          message = 'TAS_UNKSIOTYP_CV: '
     +     //' unknown stream I/O type, not f77, C fd o'
     +     //' r Zebra CF'
     +     //' '
      ELSEIF( ivalue .eq. 7684 ) THEN
          message = 'TAS_UNKTLIST_CV: '
     +     //' unknown table list type'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 7940 ) THEN
          message = 'TAS_TLISTINVALID_CV: '
     +     //' invalid table list type for operation'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 8196 ) THEN
          message = 'TAS_INVSTMODE_CV: '
     +     //' invalid I/O mode for stream operation'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 8452 ) THEN
          message = 'TAS_SETTLERR_CV: '
     +     //' error setting table list, probably did n'
     +     //' ot match a table name'
     +     //' '
      ELSEIF( ivalue .eq. 8708 ) THEN
          message = 'TAS_STNOP_CV: '
     +     //' stream is not open'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 8964 ) THEN
          message = 'TAS_INVSTFORMAT_CV: '
     +     //' invalid stream format for operation'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 9220 ) THEN
          message = 'TAS_NOTFZSTF_CV: '
     +     //' Not an FZ format stream'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 9476 ) THEN
          message = 'TAS_FZSGERR_CV: '
     +     //' error getting event in FZ stream'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 9732 ) THEN
          message = 'TAS_FZSZPERR_CV: '
     +     //' error writing FZ event'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 9988 ) THEN
          message = 'TAS_STRWLOV_CV: '
     +     //' requested more tables in read/write list'
     +     //'  than there is space'
     +     //' '
      ELSEIF( ivalue .eq. 10244 ) THEN
          message = 'TAS_STNAMUSE_CV: '
     +     //' stream name already in use'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 3585 ) THEN
          message = 'TLS_SORT_NORMAL_CV: '
     +     //' Sort or Search routine successful'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 3844 ) THEN
          message = 'TLS_SORT_ID_SHORT_CV: '
     +     //' Array passed to Id_Offsets for indices i'
     +     //' s too small.        Some rows not includ'
     +     //' ed in index table.'
      ELSEIF( ivalue .eq. 10498 ) THEN
          message = 'TLS_SORT_ID_MULTIPLE_CV: '
     +     //' Table has two or more keys with same val'
     +     //' ue. (Id_Offsets)'
     +     //' '
      ELSEIF( ivalue .eq. 10756 ) THEN
          message = 'TLS_SORT_IS_SHORT_CV: '
     +     //' Array for sorted indices too short (Inde'
     +     //' x_Sort)'
     +     //' '
      ELSEIF( ivalue .eq. 11012 ) THEN
          message = 'TLS_SORT_QS_LONGROW_CV: '
     +     //' Temporary storage space within quicksort'
     +     //'  too short.         (Quick_Sort)'
     +     //' '
      ELSEIF( ivalue .eq. 11268 ) THEN
          message = 'TLS_SORT_QS_STACKSHORT_CV: '
     +     //' Stack too short within quicksort (need r'
     +     //' ecompile).          (Quick_Sort)'
     +     //' '
      ELSEIF( ivalue .eq. 11522 ) THEN
          message = 'TLS_SORT_S_LISTSHORT_CV: '
     +     //' Array to hold indices of matched keys to'
     +     //' o short. (Search)'
     +     //' '
      ELSEIF( ivalue .eq. 11779 ) THEN
          message = 'TLS_SORT_S_NOMATCH_CV: '
     +     //' Key not found in table. (Search)'
     +     //' '
     +     //' '
      ELSEIF( ivalue .eq. 12034 ) THEN
          message = 'TLS_SORT_L_LISTSHORT_CV: '
     +     //' List to hold indices of keys too short.'
     +     //' (Locate)'
     +     //' '
      ELSEIF( ivalue .eq. 12291 ) THEN
          message = 'TLS_SORT_L_NOMATCH_CV: '
     +     //' Matching key not found in table. (Locate'
     +     //' )'
     +     //' '
      ELSE
          message = 'SCV- unknown condition value'
      ENDIF
      RETURN
      END
