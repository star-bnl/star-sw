/*------------------------------------------------------------------
fortran filename   : /star/starlib/star/dev/sys/tls/src/scv_state.F
------------------------------------------------------------------*/
/* [00755]LEXICAL-WARNING (/star/starlib/star/dev/sys/tls/src/scv_state.F) line 4
    ==>INTEGER IVALUE  ! CONDITION VALUE TO BE CHECKED */
/* [00755]LEXICAL-WARNING (/star/starlib/star/dev/sys/tls/src/scv_state.F) line 6
    ==>CHARACTER*(1) FLAG  ! INDICATES LEVEL OF SEVERITY BEING CHECKED */
/*
#define scv_state_ELEMS_3          ZTRINGV_NUM(1)
#define scv_state_ELEMLEN_3        ZTRINGV_NUM(0)
*/

 PROTOCCALLSFFUN2(LOGICAL,SCV_STATE,scv_state,INT,STRING)
#define SCV_STATE(A2,A3)  CCALLSFFUN2(SCV_STATE,scv_state,INT,STRING,A2,A3)

