#ifndef EMLLIB_INC
#define EMLLIB_INC
#endif  

c:----------------------------------------------------------------------
C: Duplicate emlLib.h 
c:----------------------------------------------------------------------

C: STAFCV - STAR Analysis Framework Condition Value
#define STAFCV_T        INTEGER*4

C: #define STAFCV_BAD      0
      INTEGER*4 stafcv_bad
      parameter (stafcv_bad=0)
C: #define STAFCV_OK       1
      INTEGER*4 stafcv_ok
      parameter (stafcv_ok=1)

#define EML_ERROR(code) EML_LOG_ERROR(code) \
		return .FALSE.
#define EML_SUCCESS(code) EML_LOG_SUCCESS(code) \
                return .TRUE.
#define EML_MESSAGE(code) WRITE(*,*)#code
#define EML_LOG_ERROR(code) EML_TRACE(#code) \
                set_staf_status(STAFCV_BAD)
#define EML_LOG_SUCCESS(code) set_staf_status(STAFCV_OK)
#define EML_TRACE(msg) WRITE(*,*)__FILE__,'-',__LINE__,msg
#define EML_WRITE WRITE(*,*)__FILE__,'-',__LINE__,

c:----------------------------------------------------------------------
C: KUIP Vectors        
c:----------------------------------------------------------------------

C: #define STAF_STATUS_VECTOR(A) IQ(STAF_STATUS_LLOW+ A-1 )
C: #define STAF_RESULT_VECTOR(A)  Q(STAF_RESULT_LLOW+ A-1 )

C: HACK ---- CHECK THIS HACK ---- endif //EMLLIB_INC

      INTEGER*4 NWPAW,IXPAWC,IHBOOK,IXHIGZ,IXKUIP,IFENCE,LMAIN
      REAL*4 WS 
      INTEGER*4 IQ,LQ
      REAL*4 Q
      INTEGER*4 IQUEST

c: From CERN Program Library Long Writeup I102 pg. 176

      COMMON /PAWC/ NWPAW,IXPAWC,IHBOOK,IXHIGZ,IXKUIP,IFENCE(5),
     +              LMAIN, WS(9989)
      DIMENSION IQ(1),Q(1),LQ(8000)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN)
C     COMMON /QUEST/ IQUEST(100)
*VP   COMMON /QUEST/ IQUEST(256)

c: Variables for STAF_STATUS and STAF_RESULT vectors.

      INTEGER*4 STAF_KUVEC_HISTORY
      PARAMETER (STAF_KUVEC_HISTORY = 256)

      CHARACTER*11 STAF_STATUS_NAME
      PARAMETER (STAF_STATUS_NAME = 'STAF_STATUS')
      CHARACTER*11 STAF_RESULT_NAME
      PARAMETER (STAF_RESULT_NAME = 'STAF_RESULT')

      INTEGER*4 STAF_STATUS_COUNT
      INTEGER*4 STAF_STATUS_LLOW,STAF_STATUS_LHIGH
      INTEGER*4 STAF_RESULT_COUNT
      INTEGER*4 STAF_RESULT_LLOW,STAF_RESULT_LHIGH

      COMMON /STAF_KUVEC_C/ STAF_STATUS_COUNT
     +                      , STAF_STATUS_LLOW,STAF_STATUS_LHIGH
     +                      , STAF_RESULT_COUNT
     +                      , STAF_RESULT_LLOW,STAF_RESULT_LHIGH

