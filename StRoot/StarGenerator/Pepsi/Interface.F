! ---------------------------------------------------------------------------
! ---------------------------------------------------------------------------
! ---------------------------------------------------------------------------
      FUNCTION get_leptou()
      Integer LOC, get_leptou

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST      

      get_leptou = LOC( CUT(1) )

      RETURN
      END
! ---------------------------------------------------------------------------
! ---------------------------------------------------------------------------
! ---------------------------------------------------------------------------
      FUNCTION get_ludat1()

      Integer LOC, get_ludat1
      
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      get_ludat1 = LOC( MSTU(1) )

      RETURN
      END
! ---------------------------------------------------------------------------
! ---------------------------------------------------------------------------
! ---------------------------------------------------------------------------
      FUNCTION get_lujets()
      INTEGER LOC, get_lujets

      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V

      get_lujets = LOC(N)

      RETURN
      END

      
! ---------------------------------------------------------------------------
! ---------------------------------------------------------------------------
! ---------------------------------------------------------------------------
      FUNCTION get_ludat3()
      INTEGER LOC, get_ludat3

      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5) 
 
      get_ludat3 = LOC(N)

      RETURN
      END

      
