
      SUBROUTINE PHO_CT14GETPARS(Xmin,Qini,Qmax,Nloops,Nfl)
C Get various parameters associated with the PDF grid
C Output: xmin  is the minimal value of x
C         Qmin  is the initial Q scale
C         Qmax  is the maximal Q scale
C         Nloop is the number of QCD loops
C         Nfl   is the maximal number of quark flavors
      IMPLICIT NONE
      DOUBLE PRECISION QINi0 , QMAx0 , XMIn0 , Xmin , Qini , Qmax
      INTEGER Nloops , IPK , IORder , Nfl , NFL0
      DOUBLE PRECISION ALFaq , QALfa
 
      COMMON /XQRANGE/ QINi0 , QMAx0 , XMIn0
      COMMON /QCDTBL/ ALFaq , QALfa , IPK , IORder , NFL0
 
      Qini = QINi0
      Nloops = IORder - 1
 
      END SUBROUTINE
