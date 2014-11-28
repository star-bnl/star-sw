      Function address_of_pyjets()
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      Integer address_of_pyjets
      Integer LOC

      COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
      address_of_pyjets = loc(N)
      return
      End

      Function address_of_pydat1()
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      Integer address_of_pydat1
      Integer LOC

      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)

      address_of_pydat1 = LOC(MSTU(1))
      return
      End

      Function address_of_pydat2()
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      Integer address_of_pydat2
      Integer LOC

      COMMON/PYDAT2/KCHG(500,4),PMAS(500,4),PARF(2000),VCKM(4,4)

      address_of_pydat2 = LOC( KCHG(1,1) )
      return 
      End

      Function address_of_pydat3()
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      Integer address_of_pydat3
      Integer LOC

      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)

      address_of_pydat3 = LOC(MDCY(1,1))
      return
      End

      Function address_of_pysubs()
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      Integer address_of_pysubs
      Integer LOC

      COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)

      address_of_pysubs = LOC(MSEL)
      return
      End

      Function address_of_pypars()
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      Integer address_of_pypars
      Integer LOC      

      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      address_of_pypars = loc(mstp(1))
      return
      End

      Function address_of_pyint5()
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      Integer address_of_pyint5
      Integer LOC

      COMMON/PYINT5/NGENPD,NGEN(0:500,3),XSEC(0:500,3)
      address_of_pyint5 = loc(NGEN(0,1))
      return
      End

