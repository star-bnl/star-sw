      Function address_of_pyjets()
      use iso_c_binding
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      type(c_ptr) :: address_of_pyjets
      COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
      integer, target :: n
      address_of_pyjets = c_loc(N)
      return
      End

      Function address_of_pydat1()
      use iso_c_binding
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      type(c_ptr) :: address_of_pydat1

      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      integer, target :: mstu

      address_of_pydat1 = C_LOC(MSTU(1))
      return
      End

      Function address_of_pydat2()
      use iso_c_binding
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      type(c_ptr) :: address_of_pydat2

      COMMON/PYDAT2/KCHG(500,4),PMAS(500,4),PARF(2000),VCKM(4,4)
      integer, target :: kchg

      address_of_pydat2 = C_LOC( KCHG(1,1) )
      return 
      End

      Function address_of_pydat3()
      use iso_c_binding
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      type(c_ptr) :: address_of_pydat3


      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
      integer, target :: mdcy 

      address_of_pydat3 = C_LOC(MDCY(1,1))
      return
      End

      Function address_of_pysubs()
      use iso_c_binding
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      type(c_ptr) :: address_of_pysubs

      COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)
      integer, target :: msel

      address_of_pysubs = C_LOC(MSEL)
      return
      End

      Function address_of_pypars()
      use iso_c_binding
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      type(c_ptr) :: address_of_pypars

      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      integer, target :: mstp

      address_of_pypars = c_loc(mstp(1))
      return
      End

      Function address_of_pyint5()
      use iso_c_binding
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)

      type(c_ptr) :: address_of_pyint5

      COMMON/PYINT5/NGENPD,NGEN(0:500,3),XSEC(0:500,3)
      integer, target :: ngenpd, ngen

      address_of_pyint5 = c_loc(NGEN(0,1))! Wait, padding?
      return
      End

