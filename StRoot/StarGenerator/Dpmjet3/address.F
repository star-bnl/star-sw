      Function address_of_dtevt1()
      use iso_c_binding
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      type(c_ptr) :: address_of_dtevt1
      COMMON /DTEVT1/ NHKK,NEVHKK,ISTHKK(200000),IDHKK(200000),
     &                JMOHKK(2,200000),JDAHKK(2,200000),
     &                PHKK(5,200000),VHKK(4,200000),WHKK(4,200000)
      integer, target :: NHKK
      address_of_dtevt1 = c_loc(NHKK)
      return
      End

      Function address_of_dtglcp()
      use iso_c_binding
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      type(c_ptr) :: address_of_dtglcp
      COMMON /DTGLCP/ RPROJ,RTARG,BIMPAC,
     &                NWTSAM,NWASAM,NWBSAM,NWTACC,NWAACC,NWBACC
      integer, target :: RPROJ
      address_of_dtglcp = c_loc(RPROJ)
      return
      End
