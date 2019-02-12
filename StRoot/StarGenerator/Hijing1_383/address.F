      Function address_of_hiparnt()
      use iso_c_binding
      type(c_ptr)  :: address_of_hiparnt
      COMMON/HIPARNT/HIPR1(100),IHPR2(50),HINT1(100),IHNT2(50)
      Integer, Target :: hipr1
      address_of_hiparnt = c_loc( hipr1(1) )
      return 
      End

      Function address_of_himain1()
      use iso_c_binding
      type(c_ptr) :: address_of_himain1
      COMMON/HIMAIN1/NATT,EATT,JATT,NT,NP,N0,N01,N10,N11
      integer, target :: natt
      address_of_himain1 = c_loc( natt )
      return 
      End

      Function address_of_himain2()
      use iso_c_binding
      type(c_ptr) :: address_of_himain2
      COMMON/HIMAIN2/KATT(130000,4),PATT(130000,4),VATT(130000,4)
      integer, target :: katt
      address_of_himain2 = c_loc(katt(1,1))
      return
      End

      Function address_of_ludat3()
      use iso_c_binding
      type(c_ptr) :: address_of_ludat3
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5) 
      integer, target :: mdcy
      address_of_ludat3 = c_loc( mdcy(1,1) )
      return
      End
