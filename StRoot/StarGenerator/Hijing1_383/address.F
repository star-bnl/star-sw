      Function address_of_hiparnt()
      Integer address_of_hiparnt, loc
      COMMON/HIPARNT/HIPR1(100),IHPR2(50),HINT1(100),IHNT2(50)
      address_of_hiparnt = loc( hipr1(1) )
      return 
      End

      Function address_of_himain1()
      Integer address_of_himain1, loc
      COMMON/HIMAIN1/NATT,EATT,JATT,NT,NP,N0,N01,N10,N11
      address_of_himain1 = loc( natt )
      return 
      End

      Function address_of_himain2()
      Integer address_of_himain2, loc
      COMMON/HIMAIN2/KATT(130000,4),PATT(130000,4),VATT(130000,4)
      address_of_himain2 = loc(katt(1,1))
      return
      End

      Function address_of_ludat3()
      Integer address_of_ludat3, loc
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5) 
      address_of_ludat3 = loc( mdcy(1,1) )
      return
      End
