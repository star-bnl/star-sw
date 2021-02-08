      Function address_of_agcpart()
      Integer :: address_of_agcpart
      Integer :: loc
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,    
     >     AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                         
      address_of_agcpart = loc(AG_CODE)
      Return
      End
       

      FUNCTION address_of_agcloca()
      Integer :: address_of_agcloca
      Integer :: loc
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       
     >AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  
     >AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         
     >AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    
     >AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          
     >AG_OPTION    
      address_of_agcloca = loc(ag_begscr)
      Return
      End
