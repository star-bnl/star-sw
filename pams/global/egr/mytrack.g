      subroutine mytrack (vert,px,py,pz,id)
      implicit  none 
+CDE,GCPHYS.

      real      px,py,pz,ub(100)
      integer   id,iv,it
      real      vert(3),plab(3)
      Logical   first/.true./

      if (first) then
          first=.false.
          {IDCAY,IANNI,IBREM,ICOMP,IHADR,IMUNU,IPAIR,IPHOT,IDRAY,IMULS}=0
          ILOSS=4
          Call GGCLOS
          Call AGPHYSI
      endif

      call GTRIGC
      call GTRIGI
    
      call gsvert(vert,0,0,ub,0, iv)
      if (iv<=0) return

      plab(1)=px
      plab(2)=py
      plab(3)=pz
      call gskine(plab,id,iv,ub,0, it)
      if (it<=0) return

      call GTReve
      end
