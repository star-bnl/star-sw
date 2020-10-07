c234567
      Subroutine closeDecays(id)

      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5) 
      LOGICAL first
      DATA first /.TRUE./
      INTEGER value, iwas
      CHARACTER *16 name, chad(5)
c--
      value=0
c--
c     If (first) Then      ! GEXEC calls after compilation
c        first=.False.
c        Return
c     EndIf
c--
c--   Output the name of the particle being modified, and the
c--   channels which are being closed
c--         
      Call pyname(id,name)
      Write (*,*) 'Closing decay channels for: ', name
c--   Loop over all channels for this decay
      istart  = MDCY(id,2)
      ifinish = MDCY(id,3) + istart - 1
      Do i=istart,ifinish
         iwas=MDME(i,1)
         MDME(i,1) = value
c--   Loop over decay products, obtain and print names
         Do 280 J=1,5
            Call pyname(KFDP(I,J),CHAD(J))
 280     Continue
         Write (*,100) i, name, (CHAD(J),J=1,5)
      EndDo
      Return
 100  Format(I4,' ',A16, ' --> ',5A16,' closed')
      End! subroutine setDecays


      Subroutine openDecay(id,idcy,value)
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5) 
      CHARACTER *16 name, chad(5)
      Integer value
      Call pyname(id,name)
      istart  = MDCY(id,2)
      ifinish = MDCY(id,3) + istart
      If ( idcy .lt. istart .or. idcy .gt. ifinish ) Then
         Write (*,*) 'WARNING: Decay channel ',idcy,' out of range for '
     >               ,name
         Return
      EndIf
      Write (*,*) 'Opening decay channel ',idcy,' for: ',name
c--   Loop over decay products, obtain and print names
      Do 280 J=1,5
         Call pyname(KFDP(IDCY,J),CHAD(J))
 280  Continue
      Write (*,100) idcy, name, (CHAD(J),J=1,5), value
      MDME(idcy,1)=value
      Return
 100  Format(I4,' ',A16, ' --> ',5A16,' value=',I4)
      End
