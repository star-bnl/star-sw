******************************************************************************
MODULE   calaDig  Analyse event 
Author   A.Pavlinov, WSU.
Created  October-16-2001
******************************************************************************
     integer AgFHIT0,AgFHIT1,i,ih,itra,NumBV(5)
     real    hits(15),Esum
     real    x(10) ! array for ntuple 1 (see kumac)
     integer idntu/1/
*
*     print *,'<I>  = AgFHIT0("CALA","CALA")', AgFHIT0('CALA','CALA')
**     if(AgFHIT0('CALA','CALA') .eq. 0) then ! hits exist
     if(AgFHIT0('CALA','SCIN') .eq. 0) then ! hits exist
       do while (AgFHIT1(ih,itra,NumBV,hits) == 0)
         if(abs(ih).eq.1) then
           x(1) = hits(1)
           x(2) = hits(2)
         else
           x(1) = x(1) + hits(1)
           x(2) = x(2) + hits(2)
         endif
       enddo 
       call hf1(101, x(1), 1.)
       call hf1(102, x(2), 1.)
     else
       print *,'<I> AgFHIT0 said -> No  HITS for SCIN'
     endif

     if(AgFHIT0('CALA','LEDS') .eq. 0) then ! hits exist
       do while (AgFHIT1(ih,itra,NumBV,hits) == 0)
         if(abs(ih).eq.1) then
           x(3) = hits(1)
         else
           x(3) = x(3) + hits(1)
         endif
       enddo 
       call hf1(103, x(3), 1.)
       call hfn(idntu, x)
     else
       print *,'<I> AgFHIT0 said -> No  HITS for LEDS'
     endif
*
     end
