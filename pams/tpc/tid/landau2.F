       real function landau2(x)

       double precision dpar(24),fitfun
       common/HCFITD/dpar,fitfun
       real newx,width,x

c
c       width=dpar(2)*0.13+0.13e-06
c
       width=dpar(3)
c   
       newx = (x-dpar(2))/width
       if ((newx.lt.-30.).or.(newx.gt.200)) then
        fitfun = 0.
        landau2 = 0.
       else
c        fitfun = hxamp*denlan(newx)   
  
        fitfun = dpar(1)*denlan(newx)
        landau2 = fitfun
       endif


       end

