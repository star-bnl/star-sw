      subroutine pion_energy_deposit(rl,p_tot,ener_subt,err_subt)
c*-
c*- Give most probable energy deposit in EMC for pion.
c*- This parametrization was written by B. Christie.
c*- rl=3 for Barrel and rl=1,2 for endcap.
c*- For the moment, same program/parameters gives most probable
c*- energy deposoit due to charged track. This should be chagnged!
c*- I guess energy deposit should be a function of energy, not p_tot?
c*- Estimation of error has not yet impremented.
c*- 
      IMPLICIT NONE
      integer rl
      real p_tot, ener_subt,err_subt
      if(rl.ge.1 .and. rl.le.3)then
         if(p_tot.le.0.35)then
            ener_subt = 0.049*p_tot
         elseif(p_tot.le.0.45)then
            ener_subt = 0.0435*p_tot
         elseif(p_tot.le.0.55)then
            ener_subt = 0.038*p_tot
         elseif(p_tot.le.0.65)then
            ener_subt = 0.032*p_tot
         elseif(p_tot.le.0.75)then
            ener_subt = 0.026*p_tot
         elseif(p_tot.le.0.85)then
            ener_subt = 0.025*p_tot
         elseif(p_tot.le.1.00)then
            ener_subt = 0.024*p_tot
         elseif(p_tot.le.1.30)then
            ener_subt = 0.022*p_tot
         elseif(p_tot.le.2.30)then
            ener_subt = 0.019*p_tot
         else
            ener_subt = 0.040
         endif      
      else
         write(*,*)'PION_ENERGY_DEPOSIT:Wrong detector:',rl
      endif
      return
      end
