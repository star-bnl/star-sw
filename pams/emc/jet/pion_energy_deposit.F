      subroutine pion_energy_deposit(det,p_tot,ener_subt,err_subt)
c*-
c*- Give most probable energy deposit in EMC for pion.
c*- This parametrization was written by B. Christie.
c*- For the moment, same program/parameters gives most probable
c*- energy deposoit due to charged track. This should be chagnged!
c*- I guess energy deposit should be a function of energy, not p_tot?
c*- Estimation of error has not yet impremented.
c*- 
#include "emc_def.h"
      IMPLICIT NONE
      integer det
      real p_tot, ener_subt,err_subt
      if(det.eq.BEMC .or. det.eq.EEMC)then
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
         err_subt=ener_subt
      else
         write(*,*)'PION_ENERGY_DEPOSIT:Wrong detector:',det
      endif
      return
      end
