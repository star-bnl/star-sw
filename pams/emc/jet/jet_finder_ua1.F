c:>------------------------------------------------------------------
C:ROUTINE:      subroutine UA1
C:DESCRIPTION:  UA1 jet algorithm from LUND JETSET called from EMC_erj
C:ARGUMENTS:    emc_energy_h, emc_energy,
C:ARGUMENTS:    et_tot, EMC_jetpar, ierror
C:RETURN VALUE: ierror=1 on error
c:>------------------------------------------------------------------
      subroutine jet_finder_UA1(
     +        ncell, ncell_tot, etc, etac, phic,
     +        njet,  etj, etaj, phij, ncellj,
     +        cone_rad, et_seed, ej_min, et_min,
     +        min_move, max_move, mode, prec_bg,
     +        ierror)
      INTEGER NMAX,JMAX
      parameter(NMAX=10000,JMAX=100)
      integer ncell, njet, ierror, ncellj(JMAX), mode, ncell_tot
      real etc(NMAX),etac(NMAX),phic(NMAX)
      real etj(JMAX),etaj(JMAX,2),phij(JMAX,2)
      real cone_rad, et_seed, ej_min, et_min
      real min_move, max_move, prec_bg
      real pi,twopi
      common/erjkeep/ pi,twopi
      integer flag(NMAX),index(NMAX)
      integer n_iter,i,j,k,l,nc
      real et_sum,et_ave,et_sum_old
      real et,eta,phi,eta0,phi0,etab,phib,ets,etas,phis
      real deta,dphi,r_int

      ierror =0
      n_iter =0
c*-sort cells by Et decending order, store the order in index
      call sortzv(etc,index,ncell,-1,1,0)   
c*-sum up total energy of all cells
      if(mode .eq. 1) then
         n_iter=1
         et_sum=0.0
         do i=1, ncell
            et_sum=et_sum+etc(i)
         enddo
         et_sum_old=et_sum
         et_ave=et_sum/float(ncell_tot)
      else
         et_ave=0.0
      endif
c*-Watch out!!! For mode=1, it can jump back to here for next iteration!!!
 999  continue
c*-kill cells (flag=2) with Et below ET_MIN after background subtraction
      call vzero(flag,ncell)
      do i=1, ncell
         if(etc(i)-et_ave .le. et_min) flag(i)=2
      enddo
      njet = 0
c*-Initiator cell is the one with highest Et of not yet used ones
      i=1
      j=index(i)
      do while(etc(j) .ge. et_seed)
         if(flag(j) .eq. 0) then
            et =etc(j)-et_ave
            eta=etac(j)
            phi=phic(j)
            eta0=eta
            phi0=phi
            etab=eta
            phib=phi
            ets =0.0
            etas=0.0
            phis=0.0
c*-weighted eta and phi. 
            do k= 1, ncell
               l=index(k)
               if(flag(l).eq.0) then
                  deta=etac(l)-eta
c*-Is the cell is in the cone?
                  if(abs(deta).le.cone_rad)then
                     dphi=phic(l)-phi
                     do while(dphi .gt. pi)
                        dphi=dphi-twopi
                     enddo
                     do while(dphi .le. -pi)
                        dphi=dphi+twopi
                     enddo
                     if(abs(dphi).le.cone_rad) then
                        r_int=sqrt(deta**2+dphi**2)
                        if(r_int.le.cone_rad)then
c*-calculate offset from initiate cell
                           deta=etac(l)-eta0
                           dphi=phic(l)-phi0
                           do while(dphi .gt. pi)
                              dphi=dphi-twopi
                           enddo
                           do while(dphi .lt. -pi)
                              dphi=dphi+twopi
                           enddo
                           et=etc(l)-et_ave
                           etas=etas+abs(et)*deta
                           phis=phis+abs(et)*dphi
                           ets =ets +et
c*-New weighted eta and phi including this cell
                           eta=eta0+etas/ets
                           phi=phi0+phis/ets                          
c*-If cone does not move much from previous cone, just go next step
                           r_int=sqrt((eta-etab)**2+(phi-phib)**2)
                           if(r_int .le. min_move) then
                              goto 159
                           endif
c*-Cone should not move more than MAX_CONE_MOVE from initiator cell
                           r_int=sqrt((etas/ets)**2+(phis/ets)**2)              
                           if(r_int .ge. max_move) then
                              eta=etab
                              phi=phib
                              goto 159
                           endif
c*-Store this loop information
                           etab=eta
                           phib=phi
                        endif
                     endif
                  endif
               endif
            enddo
 159        continue 
            
c*-sum up unused cells within required distance of given eta/phi
            nc=0
            ets=0.0
            etas=0.0
            phis=0.0
            do k= 1, ncell
               l=index(k)
               if(flag(l) .eq. 0) then
                  deta=etac(l)-eta
                  if(abs(deta).le.cone_rad)then
                     dphi=phic(l)-phi
                     do while(dphi .gt. pi)
                        dphi=dphi-twopi
                     enddo
                     do while(dphi .le. -pi)
                        dphi=dphi+twopi
                     enddo
                     if(abs(dphi).le.cone_rad) then
                        r_int=sqrt(deta**2+dphi**2)
                        if(r_int.le.cone_rad)then
                           flag(l)=-1
                           et  =etc(l)-et_ave
                           ets =ets +et
                           etas=etas+et*deta
                           phis=phis+et*dphi
                           nc  = nc + 1
                        endif
                     endif
                  endif
               endif
            enddo
            
c*-reject cluster below minimum Ej_min
            etas=eta+etas/ets
            if(ets*cosh(etas/ets).lt.ej_min) then
               do k=1,ncell
                  if(flag(k).le.0) flag(k)=0
               enddo
            else
c*-eles, store flags and jet variables
               do k=1,ncell
                  if(flag(k).eq.-1) flag(k)=1
               enddo
               phi=phi+phis/ets
               do while(phi .ge. twopi)
                  phi=phi-twopi
               enddo
               do while(phi .lt. 0.0)
                  phi=phi+twopi
               enddo
               njet=njet+1
               etj(njet) =ets
               etaj(njet,1)=eta0
               phij(njet,1)=phi0
               etaj(njet,2)=etas
               phij(njet,2)=phi
               ncellj(njet)=nc
            endif 
         endif
         i=i+1
         j=index(i)        
      enddo
      
c*-recalculate energy sum excluding used cells.
      if(mode.eq.1)then
         et_sum=0.0
         nc=0
         do i=1,ncell
            if(flag(i).ne.1)then
               et_sum=et_sum+etc(i)
            else
               nc=nc+1
            endif
         enddo
c*-if background level changes more than prec_bg, go next iteration!!!
c*-after 10 iteration, stop working and finish
         write(*,*) njet,' jet with ',n_iter,' iteration(s) : ',
     +     et_sum,et_sum/float(ncell_tot-nc)
         if(abs(et_sum-et_sum_old)/et_sum .gt. prec_bg .and.
     +      n_iter .le. 10)then
            et_ave=et_sum/float(ncell_tot-nc)
            n_iter=n_iter+1
            et_sum_old=et_sum
            goto 999
c*-Watch out!!! Here is a big jump!!! 
         endif
      endif
      
      if(njet.gt.100)then
         write(*,*)'UA1:Problem:More than 100 jets found!'
         ierror = 1
      elseif(njet.eq.0)then
         write(*,*)'UA1:Done:No jet found!'
      else
         write(*,*)
     +'UA1:Done:Found ',njet,' jet(s)'
      end if
      return
      end
