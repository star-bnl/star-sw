      subroutine mytrack (vert1,px,py,pz,id)
+CDE,TYPING,GCBANK,GCUNIT,GCFLAG,GCKING,GCKINE,GCTRAK,GCVOLU,GCMATE,GCPHYS.
      real      px,py,pz,ub(100)
      integer   id,iv,it
      real      vert1(3),plab(3)
       character cvol*4
       Integer   i,ns
       Real      VMOD
       Real e, de, r, rs, rl, drl
       common /mymate/ns,rs(100),de(100),e(100),rl(100),drl(100)
       Integer numv(15) /15*0/
       Integer iovlIn(2) /2*1/
       Integer Npred /0/
       Character *4 NameV(2) /'TPAD','TPA1'/
       Real x1(3),p1(3),x2(3),p2(3)
       Character *3 VoptB /'VO'/
       Real inputError(15)/15*0/
       Integer idParticle
       external resetNlevel
       idParticle = id
       ns = 0      
       p1(1)  = px
       p1(2)  = py
       p1(3)  = pz
       if (px**2 + py**2 + pz**2 .le. 1.e-4) return
       call ucopy(vert1,x1,3)
       call Eufilv (Npred, inputError, NameV, numv, iovlIn);
100    continue
       if (p1(1)**2 + p1(2)**2 + p1(3)**2 .le. 1.e-18) then 
C	 write (*,*) 'mytrack: Momentum is too low: ns = ', ns , ' id = ', id
C	 write (*,*) 'vert1(1) =', vert1(1), ' vert1(2) =', vert1(2), ' vert1(3) = ',vert1(3)
C	 write (*,*) 'px =', px, ' py =', py, ' pz = ',pz
C	 write (*,*) 'p1(1) =', p1(1), ' p1(2) =', p1(2), ' p1(3) = ',p1(3)
	 istop =1 
         return
       endif
       call Ertrak(x1,p1,x2,p2,idParticle,VoptB);
* initial: 
* Pvert(3) - px,py,pz at vertex
* amass    - particle mass

* current:
* ntmult   - sequential track number inside trig
* itra     - input track number
* vect(7)  - x,y,z,cx,xy,cz,p
* Getot    - total energy
* Gekin    - kinetic energy
* Nstep    - step number
* Destep   - current energy loss
* step     - current step
* inwvol   - 1: enter, 2:exit, 0: step inside

*         print *,' DEBUG in agustep '
*         print *,' nstep = ',nstep, ' step = ', step, ' destep =', destep 
         if (sleng <=0)then
#if 0
          print *,' sleng = ',sleng 
#endif
          return
         endif
         r = vmod(x2,2)
         if (r>60)then 
#if 0
          print *,' r = ', r 
#endif
          istop =1 
          return
         endif
         ns=ns+1
         if (ns>100)then
          print *,' ns = ',ns
          return
         endif
         if(ns.eq.1)then
          e(1)=destep
          de(1)=destep
          rl(1)= sleng/radl
          drl(1)= sleng/radl
         else
          de(ns)=destep
          e(ns)=e(ns-1)+destep
          rl(ns)=rl(ns-1)+sleng/radl
          drl(ns)=sleng/radl
         endif
         rs(ns)=r
	 call ucopy(x2,x1,3)
         call ucopy(p2,p1,3)
#if 0
         call UHTOC(names(nlevel),4,cvol,4)  ! current volume name
	print *,'ns,r:',ns,r
	print *,'(x2(i),i=1,3):',(x2(i),i=1,3)
	print *,'de(ns),e(ns):',de(ns),e(ns)
	print *,'rl(ns),drl(ns):',rl(ns),drl(ns)
	print *,'step,sleng,radl,cvol:',step,sleng,radl,' ',cvol
#endif
         go to 100       
      end
      subroutine resetNlevel
+CDE, GCVOLU.
      nlevel = 0
      end
