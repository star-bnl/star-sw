****************************************************************************
                SUBROUTINE  A G U S T E P
*                                                                          *
*  Description: process one geant  step throu a volume                     *
****************************************************************************
+CDE,TYPING,GCBANK,GCUNIT,GCFLAG,GCKING,GCKINE,GCTRAK,GCVOLU,GCMATE.
       character cvol*4
       Integer   i,ns
       Real      VMOD
       Real e, de, r, rs, rl, drl
       common /mymate/ns,rs(100),de(100),e(100),rl(100),drl(100)


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
*         print *,' nstep = ',nstep
         if (nstep==1) ns=0
         if (step <=0)then
*          print *,' step = ',step 
          return
         endif
         r = vmod(vect,2)
         if (r>60)then 
*          print *,' r = ', r 
          istop =1 
          return
         endif
         call UHTOC(names(nlevel),4,cvol,4)  ! current volume name
*         print *,' nstep,ns,r,x,y,z,destep,e,step,sleng,radl ='
*         print *,nstep,ns,r,(vect(i),i=1,3),destep,e(ns),step,sleng,radl,' ',cvol
         ns=ns+1
         if (ns>100)then
*          print *,' ns = ',ns
          return
         endif
         if(ns.eq.1)then
          e(1)=destep
          de(1)=destep
          rl(1)= step/radl
          drl(1)= step/radl
         else
          de(ns)=destep
          e(ns)=e(ns-1)+destep
          rl(ns)=rl(ns-1)+step/radl
          drl(ns)=step/radl
         endif
         rs(ns)=r
        
 END
 
 

