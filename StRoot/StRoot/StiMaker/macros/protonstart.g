* ******************************************************
Module antiProtonSTART to prepare some routines
* ******************************************************
Author N.Smirnov
Created during Nov 00
* 
+CDE,AGECOM,GCONST.

 end

*
      Subroutine AgUsOpen(fname)
* ===============================

      implicit none

+CDE,gckine.
*
      character*64 fname
*  ..........................................................

      print *,'openning file=',fname,ikine 
      close(20,err=99)         
 99   OPEN (20,file=fname,status='OLD',Err=98)
      return
 98   print *,' file ',fname,' does not exist' 
      end


      Subroutine AgUsRead(ier)
*  ==============================

*      implicit none


+CDE,AGECOM,GCONST.

      integer i,j,nt,nv, Jpart, special
      real  PP(3),Ubuf(10)
      real pt,px,py,pz,xx, pzpt
      real  vert(3)/0.,0.,0./
      integer Ipart/6/
* momenutm of the particle!
*      real Ppart/1.1/
      real Ppart/1./
      real Rphi/297./
      real Rphi_lim/360./
      real pzpt_lim/0.9/
* number of particles
      integer jimbed/25/
      real vzlim/10./
* ................................................

	 
       if(jimbed .gt. 0 ) then

         vert(3) = vzlim*(2.*rndm(xx)-1.)

         call GSVERT(vert,0,0,Ubuf,0,nv)
	do i = 1, jimbed  
	  
	
	
	 Jpart = Ipart	 
	 pzpt = pzpt_lim*rndm(xx)
	 pt=Ppart/sqrt(1.+pzpt**2)
	 pz=pt*pzpt
	 if( rndm(xx) .le. 0.5) pz = -pz
	 phi= Rphi + Rphi_lim*(2.*rndm(xx)-1.)
	 phi = phi*degrad
	 if( (-1.)**i .eq. 1.) then
	  phi = phi + 180.0*degrad
	 else
	  phi = phi
	 endif

	 px= pt*cos(phi)
	 py= pt*sin(phi)

	 pp(1)=px
	 pp(2)=py
	 pp(3)=pz

	 call GSKINE(PP,Jpart,nv,Ubuf,0,nt)

	enddo

       endif

       return
       end
