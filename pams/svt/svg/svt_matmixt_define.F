      subroutine svt_matmixt_define

      IMPLICIT NONE
C     
      INTEGER NMAT,ISVOL,IFIELD,NWBUF
C     
      REAL FIELDM,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN, AAA(3), ZZZ(3),
     &     WMAT(3),UBUF(10),DDD
      
      integer		gustmed,gusmixt,gusmate
      integer           istat
      REAL A_arr(2), Z_arr(2), W_arr(2)
      REAL A_arr2(2), Z_arr2(2), W_arr2(2)

c cp attention Oxigen has z=8 and a=16 NOT 16,32...
c cp      DATA A_arr,Z_arr,W_arr /26.98,32.01,13.,16.,2.,3./	
c cp      DATA A_arr2,Z_arr2,W_arr2 /1.,32.01,1.,16.,2.,1./	
      DATA A_arr,Z_arr,W_arr /26.98,16.01,13.,8.,2.,3./	
      DATA A_arr2,Z_arr2,W_arr2 /1.,16.01,1.,8.,2.,1./	

c      REAL ASCI(2), ZSCI(2), WSCI(2)
c      DATA ASCI,ZSCI,WSCI /12.01,1.01,6.,1.,1.,1./	
c      REAL ABAF(2), ZBAF(2), WBAF(2)
c      DATA ABAF /22.989, 126.9045 /
c      DATA ZBAF / 11.  ,  53.     /
c      DATA WBAF / 1. ,  1. /
c   
      open (file='for020.dat',unit=20,form='formatted',status='unknown')
      open (file='for021.dat',unit=21,form='formatted',status='unknown')
      open (file='for022.dat',unit=22,form='formatted',status='unknown')

c   Define some materials   
      
      nwbuf = 0  
      istat = gusmate(1,'SILICON$',28.09,14.0,2.33,9.36,45.49,ubuf
     &,                                                        nwbuf)    
      istat = gusmixt(2,'AlOxide$',A_arr,  Z_arr,  3.965, -2, w_arr)  
      istat = gusmixt(3,'Water$',  A_arr2, Z_arr2, 1.,    -2, w_arr2)  
c     
c     Tracking media # 10 - silicon 
      
      nmat = 1                  ! really silicon !
      isvol = 1                 !   sensitive
      ifield = 2                ! no magnetic field
      fieldm = 45.0             ! max field
      tmaxfd = 45.0             ! maximum angle due to field (one step) in degrees
      dmaxms = 0.2              ! max disp. due to mulsct. in one step (cm)
      deemax = 0.1              ! max fractional energy loss in one step
      epsil = .00001            ! tracking precision (cm)
      stmin = 0.001             ! min step due to e loss or mulsct. (cm)
      istat = gustmed(1,'Silicon$',nmat,isvol,ifield,fieldm,tmaxfd,
     &     dmaxms,deemax,epsil,stmin,ubuf,nwbuf)

      istat = gustmed(2,'AIR$',-15,1,2,45.0,45.,.2,.1,.00001,.001,ubuf
     &,                                                         nwbuf)
      
c     Tracking media # 22 - beryllium(for beam-pipe) 

      nmat = -5                 ! beryllium
      isvol = 1                 ! sensitive 
      ifield = 2                ! no magnetic field 
      fieldm = 45.0             ! max field  
      tmaxfd = 45.0             ! maximum angle due to field (one step) in degrees  
      dmaxms = 30.00            ! max disp. due to mulsct. in one step (cm) 
      deemax = 0.1              ! max fractional energy loss in one step 
      epsil = .0001             ! tracking precision (cm) 
      stmin = 0.01              ! min step due to e loss or mulsct. (cm) 
      istat = gustmed(3,'Beryll$',nmat,isvol,ifield,fieldm,tmaxfd, 
     &     dmaxms,deemax,epsil,stmin,ubuf,nwbuf)  

c   Tracking media # 31 - Al2O3 
      nmat = 2                  ! Aluminum Oxide
      isvol = 1                 ! sensitive 
      ifield = 0                ! no magnetic field 
      fieldm = 0.0              ! max field  
      tmaxfd = 45.0             ! maximum angle due to field (one step) in degrees  
      dmaxms = 1.00             ! max disp. due to mulsct. in one step (cm) 
      deemax = 0.1              ! max fractional energy loss in one step 
      epsil = .0001             ! tracking precision (cm) 
      stmin = 0.01              ! min step due to e loss or mulsct. (cm) 
      istat = gustmed(4,'AlOxide$',nmat,isvol,ifield,fieldm,tmaxfd, 
     &     dmaxms,deemax,epsil,stmin,ubuf,nwbuf)  

c     Tracking media # 32 - H2O
      nmat = 3                  ! water
      isvol = 1                 ! sensitive 
      ifield = 0                ! no magnetic field 
      fieldm = 0.0              ! max field  
      tmaxfd = 45.0             ! maximum angle due to field (one step) in degrees  
      dmaxms = 1.00             ! max disp. due to mulsct. in one step (cm) 
      deemax = 0.1              ! max fractional energy loss in one step 
      epsil = .0001             ! tracking precision (cm) 
      stmin = 0.01              ! min step due to e loss or mulsct. (cm) 
      istat = gustmed(5,'Water$',nmat,isvol,ifield,fieldm,tmaxfd, 
     &     dmaxms,deemax,epsil,stmin,ubuf,nwbuf)  
      
      close(20)
      close(21)
      close(22)
      
      RETURN
      END	 
      
      integer function gusmixt(imate,namate,a,z,dens,nlmat,wmat)
      
      IMPLICIT NONE
      
      
      integer    	imate,nlmat,i
      real            dens,a(10),z(10),wmat(10)
      character*(*)	namate 
      
 1000 format(' $Mixture'/
     &     '       MIX_IMATE     =',i5,','/
     &     '       MIX_NAMATE    =''',a,'''',','/
     &     '       MIX_NLMAT     =',i5,',')
 1001 format('       MIX_A(',i1,')    =',f14.6,',')      
 1002 format('       MIX_Z(',i1,')    =',f14.6,',')
 1003 format('       MIX_DENS    =',f14.6,',')
 1004 format('       MIX_WMAT(',i1,')  =',f14.6,',')
 1005 format('       $END')
      
      write(20,1000) imate,namate,NLMAT
      write(20,1001) (i,a(i),i=1,abs(nlmat))      
      write(20,1002) (i,z(i),i=1,abs(nlmat))
      write(20,1003) dens     
      write(20,1004) (i,wmat(i),i=1,abs(nlmat))      
      write(20,1005)

c     call gsmixt(imate,namate,a,z,dens,nlmat,wmat)

      gusmixt=0
      return
      end      
      
      integer function gustmed(itmed,natmed,nmat,isvol,ifield,fieldm,
     &     tmaxfd,dmaxms,deemax,epsil,stmin,ubuf,nwbuf)
      
      IMPLICIT NONE

      integer    	itmed,nmat,isvol,ifield,ntmed_ubuf,nwbuf
      real            fieldm,tmaxfd,dmaxms,deemax
      real		epsil,stmin,ubuf(10)
      character*(*)	natmed 

 1000 format(' $Tracking_Medium'/
     &     '       ITMED      =',i5,','/
     &     '       NATMED     =''',a,'''',','/
     &     '       NMAT       =',i5,','/
     &     '       ISVOL      =',i5,','/
     &     '       IFIELD     =',i5,','/
     &     '       FIELDM     =',f14.6,','/
     &     '       TMAXFD     =',f14.6,','/
     &     '       DMAXMS     =',f14.6,','/
     &     '       DEEMAX     =',f14.6,','/
     &     '       EPSIL      =',f14.6,','/
     &     '       STMIN      =',f14.6,','/
     &     '       NTMED_UBUF =',i5,','/
     &     '       $END')
      
c     call gstmed(itmed,natmed,nmat,isvol,ifield,fieldm,tmaxfd,dmaxms,
c     &		deemax,epsil,stmin,ubuf,nwbuf)

      write(21,1000)itmed,natmed,nmat,isvol,ifield,fieldm,tmaxfd,dmaxms,
     &     deemax,epsil,stmin,ntmed_ubuf

      gustmed=0
      return
      end      

      integer function gusmate(imate,namate,a,z,dens,radl,absl,
     &     ubuf,nwbuf)

      IMPLICIT NONE
      
      
      integer    	imate,nwbuf,nmate_ubuf
      real              a,z,dens,radl,absl,ubuf(10)
      real		epsil,stmin
      character*(*)	nAmate 
      
 1000 format(' $Material'/
     &     '       IMATE      =',i5,','/
     &     '       NAMATE     =''',a,'''',','/
     &     '       A          =',f14.6,','/
     &     '       Zs         =',f14.6,','/
     &     '       DENS       =',f14.6,','/
     &     '       RADL       =',f14.6,','/
     &     '       ABSL       =',f14.6,','/
     &     '       NMATE_UBUF =',i5,','/
     &     '       $END')
      
c      call gsmate(imate,namate,a,z,dens,radl,absl,ubuf,nWbuf)
      
      write(22,1000)imate,namate,a,z,dens,radl,absl,nMATE_ubuf
           
      gusmate=0
      return
      end      
