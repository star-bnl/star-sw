C>-----------------------------------------------------------------------------
C     
C     gusvolu - a function to define the GEANT volumes and print namelist parameters
C     
C     DESCRIPTION:
C     this function defines the GEANT geometry of a volume and prints out to a
C     namelist file.
C     
C     ARGUMENTS:
C     integer  name    : volume name
C     integer  shape   : volume shape
C     integer  nmed    : tracking medium
C     integer  par(*)  : shape parameters
C     integer  npar    : number of shape parameters passed
C     integer  ivolu   : volume index returned by GEANT
C     
C     AUTHOR:
C     Claude Pruneau, Wayne State University, 313-577-1813
C     
C     CREATION DATE:
C     25-jul-1993,cap written
C     copyright 1993 Lawrence Berkeley Laboratory
C     
C<-----------------------------------------------------------------------------
      INTEGER FUNCTION gusvolu(name,shape,nmed,par,npar,ivolu,nvols)
      
      IMPLICIT NONE
      
C     Argument Declarations
C     =====================
      character*4  name
      character*4  shape
      integer      nmed
      real         par(*)
      integer      npar
      integer      ivolu,nvols

       
C     Local Declarations:
C     ===================
      integer  i

C     Returned Values:  0 : no error
C     ================
C     Format:
C     =======
 1000 format(' $Volume'/
     &       '       VOL_NAME   =''',a,''''/
     &       '       SHAPE      =''',a,''''/
     &       '       NMED       =',i5/
     &       '       NPAR       =',i5)
 1001 format('       PAR(',i2,')    =',f14.6)
 1002 format('       $END')

C     Executable Code:
C     ================
      nvols=nvols+1
c      call gsvolu(name,shape,nmed,par,npar,ivolu)  

      write(9,1000) name,shape,nmed,NPAR
      write(9,1001) (i,par(i),i=1,npar)
      write(9,1002) 

      gusvolu = nvols
      return
      end
c
      INTEGER FUNCTION gusdet(iuset,iudet,nv,namesv,nbitsv,idtype,
     1                                    nwpa,nwsa,iset,idet,numb)

      IMPLICIT NONE

C     Argument Declarations
C     =====================
      character*4  iuset,namesv(11)
      character*4  iudet
      integer      nmed,nbitsv(11),idtype,nwpa,nwsa
      real         par(11)
      integer      npar,iset,idet,nv,ndet
      integer      ivolu,numb

       
C     Local Declarations:
C     ===================
      integer  i

C     Returned Values:  0 : no error
C     ================
C     Format:
C     =======
 1000 format(' $Detector'/
     &     '       IUSET  =''',a,''''/
     &     '       IUDET  =''',a,''''/
     &     '       IDTYPE =',i5/
     &     '       NWHI   =',i5/
     &     '       NWDI   =',i5/
     &     '       Alias  = .FALSE.'/
     &     '       $END')

C     Executable Code:
C     ================
c      call gsdet(iuset,iudet,nv,namesv,nbitsv,
c     & 		idtype,nwpa,nwsa,iset,idet)  

      write(12,1000) IUSET,IUDET,IDTYPE,NWPA,NWSA

      numb = numb + 1

      gusdet = 0
      return
      end
c


      INTEGER FUNCTION gusdeth(set_id,vol_name,nh,namesh,nbitsh,orig,
     1                                                     fact,ndeth)

      IMPLICIT NONE

C     Argument Declarations
C     =====================
      character*4  set_id,vol_name,namesh(11)
      integer      nbitsh(11),nh,ndeth
      real         orig(11),fact(11)

       
C     Local Declarations:
C     ===================
      integer  i

C     Returned Values:  0 : no error
C     ================
C     Format:
C     =======
 1000 format(' $Hits'/
     &     '       HIT_IUSET  =''',a4,''''/
     &     '       HIT_IUDET  =''',a4,''''/
     &     '       NH         =',i5)
 1001 format('       NAMESH(',I2, ')  =''',a4,''''/)
 1002 format('       NBITSH(',I2, ')  =',i5/)
 1003 format('       ORIG(',  I2, ')    =',f14.2/)
 1004 format('       FACT(',  I2, ')    =',f14.2/)
 1005 format('       $END')

C     Executable Code:
C     ================
c      call gsdeth(set_id,vol_name,nh,namesh,nbitsh,
c     & 		orig,fact)  

      write(13,1000) set_id,vol_name,nh
      write(13,1001) (i,namesh(i),i=1,10)
      write(13,1002) (i,nbitsh(i),i=1,10)
      write(13,1003) (i,orig(i),i=1,10)
      write(13,1004) (i,fact(i),i=1,10)
      write(13,1005)
     
      ndeth=ndeth+1

      gusdeth = 0
      return
      end
      
C>-----------------------------------------------------------------------------
C
C guspos - a function to position the GEANT volumes and print namelist parameters
C
C DESCRIPTION:
C this module call the GEANT routine GSPOS that position the volume. It also
C prints out the corresponding name list file.
C
C ARGUMENTS:
C     integer  name    : volume name
C     integer  nr      : volume copy number
C     integer  mname   : mother volume name
C     real     x,y,z   : coordinate of the center of the volume within mother volume
C     integer  irot    : rotation matrix index
C     integer  konly   : ONLY 
C     
C AUTHOR:
C Claude Pruneau, Wayne State University, 313-577-1813
C
C CREATION DATE:
C  25-jul-1993,cap written
C Copyright 1993 Lawrence Berkeley Laboratory
C 
C<-----------------------------------------------------------------------------
      INTEGER FUNCTION guspos(name,nr,mname,x,y,z,irot,konly,npos)

      IMPLICIT NONE
            
C     Argument Declarations
C     =====================
      character*4 name, mname, konly
      integer      nr, irot,npos
      real         x,y,z
       
C     Local Declarations: none
C     ===================
C     Returned Values:  0 : no error
C     ================
C     Format:
C     =======
 1000 format(' $Position'/
     &       '       POS_NAME   =''',a,''''/
     &       '       NR         =',i3,/
     &       '       POS_MOTHER =''',a,''''/
     &       '       X          =',f14.6,/
     &       '       Y          =',f14.6,/
     &       '       Z          =',f14.6,/
     &       '       POS_IROT   =',i5,/
     &       '       KONLY      =''',a,''''/
     &       '       $END')

C     Executable Code:
C     ================
      npos=npos+1
c      call gspos(name,nr,mname,x,y,z,irot,konly)

      write(8,1000) name,nr,mname,x,y,z,irot,konly

      guspos = npos

      return
      end

C>-----------------------------------------------------------------------------
C
C gusatt - a function to define the attribute of a volume
C
C DESCRIPTION:
C this function calls the GEANT routine GSATT to define the attributes
C of a volume and prints out the corresponding name list.
C
C ARGUMENTS:
C     integer   att_name : volume name
C     integer   att_opt  : attribute name
C     integer   att_val  : attribute value
C     
C AUTHOR:
C Claude Pruneau, Wayne State University, 313-577-1813
C
C CREATION DATE:
C  25-jul-1993,cap written
C Copyright 1993 Lawrence Berkeley Laboratory
C 
C<-----------------------------------------------------------------------------
      INTEGER FUNCTION gusatt(att_name,att_opt,att_val,natt)

      IMPLICIT NONE
            
C     Argument Declarations
C     =====================
      character*4   att_name
      character*4   att_opt
      integer   att_val,natt
       
C     Local Declarations: none
C     ===================
C     Returned Values:  0 : no error
C     ================
C     Format:
C     =======
 1001 format(' $Attributes'/
     &       '       Att_Name =''',a,''''/
     &       '       ATT_Opt  =''',a,''''/
     &       '       ATT_VAL  = ',i8,/
     &       '       $END')
      
C     Executable Code:
C     ================      
      natt=natt+1
c      call gsatt(att_name,att_opt,att_val)
      write(10,1001) att_name,att_opt,att_val

      gusatt = natt

      return
      end

C>-----------------------------------------------------------------------------
C
C gusrotm - a function to define a rotation matrix
C
C DESCRIPTION:
C this function calls the GEANT routine GSROTM to define a rotation
C matrix and print the corresponding name list.
C
C ARGUMENTS:
C     integer irot
C     real    th1   : polar angle axis 1
C     real    th2   : polar angle axis 2
C     real    th3   : polar angle axis 3
C     real    ph1   : azimuthal angle axis 1
C     real    ph2   : azimuthal angle axis 2
C     real    ph3   : azimuthal angle axis 3
C     
C AUTHOR:
C Claude Pruneau, Wayne State University, 313-577-1813
C
C CREATION DATE:
C  25-jul-1993,cap written
C Copyright 1993 Lawrence Berkeley Laboratory
C 
C<-----------------------------------------------------------------------------
      INTEGER FUNCTION gusrotm(irot,th1,ph1,th2,ph2,th3,ph3,nrot)

      IMPLICIT NONE
            
C     Argument Declarations
C     =====================
      integer irot,nrot
      real    th1,th2,th3,ph1,ph2,ph3
      
C     Local Declarations: none
C     ===================
C     Returned Values:  0 : no error
C     ================
C     Format:
C     =======
 1000 format(' $Rotation'/
     &     '       IROT   = ',i4,/
     &     '       THETA1 =',f14.6,/
     &     '       PHI1   =',f14.6,/
     &     '       THETA2 =',f14.6,/
     &     '       PHI2   =',f14.6,/
     &     '       THETA3 =',f14.6,/
     &     '       PHI3   =',f14.6/
     &     '       $END')
      
      
C     Executable Code:
C     ================            
	if (th1.ge.360.) then
          th1=th1-360.
         endif
	if (th2.ge.360.) then
          th2=th2-360.
         endif
	if (th3.ge.360.) then
          th3=th3-360.
         endif
	if (ph1.ge.360.) then
          ph1=ph1-360.
         endif
	if (ph2.ge.360.) then
          ph2=ph2-360.
         endif
	if (ph3.ge.360.) then
          ph3=ph3-360.
         endif
      nrot=nrot+1
c      call gsrotm(irot,th1,ph1,th2,ph2,th3,ph3)
      
      write(11,1000) irot,th1,ph1,th2,ph2,th3,ph3

      gusrotm = nrot
      
      return
      end



