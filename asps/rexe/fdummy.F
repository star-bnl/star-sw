************************************************************************
      SUBROUTINE A G D U M M Y                                          
*  description - a routine to force loading of library routines  *
*                frequently used in some users codes                    
************************************************************************
C - FLAGS AND VARIABLES TO CONTROL THE RUN                              
      INTEGER IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN,IEOTRI,     
     *IEVENT,ISWIT,IFINIT,NEVENT,NRNDM                                  
      COMMON/GCFLAG/ IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN,     
     *IEOTRI, IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)               
      INTEGER SYSTEMF,IX/0/                                             
C GEANT                                                                 
C VECTOR                                                                
C MINUIT                                                                
C MATRIX                                                                
C FFREAD                                                                
C RANDOM                                                                 
C RANDOM                                                                
C COMIS                                                                 
C INTERP                                                                
C ZEBRA                                                                 
C HBOOK                                                                 
C SOMESG                                                                
C MATH                                                                  
      EXTERNAL  GFKINE,GFVERT,GFPART,GFPATH,GFHEAD,GDTOM,GMTOD,GLVOLU,  
     *GPKINE,GLAST,GSHEAD,GSCKOV,GPIONS,GRNDM,GRANOR,GFNHIT,GPRUNG,     
     *VDIST,VSCALE,VADD,VMOD,SORTZV,VFILL,UBLANK,UFILL,UZERO,IULAST,    
     *MNSETI,MNINIT,MNSTAT,MNEXCM,MNPOUT,MNPARM,  RSINV,DSINV,RINV,DINV,
     *FFINIT,FFSET,FFKEY,FFGET,FFGO,  POISSN,NORRAN,RNDM,RNORML,RDMIN,
     *RDMOUT,DIRCOS,  BINOMI,RNGAMA,GAUSS,PROXIM,JUMPAD,JBYT,  
     *FINT,POLINT,LFIT,DIVDIF,LFITW,LVSIMX,  MZVOLM,MZDRED,ZVERIF,      
     *HPLFUN,HPAGSZ,HNOENT,HNFORM,  RM48,RM48IN,RSFACT,RSFINV,RSFEQN,   
     *DSFACT,RFFT,CFFT,  SORTRQ,DGMLT1,DGMLT2,
     *erf,erfc                           
C SIND,COSD                                                             
* make sure that real calls will never be done even if this routine is c
      IX = IX+1                                                         
C Check IX<=0                                                           
      IF (IX.LE.0) GO TO 1                                              
      IX = IX+1                                                         
C Check IX>=0                                                           
      IF (IX.GE.0) GO TO 1                                              
      FDUM1 = ATAND(0.0)                                                
      FDUM1 = ATAN2D(0.0,0.0)                                           
      FDUM2 = SIND(0.0)                                                 
      FDUM3 = COSD(0.0)                                                 
      FDUM4 = TAND(0.0)                                                 
      FDUM1 = DATAND(0.0D0)                                             
      FDUM1 = DATAN2D(0.0D0,0.0D0)                                      
      FDUM2 = DSIND(0.0D0)                                              
      FDUM3 = DCOSD(0.0D0)                                              
      FDUM4 = DTAND(0.0D0)                                              
      IX=SYSTEMF(' ')                                                   
* Erf
      FDUM1 = ERF (0.0)
      fDUM1 = ERFC(0.0)
* Landau
      FDUM1 = DENLAN(0.0)
      FDUM1 = DSTLAN(0.0)
      FDUM1 = DIFLAN(0.0)
      FDUM1 = XM1LAN(0.0)
      FDUM1 = XM2LAN(0.0)
      FDUM1 = RANLAN(0.0)
* now fake calls to library - no need for arguments
      CALL GFKINE                                                       
      CALL GFVERT                                                       
      CALL GFPART                                                       
      CALL GFPATH                                                       
      CALL GFHEAD                                                       
      CALL GDTOM                                                        
      CALL GMTOD                                                        
      CALL GLVOLU                                                       
      CALL GPKINE                                                       
      CALL GLAST                                                        
      CALL GSHEAD                                                       
      CALL GSCKOV                                                       
      CALL GPIONS                                                       
      CALL GRNDM                                                        
      CALL GRANOR                                                       
      CALL GFNHIT                                                       
      CALL GPRUNG                                                       
      CALL VDIST                                                        
      CALL VSCALE                                                       
      CALL VADD                                                         
      CALL VMOD                                                         
      CALL SORTZV                                                       
      CALL VFILL                                                        
      CALL UBLANK                                                       
      CALL UFILL                                                        
      CALL UZERO                                                        
      CALL IULAST                                                       
      CALL MNSETI                                                       
      CALL MNINIT                                                       
      CALL MNSTAT                                                       
      CALL MNEXCM                                                       
      CALL MNPOUT                                                       
      CALL MNPARM                                                       
      CALL RSINV                                                        
      CALL DSINV                                                        
      CALL RINV                                                         
      CALL DINV                                                         
      CALL FFINIT                                                       
      CALL FFSET                                                        
      CALL FFKEY                                                        
      CALL FFGET                                                        
      CALL FFGO                                                         
      CALL POISSN                                                       
      CALL NORRAN                                                       
      CALL RNDM                                                         
      CALL RNORML                                                       
      CALL DIRCOS                                                       
      CALL BINOMI                                                       
      CALL RNGAMA                                                       
      CALL GAUSS                                                        
      CALL PROXIM                                                       
      CALL JUMPAD                                                       
      CALL JBYT                                                         
      CALL FINT                                                         
      CALL POLINT                                                       
      CALL LFIT                                                         
      CALL DIVDIF                                                       
      CALL LFITW                                                        
      CALL LVSIMX                                                       
      CALL MZVOLM                                                       
      CALL MZDRED                                                       
      CALL ZVERIF                                                       
      CALL HPLFUN                                                       
      CALL HPAGSZ                                                       
      CALL HNOENT                                                       
      CALL HNFORM                                                       
      CALL RM48                                                         
      CALL RM48IN                                                       
      CALL RSFACT                                                       
      CALL RSFINV                                                       
      CALL RSFEQN                                                       
      CALL DSFACT                                                       
      CALL RFFT                                                         
      CALL CFFT                                                         
      CALL SORTRQ                                                       
      CALL DGMLT1                                                       
      CALL DGMLT2                                                       
      CALL iucomp
      CALL lvmaxa
      CALL lvmax 
      CALL lvmina
      CALL lvmin 
      CALL lvsmi 
      CALL lvsmx 
      CALL lvsdmi
      CALL lvsdmx
      CALL lvsimi
      CALL lvsimx
      
*
1     CONTINUE                                                          
      END                                                               
*-- Author :    Pavel Nevski
******************************************************************
*                                                                *
                Function    R N D M (x)
*                                                                *
******************************************************************
      logical first/.true./
*
       if (first) then
          print *,' *******  RNDM substituted by GRNDM  *******'
          first=.false.
          CALL GRNDMQ(0,0,1,' ')
       endif 
       call GRNDM(r,1)
        rndm=r
      return
      entry IRNDM
        if (first) then
          print *,' *******  RNDM substituted by GRNDM  *******'
          first=.false.
          CALL GRNDMQ(0,0,1,' ')
        endif 
        call GRNDM(i,1)
        irndm=i
        return
      END
*
      subroutine NORRAN (a)
      real    a,b/99999/
      logical first/.true./
       if (first) then
          print *,' *******  NORRAN substituted by RANNOR  *******'
          CALL GRNDMQ(0,0,1,' ')
       endif
       if (b .eq. 99999) then 
         call rannor(a,b)
         first=.false. 
       else 
         a=b
         b=99999 
       endif
      end
*
      function  RANF (a)
      real      RANF,a,b
      logical   first/.true./
       if (first) then 
         print *,' *******  RANF substituted by GRNDM  *******'
         first=.false.
          CALL GRNDMQ(0,0,1,' ')
       endif 
       Call GRNDM(b,1)
       RANF=b
      end
*
      function  RN32(a)
      real      RN32,a,b
      logical   first/.true./
       if (first)  then
         print *,' *******  RN32 substituted by GRNDM  *******'
         first=.false.
          CALL GRNDMQ(0,0,1,' ')
       endif
       Call GRNDM(b,1)
       RN32=b
      end
*
      function  RG32(d)
      real      RG32,d,a,b/99999/
      logical   first/.true./
       if (first) then
        print *,' *******  RG32 substituted by RANNOR  *******'
          CALL GRNDMQ(0,0,1,' ')
       endif
       if (b .eq. 99999) then
         call rannor(a,b)
         first=.false. 
        else 
          a=b
          b=99999 
       endif
       RG32=a
      end
*
      Subroutine RDMIN
      entry      RDMOUT
        print 
     &  *,' RNDM substituted by GRNDM, set seed using G/CONT/RNDM'
      end
*
      Subroutine RN32IN
      entry      RN32OT
        print 
     &  *,' RN32 substituted by GRNDM, set seed using G/CONT/RNDM'
      end
*
      Subroutine RG32IN
      entry      RG32OT
        print 
     &  *,' RG32 substituted by RANNOR, set seed using G/CONT/RNDM'
      end
*
      subroutine gufld  (x,b)
      real x(3), b(3)
      b(1) = 0.0
      b(2) = 0.0
      b(3) = 5.0
      end 
 
      function hdummy()
      data N/13/
      save N

      entry    ihdummy
      entry hbook1
      entry hbook2
      entry hbookn
      entry hdelet
      entry hexist
      entry hf1
      entry hfill
      entry hfitga
      entry hfith
      entry hfitv
      entry hfn
      entry hi
      entry hie
      entry hif
      entry hij
      entry hmax
      entry hmin
      entry hrend
      entry hreset
      entry hrndm1
      entry hropen
      entry hrout
      entry hspfun
      entry hstati
      entry hsum
      entry hx
      entry hxe
      entry hxy
      entry kuvcre
      entry kuvect
      hdummy = 0.0
      ihdummy = 0
      if (N.lt.0) return
      N = N-1
      write (*,*) 'HDUMMY. Dummy HBOOK call'
      end
 
