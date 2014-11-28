C      ********************************************************
C      *                                                      *
C      *                   P E P S I  2                       *
C      *                                                      *
C      *   MONTE CARLO PROGRAM FOR POLARIZED LEPTOPRODUCTION  *
C      *   october 1991                                       *
C      ********************************************************
C
C      PEPSI2 is a modification of the LEPTO6.5 code written
C      by G. Ingelman. The authors have modified LEPTO to the extent
C      necessary to simulate scattering off a polarized target.
C
C      PEPSI code is written by:
C
C      L. Mankiewicz Institut fuer Theoretische Physik
C                    der Universitaet Heidelberg
C                    Philosephenweg 16, W-6900 Heidelberg, Germany
C
C      A. Schaefer   Institut fuer Theoretische Physik
C                    Universitaet Frankfurt
C                    PO Box 111932 60054 Frankfurt, Germany
C
C      M. Veltri     Max-Planck Institut fuer Kernphysik
C                    PO Box 103980 W-6900 Heidelberg, Germany
C                    vlt@dxnhd1.mpi-hd.mpg.de - 13616::VLT
C
C       H. Ihssen    HERMES group, DESY, Notkestrasse 85,
C                    D-22603 Hamburg 52, Germany
C
C       M. Maul      Institut fuer Theoretische Physik
C                    Universitaet Frankfurt
C
C
C***************************************************************
C
C  SUBROUTINES FROM LEPTO
C ltimex 
C leptod 
C linit  
C lepto  
C leptox 
C lkinem 
C lqcdpr 
C lqev   
C lqgev  
C lqqbev 
C lxp   
C lzp   
C lqmcut 
C lazimu 
C dsigma 
C dsigm2 
C dqcd   
C dqcdi  
C lflav  
C lremh  
C lprikt 
C lframe 
C lwbb   
C lweits 
C lprwts 
C lsigmx 
C lxsect 
C riwibd 
C dvnopt 
C dfun   
C riwfun 
C dcross 
C dlower 
C dupper 
C fltabl 
C flipol 
C flintg 
C flqint 
C flgint 
C fltint 
C lsci   
C leaswi 
C lecswi 
C lsmall 
C lshowr 
C lmeps  
C lscale 
C lysspa 
C lyremn 
C lyspli 
C lmcmnd 
C lminto 
C lmidat 
C lminew 
C lmprin 
C lmpint 
C lmrazz 
C lmsimp 
C gadap  
C gadap2 
C gadapf 
C lnstrf 
C lystfu 
C
C SUBROUTINES UNIQUE IN PEPSI
C
C poldqcd 
C poldqcdi
C pollazimu
C pollzp  
C poldsigma
C polleptox
C pollinit 
C dstfu  
C dnstrf 
C parton 
C polpar 
C rdarry 
C polini 
C grv94lo
C grv94ho
C grv94di
C fv 
C fw 
C fws
C parpol 
C dfint
C polnlo
C rdarrynlo
C nloini      
c inideflo
      
C######################################################################C
C                                                                      C
C                          L E P T O                                   C
C                                                                      C
C                  A Monte Carlo Generator for                         C
C            Deep Inelastic Lepton-Nucleon Scattering                  C
C                                                                      C
C                  version 6.5,  April 20, 1996                        C
C                                                                      C
C   Authors:                                                           C
C   Gunnar Ingelman, DESY theory group     Dept. of Radiation Sciences C
C                    (room 202  bldg 2a)   Uppsala University          C
C                    D-22603 Hamburg, FRG  S-751 21 Uppsala, Sweden    C
C             phone: +49(40)8998-2795      +46(18)18-3884              C
C               fax:            -2777               -3833              C
C            e-mail: ingelman@desy.de      (ingelman@tsl.uu.se)        C
C   Anders Edin, Dept. of Radiation Sciences, edin@tsl.uu.se           C
C   Johan Rathsman, Dept. of Radiation Sciences, rathsman@tsl.uu.se    C
C                                                                      C
C   Contributions on parton cascades: M. Bengtsson, T. Sjostrand       C
C                                                                      C
C   Home page: http: 
C                                                                      C
C   Manual: G.Ingelman, A.Edin, J.Rathsman, DESY 96-057                C
C                                                                      C
C   Please report any problems or suggestions for improvements.        C
C                                                                      C
C######################################################################C




      SUBROUTINE LTIMEX(TIME)
      
      IMPLICIT NONE
      REAL TIME      
C...Interface routine to transfer a call to some machine-dependent
C...routine to get the execution time used since job started.
C...Nice, but not necessary information. Can also be called by user.

      TIME=0.
C...Use of CERN library routine Z007, replace/delete if not available.
C     CALL TIMEX(TIME)
      RETURN
      END
