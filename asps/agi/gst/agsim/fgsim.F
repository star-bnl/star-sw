*CMZ :  2.00/03 17/02/2000  17.37.34  by  Pavel Nevski
*CMZ :  2.00/00 28/09/98  01.03.58  by  Pavel Nevski
*CMZ :  1.40/05 24/08/98  22.02.41  by  Pavel Nevski
*CMZ :  1.30/00 29/04/97  23.40.13  by  Pavel Nevski
*-- Author :    Pavel Nevski   29/03/97
c***********************************************************************
      SUBROUTINE AGSUSER
c
c***  Subroutine called by GUINTI to process interactive commands
c***  New, highly simplified version for STAR Geant   pmj 6/6/95
c
c***********************************************************************
*
*KEEP,TYPING.
      IMPLICIT NONE
*KEEP,GCFLAG.
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
      COMMON/GCFLAX/BATCH, NOLOG
      LOGICAL BATCH, NOLOG
C
      INTEGER       IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM
C
*KEEP,QUEST.
      INTEGER      IQUEST
      COMMON/QUEST/IQUEST(100)
*KEEP,GCKINE.
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      INTEGER       IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      REAL          PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT
C
*KEEP,agckine.
*    AGI general data card information
      Integer          IKineOld,IdInp,Kevent,
     >                 Iback,IbackOld,IbMode,IbBefor,IbAfter,
     >                 IbCurrent,IvCurrent,Ioutp,IoutpOld
      Real             AVflag,AVcoor,AVsigm,Ptype,PTmin,PTmax,
     >                 Etamin,Etamax,PhiMin,PhiMax,Ptflag,
     >                 Zmin,Zmax,BgMult,BgTime,BgSkip,
     >                 Pxmin,Pxmax,Pymin,Pymax,Pzmin,Pzmax
      COMMON /AgCKINE/ IKineOld,IdInp,Kevent(3),
     >                 AVflag,AVcoor(3),AVsigm(3),
     >                 Ptype,PTmin,PTmax,Etamin,Etamax,
     >                 PhiMin,PhiMax,Ptflag,Zmin,Zmax,
     >                 Pxmin,Pxmax,Pymin,Pymax,Pzmin,Pzmax
      COMMON /AgCKINB/ Iback,IbackOld,IbMode,IbBefor,IbAfter,
     >                 BgMult,BgTime,BgSkip,IbCurrent,IvCurrent
      COMMON /AgCKINO/ Ioutp,IoutpOld
      Character*20     CoptKine,CoptBack,CoptOutp
      COMMON /AgCKINC/ CoptKine,CoptBack,CoptOutp
      Character*20     StrmKine,StrmBack,StrmOutp
      COMMON /AgCKINS/ StrmKine,StrmBack,StrmOutp
      Character*20     CrunType
      COMMON /AgCKINR/ CrunType
      Integer          Ncommand
      Character*20     Ccommand
      COMMON /AgCCOMD/ Ncommand,Ccommand
      Integer          IUHIST
      Character*80            CFHIST,CDHIST
      COMMON /AgCHIST/ IUHIST,CFHIST,CDHIST
*
      Integer          NtrSubEV,NkineMax,NhitsMax,NtoSkip,NsubToSkip,
     >                 Nsubran,ItrigStat,NsubEvnt,IsubEvnt,
     >                 Make_Shadow,Flag_Secondaries
      Real             Cutele_Gas,VertexNow
      COMMON /AgCSUBE/ NtrSubEV,NkineMax,NhitsMax,
     >                 NtoSkip,NsubToSkip,Nsubran(2)
      COMMON /AgCSTAR/ Make_Shadow,Cutele_Gas,Flag_Secondaries
      COMMON /AgCstat/ ItrigSTAT,NsubEvnt,IsubEvnt,VertexNow(3)
*
*    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
*KEND.
c
c*** defines structures for uniform decay of specified particles
c
      integer udecay_count
     >       ,udecay_count_max
     >       ,udecay_nstep
     >       ,udecay_nstep_max
     >       ,i_udecay
     >       ,udecay_nstep_last
 
      logical udecay_track, udecay_in_region_last_step
      real    udecay_pathlength_entrance
      parameter (udecay_count_max=10)
      parameter (udecay_nstep_max=5000)
 
      structure /udecay_t/
         integer parent
         integer new_parent
         real    r_in
         real    r_out
         integer daughter_1
         integer daughter_2
         integer daughter_3
         real    mass_p
         real    mass_1
         real    mass_2
         real    mass_3
         integer nbody
      end structure
      record /udecay_t/ udecay(udecay_count_max)
 
      structure /udecay_step_t/
         real x
         real y
         real z
         real px
         real py
         real pz
         real pathlength
      end structure
      record /udecay_step_t/ udecay_step(udecay_nstep_max)
 
      common/c_udecay/
     >         udecay_count
     >        ,udecay
     >        ,udecay_nstep
     >        ,udecay_step
     >        ,i_udecay
     >        ,udecay_track
     >        ,udecay_in_region_last_step
     >        ,udecay_nstep_last
     >        ,udecay_pathlength_entrance
*---------------------------------------------------------------------------
 
      Integer       CSADDR, LENOCC, Input, Li, Ifile, Iadr, L, L1, token
      character*32  command
      character*256 string1/' '/, string/' '/
      character*1   blank  /' '/, char, C
      integer       npar, length1, length, line1, line
      logical       exist
      integer       i_part_dk
      equivalence   (string1,line1), (string,line)
 
c*** udecay stuff : local variables used in calls to gfpart
 
      character*20  p_name
      integer       p_itrtyp, p_nwbuf, i, j, ier, IPU(5)
      real          p_charge, p_tlife, p_ubuf
 
C========+=========+=========+=========+=========+=========+=========+
 
c***  Get command and number of parameters passed
 
       Ncommand = Ncommand+1
       call KUPATL( command, npar )
       if ( command .eq. 'INPUT' ) then
*     ----
         input = 0
         call kugetc( string, length )
         if (string(1:length) .eq. 'TX')      input    =  1
         if (string(1:length) .eq. 'TXOLD')   input    =  2
         if (string(1:length) .eq. 'TXOTX')   input    =  3
         if (string(1:length) .eq. 'FZ')      input    =  4
         if (string(1:length) .eq. 'FZTX')    input    =  5
         if (string(1:length) .eq. 'FZTXOLD') input    =  6
         IKINE    = -3
         Ccommand = ' '
*
         Do 500 Ifile = 1,Npar-1
*        - - - -
            call kugets (string1, length1)
            inquire (file=string1(1:length1), exist=exist)
            if ( Input.gt.0 .and. .not.exist ) then
               write(6,*)' file ',string1(1:length1),' does not exist'
               goto 500
            endif
 
            C  = ' '
            L  = LENOCC(Ccommand)+1
            Li = 21-L
            If (input .ge. 4) then
                input = input - 4
                write(6,*)'*** input: EGZ(FZ) file ',string1(1:length1)
                Call AgZOPEN ('P',string1(1:length1),'E',Ier)
                Call AGZREAD ('P',ier)
                IKINE = -3
                If (Ier.ne.0) go to 490
                C='e'
            else If (input .ge. 2) then
                input = input - 2
                write(6,*)'*** input:  Old TX file ',string1(1:length1)
                Call AgFOPEN (li,string1(1:Length1),ier)
                If (ier.ne.0) goto 490
                C='t'
            else If (input .ge. 1) then
                input = input - 1
                write(6,*)'*** input:  New TX file ',string1(1:length1)
                Call AgFOPEN (li,string1(1:Length1),ier)
                If (ier.ne.0) goto 490
                C='t'
            else
                Iadr=CSADDR('agusopen')
                if (Iadr.eq.0) then
                   print *,' AgSUSER: no agusopen found for this input'
                   goto 500
                endif
*               Comis pass whole words only (both start and length !)
                L1    = 4*((Length1+3)/4)
                call CSJCAL1S(Iadr,string1(1:L1))
                IKINE = -4
            endif
*
            IKineOld = Ikine
            if (C.ne.' ') Ccommand(L:L)=C
            go to 500
*
 490        write(6,*)' AgSUSER: error opening file ',string1(1:length1)
 500     Continue
*        - - - -
*
      else if ( command .eq. 'OUTPUT' ) then
*     ----
          call KUGETC ( string, length )
          char = string(1:1)
 
          if ( char .eq. 'O' ) then                     !  open output
 
              call KUGETS ( string1, length1 )
              write(6,*) ' GXUSER: opening output file ',
     >                              string1(1:length1)
              Call AgZOPEN  ('O',string1(1:length1),'GEKHD',Ier)
*             PN,03.09.98 - spool output until the first trig
*             Call AGZWRITE ('O',ier)
 
          else                                          !  close output
              Call AgZOPEN('O',' ',' ',Ier)
          endif
*
      else if (command.eq.'PHASESPACE'.or.command.eq.'MOMENTUMBIN') then
*     ----
              Call AGXUSER
 
      else if (command.eq.'SKIP') then
 
         call KUGETI( NtoSkip )
         call KUGETI( NsubToSkip )
         call KUGETI( NsubRan(1) )
         call KUGETI( NsubRan(2) )
         If (NtoSkip.lt.0) then
            CALL ATOKENS(token)
            If (token.gt.0) NtoSkip=(token-1)*abs(NtoSkip)
         endif
         NEVENT = IEVENT+NtoSKIP
         CALL QNEXT
*
      else if (command.eq.'UDECAY') then
*     ----
         if ( udecay_count .gt. udecay_count_max ) then
 
            write(6,*)' Too many uniform decay modes specified '
            goto 5000
 
         endif
 
         i = udecay_count + 1
         udecay(i).nbody  = 2
         call KUGETI( udecay(i).parent )
         call KUGETR( udecay(i).r_in )
         call KUGETR( udecay(i).r_out )
         call KUGETI( udecay(i).daughter_1 )
         call KUGETI( udecay(i).daughter_2 )
 
         if( npar .gt. 5 ) then
            udecay(i).nbody = 3
            call KUGETI( udecay(i).daughter_3 )
         endif
 
         udecay(i).r_in  = max (0.,             udecay(i).r_in)
         udecay(i).r_out = max (udecay(i).r_in, udecay(i).r_out)
 
c*** check to see if all particles exist in Geant:
 
         IPU(1)=udecay(i).parent
         IPU(2)=udecay(i).daughter_1
         IPU(3)=udecay(i).daughter_2
         IPU(4)=udecay(i).daughter_3
 
         Do J = udecay(i).nbody+1,1,-1
            call gfpart (IPU(i) , p_name, p_itrtyp, udecay(i).mass_p,
     >                            p_charge, p_tlife, p_ubuf, p_nwbuf)
 
            if ( p_itrtyp .le. 0 ) then
               write(6,*) '*** UDECAY: unknown particle id ',IPU(i)
               goto 5000
            endif
         enddo
 
c*** define new parent with identical properties but with infinite lifetime
 
         p_tlife = 1.e10
         p_name  = 'udecay_'//p_name(1:13)
         udecay(i).new_parent = udecay(i).parent + 200
 
         write(6,*)'*** Defining new udecay parent ',p_name,
     >             '  pid =',udecay(i).new_parent
 
         call gspart( udecay(i).new_parent, p_name, p_itrtyp,
     >           udecay(i).mass_p, p_charge, p_tlife, p_ubuf, p_nwbuf)
 
         call gprint( 'PART', udecay(i).new_parent )
 
c*** increment counter of number of defined udecay particles
 
         udecay_count = udecay_count + 1
*
      else if (command.eq.'SPARTSTAR') then
*     ----
         WRITE(6,*) '  => use  GEANT/CONTROL/SPART command '
*
      else if (command.eq.'GFDK') then
*     ----
         call KUGETI( i_part_dk )
         call GPDCAY( i_part_dk )
*
      else If (command.eq.'SECONDARIES') then
*     ----
         Call KUGETI(flag_secondaries)
         If(flag_secondaries.eq.0) write (6,7001)'ignored'
         If(flag_secondaries.eq.1) write (6,7001)'saved to jstak'
         If(flag_secondaries.eq.2) write (6,7001)'written to jkine'
 7001    format('  secondary products will be ',a)
*
      else If (command.eq.'VXYZ') then
*     ----
         do i=1,3
            Call KUGETR ( AvCOOR(i) )
         enddo
         write (6,*) ' primary vertex set to ',AvCOOR
*
      else If (command.eq.'VSIG') then
*     ----
         call KUGETR ( AvSIGM(1) )
         AvSIGM(2)  =  AvSIGM(1)
         call KUGETR ( AvSIGM(3) )
         write (6,*) ' primary vertex spread set to',AvSIGM(1),AVSIGM(3)
*
      else If (command.eq.'SUBEVENT') then
*     ----
         call KUGETI ( NtrSubEv )
         call KUGETI ( NkineMax )
         call KUGETI ( NhitsMax )
 
         If (NtrSubEv. gt. 0) write (6,*)
     >   ' Number of tracks per sub-event will be',NtrSubEv
 
         If (NtrSubEv. le. 0) write (6,*)
     >   ' Events will not be splitted into subevents '
*
      else If (command.eq.'SHADOW') then
*     ----
c***    flag to set tracking thresholds in dense materials very high.
c***    this will prevent showering in magnets and other dense
c***    objects but they will still block uphysical tracks
*
         call KUGETI ( make_shadow )
 
         if (make_shadow.eq.0) write (6,*)
     >      ' normal tracking in dense material required '
 
         if (make_shadow.eq.1) write (6,*)
     >      ' tracking thresholds in dense materials will be set high'
*
      else If (command.eq.'SENSECUTE') then
*     ----
**** PN, 1/04/96: *** flag for electron tracking cut in sensitive gases
         call KUGETR ( cutele_gas )
         write (6,*) ' cut for electron tracking in sensitive gases =',
     >                 cutele_gas
*
      else
*     ----
         write (6,*) ' unknown user command '
*
      endif
*     ----
c----------------------------------------------------------------------
 5000  continue
       return
       end
 
 
*CMZ :  2.00/01 02/01/2000  21.01.14  by  Pavel Nevski
*CMZ :  2.00/00 27/08/99  19.38.31  by  Pavel Nevski
*CMZ :  1.40/05 26/08/98  20.16.56  by  Pavel Nevski
*CMZ :  1.40/05 26/08/98  19.34.12  by  Pavel Nevski
*CMZ :  2.06/13 01/02/95  09.38.53  by  O.Couet
*-- Author :    Fons Rademakers   20/11/93
      SUBROUTINE PALOGO
      CHARACTER*240     PROG
      COMMON/AgPROGRAM/ PROG
*
*     Print STAF version identification
*
      CHARACTER  CHPAW*8, MONTH(12)*12
      DATA MONTH/'January','February','March','April','May','June',
     > 'July','August','September','October','November','December'/
 
*KEEP,agcvers.
      COMMON /AGCVERS/IDATQQ,ITIMQQ,IVERSQ,VERSQQ,QFTITLCH
      INTEGER         IDATQQ,ITIMQQ,IVERSQ
      CHARACTER       VERSQQ*8,QFTITLCH*60
*     - - - - - - - - - - - - - - - - - - - -
*KEND.
      CALL AVERSION
      CHPAW  = PROG
      IDAY   = mod(IDATQQ,100)
      IMONTH = mod(IDATQQ/100,100)
      IYEAR  = IDATQQ/10000
      NCM    = LENOCC(MONTH(IMONTH))
 
      IF      (IYEAR.LT.90)  then
           IYEAR=2000+IYEAR
      ELSE IF (IYEAR.LT.100) then
           IYEAR=1900+IYEAR
      ENDIF
      PRINT 3000,CHPAW,VERSQQ,IDAY,MONTH(IMONTH)(1:NCM),
     >           IYEAR,MONTH(IMONTH)(NCM+1:)
 3000 FORMAT(' ******************************************************'/,
     >       ' *                                                    *'/,
     >       ' *            W E L C O M E    to   ', A7,'           *'/,
     >       ' *                                                    *'/,
     >       ' *        Version ',A8,4X,I2,1X,A,1X,I4,A,4X,        '*'/,
     >       ' *                                                    *'/,
     >       ' ******************************************************')
      END
*CMZ :  2.00/00 28/10/98  00.25.15  by  Pavel Nevski
*-- Author :    Pavel Nevski   28/10/98
        function ETIME(XX)
        real  ETIME,XX(2),tt
        call timex(tt)
        etime=tt
        XX(1)=tt
        XX(2)=tt
        end
*CMZ :  2.00/00 16/12/99  15.43.29  by  Pavel Nevski
*CMZ :  2.02/00 20/11/92  10.10.02  by  R. DeWolf
*-- Author :    R. DeWolf   06/03/91
      SUBROUTINE GNZPRIN (ILINK,LEVL)
C------------------------------------------------------------------------------
C GNZPRIN - print out contents of gate on link set ILINK with print level LEVL.
C
C In this subroutine, I constantly have to make sure that the offset IGNOFF
C is applied to anything inside array LGN, except on the pointers IGNLKP(i),
C which are calculated with this already in mind.
C
C Input : ILINK  - link set specifier
C         LEVL   - print level
C                  <=0 no printing at all
C                  = 1 print summary
C                  = 2 list stable particles
C                  = 3 list all particles
C                  = 4  "    "     "     along with decay vtx information
C                  > 4  "    "     "        "
C Original Version.                                 R.DeWolf   18/02/91
C------------------------------------------------------------------------------
*KEEP,TYPING.
      IMPLICIT NONE
*KEEP,GNCSTO.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      real      GN(1)
      integer   IGN(1),LGN
*     external  IGN,LGN
      integer   IGNOFF/0/
      integer   ignsto,LENB
      character CTYP*4
      common   /gncsto/ ignsto,LENB,CTYP
      EQUIVALENCE (Q,GN), (IQ,IGN)
*KEEP,GNCCON.
      INTEGER       IGNVER,IGNZBV,IGNDAT,IGNTIM,IGNMAC,IGNJOB,
     >              MMGENR,MMGENE,MMRUNT,MMEVNT,MMPASS,MMGENP,IOCHED
      REAL          GNCELE,GNFACM
      COMMON/GNCCON/IGNVER,IGNZBV,IGNDAT,IGNTIM,IGNJOB(4),IGNMAC,
     >              MMGENR,MMGENE,MMEVNT,MMRUNT,MMPASS,MMGENP,
     >              IOCHED(2),GNCELE,GNFACM
      INTEGER       LGNPRI/6/
      REAL          TVTMIN/0/,GNZMAG(3)/0,0,0/
C
*KEEP,GNCEVT.
      REAL           GNVERT,GNWEIG
      INTEGER        IGNWFL,IGNGAT
      LOGICAL        LGNBKR,LGNPRR,LGNWTR,LGNUPR,LGNMAG
      COMMON/GNCEVT/ GNVERT(4),IGNWFL,GNWEIG,LGNBKR,LGNPRR,LGNWTR,
     +               LGNUPR,LGNMAG,IGNGAT
C
C GNVERT(4) -current default vertex: x,y,z,time (metres,seconds),
C            relative to coordinate origin and gate reference time.
C IGNWFL    -weight flag.
C GNWEIG    -weight of event. Meaning dependent on IGNWFL
C LGNBKR    -flag  .TRUE.= BooK Run is pending
C LGNPRR    -flag  .TRUE.= Print Run information is pending
C LGNWTR    -flag  .TRUE.= Write Run tree is pending
C LGNUPR    -flag  .TRUE.= UnPack Run information to HEPEVT is pending
C LGNMAG    -flag  .TRUE.= There is non-zero magnetic field.
C IGNGAT    -current internal gate number (used when necessary)
 
*KEEP,SLAC_DO.
C Data offsets for standard SLAC words
      INTEGER    NHDWAC,NPAKAC,JOSYS1,JOSYS2,JOSYS3
      common /slac_do_com/ NHDWAC
*     PARAMETER (NHDWAC = 3)
      PARAMETER (NPAKAC = 100000)
      PARAMETER (JOSYS1 = 1)
      PARAMETER (JOSYS2 = 2)
      PARAMETER (JOSYS3 = 3)
 
*KEEP,GENP_DO.
C particle bank offsets
      INTEGER        JOSTAT,JOPDGC,JOMOT1,JOMOT2,JODAU1,JOVXVX,JOVYVY,
     +        JOVZVZ,JOPXPX,JOPYPY,JOPZPZ,JOMASS,JOTIME,JOADIN,NAGENP
      COMMON/Joofss/ JOSTAT,JOPDGC,JOMOT1,JOMOT2,JODAU1,JOVXVX,JOVYVY,
     +        JOVZVZ,JOPXPX,JOPYPY,JOPZPZ,JOMASS,JOTIME,JOADIN,NAGENP
 
*KEEP,GENE_DO.
C event bank offsets
      INTEGER NAGENE
      INTEGER JOGRUN,JOGEVT,JOGNAM,JOVRTX,JOVRTY,JOVRTZ,JOVRTT,
     +        JOWTFL,JOWEIG
      PARAMETER (NAGENE = 9)
      PARAMETER (JOGRUN = 4)
      PARAMETER (JOGEVT = 5)
      PARAMETER (JOGNAM = 6)
      PARAMETER (JOVRTX = 7)
      PARAMETER (JOVRTY = 8)
      PARAMETER (JOVRTZ = 9)
      PARAMETER (JOVRTT = 10)
      PARAMETER (JOWTFL = 11)
      PARAMETER (JOWEIG = 12)
 
*KEEP,GENR_DO.
C Data offsets
      INTEGER NAGENR
      INTEGER JOGJID,JOGRRU,JOGTIM,JOGDAT,JOMACH,JOGRNA,JOGVER,
     +        JOGVDT,JOGZVR,JOZBVR
      PARAMETER (NAGENR = 10)
      PARAMETER (JOGJID = 4)
      PARAMETER (JOGRRU = 5)
      PARAMETER (JOGTIM = 6)
      PARAMETER (JOGDAT = 7)
      PARAMETER (JOMACH = 8)
      PARAMETER (JOGRNA = 9)
      PARAMETER (JOGVER = 10)
      PARAMETER (JOGVDT = 11)
      PARAMETER (JOGZVR = 12)
      PARAMETER (JOZBVR = 13)
 
*KEND.
      INTEGER  IGNLKP
      EXTERNAL IGNLKP
C Maximum number in particle decay stacks:
      INTEGER      NMAXS,ILINK,LEVL
      PARAMETER   (NMAXS=500)
      INTEGER      IPSTK(NMAXS)
      CHARACTER*8  CHSTK(NMAXS)
      CHARACTER*16 CPNAM
      CHARACTER*8  CSHOR
      LOGICAL LSTAB,LPRI
      INTEGER MODUL,LPQR,LPQE,ND,NDAUG,NEED,IGATE,ISTAT
      INTEGER NEVTS,NPART,NZBANK,NDAU,NMOTH,NSTA1,NSTA2,NLIN,NPERL
      INTEGER IE,IM,IP,IL,JM(2),JD(2),IDAU,IVENT,IPDG,J1,J2,J,IMVRT
      REAL    VERTX(4),PLAB(4),VMOD,VXYZT(4),PDUM(3),AROT,ROTMA(3,3)
      INTEGER LEVNT,LRUNT,LGENE,LGENR,LOFF,LMOTH,LDAU,LGENP,LD1,NGAP
C
C If LEVL <= 0, just return
      IF (LEVL.LE.0) GOTO 999
C
C Assume that there are no ZEBRA calls in this routine. Pointers are static.
 
      IF(ILINK.EQ.1)THEN
         LPQR = LGN(IGNLKP(1))
         LPQE = LGN(IGNLKP(2))
      ELSE IF(ILINK.EQ.2)THEN
         LPQR = LGN(IGNLKP(3))
         LPQE = LGN(IGNLKP(4))
      ELSE
         WRITE(LGNPRI,10000)ILINK
10000    FORMAT(' GZNPRIN: Illegal link set specifier ',I8,';'/,
     +          10X,'Use 1 for primary set, 2 for secondary.')
         GOTO 999
      ENDIF
C
C Make these the correct pointers for LGN:
      LRUNT = LPQR+IGNOFF
      LEVNT = LPQE+IGNOFF
C
C Get pointers to GENZ banks:
C First GENR if the run is new:
      IF(LGNPRR.AND.LPQR.GT.0)THEN
         LGNPRR=.FALSE.
         LGENR = 0
         IF(IGN(LRUNT-2).GE.2.AND.LGN(LRUNT-2).GT.0)THEN
            LGENR = LGN(LRUNT-2)
         ELSE
            WRITE (LGNPRI,10600)LGN(LRUNT-2)
10600       FORMAT(' GNZPRIN: No GENR bank present, LGN(LRUNT-2)= ',I15)
         ENDIF
C
C Dump the run information:
C Check if the run header is present
         IF(LGENR.GT.0)THEN
            LGENR = LGENR + IGNOFF
            WRITE(LGNPRI,10400)
10400       FORMAT('0',76('='))
            WRITE(LGNPRI,10300)IGN(LGENR+JOGRNA),IGN(LGENR+JOGRRU),
     +      IGN(LGENR+JOGJID),IGN(LGENR+JOGDAT),IGN(LGENR+JOGTIM),
     +      IGN(LGENR+JOMACH)
10300       FORMAT(' GNZPRIN: Run Header: Generator ',A4,'   Run',I10,
     +             '   Generator JobID ',I10,/,
     +             10X,'Date= ',I10,'  Time= ',I10,'   Machine= ',A4)
            WRITE(LGNPRI,10500)
10500       FORMAT(1X, 76('-'))
 
         ENDIF
C
      ENDIF
C
C Then GENE:
C
      IF(LPQE.LE.0)THEN
         WRITE(LGNPRI,10700)
10700    FORMAT(' GNZPRIN: No event bank (EVNT) present. ')
         GOTO 90
      ENDIF
C
      LGENE = 0
      IF(IGN(LEVNT-2).GE.2.AND.LGN(LEVNT-2).GT.0)THEN
         LGENE = LGN(LEVNT-2)+IGNOFF
      ELSE
         WRITE(LGNPRI,10800)LGN(LEVNT-2)
10800    FORMAT(' GNZPRIN: No GENE bank present!!! LGENE=',I10)
         GOTO 90
      ENDIF
C
C Dump the event information:
C
C this uses the value relative to correct array:
      NEVTS = NZBANK(IGNSTO,LGENE-IGNOFF)
      IVENT = 0
      NSTA2 = 0
C
      WRITE(LGNPRI,10400)
      IGATE = IGN(LEVNT+6)
      WRITE(LGNPRI,10100)IGATE,NEVTS
10100 FORMAT(' GNZPRIN: Gate number',I7,' contains',I4,' events.')
C
      DO 70 IE = 1,NEVTS
         IVENT = IVENT + 1
         NSTA1=0
         IF(IE.GT.1)THEN
            IF(LGN(LGENE).LE.0)GOTO 80
            LGENE = LGN(LGENE)
C Make this the correct pointer for LGN:
            LGENE = LGENE + IGNOFF
         ENDIF
C
C
C Get particle data
         LGENP = 0
         IF(IGN(LGENE-2).GE.1.AND.LGN(LGENE-1).GT.0)THEN
C Make this the correct pointer for LGN:
            LGENP = LGN(LGENE-1) + IGNOFF
         ELSE
            WRITE(LGNPRI,10900)
10900       FORMAT(' GNZPRIN: No GENP bank present !!! ')
            GOTO 70
         ENDIF
C
C===>
         NPART = -1
*         print *,' ctyp=',ctyp
         CALL UHTOC(IGN(LGENP-4),4,CTYP,4)
         LENB =     IGN(LGENP-1)
         ND   =     IGN(LGENP-1)
 
         IF       (CTYP.eq.'GENP') then
            NPART = MOD(IGN(LGENP+JOSYS2),NPAKAC)
            MODUL = MOD(IGN(LGENP+JOSYS3),NPAKAC)
            NEED  = NPART * MODUL + NHDWAC
            NAGENP = 13
            JOSTAT = 1
            JOPDGC = 2
            JOMOT1 = 3
            JOMOT2 = 4
            JODAU1 = 5
            JOPXPX = 6
            JOPYPY = 7
            JOPZPZ = 8
            JOMASS = 9
            JOTIME = 10
            JOVXVX = 11
            JOVYVY = 12
            JOVZVZ = 13
            JOADIN = 14
            NHDWAC = 3
         ELSE IF  (CTYP.eq.'GENT') then
            MODUL = 15
            NPART = LENB/MODUL
            NEED  = NPART * MODUL
            NAGENP = 15
            JOSTAT = 1
            JOPDGC = 2
            JOMOT1 = 3
            JOMOT2 = 4
            JODAU1 = 5
            JOPXPX = 7
            JOPYPY = 8
            JOPZPZ = 9
            JOMASS = 11
            JOTIME = 15
            JOVXVX = 12
            JOVYVY = 13
            JOVZVZ = 14
            JOADIN = 10
            NHDWAC = 0
         ENDIF
C
C Event information:
C
         WRITE(LGNPRI,10500)
         WRITE(LGNPRI,11900)IVENT,IGN(LGENE+JOGRUN),IGN(LGENE+JOGEVT),
     +   IGN(LGENE+JOGNAM),NPART
11900    FORMAT(' Event',I4,' of gate: Run #',I11,' Evt #',I7,
     +             '   Gen ',A4,I8,' particles.')
         WRITE(LGNPRI,12000)(GN(LGENE+J),J=JOVRTX,JOVRTT),
     +     IGN(LGENE+JOWTFL),GN(LGENE+JOWEIG)
12000    FORMAT(' Interaction at ',3F9.4,' metres ',1X,E10.3,' seconds',
     +            /' Weight flag=',I2,'   Weight= ',E10.3)
         WRITE(LGNPRI,10500)
C
C Check NP against length of bank:
C
 
         IF(NEED.GT.ND)THEN
            WRITE(LGNPRI,10200)ND,NPART,NEED
10200       FORMAT(' GNZPRIN: Bank length,',I7,
     +      ', is less than that required for',I5,
     +      ' particles,(',I7,')')
            GOTO 90
         ENDIF
C
         DO 60 , IP = 1,NPART
C
C Offset for this particle
            LOFF = LGENP + MODUL*(IP-1) + NHDWAC
C
            IPDG  = IGN(LOFF+JOPDGC)
            ISTAT = IGN(LOFF+JOSTAT)
            IF(ISTAT.LE.0)GOTO 60
C
            CALL GNZPNAM(IPDG,CPNAM)
            CSHOR = CPNAM(1:8)
            CALL UCOPY(GN(LOFF+JOPXPX),PLAB(1),3)
C
            PLAB(4) = VMOD(GN(LOFF+JOPXPX),4)
C If far away enough from the vertex, and there is magnetic field
C calculate new momentum:
            IF(GN(LOFF+JOTIME).GT.TVTMIN.AND.LGNMAG)THEN
               CALL GNZGETV(ILINK,IE,IP,VERTX,IMVRT,PLAB,AROT,ROTMA)
            ENDIF
 
C get pointers, calculate number of mothers, stability...
            JM(1) = IGN(LOFF+JOMOT1)
            JM(2) = IGN(LOFF+JOMOT2)
            JD(1) = IGN(LOFF+JODAU1)
C
C Stable is JD(1) <= 0 and status =1
            LSTAB = .FALSE.
            IF(JD(1).LE.0.AND.ISTAT.EQ.1)LSTAB=.TRUE.
C Does this one get printed?
            LPRI = .FALSE.
            IF(LSTAB.AND.LEVL.GE.2)LPRI=.TRUE.
            IF(LEVL.GE.3)LPRI=.TRUE.
C
            NMOTH = 0
            NGAP = ABS(JM(2)) - JM(1)
            IF(NGAP.EQ.0)NGAP = 1
            IF(JM(2).LT.0)NGAP = 1
            DO 10 IM = JM(1),ABS(JM(2)),NGAP
               IF(IM.EQ.0)GOTO 20
               IF(NMOTH.EQ.NMAXS)GOTO 10
               LMOTH = LGENP + MODUL*(IM-1)+ NHDWAC
               NMOTH = NMOTH + 1
               IPSTK(NMOTH)= IM
               CALL GNZPNAM(IGN(LMOTH+JOPDGC),CPNAM)
               CHSTK(NMOTH)=CPNAM(1:8)
   10       CONTINUE
C
   20       CONTINUE
C
            IF(LPRI)THEN
               IF(NMOTH.LT.1)THEN
                  WRITE(LGNPRI,11500)IP,CSHOR,PLAB
11500             FORMAT(1X,I5,2X,A8,'  PxyzE',4F9.3,'  No Parents. ')
               ELSE IF(NMOTH.EQ.1)THEN
                  WRITE(LGNPRI,11600)IP,CSHOR,PLAB,IPSTK(1),CHSTK(1)
11600             FORMAT(I6,2X,A8,'  PxyzE',4F9.3,'  Mother ',I5,' ',A8)
               ELSE
                  WRITE(LGNPRI,11700)IP,CSHOR,PLAB,IPSTK(1),CHSTK(1)
11700             FORMAT(I6,2X,A8,'  PxyzE',4F9.3,'  Mothers',I5,' ',A8)
                  DO 30 , IM = 2,NMOTH
                     WRITE(LGNPRI,11800)IPSTK(IM),CHSTK(IM)
11800                FORMAT(67X,I5,' ',A8)
   30             CONTINUE
               ENDIF
            ENDIF
C
C Now handle the daughters ( having got rid of the parents !)
            IF(JD(1).GT.0)THEN
               LD1 = LGENP + MODUL*(JD(1)-1) + NHDWAC
C Write out decay point if far enough away in x,y:
               IF(LEVL.GE.4)THEN
                  IF(GN(LD1+JOTIME).GT.TVTMIN)THEN
                     CALL GNZGETV(ILINK,IE,JD(1),VXYZT,
     +                            IMVRT,PDUM,AROT,ROTMA)
                     WRITE(LGNPRI,11000) VXYZT
11000                FORMAT(9X,'|....decays at xyz:',3F11.6,
     >                            ', time: ',E10.3)
                  ENDIF
               ENDIF
C
C Arrange for stack of descendants:
               NDAUG = NMAXS
               CALL GNZDLIS(IGN(LGENP+1),IP,NDAUG,IPSTK)
               NDAU = NDAUG
C
C Stack the numbers and names of the daughters:
               DO 40 IDAU = 1,NDAU
                  J = IPSTK(IDAU)
                  LDAU = LGENP + MODUL*(J-1) + NHDWAC
                  CALL GNZPNAM(IGN(LDAU+JOPDGC),CPNAM)
                  CHSTK(IDAU)=CPNAM(1:8)
   40          CONTINUE
C
C Write them out, NPERL per line.
C
               NPERL = 4
               NLIN = (NDAU-1)/NPERL + 1
               DO 50 IL = 1,NLIN
                  J1 = (IL-1)*NPERL + 1
                  J2 = IL*NPERL
                  IF(J2.GT.NDAU)J2=NDAU
                  IF(J1.GT.NDAU)J1=NDAU
                  IF(LPRI)THEN
                     IF(IL.EQ.1)THEN
                        WRITE(LGNPRI,11100)(IPSTK(J),CHSTK(J),J=J1,J2)
11100                   FORMAT(9X,'+----->',5(I5,' ',A8,3X) )
                     ELSE
                        WRITE(LGNPRI,11200)(IPSTK(J),CHSTK(J),J=J1,J2)
11200                   FORMAT(16X,5(I5,' ',A8,3X) )
                     ENDIF
                  ENDIF
   50          CONTINUE
C
            ELSE
               NSTA1 = NSTA1 + 1
               NSTA2 = NSTA2 + 1
            ENDIF
C
C End of particle loop:
   60    CONTINUE
         WRITE(LGNPRI,11300)NSTA1,IVENT
11300    FORMAT(/,1X,I6,' stable particles in event',I5)
C
C End of event loop:
   70 CONTINUE
      WRITE(LGNPRI,10500)
   80 WRITE(LGNPRI,11400)IGATE,NSTA2,IVENT
11400 FORMAT(' Gate',I6,' has a total of',I6,' particles in',I5,
     +' events.')
C
   90 CONTINUE
      WRITE(LGNPRI,10400)
C
  999 END
 
 
 
*CMZ :  2.00/00 16/12/99  01.15.03  by  Pavel Nevski
*CMZ :  2.02/00 29/07/93  22.53.25  by  R. DeWolf
*CMZ :  2.00/01 11/10/91  13.58.13  by  R. DeWolf
*-- Author :    R. DeWolf   12/03/91
      SUBROUTINE GNZPNAM(IDPDG,CPNAM)
C---------------------------------------------------------------------
C GNPNAM
C Return the particle name CPNAM, given the Particle Data Group
C particle code IDPDG. This routine is faster for the particle codes
C under 30.
C
C Input : IDPDG - PD Group code for particle
C
C Output: CPNAM - Name of particle
C
C Original Version.                                 R. DeWolf   12/03/91
C---------------------------------------------------------------------
*KEEP,TYPING.
      IMPLICIT NONE
*KEND.
      CHARACTER*(*) CPNAM
      INTEGER IDPDG
C
      INTEGER JDPDG,I,IER
      REAL PMASS,PCHAR
      CHARACTER*8 CNAM1,CNAM2
      CHARACTER*8 CQ(10),CQA(10),CL(10),CLA(10)
      CHARACTER*8 CB(10),CBA(10)
      CHARACTER*8 CMIS(4),CMISA(4)
      SAVE CL,CLA,CB,CBA,CQ,CQA,CMIS,CMISA
C
      DATA (CQ(I),I=1,4 )/'d       ','u       ','s       ','c       '/
      DATA (CQ(I),I=5,8 )/'b       ','t       ','l       ','h       '/
      DATA (CQ(I),I=9,10)/'g       ','????????'/
      DATA (CQA(I),I=1,4)/'d-bar   ','u-bar   ','s-bar   ','c-bar   '/
      DATA (CQA(I),I=5,8)/'b-bar   ','t-bar   ','l-bar   ','h-bar   '/
      DATA (CQA(I),I=9,10)/'g       ','????????'/
      DATA (CL(I),I=1,4 )/'e-      ','nu_e    ','mu-     ','nu_mu   '/
      DATA (CL(I),I=5,8 )/'tau-    ','nu_tau  ','hl-     ','nu_hlep '/
      DATA (CL(I),I=9,10)/'????????','????????'/
      DATA (CLA(I),I=1,4)/'e+      ','nu_e-bar','mu+     ','nu_mu-b '/
      DATA (CLA(I),I=5,8)/'tau+    ','nu_tau-b','hl+     ','nu-hl-b '/
      DATA (CLA(I),I=9,10)/'????????','????????'/
      DATA (CB(I),I=1,4)/'gluon   ','photon  ','Z0      ','W+      '/
      DATA (CB(I),I=5,8)/'H0      ','????????','????????','????????'/
      DATA (CB(I),I=9,10)/'????????','????????'/
      DATA (CBA(I),I=1,4)/'gluon   ','photon  ','Z0      ','W-      '/
      DATA (CBA(I),I=5,8)/'H0      ','????????','????????','????????'/
      DATA (CBA(I),I=9,10)/'????????','????????'/
C
      DATA (CMIS(I),I=1,4) /'pi +    ','pi 0    ','eta     ','K +     '/
      DATA (CMISA(I),I=1,4)/'pi -    ','pi 0    ','eta     ','K -     '/
C
C By default, write in the code itself:
      CPNAM = '?ERROR!        '
      CNAM1 = '????????'
C
      JDPDG=ABS(IDPDG)
C photons most common?
      IF (JDPDG.EQ.22) THEN
         WRITE(CNAM1,10000)CB(2)
C special treatment for pi and pi0,too:
      ELSE IF (JDPDG.EQ.211) THEN
         IF(IDPDG.GE.0) wRITE(CNAM1,10000) CMIS(1)
         IF(IDPDG.LT.0) wRITE(CNAM1,10000) CMISA(1)
      ELSE IF (JDPDG.EQ.111) THEN
         IF(IDPDG.GE.0) wRITE(CNAM1,10000) CMIS(2)
         IF(IDPDG.LT.0) wRITE(CNAM1,10000) CMISA(2)
      ELSE IF (JDPDG.EQ.221) THEN
         IF(IDPDG.GE.0) wRITE(CNAM1,10000) CMIS(3)
         IF(IDPDG.LT.0) wRITE(CNAM1,10000) CMISA(3)
      ELSE IF (JDPDG.EQ.321) THEN
         IF(IDPDG.GE.0) wRITE(CNAM1,10000) CMIS(4)
         IF(IDPDG.LT.0) wRITE(CNAM1,10000) CMISA(4)
      ELSE IF(JDPDG.EQ.0)THEN
         CNAM1 = '?      0'
      ELSE IF(JDPDG.LE.100)THEN
         IF(JDPDG.LT.11)THEN
            IF(IDPDG.GE.0) wRITE(CNAM1,10000)CQ(JDPDG)
            IF(IDPDG.LT.0) wRITE(CNAM1,10000)CQA(JDPDG)
         ELSE IF(JDPDG.LT.20)THEN
            IF(IDPDG.GE.0) wRITE(CNAM1,10000)CL(JDPDG-10)
            IF(IDPDG.LT.0) wRITE(CNAM1,10000)CLA(JDPDG-10)
         ELSE IF(JDPDG.LT.30)THEN
            IF(IDPDG.GE.0) wRITE(CNAM1,10000)CB(JDPDG-20)
            IF(IDPDG.LT.0) wRITE(CNAM1,10000)CBA(JDPDG-20)
         ENDIF
C
      ENDIF
C
C If this did not work do other things:
C
      IF(CNAM1(1:1).EQ.'?')THEN
         CALL GNZPDAT(IDPDG,CNAM2,PMASS,PCHAR,IER)
         IF(IER.LT.0)GOTO 10
         IF(IER.EQ.0)CNAM1 = CNAM2
         IF(IER.GT.0)THEN
            WRITE(CNAM1,10100)IDPDG
         ENDIF
      ENDIF
C
      CPNAM = CNAM1
C
   10 CONTINUE
10000 FORMAT(A8)
10100 FORMAT('?',I7)
      END
*CMZ :  2.00/02 19/01/2000  23.42.44  by  Pavel Nevski
*CMZ :  2.00/00 16/12/99  15.45.32  by  Pavel Nevski
*CMZ :  2.02/00 20/11/92  10.03.42  by  R. DeWolf
*-- Author :    R. DeWolf   25/09/91
      SUBROUTINE GNZGETV(IL,IE,IP,VSTAR,IMVRT,PRET,AROT,ROTMA)
C-----------------------------------------------------------------------
C
C Input : IL    - Link number : 1 = primary link
C                               2 = secondary link
C         IE    - Number of event in gate
C         IP    - Index of particle in event record
C
C
C Output: VSTAR(4) - starting position,time for particle relative to
C                    primary interaction. In metres, seconds.
C                    (Note that VSTAR(4) is just taken from particle IP's
C                    event record.)
C
C         IMVRT    - Index of particle whose end point is the vertex for
C                    particle IP. IMVRT should be a unique vertex tag for
C                    this vertex. 0 is returned for the primary vertex.
C                     -1 is returned as an error condition.
C
C         PRET(3)  - returned 3-momentum after taking the effects of a
C                    uniform magnetic field (set using GNZPARR) into account.
C                    This 3-momentum is the same as that from GNZGETP if there
C                    is less than the minimum magnetic field present.
C
C         AROT     - angle of rotation generated during the flights of the
C                    ancestors of particle IP (and not IP's own flight) measured
C                    in radians about the magnetic field axis. This number
C                    can be used along with the rotation matrix ROTMA to rotate
C                    all particles from the current physical vertex, IMVRT.
C                    Note that the rotation has already been applied to PRET.
C
C         ROTMA(3,3) - "unit" rotation matrix describing the rotation of
C                    generated during the flights of ancestors of particle IP.
C                    The user may use this along with AROT and subroutine
C                    GNZROTA to rotate sister particles of IP.
C
C
C This routine uses the parameter TVTMIN (minimum vertex time), which
C can be set using GNZSETP, as a cut-off to very short tracks. VSTAR(4)
C is calculated by looping back through generations until the current
C point is within TVTMIN of zero, ie closer than TVTMIN times the speed
C of light to the primary vertex point.
C
C The index IMVRT is the particle index of the particle first encountered
C in this traceback that has laboratory lifetime greater than TVTMIN.
C A check is done that this particle is unique mother of IP. This means
C that IMVRT can be used as a vertex label. 0 is returned for the
C primary vertex.
C
C The magnetic field return arguments AROT will be returned as zero for
C no rotation (for instance, if there is no magnetic field ). ROTMA will
C only be set, however, if there is a field.
C
C If the magnetic field is weak, or the maximum decay length small ( for
C instance of Kaons are counted as stable), not setting a magnetic field
C in GENZ should be considered according to the subsequent errors in decay
C position and direction of secondary particles; using a magnetic field in GENZ
C may result in significant CPU increases during this routine.
C
C The flag that turns on the handling of the magnetic field is LGNMAG. This
C is set if the magnetic field set by GNZPARR is greater than the parameter
C BMIVAL, also settable by GNZPARR (Key = 'BMIN'). The default value for
C this is 0.0001 Tesla ( 1 Gauss), a rather low value.
C
C Original Version.                                R. DeWolf   25/09/91
C-----------------------------------------------------------------------
*KEEP,TYPING.
      IMPLICIT NONE
*KEND.
*CDE,GNCPAR
*KEEP,GNCSTO.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      real      GN(1)
      integer   IGN(1),LGN
*     external  IGN,LGN
      integer   IGNOFF/0/
      integer   ignsto,LENB
      character CTYP*4
      common   /gncsto/ ignsto,LENB,CTYP
      EQUIVALENCE (Q,GN), (IQ,IGN)
*KEEP,GNCCON.
      INTEGER       IGNVER,IGNZBV,IGNDAT,IGNTIM,IGNMAC,IGNJOB,
     >              MMGENR,MMGENE,MMRUNT,MMEVNT,MMPASS,MMGENP,IOCHED
      REAL          GNCELE,GNFACM
      COMMON/GNCCON/IGNVER,IGNZBV,IGNDAT,IGNTIM,IGNJOB(4),IGNMAC,
     >              MMGENR,MMGENE,MMEVNT,MMRUNT,MMPASS,MMGENP,
     >              IOCHED(2),GNCELE,GNFACM
      INTEGER       LGNPRI/6/
      REAL          TVTMIN/0/,GNZMAG(3)/0,0,0/
C
*KEEP,GNCEVT.
      REAL           GNVERT,GNWEIG
      INTEGER        IGNWFL,IGNGAT
      LOGICAL        LGNBKR,LGNPRR,LGNWTR,LGNUPR,LGNMAG
      COMMON/GNCEVT/ GNVERT(4),IGNWFL,GNWEIG,LGNBKR,LGNPRR,LGNWTR,
     +               LGNUPR,LGNMAG,IGNGAT
C
C GNVERT(4) -current default vertex: x,y,z,time (metres,seconds),
C            relative to coordinate origin and gate reference time.
C IGNWFL    -weight flag.
C GNWEIG    -weight of event. Meaning dependent on IGNWFL
C LGNBKR    -flag  .TRUE.= BooK Run is pending
C LGNPRR    -flag  .TRUE.= Print Run information is pending
C LGNWTR    -flag  .TRUE.= Write Run tree is pending
C LGNUPR    -flag  .TRUE.= UnPack Run information to HEPEVT is pending
C LGNMAG    -flag  .TRUE.= There is non-zero magnetic field.
C IGNGAT    -current internal gate number (used when necessary)
 
*KEEP,SLAC_DO.
C Data offsets for standard SLAC words
      INTEGER    NHDWAC,NPAKAC,JOSYS1,JOSYS2,JOSYS3
      common /slac_do_com/ NHDWAC
*     PARAMETER (NHDWAC = 3)
      PARAMETER (NPAKAC = 100000)
      PARAMETER (JOSYS1 = 1)
      PARAMETER (JOSYS2 = 2)
      PARAMETER (JOSYS3 = 3)
 
*KEEP,GENP_DO.
C particle bank offsets
      INTEGER        JOSTAT,JOPDGC,JOMOT1,JOMOT2,JODAU1,JOVXVX,JOVYVY,
     +        JOVZVZ,JOPXPX,JOPYPY,JOPZPZ,JOMASS,JOTIME,JOADIN,NAGENP
      COMMON/Joofss/ JOSTAT,JOPDGC,JOMOT1,JOMOT2,JODAU1,JOVXVX,JOVYVY,
     +        JOVZVZ,JOPXPX,JOPYPY,JOPZPZ,JOMASS,JOTIME,JOADIN,NAGENP
 
*KEND.
 
C
      INTEGER IL,IE,IP,IMVRT
      REAL    ROTMA(3,*),VSTAR(*),PRET(*),AROT
C
      REAL    VSTEP(3),DELT,DELTE,ENER,VMOD,TNOW,TDIFF
      REAL    QQ,PMASS,QNEG,PIN(3),AM,ANGR,POUT(3),TSTAR
      INTEGER LGENE,LGENP,LGNFIND,LZFIND,IER
      INTEGER I1,I2,IOFF,IOFF1,IOFF2,NPART,MODUL,IGEN,MAXGEN
      LOGICAL LUNIQ
      CHARACTER*8 CPNAM
      PARAMETER (MAXGEN=1000)
C
      VSTAR(1) = 0.
      VSTAR(2) = 0.
      VSTAR(3) = 0.
      VSTAR(4) = 0.
      VSTEP(1) = 0.
      VSTEP(2) = 0.
      VSTEP(3) = 0.
      IMVRT = 0
C Zero total rotation
      AROT = 0.
      ANGR = 0.
      LUNIQ=.TRUE.
      TSTAR = 0.
C
      IF(IL.EQ.1.OR.IL.EQ.2)THEN
         LGENE = LGNFIND(IL,'GENE')
      ELSE
         WRITE(LGNPRI,10000) IL
10000    FORMAT(' GNZGETV: Link #',I4,' not recognized by GENZ!')
         IMVRT = -1
         GOTO 30
      ENDIF
C
      IF(LGENE.LE.0)THEN
         WRITE(LGNPRI,10100)IL
10100    FORMAT(' GNZGETV: Link value for link #',I4,' is <= 0 !')
         IMVRT = -1
         GOTO 30
      ENDIF
C
C Find the event with the desired IDN:
      LGENE = LZFIND(IGNSTO,LGENE,IE,-5)
      IF(LGENE.LE.0)THEN
         WRITE(LGNPRI, 10200)IE
10200    FORMAT(' GNZGETV: GENE bank for event',I5,' not found.')
         IMVRT = -1
         GOTO 30
      ENDIF
C
      LGENP = LGN(LGENE+IGNOFF-1)
      IF(LGENP.LE.0)THEN
         WRITE(LGNPRI, 10300)IE
10300    FORMAT(' GNZGETV: GENP bank for event',I5,' not found.')
         IMVRT = -1
         GOTO 30
      ENDIF
C
C Go to GENZ frame of reference
C
      LGENP = LGENP + IGNOFF
      NPART = MOD(IGN(LGENP+JOSYS2),NPAKAC)
      MODUL = MOD(IGN(LGENP+JOSYS3),NPAKAC)
C
C add offset for SLAC system words
C
      LGENP = LGENP + NHDWAC
C
      IOFF = MODUL*(IP-1)
C
C Start time is available immediately:
C
C Default momentum ready for returning
      CALL UCOPY(GN(LGENP+IOFF+JOPXPX),PRET,3)
      VSTAR(4) = GN(LGENP+IOFF+JOTIME)
C
      IGEN = 1
C
C Jump out if this particle starts within the minimum time TVTMIN.
      IF(VSTAR(4).LT.TVTMIN) GOTO 20
C
C Loop back through maximum of MAXGEN generations
C Initialize I1 so that FORTRAN checkers are satisfied
      I1 = 0
C
      DO 10 IGEN = 1,MAXGEN
         IF(IGEN.EQ.1)THEN
C For first generation (going backwards), the end point is the present
C particle:
            I2 = IP
         ELSE
C else it is the last generation's start:
            I2 = I1
         ENDIF
C
         IOFF2 = MODUL*(I2-1)
C
C Get track of mother
C
         I1 = IGN(LGENP+IOFF2+JOMOT1)
C Turn off LUNIQ if more than one mother found:
         IF(IGN(LGENP+IOFF2+JOMOT2).NE.0)LUNIQ=.FALSE.
 
         IF(I1 .GT.0.AND.I1.LE.NPART)THEN
            IOFF1 = MODUL*(I1-1)
C Time difference ( in sec) for last section
            DELT = GN(LGENP+IOFF2+JOTIME)-GN(LGENP+IOFF1+JOTIME)
C If laboratory lifetime of mother is greater than cut-off and mother
C has been unique, then store this as mother particle in the measureable
C sense. First check that IMVRT not yet set:
            IF(IMVRT.EQ.0)THEN
               IF(DELT.GT.TVTMIN.AND.LUNIQ)THEN
                  IMVRT = I1
               ENDIF
            ENDIF
 
C Calculate distance travelled by mother:
            IF(DELT.GT.0.0)THEN
C
C Do curving step if Magnetic field corrections are turned on:
               IF(LGNMAG)THEN
                  CALL VSCALE(GN(LGENP+IOFF1+JOPXPX),-1.,PIN,3)
C Get current rotation...
                  CALL GNZGROT(AROT,GNZMAG,ROTMA)
C Apply current rotation..
                  CALL GNZROTA(PIN,ROTMA,PIN)
C We need to know the charge...
                  CALL GNZPDAT(IGN(LGENP+IOFF1+JOPDGC),CPNAM,PMASS,QQ,
     +            IER)
                  IF(IER.NE.0)THEN
                     WRITE(LGNPRI,10400)IGN(LGENP+IOFF1+JOPDGC)
10400                FORMAT(' GNZSTEP: Warning! Unknown PDG code',I10)
                     QNEG = 0.
                  ELSE
                     QNEG = - QQ
                  ENDIF
                  AM = GN(LGENP+IOFF1+JOMASS)
                  CALL GNZSTEP(PIN,GNZMAG,DELT,QNEG,AM,POUT,VSTEP,ANGR)
                  AROT = AROT + ANGR
               ELSE
C Form ratio of Delta(t)/E for mother particle:
C Multiply this ratio by mother's momentum, propogating backwards:
                  DELTE=0.
                  IF(GN(LGENP+IOFF1+JOMASS).GT.0.0)THEN
                     ENER = VMOD(GN(LGENP+IOFF1+JOPXPX),4)
                     IF(ENER.GT.0.0)DELTE = DELT/ENER * GNCELE
                  ENDIF
                  CALL VSCALE(GN(LGENP+IOFF1+JOPXPX),-DELTE,VSTEP(1),3)
               ENDIF
C Now add this to original particle
               CALL VADD(VSTAR(1),VSTEP(1),VSTAR(1),3)
C record total time to compare with VSTAR(4) at the end:
               TSTAR = TSTAR + DELT
               TNOW  = GN(LGENP+IOFF1+JOTIME)
 
               IF(TNOW.LT.TVTMIN)GOTO 20
            ENDIF
         ELSE
C
C Escape from loop
            GOTO 20
         ENDIF
C
   10 CONTINUE
C
   20 CONTINUE
C
C reverse VSTAR for no magnetic field; reverse and then rotate for the
C field ON case:
C
      VSTAR(1)=-VSTAR(1)
      VSTAR(2)=-VSTAR(2)
      VSTAR(3)=-VSTAR(3)
 
      IF(LGNMAG)THEN
C
C AROT arrived at above is the negative of the true rotation, so
C change it back:
C
         AROT = - AROT
C Get the rotation:
         CALL GNZGROT(AROT,GNZMAG,ROTMA)
         CALL GNZROTA(VSTAR,ROTMA,VSTAR)
C Rotate current particle's momentum for returning:
         CALL GNZROTA(GN(LGENP+IOFF+JOPXPX),ROTMA,PRET)
      ENDIF
 
      TDIFF = ABS(VSTAR(4) - TSTAR)
C     IF(TDIFF.GT.TVTMIN)THEN
C MORE LENIENT THRESHOLD FOR GENERATORS WHERE INTERNAL PRECISION IS BAD
      IF(TDIFF.GT.1E-12)THEN
         WRITE(LGNPRI,10500)VSTAR(4),TSTAR,TDIFF
10500    FORMAT(' GNZGETV: ERROR! VSTAR,TSTAR,DIFF : ',/,
     +   E13.7,2X,E13.7,2X,E13.7)
      ENDIF
C
   30 CONTINUE
      END
 
*CMZ :  2.00/00 03/11/99  21.31.21  by  Pavel Nevski
*-- Author :
       function LGN(i)
*KEEP,GNCSTO.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      real      GN(1)
      integer   IGN(1),LGN
*     external  IGN,LGN
      integer   IGNOFF/0/
      integer   ignsto,LENB
      character CTYP*4
      common   /gncsto/ ignsto,LENB,CTYP
      EQUIVALENCE (Q,GN), (IQ,IGN)
*KEEP,SCLINK.
C SLUG link area :    Permanent Links for SLUG:
      INTEGER         LKSLUG,NSLINK
      PARAMETER       (NSLINK=40)
      COMMON /SCLINK/ LKSLUG(NSLINK)
C The following names are equivalenced to LKSLUG.
C The equivalence name is the one used in SLINIB.
      INTEGER LKGLOB,LKDETM,LKTFLM,LKTFLT,LKAMOD,LKAGEV,LKAMCH,LKADIG,
     +        LKMAPP,LKMFLD,LKRUNT,LKEVNT,LKARAW,LKATRI,LKAPRE,LKARP1,
     +        LKARP2,LKARP3,LKDSTD,LKRUN2,LKEVN2,LKVER2,LKKIN2,LKHIT2,
     +        LKGENE
C                                       Link to:
      EQUIVALENCE (LKSLUG(1),LKGLOB)   ! top of temporary HEPEVT Zebra tree
      EQUIVALENCE (LKSLUG(2),LKDETM)   ! top of subdetector structure
      EQUIVALENCE (LKSLUG(3),LKTFLM)   ! permanent track filter structure
      EQUIVALENCE (LKSLUG(4),LKTFLT)   ! temporary track filter structure
      EQUIVALENCE (LKSLUG(5),LKAMOD)   ! MODule parameters (Dont know this)
      EQUIVALENCE (LKSLUG(6),LKAGEV)   ! event KINE copy before splitting
      EQUIVALENCE (LKSLUG(7),LKAMCH)   ! MonteCarlo Hits ( not GEANT I guess)
      EQUIVALENCE (LKSLUG(8),LKADIG)   ! DIGitized hits (again not GEANT...?)
      EQUIVALENCE (LKSLUG(9),LKMAPP)   ! Event direct access map structure
      EQUIVALENCE (LKSLUG(10),LKMFLD)  ! magnetic field banks
      EQUIVALENCE (LKSLUG(11),LKRUNT)  ! run tree bank (vertical structure)
      EQUIVALENCE (LKSLUG(12),LKEVNT)  ! event tree bank (vertical struct)
      EQUIVALENCE (LKSLUG(13),LKARAW)  ! raw data structure
      EQUIVALENCE (LKSLUG(14),LKATRI)  ! trigger banks
      EQUIVALENCE (LKSLUG(15),LKAPRE)  ! preprocessed hits
      EQUIVALENCE (LKSLUG(16),LKARP1)  ! reconstuction phase 1 banks
      EQUIVALENCE (LKSLUG(17),LKARP2)  ! reconstuction phase 2 banks
      EQUIVALENCE (LKSLUG(18),LKARP3)  ! reconstuction phase 3 banks
      EQUIVALENCE (LKSLUG(19),LKDSTD)  ! DST data banks
      EQUIVALENCE (LKSLUG(20),LKRUN2)  ! run tree bank for secondary run
      EQUIVALENCE (LKSLUG(21),LKEVN2)  ! event tree bank for secondary events
      EQUIVALENCE (LKSLUG(22),LKVER2)  ! secondary GEANT VERT bank
      EQUIVALENCE (LKSLUG(23),LKKIN2)  ! secondary GEANT KINE bank
      EQUIVALENCE (LKSLUG(24),LKHIT2)  ! secondary GEANT HITS bank
      EQUIVALENCE (LKSLUG(26),LKGENE)  ! old slug ZEBRA generator structure
*KEND.
 
C           IF(ILINK.EQ.1)THEN
C         LPQR = LGN(IGNLKP(1))
C         LPQE = LGN(IGNLKP(2))
C      ELSE IF(ILINK.EQ.2)THEN
C         LPQR = LGN(IGNLKP(3))
C         LPQE = LGN(IGNLKP(4))
 
       L    = 0
       LENB = 0
       if (0.lt.i .and. i.lt.5) then
          ide    = (i+1)/2
          m      = mod(i,2)
          ignsto = IxDiv
          if (m.eq.1)  L=LZFIND(IxDiv,LkRunt,IDE,-5)
          if (m.eq.0)  L=LZFIND(IXDIV,LkEvnt,IDE,-5)
          if (L.gt.0)  CALL UHTOC(IQ(L-4),4,CTYP,4)
          if (L.gt.0)  LENB =     IQ(L-1)
       else if (i.gt.1000) then
          L=LQ(i)
       endif
          LGN=L
       end
 
       function IGNLKP(i)
       IGNLKP=i
       end
 
 
*================================================================
*CMZ :  2.00/00 16/12/99  15.45.32  by  Pavel Nevski
*CMZ :  2.02/04 30/09/96  06.05.09  by  Steve ONeale
*CMZ :  2.02/00 17/08/92  12.45.07  by  R. DeWolf
*-- Author :    R. DeWolf   06/03/91
      SUBROUTINE GNZDLIS(IGENP,IMOTH,NDAU,IDAU)
C---------------------------------------------------------------------
C GNZDLIS
C
C Input : IGENP,IMOTH,NDAU
C
C Output: NDAU,IDAU
C
C Original Version.                                 R. DeWolf   04/03/91
C Printing added for truncated list                     "       17/08/92
C Modification: Fortran compliance (PC Compiler)    S.O'Neale 29/09/96
C---------------------------------------------------------------------
*KEEP,TYPING.
      IMPLICIT NONE
*KEND.
*CDE, GNCPAR.
*KEEP,GNCCON.
      INTEGER       IGNVER,IGNZBV,IGNDAT,IGNTIM,IGNMAC,IGNJOB,
     >              MMGENR,MMGENE,MMRUNT,MMEVNT,MMPASS,MMGENP,IOCHED
      REAL          GNCELE,GNFACM
      COMMON/GNCCON/IGNVER,IGNZBV,IGNDAT,IGNTIM,IGNJOB(4),IGNMAC,
     >              MMGENR,MMGENE,MMEVNT,MMRUNT,MMPASS,MMGENP,
     >              IOCHED(2),GNCELE,GNFACM
      INTEGER       LGNPRI/6/
      REAL          TVTMIN/0/,GNZMAG(3)/0,0,0/
C
*KEEP,GNCEVT.
      REAL           GNVERT,GNWEIG
      INTEGER        IGNWFL,IGNGAT
      LOGICAL        LGNBKR,LGNPRR,LGNWTR,LGNUPR,LGNMAG
      COMMON/GNCEVT/ GNVERT(4),IGNWFL,GNWEIG,LGNBKR,LGNPRR,LGNWTR,
     +               LGNUPR,LGNMAG,IGNGAT
C
C GNVERT(4) -current default vertex: x,y,z,time (metres,seconds),
C            relative to coordinate origin and gate reference time.
C IGNWFL    -weight flag.
C GNWEIG    -weight of event. Meaning dependent on IGNWFL
C LGNBKR    -flag  .TRUE.= BooK Run is pending
C LGNPRR    -flag  .TRUE.= Print Run information is pending
C LGNWTR    -flag  .TRUE.= Write Run tree is pending
C LGNUPR    -flag  .TRUE.= UnPack Run information to HEPEVT is pending
C LGNMAG    -flag  .TRUE.= There is non-zero magnetic field.
C IGNGAT    -current internal gate number (used when necessary)
 
*KEND.
*CDE, GNCHEP.
*KEEP,SLAC_DO.
C Data offsets for standard SLAC words
      INTEGER    NHDWAC,NPAKAC,JOSYS1,JOSYS2,JOSYS3
      common /slac_do_com/ NHDWAC
*     PARAMETER (NHDWAC = 3)
      PARAMETER (NPAKAC = 100000)
      PARAMETER (JOSYS1 = 1)
      PARAMETER (JOSYS2 = 2)
      PARAMETER (JOSYS3 = 3)
 
*KEEP,GENP_DO.
C particle bank offsets
      INTEGER        JOSTAT,JOPDGC,JOMOT1,JOMOT2,JODAU1,JOVXVX,JOVYVY,
     +        JOVZVZ,JOPXPX,JOPYPY,JOPZPZ,JOMASS,JOTIME,JOADIN,NAGENP
      COMMON/Joofss/ JOSTAT,JOPDGC,JOMOT1,JOMOT2,JODAU1,JOVXVX,JOVYVY,
     +        JOVZVZ,JOPXPX,JOPYPY,JOPZPZ,JOMASS,JOTIME,JOADIN,NAGENP
 
*KEND.
C
      INTEGER IGENP(*),IMOTH,NDAU,IDAU(*)
      INTEGER I,IOFF,MODUL,NDAUM,JM1,JM2,NPA
      CHARACTER*4 CIDH
C
C Is this the right bank?
C
      CALL UHTOC(IGENP(-4),4,CIDH,4)
      IF(CIDH.NE.'GENP' .and. CIDH.NE.'GENT') THEN
         WRITE(LGNPRI,10000)CIDH
10000    FORMAT(' GNZDLIS: Bank passed is not GENP,  IDH = ',A4,'!')
         GOTO 999
      ENDIF
C
C Get modularity. This value is returned.
C
C===>
      IF      (CIDH.EQ.'GENP') then
         MODUL  = MOD(IGENP(JOSYS3),NPAKAC)
         NPA    = MOD(IGENP(JOSYS2),NPAKAC)
         NHDWAC = 3
      else IF (CIDH.EQ.'GENT') then
         MODUL  = 15
         NPA    = IGENP(-1)/MODUL
         NHDWAC = 0
      endif
C===>
C
C Check if the modularity is big enough:
C
      IF(MODUL.LT.10)THEN
         WRITE(LGNPRI,10100)MODUL
10100    FORMAT(' GNZDLIS: MODUL =',I4,',  Cannot decode event! ')
         GOTO 999
      ENDIF
C
C Get number of particles:
C
C
C Loop over particles
      NDAUM = NDAU
      NDAU = 0
      DO 10 I=1,NPA
         IOFF = (I-1)* MODUL + NHDWAC
C Store the array contents:
         JM1 = IGENP(IOFF+JOMOT1)
         JM2 = IGENP(IOFF+JOMOT2)
         IF(JM2.GE.0)THEN
            IF(ABS(JM1).EQ.IMOTH.OR.JM2.EQ.IMOTH)THEN
               NDAU = NDAU + 1
               IF(NDAU.GT.NDAUM)THEN
                  WRITE(LGNPRI,10200)NDAUM
10200 FORMAT(' GNZDLIS: Daughter list truncated after',I5,
     + ' particles.')
                  GOTO 20
               ENDIF
               IDAU(NDAU) = I
            ENDIF
         ELSE
            IF(ABS(JM1).LE.IMOTH.AND.IMOTH.LE.ABS(JM2))THEN
               NDAU = NDAU + 1
               IF(NDAU.GT.NDAUM)THEN
                  WRITE(LGNPRI,10200)NDAUM
                  GOTO 20
               ENDIF
               IDAU(NDAU) = I
            ENDIF
         ENDIF
C
   10 CONTINUE
C
   20 CONTINUE
C
  999 END
 
****************************** dummy *****************************
           subroutine GNZGROT(AROT,GNZMAG,ROTMA)
C Get current rotation...
           end
           subroutine GNZROTA(PIN,ROTMA,PINO)
C Apply current rotation..
           end
           subroutine GNZSTEP(PIN,GNZMAG,DELT,QNEG,AM,POUT,VSTEP,ANGR)
           end
 
