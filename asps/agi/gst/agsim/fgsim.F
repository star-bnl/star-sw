*CMZ :          03/09/98  14.12.13  by  Pavel Nevski
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
 
      Integer       CSADDR, LENOCC, Input, Li, Ifile, Iadr, L, L1
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
                Call AgZOPEN ('P',string1(1:length1),'E',0,0)
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
              Call AgZOPEN  ('O',string1(1:length1),'GEKHD',0,0)
*             PN,03.09.98 - spool output until the first trig
*             Call AGZWRITE ('O',ier)
 
          else                                          !  close output
              Call AgZOPEN('O',' ',' ',0,0)
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
 
 
*CMZ :          07/09/98  17.34.19  by  Pavel Nevski
*CMZ :  1.40/05 26/08/98  20.16.56  by  Pavel Nevski
*CMZ :  1.40/05 26/08/98  19.34.12  by  Pavel Nevski
*CMZ :  2.06/13 01/02/95  09.38.53  by  O.Couet
*-- Author :    Fons Rademakers   20/11/93
      SUBROUTINE PALOGO
*
*     Print STAF version identification
*
      CHARACTER  CHPAW*8, VERSQQ*8, MONTH(12)*12
      DATA MONTH/'January','February','March','April','May','June',
     > 'July','August','September','October','November','December'/
 
*KEEP,VIDQQ.
      CHARACTER*68 VIDQQ
      DATA VIDQQ/
     +'@(#)* Advanced Geant Inteface   1.40/05   C: 14/09/98  01.17.35
     +'/
*KEEP,DATEQQ.
      IDATQQ =   980914
*KEEP,TIMEQQ.
      ITIMQQ =    117
*KEEP,VERSQQ.
      VERSQQ = ' 1.40/05'
      IVERSQ =  14005
*KEND.
      CHPAW  = 'S T A F'
      IDAY   = mod(IDATQQ,100)
      IMONTH = mod(IDATQQ/100,100)
      IYEAR  = IDATQQ/10000
      NCM    = LENOCC(MONTH(IMONTH))
      IF(IYEAR.LT.90) MILLE=2000+IYEAR
      IF(IYEAR.GE.90) MILLE=1900+IYEAR
 
      PRINT 3000,CHPAW,VERSQQ,IDAY,MONTH(IMONTH)(1:NCM),
     >           MILLE,MONTH(IMONTH)(NCM+1:)
 3000 FORMAT(' ******************************************************'/,
     >       ' *                                                    *'/,
     >       ' *            W E L C O M E    to   ', A7,'           *'/,
     >       ' *                                                    *'/,
     >       ' *        Version ',A8,4X,I2,1X,A,1X,I4,A,4X,        '*'/,
     >       ' *                                                    *'/,
     >       ' ******************************************************')
      END
