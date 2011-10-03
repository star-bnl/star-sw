* $Id: agsuser.F,v 1.1.1.1 2004/01/12 23:49:38 potekhin Exp $
* $Log: agsuser.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:38  potekhin
*
* Revision 1.2  2001/02/28 17:16:15  nevski
* agckine.inc moved to commons
*
* Revision 1.1  2001/02/27 10:14:47  nevski
*  first working release
*
c***********************************************************************
      SUBROUTINE AGSUSER
c
c***  Subroutine called by GUINTI to process interactive commands
c***  New, highly simplified version for STAR Geant   pmj 6/6/95
c
c***********************************************************************
*
#include "commons/typing.inc"
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "commons/agckine.inc"
#include "atlsim/agcudec.inc"
c
      Integer       CSADDR, LENOCC, Input, Li, Ifile, Iadr, L, L1, token
      character*32  command
      character*256 string1/' '/, string/' '/
      character*1   char, C
      integer       npar, length1, length, line1, line
      logical       exist
      integer       i_part_dk,ier,i
      equivalence   (string1,line1), (string,line)

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
         call KUGETI( IudPart)
         call KUGETR( RudMin )
         call KUGETR( RudMax )
         Call Vzero ( Iudd,3 )
         if (npar .gt. 3 ) call KUGETI( Iudd(1))
         if (npar .gt. 4 ) call KUGETI( Iudd(2))
         if (npar .gt. 5 ) call KUGETI( Iudd(3))
         RudMin = max(0.,RudMin)
         RudMax = max(RudMin,RudMax)

c*** check to see if all particles exist in Geant:
*         ??? do i need it ???

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


