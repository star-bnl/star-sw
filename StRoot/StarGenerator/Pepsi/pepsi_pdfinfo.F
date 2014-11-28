       subroutine pepsi_pdfinfo

       implicit none

       integer i
       logical ok

       write(6,'(5x,a)') 
     &    '101: Schaefer, Phys. Lett. B 208,2 (1988) 175'
       write(6,'(5x,a)') 
     &    '104: Schaefer  et al hep-ph/9505306'
       write(6,'(5x,a)') 
     &    '105: Bartelski et al hep-ph/9502271 Set II(p,n)'
       write(6,'(5x,a)') 
     &    '106: Bartelski et al hep-ph/9502271 Set II(P,n)'
       write(6,'(5x,a)') 
     &    '107: Gehrmann hep-ph/9512406 Gluon A (NLO) + DGLAP'
       write(6,'(5x,a)') 
     &    '108: Gehrmann hep-ph/9512406 Gluon B (NLO) + DGLAP'
       write(6,'(5x,a)') 
     &    '109: Gehrmann hep-ph/9512406 Gluon C (NLO) + DGLAP'
       write(6,'(5x,a)') 
     &    '110: Gehrmann  et al hep-ph/9512406 Gluon A (LO)'
       write(6,'(5x,a)') 
     &    '111: Gehrmann  et al hep-ph/9512406 Gluon B (LO)'
       write(6,'(5x,a)') 
     &    '112: Gehrmann  et al hep-ph/9512406 Gluon C (LO)'
       write(6,'(5x,a)') 
     &    '113: Gehrmann  et al hep-ph/9512406 Gluon A (LO) + (DGLAP)'
       write(6,'(5x,a)') 
     &    '114: Gehrmann  et al hep-ph/9512406 Gluon B (LO) + (DGLAP)'
       write(6,'(5x,a)') 
     &    '115: Gehrmann  et al hep-ph/9512406 Gluon C (LO) + (DGLAP)'
       write(6,'(5x,a)') 
     &    '116: GRSV DO-TH 95/13, RAL-TR-95-042, standard scenario, NLO'
       write(6,'(5x,a)') 
     &    '117: GRSV DO-TH 95/13, RAL-TR-95-042, valence scenario, NLO'
       write(6,'(5x,a)') 
     &    '118: GRSV DO-TH 95/13, RAL-TR-95-042, standard scenario, LO'
       write(6,'(5x,a)') 
     &    '119: GRSV DO-TH 95/13, RAL-TR-95-042, valence scenario, LO'
       write(6,'(5x,a)') 
     &    '120: Stanley J.Brodsky Nucl.Phys. B441(1995)'
       write(6,'(5x,a)') 
     &    '121: S.Keler & J.F.Owens  PLB266(1991), PRD19(1994)1199'
       write(6,'(5x,a)') 
     &    '122: D.W.Duke & J.F.Owens PRD30(11984) SET1 (unpol only)'
       write(6,'(5x,a)') 
     &    '123: D.W.Duke & J.F.Owens PRD30(11984) SET2 (unpol only)'
       write(6,'(5x,a)') 
     &    '124: D.deFlorian et al., hep-ph/9711440  LO set 1'
       write(6,'(5x,a)') 
     &    '125: D.deFlorian et al., hep-ph/9711440  LO set 2'
       write(6,'(5x,a)') 
     &    '126: D.deFlorian et al., hep-ph/9711440  LO set 3'
       write(6,'(5x,a)') 
     &    '127: D.deFlorian et al., hep-ph/9711440 NLO set 1'
       write(6,'(5x,a)') 
     &    '128: D.deFlorian et al., hep-ph/9711440 NLO set 2'
       write(6,'(5x,a)') 
     &    '129: D.deFlorian et al., hep-ph/9711440 NLO set 3'
       write(6,'(5x,a)') 
     &    '130: unpol Gehrmann hep-ph/9512406 with du=u/2, dd=0'
       write(6,'(5x,a)') 
     &    '131: unpol Gehrmann hep-ph/9512406 with dd=d/2, du=0'
       write(6,'(5x,a)') 
     &    '132: fit routine (outside official code)'
       write(6,'(5x,a)') 
     &    '133: unpol CTEQ4 Low-Q2, with du=u/2, dd=-0.3d'
       write(6,'(5x,a)') 
     &    '137: unpol MRS95 Low-Q2, with du=u/2, dd=-0.3d'
       write(6,'(5x,a)') 
     &    '144: pol GRSV 2000 LO standard scenario'
       write(6,'(5x,a)') 
     &    '145: pol GRSV 2000 LO valence scenario'
       write(6,'(5x,a)') 
     &    '146: pol GRSV 2000 NLO standard scenario'
       write(6,'(5x,a)') 
     &    '147: pol GRSV 2000 NLO valence scenario'
       write(6,'(5x,a)') 
     &    '150: CTEQ5L LO'
       write(6,'(5x,a)') 
     &    '151: CTEQ5M NLO MSBAR'
       write(6,'(5x,a)') 
     &    '152: CTEQ5M1 NLO MSBAR (update)'
       write(6,'(5x,a)') 
     &    '161: MRS99 cor01 '
       write(6,'(5x,a)') 
     &    '162: MRS99 cor02 '
       write(6,'(5x,a)') 
     &    '163: MRS99 cor03 '
       write (6,'(5x,a)') 
     &    '164: MRS99 cor04 '
       write(6,'(5x,a)') 
     &    '165: MRS99 cor05 '
       write(6,'(5x,a)') 
     &    '166: MRS99 cor06 '
       write(6,'(5x,a)') 
     &    '167: MRS99 cor07 '
       write(6,'(5x,a)') 
     &    '168: MRS99 cor08 '
       write (6,'(5x,a)') 
     &    '169: MRS99 cor09 '
       write(6,'(5x,a)') 
     &    '170: MRS99 cor10 '
       write (6,'(5x,a)') 
     &    '171: MRS99 cor11 '
       write(6,'(5x,a)') 
     &    '172: MRS99 cor12 '
        write(6,'(5x,a)')
     &    '173: CTEQ6L LO '
        write(6,'(5x,a)')
     &    '174: CTEQ6D NLO DIS '
        write(6,'(5x,a)')
     &    '175: CTEQ6M NLO MSBAR '  

       return

!----------------------------------------------------------

       entry pepsi_pdfcheck (i,ok)

       ok = .true.
       if (.not.( (i.eq.101).or.
     +       (i.ge.104.and.i.le.133).or.
     +       (i.eq.137).or.(144.le.i.and.i.le.147).or.
     +       (i.ge.150.and.i.le.152).or.(i.ge.161.and.i
     +         .le.175))) then
         ok = .false.
       endif
       
       return

       end
