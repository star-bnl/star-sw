*
* $Id: sclink.inc,v 1.1 1998/04/10 15:30:49 fisyak Exp $
*
* $Log: sclink.inc,v $
* Revision 1.1  1998/04/10 15:30:49  fisyak
* Move gstar into CVS
*
*
*
* sclink.inc
*
*CMZ :  1.30/00 02/04/97  16.22.41  by  Pavel Nevski
*CMZ :  1.00/00 12/08/95  19.37.12  by  Pavel Nevski
*-- Author :
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
      EQUIVALENCE (LKSLUG(6),LKAGEV)   ! Link to general event structure
      EQUIVALENCE (LKSLUG(7),LKAMCH)   ! MonteCarlo Hits ( not GEANT I guess)
      EQUIVALENCE (LKSLUG(8),LKADIG)   ! DIGitized hits (again not GEANT...?)
      EQUIVALENCE (LKSLUG(9),LKMAPP)   ! map structure
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
