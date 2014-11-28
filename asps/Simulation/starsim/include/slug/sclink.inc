* $Id: sclink.inc,v 1.1.1.1 2004/01/12 23:49:49 potekhin Exp $
* $Name:  $
C ROOT link area for I/O control and link book-keeping
      COMMON /SCROOT/ LKROOT
      INTEGER         LKROOT, LVROOT(2)
      EQUIVALENCE     (LKROOT, LVROOT  )
C SLUG link area :    Permanent Links for SLUG:
C                     LKSLUG(1)          First word in link space
C                     LKSLUG(NSLINK)     Last word in link space
      INTEGER         LKSLUG,NSLINK
      PARAMETER       (NSLINK=40)
      COMMON /SCLINK/ LKSLUG(NSLINK)
C The following names are equivalenced to LKSLUG.
C The equivalence name is the one used in SLINIB.
      INTEGER LKGLOB,LKDETM,LKTFLM,LKTFLT
      INTEGER LKAMOD,LKRAND,LKAMCH,LKADIG
      INTEGER LKMAPP,LKMFLD,LKRUNT,LKEVNT
      INTEGER LKARAW,LKATRI,LKAPRE,LKARP1,LKARP2,LKARP3,LKDSTD
      INTEGER LKRUN2,LKEVN2,LKVER2,LKKIN2,LKHIT2,LKARW2
      INTEGER LKGENE,LKGEVT,LKGEN2
C                                       Link to:
      EQUIVALENCE (LKSLUG(1),LKGLOB)   ! top of temporary HEPEVT Zebra tree
      EQUIVALENCE (LKSLUG(2),LKDETM)   ! top of subdetector structure
      EQUIVALENCE (LKSLUG(3),LKTFLM)   ! permanent track filter structure
      EQUIVALENCE (LKSLUG(4),LKTFLT)   ! temporary track filter structure
      EQUIVALENCE (LKSLUG(5),LKAMOD)   ! MODule parameters
      EQUIVALENCE (LKSLUG(6),LKRAND)   ! Link to Random Number Seed Store
C                                      ! Call SWON if you use(d) LKAGEV
      EQUIVALENCE (LKSLUG(7),LKAMCH)   ! MonteCarlo Hits  not GEANT
      EQUIVALENCE (LKSLUG(8),LKADIG)   ! DIGitized hits   not GEANT
      EQUIVALENCE (LKSLUG(9),LKMAPP)   ! map structure
      EQUIVALENCE (LKSLUG(10),LKMFLD)  ! magnetic field banks
      EQUIVALENCE (LKSLUG(11),LKRUNT)  ! run tree bank   vertical structure
      EQUIVALENCE (LKSLUG(12),LKEVNT)  ! event tree bank   vertical struct
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
      EQUIVALENCE (LKSLUG(25),LKARW2)  ! secondary raw data bank
C                                       DEPRECATED LINKS to the top of
      EQUIVALENCE (LKSLUG(26),LKGENE)  ! old slug ZEBRA generator structure
      EQUIVALENCE (LKSLUG(27),LKGEVT)  ! temporary ZEBRA generator structure
      EQUIVALENCE (LKSLUG(28),LKGEN2)  ! old slug secondary event structure
C
      INTEGER     IIRANC
      PARAMETER   (IIRANC=3)           ! LRANC=LQ(LKGLOB-IIRANC)
