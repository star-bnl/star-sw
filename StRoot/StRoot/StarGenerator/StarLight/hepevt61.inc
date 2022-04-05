* ... HEPEVT common block for pythia 6.1 and upper
      Integer    NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      DOUBLE PRECISION PHEP,VHEP
      PARAMETER (NMXHEP=4000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     >JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)

      Integer          LMXHEP,NBytesPw,IRunNum
      character*8                      cEVGEN
      Common /HEPEVTD/ LMXHEP,NBytesPw,IRunNum,cEVGEN

