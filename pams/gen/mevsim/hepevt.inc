        INTEGER isthep !  status code of the entry 0 
        INTEGER idhep !  particle identity, accordingly to the PDG standard 0 
        INTEGER jmohep !  pointer(s) to position where the mother(s) stored 0 
        INTEGER jdahep !  pointers to position of the first/last daughter 0 
         REAL*4 phep !  p4 and mass (GeV) 0 
         REAL*4 vhep !  production vertex (mm) and time (mm/c) 0 
         DIMENSION isthep(30000),idhep(30000),jmohep(2,30000)
         DIMENSION jdahep(2,30000)
         DIMENSION phep(5,30000),vhep(4)     