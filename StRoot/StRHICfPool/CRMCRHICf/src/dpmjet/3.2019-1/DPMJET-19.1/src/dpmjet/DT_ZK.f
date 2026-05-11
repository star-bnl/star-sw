
      BLOCK DATA DT_ZK
 
      IMPLICIT NONE
      SAVE 
 
C decay channel information for HADRIN
      INCLUDE 'inc/hnaddh'
C decay channel information for HADRIN
      INCLUDE 'inc/hnaddn'
 
C     Particle masses in GeV                                           *
      DATA AMZ/3*2.2D0 , 0.9576D0 , 3*1.887D0 , 2.4D0 , 2.03D0 , 
     &     2*1.44D0 , 2*1.7D0 , 3*0.D0/
C     Resonance width Gamma in GeV                                     *
      DATA GAZ/3*.2D0 , .1D0 , 4*.2D0 , .18D0 , 2*.2D0 , 2*.15D0 , 
     &     3*0.D0/
C     Mean life time in seconds                                        *
      DATA TAUz/16*0.D0/
C     Charge of particles and resonances                               *
      DATA ICHz/0 , 1 , 3*0 , 1 , -1 , 0 , 1 , -1 , 0 , 0 , 1 , 3*0/
C     Baryonic charge                                                  *
      DATA IBArz/2 , 7*0 , 1 , -1 , -1 , 1 , 1 , 3*0/
C     First number of decay channels used for resonances               *
C     and decaying particles                                           *
      DATA K1Z/308 , 310 , 313 , 317 , 322 , 365 , 393 , 421 , 425 , 
     &     434 , 440 , 446 , 449 , 3*460/
C     Last number of decay channels used for resonances                *
C     and decaying particles                                           *
      DATA K2Z/309 , 312 , 316 , 321 , 364 , 392 , 420 , 424 , 433 , 
     &     439 , 445 , 448 , 451 , 3*460/
C     Weight of decay channel                                          *
      DATA WTZ/.17D0 , .83D0 , 2*.33D0 , .34D0 , .17D0 , 2*.33D0 , 
     &     .17D0 , .01D0 , .13D0 , .36D0 , .27D0 , .23D0 , .0014D0 , 
     &     .0029D0 , .0014D0 , .0029D0 , 4*.0007D0 , .0517D0 , .0718D0 , 
     &     .0144D0 , .0431D0 , .0359D0 , .0718D0 , .0014D0 , .0273D0 , 
     &     .0014D0 , .0431D0 , 2*.0129D0 , .0259D0 , .0517D0 , .0359D0 , 
     &     .0014D0 , 2*.0144D0 , .0129D0 , .0014D0 , .0259D0 , .0359D0 , 
     &     .0072D0 , .0474D0 , .0948D0 , .0259D0 , .0072D0 , .0144D0 , 
     &     .0287D0 , .0431D0 , .0144D0 , .0287D0 , .0474D0 , .0144D0 , 
     &     .0075D0 , .0057D0 , .0019D0 , .0038D0 , .0095D0 , 2*.0014D0 , 
     &     .0191D0 , .0572D0 , .1430D0 , 2*.0029D0 , 5*.0477D0 , 
     &     .0019D0 , .0191D0 , .0686D0 , .0172D0 , .0095D0 , .1888D0 , 
     &     .0172D0 , .0191D0 , .0381D0 , 2*.0571D0 , .0190D0 , .0057D0 , 
     &     .0019D0 , .0038D0 , .0095D0 , .0014D0 , .0014D0 , .0191D0 , 
     &     .0572D0 , .1430D0 , 2*.0029D0 , 5*.0477D0 , .0019D0 , 
     &     .0191D0 , .0686D0 , .0172D0 , .0095D0 , .1888D0 , .0172D0 , 
     &     .0191D0 , .0381D0 , 2*.0571D0 , .0190D0 , 4*.25D0 , 2*.2D0 , 
     &     .12D0 , .1D0 , .07D0 , .07D0 , .14D0 , 2*.05D0 , .0D0 , 
     &     .3334D0 , .2083D0 , 2*.125D0 , .2083D0 , .0D0 , .125D0 , 
     &     .2083D0 , .3334D0 , .2083D0 , .125D0 , .3D0 , .05D0 , .65D0 , 
     &     .3D0 , .05D0 , .65D0 , 9*1.D0/
C     Particle numbers in decay channel                                *
      DATA NZK1/8 , 1 , 2 , 9 , 1 , 2 , 9 , 2 , 9 , 7 , 13 , 31 , 15 , 
     &     24 , 23 , 13 , 23 , 13 , 2*23 , 14 , 13 , 23 , 31 , 98 , 
     &     2*33 , 32 , 23 , 14 , 13 , 35 , 2*23 , 14 , 13 , 33 , 23 , 
     &     98 , 31 , 23 , 14 , 13 , 35 , 2*33 , 32 , 23 , 35 , 33 , 32 , 
     &     98 , 5*35 , 4*13 , 23 , 13 , 98 , 32 , 33 , 23 , 13 , 23 , 
     &     13 , 14 , 13 , 32 , 13 , 98 , 23 , 13 , 2*32 , 13 , 33 , 32 , 
     &     98 , 2*35 , 4*14 , 23 , 14 , 98 , 2*34 , 23 , 14 , 23 , 
     &     2*14 , 13 , 34 , 14 , 98 , 23 , 14 , 2*34 , 14 , 33 , 32 , 
     &     98 , 2*35 , 104 , 61 , 105 , 62 , 1 , 17 , 21 , 17 , 22 , 
     &     2*21 , 22 , 21 , 2 , 67 , 68 , 69 , 2 , 2*9 , 68 , 69 , 70 , 
     &     2 , 9 , 2*24 , 15 , 2*25 , 16 , 9*0/
      DATA NZK2/2*8 , 1 , 8 , 9 , 2*8 , 2*1 , 7 , 14 , 13 , 16 , 25 , 
     &     23 , 14 , 23 , 14 , 31 , 33 , 32 , 34 , 35 , 31 , 23 , 31 , 
     &     33 , 34 , 31 , 32 , 34 , 31 , 33 , 32 , 2*33 , 35 , 31 , 33 , 
     &     31 , 33 , 32 , 34 , 35 , 31 , 33 , 34 , 35 , 31 , 4*33 , 32 , 
     &     3*35 , 2*23 , 13 , 31 , 32 , 33 , 13 , 31 , 32 , 2*31 , 32 , 
     &     33 , 32 , 32 , 35 , 31 , 2*32 , 33 , 31 , 33 , 35 , 33 , 
     &     3*32 , 35 , 2*23 , 14 , 31 , 34 , 33 , 14 , 31 , 33 , 2*31 , 
     &     34 , 32 , 33 , 34 , 35 , 31 , 2*34 , 33 , 31 , 33 , 35 , 33 , 
     &     2*34 , 33 , 35 , 1 , 2 , 8 , 9 , 25 , 13 , 35 , 2*32 , 33 , 
     &     31 , 13 , 23 , 31 , 13 , 23 , 14 , 79 , 80 , 31 , 13 , 23 , 
     &     14 , 78 , 79 , 8 , 1 , 8 , 1 , 8 , 1 , 9*0/
      DATA NZK3/23 , 14 , 2*13 , 23 , 13 , 2*23 , 14 , 0 , 7 , 14 , 
     &     4*0 , 2*23 , 10*0 , 33 , 2*31 , 0 , 33 , 34 , 32 , 34 , 0 , 
     &     35 , 0 , 31 , 3*35 , 0 , 3*31 , 35 , 31 , 33 , 34 , 31 , 33 , 
     &     34 , 31 , 33 , 35 , 0 , 23 , 14 , 6*0 , 32 , 3*33 , 32 , 34 , 
     &     0 , 35 , 0 , 2*35 , 2*31 , 35 , 32 , 34 , 31 , 33 , 32 , 0 , 
     &     23 , 13 , 6*0 , 34 , 2*33 , 34 , 33 , 34 , 0 , 35 , 0 , 
     &     2*35 , 2*31 , 35 , 2*34 , 31 , 2*34 , 25*0 , 23 , 2*14 , 23 , 
     &     2*13 , 9*0/
C     Particle  names                                                  *
      DATA ANAmz/'NNPI' , 'ANPPI' , 'ANNPI' , ' ETS  ' , ' PAP  ' , 
     &     ' PAN  ' , 'APN' , 'DEO   ' , 'S+2030' , 'AN*-14' , 
     &     'AN*014' , 'KONPI ' , 'AKOPPI' , 3*'BLANK'/
C     Name of decay channel                                            *
      DATA ZKNam4/'NNPI0' , 'PNPI-' , 'APPPI+' , 'ANNPI+' , 'ANPPI0' , 
     &     'APNPI+' , 'ANNPI0' , 'APPPI0' , 'ANPPI-'/
      DATA ZKNam5/' GAGA ' , 'P+P-GA' , 'ETP+P-' , 'K+K-  ' , 'K0AK0 ' , 
     &     ' POPO ' , ' P+P- ' , 'POPOPO' , 'P+P0P-' , 'P0ET  ' , 
     &     '&0R0  ' , 'P-R+  ' , 'P+R-  ' , 'POOM  ' , ' ETET ' , 
     &     'ETSP0 ' , 'R0ET  ' , ' R0R0 ' , 'R+R-  ' , 'P0ETR0' , 
     &     'P-ETR+' , 'P+ETR-' , ' OMET ' , 'P0R0R0' , 'P0R+R-' , 
     &     'P-R+R0' , 'P+R-R0' , 'R0OM  ' , 'P0ETOM' , 'ETSR0 ' , 
     &     'ETETET' , 'P0R0OM' , 'P-R+OM' , 'P+R-OM' , 'OMOM  ' , 
     &     'R0ETET' , 'R0R0ET' , 'R+R-ET' , 'P0OMOM' , 'OMETET' , 
     &     'R0R0R0' , 'R+R0R-' , 'ETSRET' , 'OMR0R0' , 'OMR+R-' , 
     &     'OMOMET' , 'OMOMR0' , 'OMOMOM' , ' P+PO ' , 'P+POPO' , 
     &     'P+P+P-' , 'P+ET  ' , 'P0R+  ' , 'P+R0  ' , 'ETSP+ ' , 
     &     'R+ET  ' , ' R0R+ ' , 'POETR+' , 'P+ETR0' , 'POR+R-' , 
     &     'P+R0R0' , 'P-R+R+' , 'P+R-R+' , 'R+OM  ' , 'P+ETOM' , 
     &     'ETSR+ ' , 'POR+OM' , 'P+R0OM' , 'R+ETET' , 'R+R0ET' , 
     &     'P+OMOM' , 'R0R0R+' , 'R+R+R-' , 'ETSR+E' , 'OMR+R0' , 
     &     'OMOMR+' , 'P-PO  ' , 'P-POPO' , 'P-P-P+' , 'P-ET  ' , 
     &     'POR-  ' , 'P-R0  ' , 'ETSP- ' , 'R-ET  ' , 'R-R0  ' , 
     &     'POETR-' , 'P-ETR0' , 'POR-R0' , 'P-R+R-' , 'P-R0R0'/
      DATA ZKNam6/'P+R-R-' , 'R-OM  ' , 'P-ETOM' , 'ETSR- ' , 'POR-OM' , 
     &     'P-R0OM' , 'R-ETET' , 'R-R0ET' , 'P-OMOM' , 'R0R0R-' , 
     &     'R+R-R-' , 'ETSR-E' , 'OMR0R-' , 'OMOMR-' , 'PAN-14' , 
     &     'APN+14' , 'NAN014' , 'ANN014' , 'PAKO  ' , 'LPI+  ' , 
     &     'SI+OM' , 'LAMRO+' , 'SI0RO+' , 'SI+RO0' , 'SI+ETA' , 
     &     'SI0PI+' , 'SI+PI0' , 'APETA ' , 'AN=P+ ' , 'AN-PO ' , 
     &     'ANOPO ' , 'APRHOO' , 'ANRHO-' , 'ANETA ' , 'AN-P+ ' , 
     &     'AN0PO ' , 'AN+P- ' , 'APRHO+' , 'ANRHO0' , 'KONPIO' , 
     &     'KOPPI-' , 'K+NPI-' , 'AKOPPO' , 'AKONP+' , 'K-PPI+' , 
     &     9*'BLANK'/
C=                                               end*block.zk      *
      END BLOCK DATA
