C*********************************************************************** 

      FUNCTION IDISA(IDHEP)

      DIMENSION ITRAN(549)
      DATA ITRAN/12,11,14,13,16,15,5*70000,10,110,0,111,0,20000,120,0
     1,121,30000,10000,60000,3*0,60000,220,60000,221,0,20000,230,0,231
     2,0,231,0,231,3*0,130,0,131,0,131,0,131,3*0,20000,330,331,-240,331
     3,-241,60000,-241,60000,0.,60000,2*0,-140,0,-141,50000,-141,2*0
     4,50000,0,50000,-340,0,-341,0,-341,5*0,440,0,441,250,441,251,0
     5,251,5*0,150,0,151,0,151,5*0,350,0,351,0,351,5*0,450,0,451,0,451
     6,5*0,550,0,551,0,551,5*0,70000,60000,2221,60000,70000,60000,70000
     7,2*0,0,0,0,100000,1220,50000,1221,50000,60000,50000,60000,0,0,0
     8,1120,40000,1121,1111,50000,40000,50000,40000,2230,2130,100000
     9,90000,100000,90000,100000,90000,1230,1130,90000,80000,90000
     1,80000,90000,80000,2330,1330,2331,1331,2331,1331,3*0,3331,2240,0
     2,2241,0,2440,0,2441,3*0,2140,3*0,1440,0,1441,3*0,3240,1240,0,1241
     3,3440,0,3441,4*0,1140,0,1141,0,0,4441,0,4350,2*0,3140,2340,0,2341
     4,7*0,1340,0,1341,7*0,3340,0,3341,2250,0,2251,7*0,2150,9*0,3250
     5,1250,0,1251,6*0,4250,1150,0,1151,7*0,3150,2350,2450,2351,2451
     6,5*0,4150,1350,1450,1351,1451,6*0,3350,3450,3351,20000,0,20000,0
     7,20000,30000,10000,30000,10000,30000,10000,4*0,111,20000,0,20000
     8,20000,0,20000,30000,10000,30000,10000,7*0,20000,20000,20000,3*0
     9,231,0,231,0,231,5*0,131,0,131,0,131,5*0,331,0.,331,2*0,231,0
     1,231,7*0,131,0,131,7*0,331,20000,30000,10000,7*0,20000,0,20000
     2,231,9*0,131,3*0,231,20000,20000,3*0,331,20000,30000,10000,131
     3,-241,-241,-241,60000,50000,3451,50000,60000,50000,60000,-141
     4,-141,-141,4450,40000,4451,40000,50000,40000,50000,-341,-341
     5,-341,70000,60000,70000,60000,70000,60000,50000,441,441,441,441
     6,251,251,251,2550,50000,2551,50000,50000,90000,50000,151,151,151
     7,1550,40000,1551,40000,2*0,40000,351,351,351,3550,60000,3551
     8,60000,2331,2331,2331,451,451,451,4550,90000,4551,50000,1331
     9,1331,1331,551,551,551,551,90000,5551,100000,90000,100000,90000
     1,100000,90000,100000,90000,100000,90000,90000,80000,90000,80000
     2,90000,80000,90000,80000,90000,80000,90000/




C...Initial values.
        IDISA = 0
        IOFF = 0
        ISIGN = 1
        IAHEP = IABS(IDHEP)

C...Go through all possibilities.
        IF(IAHEP.NE.IDHEP) ISIGN = -1
        IF(IAHEP.GT.9999) GO TO 500
        IF(IAHEP.GT.999) GO TO 400
        IF(IAHEP.LT.99) THEN
          IOFF = -10
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.199) THEN
          IOFF = -98
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.299) THEN
          IOFF = -193
          IF(IDHEP.EQ.-215) THEN
            ISIGN = 1
            IOFF = -194
          ENDIF
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.399) THEN
          IOFF = -278
          IF(IAHEP.EQ.331) IOFF=-277
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.499) THEN
          IOFF = -355
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.599) THEN
          IOFF = -422
          GO TO 1000
        ENDIF
400     IF(IAHEP.LT.1119) THEN
          IOFF = -973
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.1219) THEN
          IOFF = -1072
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.2119) THEN
          IOFF = -1960
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.2199) THEN
          IOFF = -1969
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.2219) THEN
          IOFF = -2050
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.2999) THEN
          IOFF = -2059
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.3119) THEN
          IOFF = -2942
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.3199) THEN
          IOFF = -2951
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.3219) THEN
          IOFF = -3034
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.3299) THEN
          IOFF = -3043
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.3319) THEN
          IOFF = -3126
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.3329) THEN
          IOFF = -3135
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.3999) THEN
          IOFF = -3139
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.4199) THEN
          IOFF = -3916
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.4299) THEN
          IOFF = -3995
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.4399) THEN
          IOFF = -4074
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.4999) THEN
          IOFF = -4212
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.5199) THEN
          IOFF = -4851
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.5299) THEN
          IOFF = -4930
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.5339) THEN
          IOFF = -5009
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.5433) THEN
          IOFF = -5108
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.5499) THEN
          IOFF = -4980
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.5999) THEN
          IOFF = -5026
          GO TO 1000
        ENDIF
500     IF(IAHEP.GT.19999) GO TO 550
        IF(IAHEP.LT.10199) THEN
          IOFF = -9785
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.10299) THEN
          IOFF = -9879
          IF(ISIGN.EQ.-1) THEN
            IOFF = -9880
            ISIGN = 1
          ENDIF
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.10399) THEN
          IOFF = -9946
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.10499) THEN
          IOFF = -9962
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.10599) THEN
          IOFF = -10028
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.11199) THEN
          IOFF = -11105
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.11299) THEN
          IOFF = -11189
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.12199) THEN
          IOFF = -12053
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.12999) THEN
          IOFF = -11759
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.13199) THEN
          IOFF = -12583
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.13299) THEN
          IOFF = -12682
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.19999) THEN
          IOFF = -12802
          GO TO 1000
        ENDIF
550     IF(IAHEP.GT.29999) GO TO 600
        IF(IAHEP.LT.20199) THEN
          IOFF = -19766
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.20299) THEN
          IOFF = -19862
          IF(ISIGN.EQ.-1) THEN
            IOFF = -19863
            ISIGN = 1
          ENDIF
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.20399) THEN
          IOFF = -19923
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.20499) THEN
          IOFF = -19963
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.20599) THEN
          IOFF = -20029
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.21199) THEN
          IOFF = -20640
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.21299) THEN
          IOFF = -20739
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.22199) THEN
          IOFF = -21656
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.22299) THEN
          IOFF = -21725
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.23199) THEN
          IOFF = -22577
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.23299) THEN
          IOFF = -22676
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.29999) THEN
          IOFF = -22801
          GO TO 1000
        ENDIF
600     IF(IAHEP.GT.39999) GO TO 650
        IF(IAHEP.LT.30199) THEN
          IOFF = -29702
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.30299) THEN
          IOFF = -29800
          IF(ISIGN.EQ.-1) THEN
            IOFF = -29801
            ISIGN = 1
          ENDIF
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.30399) THEN
          IOFF = -29889
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.31199) THEN
          IOFF = -30638
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.31299) THEN
          IOFF = -30737
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.32199) THEN
          IOFF = -31605
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.32299) THEN
          IOFF = -31722
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.39999) THEN
          IOFF = -32631
          GO TO 1000
        ENDIF
650     CONTINUE
        IF(IAHEP.LT.40199) THEN
          IOFF = -39668
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.40299) THEN
          IOFF = -39766
          IF(ISIGN.EQ.-1) THEN
            IOFF = -39767
            ISIGN = 1
          ENDIF
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.40399) THEN
          IOFF = -39875
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.42199) THEN
          IOFF = -41660
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.42299) THEN
          IOFF = -41734
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.49999) THEN
          IOFF = -42605
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.50299) THEN
          IOFF = -49784
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.59999) THEN
          IOFF = -52595
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.60299) THEN
          IOFF = -59783
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.100499) THEN
          IOFF = -99961
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.100599) THEN
          IOFF = -100027
          GO TO 1000
        ENDIF

C...Assign found particle id, if search sucessful, else return with 0.
1000    ISM=IAHEP+IOFF
        IF(ISM.GT.0.AND.ISM.LE.549) IDISA = ITRAN(IAHEP+IOFF)*ISIGN


        RETURN
        END
