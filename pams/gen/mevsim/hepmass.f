      REAL FUNCTION HEPMASS(IDHEP)
        Implicit None
      real*4 TRAN
      INTEGER IOFF, IAHEP, IDHEP
      DIMENSION TRAN(355)
      DATA TRAN/.000511,0.,.1057,0.,1.777,7*0.,.135,0.,.7683,0.
     1,1.318,.1396,0.,.7669,1.318,1.318,5*0.,.5475,0.,.782,0.,1.275
     2,.4977,0.,.8961,7*0.,.4936,0.,.8916,7*0.,.9578,0.,1.019,1.869,0.
     3,2.01,7*0.,1.865,0.,2.007,7*0.,1.969,0.,2.11,7*0.,2.979,0.,3.097
     4,5.279,0.,5.325,7*0.,5.279,0.,5.325,7*0.,5.48,0.,5.507,7*0.,6.594
     5,0.,6.602,7*0.,9.4,0.,9.46,1.234,.9396,0.,1.233,.9383,0.,1.232
     6,9*0.,1.231,1.197,0.,1.387,7*0.,1.116,1.193,0.,1.384,7*0.,1.189,0.
     7,1.383,1.321,9*0.,1.315,11*0.,1.672,2.453,0.,2.5,7*0.,2.285,9*0.
     8,2.473,2.453,0.,2.5,7*0.,2.453,0.,2.5,7*0.,2.466,2.55,0.,2.63,7*0.
     9,2.55,0.,2.63,7*0.,2.73,0.,2.8,5.8,0.,5.81,7*0.,5.641,9*0.,5.84
     1,5.8,0.,5.81,7*0.,5.8,0.,5.81,7*0.,5.84,5.96,0.,5.97,7*0.,5.96,0.
     2,5.97,7*0.,6.12,0.,6.13,.9827,0.,1.232,.983,.983,1.232,1.232,7*0.
     3,1.,0.,1.17,1.26,1.26,1.26,9*0.,1.282/
        HEPMASS=0.0
        IF(IDHEP.EQ.700201) THEN
           HEPMASS=1.875613
           RETURN
        ENDIF
        IOFF = 0
        IAHEP = IABS(IDHEP)
        IF(IAHEP.LT.6) RETURN
        IF(IAHEP.GT.9999) GO TO 500
        IF(IAHEP.LT.99) THEN
          IF(IAHEP.EQ.24) THEN
             HEPMASS=80.22
             RETURN
          ENDIF
          IF(IAHEP.EQ.23) THEN
             HEPMASS=91.187
             RETURN
          ENDIF
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
            IOFF = -194
          ENDIF
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.399) THEN
          IOFF = -278
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
        IF(IAHEP.EQ.1114) THEN
          IOFF = -982
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.2199) THEN
          IOFF = -1979
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.2999) THEN
          IOFF = -2076
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.3199) THEN
          IOFF = -2963
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.3299) THEN
          IOFF = -3052
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
        IF(IAHEP.LT.4999) THEN
          IOFF = -4074
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
        IF(IAHEP.LT.5999) THEN
          IOFF = -5009
          GO TO 1000
        ENDIF
500     IF(IAHEP.LT.10199) THEN
          IOFF = -9785
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.10299) THEN
          IOFF = -9881
          GO TO 1000
        ENDIF
        IF(IAHEP.EQ.20113) THEN
          IOFF = -19770
          GO TO 1000
        ENDIF
        IF(IAHEP.LT.20299) THEN
          IOFF = -19868
          GO TO 1000
        ENDIF
1000    HEPMASS = TRAN(IAHEP+IOFF)
        RETURN
        END
