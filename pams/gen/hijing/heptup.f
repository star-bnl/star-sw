C: definitions from /afs/cern.ch/user/n/nevski/bin/geant3
*******************************************************************************
      SUBROUTINE HEPTUP                                                   2
      CALL HERMES (0)                                                     3
      CALL HEPHELP                                                        4
      END                                                                 5
*******************************************************************************
      SUBROUTINE HEPEXAMPLE                                               8
      INTEGER MM(2)/0,0/,DD(2)/0,0/,IW(2)/90,91/,P/0/                     9
      REAL PP(3),VV(3)                                                    10
*   call HEPfat
*   call HEPdense
      INTEGER N/1/,K                                                      13
C *                                                                       15
      DO 5011 K=1,N                                                       15
         NP=12000                                                         16
C    *                                                                    17
         DO 5021 J=1,4300                                                 17
*                                      B,F,  Et,At,    A1,Z1,A2,Z2
            CALL HEPEVENT ('hijing',0,NP, 3.,1.5,100.,0.1, 197.,97.,      19
     *      197.,97.)                                                     19
*      more information like number of participants etc
*      call hepinfo  ( 101,102,104,105)
C       *                                                                 22
            DO 5031 I=1,NP                                                22
               PP(1)=1                                                    22
               PP(2)=2                                                    22
               PP(3)=3*RNDM(1)                                            22
               VV(1)=0                                                    22
               VV(2)=0                                                    23
               VV(3)=.01*RNDM(1)                                          23
               CALL HEPPART (I,1,421,MM,DD,PP,10.,1.,VV,0.)               24
5031        CONTINUE                                                      25
5032        CONTINUE                                                      25
5021     CONTINUE                                                         26
5022     CONTINUE                                                         26
5011  CONTINUE                                                            27
5012  CONTINUE                                                            27
      N=2*N                                                               28
*   call hepend('z')
      END                                                                 30
*******************************************************************************
      SUBROUTINE HEPHELP                                                  34
      PRINT *,'*********************************************************  36
     ******************'                                                  36
      PRINT *,'* A utility set to write a standard HEPEVNT n-tuple 999 i  37
     *n evgen.run.nt  *'                                                  37
      PRINT *,'*********************************************************  38
     ******************'                                                  38
      PRINT *,'*          mandatory Calles:                               39
     *                *'                                                  39
      PRINT *,'* HEPEvent (generator, run, Npart, B,F,Et,At, A1,Z1,A2,Z2  40
     *) - new event   *'                                                  40
      PRINT *,'* HEPPart  (ipa,ist,pdg, moth,idau,pp, Ep,Am,vv,vt) - wri  41
     *te new particle *'                                                  41
      PRINT *,'* HEPEnd   (option) - close ntuple and compress it on "z"  42
     * option         *'                                                  42
      PRINT *,'*          optional Calls:                                 43
     *                *'                                                  43
      PRINT *,'* HEPinfo  (i1,i2,i3,i4) - add more information to the ne  44
     *w event header  *'                                                  44
      PRINT *,'* HEPdens  - dense packing: no mother-daughter relations,  45
     * no vertex info *'                                                  45
      PRINT *,'* HEPfat   - fat packing: precise vertex info              46
     *                *'                                                  46
      PRINT *,'* HEPnormal- return to default packing: vertex limited wi  47
     *thin 1 mk       *'                                                  47
      PRINT *,'*          experts Call:                                   48
     *                *'                                                  48
      PRINT *,'* HEPmax (IPdg, IRef, NPart, Vxyzt, Nbit) - set limits on  49
     * HEP variables  *'                                                  49
      PRINT *,'*********************************************************  50
     ******************'                                                  50
      END                                                                 51
*******************************************************************************
      SUBROUTINE HEPRUN (RUN)                                             55
      IMPLICIT NONE                                                       56
      INTEGER SYSTEMF,GETPID,LENOCC,HEPNUMB,RUN,RID,IS,LUN/98/,LUX/2/     57
      INTEGER NP,IDRUN,IEVT,IDAT,ITIM,IGEN,PID,IRNDM                      58
      COMMON /HEP_HEAD/ NP,IDRUN,IEVT,IDAT,ITIM,IGEN,PID,IRNDM            59
      LOGICAL BTEST                                                       61
      CHARACTER*8 CP,CR                                                   62
      CHARACTER LINE*80/' '/                                              63
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Check RUN>0                                                             66
      IF (RUN.GT.0) RETURN                                                66
      RUN = 0                                                             66
      IDRUN = -1                                                          66
*                                  create new lock
C *                                                                       69
C    Loop here                                                            69
         DO 5011 IS=1,1000000                                             69
         PID = GETPID()                                                   69
         CALL HEPNUMBER (PID,CP)                                          70
         OPEN (LUN,FILE='run.lock',STATUS='NEW',ERR=  5020)               71
         CLOSE (LUN)                                                      71
         GO TO 5012                                                       71
5020     CONTINUE                                                         72
5011  CONTINUE                                                            73
5012  CONTINUE                                                            73
C Check IS>100000                                                         73
      IF (IS.GT.100000) STOP 'cannot create a run.lock file'              73
*                                  increment run number
C *                                                                       77
C    Loop here                                                            77
5031     CONTINUE                                                         77
         OPEN (LUN,FILE='run.data', STATUS='OLD', ERR=  5040)             77
C    *                                                                    77
C       Loop here                                                         77
5051        CONTINUE                                                      77
            READ (LUN,'(a)', END=5040 ) LINE                              77
         GO TO 5051                                                       78
5052     CONTINUE                                                         78
5040     CLOSE (LUN)                                                      78
         RUN=HEPNUMB(LINE,'run=')                                         80
         RID=HEPNUMB(LINE,'pid=')                                         80
C    Check RUN==IDRUN & RID==PID                                          82
         IF (RUN.EQ.IDRUN .AND. RID.EQ.PID)GO TO 5032                     82
         RUN=RUN+1                                                        83
         CALL HEPNUMBER (RUN,CR)                                          84
         LINE = 'echo `date` `uname -n` pid=' //CP(1:LENOCC(CP))// ' run  85
     *=' //CR(1:LENOCC(CR))                                               85
         IS = SYSTEMF (LINE(1:LENOCC(LINE))//' >> run.data')              85
         IDRUN=RUN                                                        85
      GO TO 5031                                                          87
5032  CONTINUE                                                            87
*                                   release lock
      IS = SYSTEMF ('/bin/rm -f run.lock')                                89
*
      IRNDM = PID                                                         91
C *                                                                       91
C    Loop here                                                            91
         DO 5061 IS=0,31                                                  91
C    Check BTEST(IDRUN,IS)                                                91
         IF (BTEST(IDRUN,IS)) IRNDM=IBSET(IRNDM,30-IS)                    91
5061  CONTINUE                                                            92
5062  CONTINUE                                                            92
      CALL RLUXGO (LUX,IRNDM,0,0)                                         93
      PRINT *,'CALL RANLUX(RVEC,LEN) to generate random numbers (V115)'   94
*
      END                                                                 96
*******************************************************************************
      SUBROUTINE HEPEVENT (GENERATOR, RUN, NPART, B,F,ET,AT, A1,Z1,A2,    100
     *Z2)                                                                 100
      IMPLICIT NONE                                                       101
      INTEGER GETPID,IVER/11/,IPMX/1000000/,MXRF/1/,MXPA/65000/,NV/16/    102
      INTEGER MAXIP,MAXRF,MAXPA,MAXNV,K,IC/0/,ID/999/,LUX/2/,I1,I2,I3,    103
     *I4                                                                  103
      REAL VXMAX,VXMX/0.001/,VXMM                                         104
* Input parameters:
      CHARACTER GENERATOR*(*)                                             106
      INTEGER RUN,NPART,IPA,IST,PDG,MOTH(2),IDAU(2)                       107
      REAL B,F,ET,AT,A1,Z1,A2,Z2,PP(3),EP,AM,VV(3),VT                     108
* Cernlib related:
      INTEGER NWPAW,IPAW, LENOCC, SYSTEMF, LREC,BSIZE, IQUEST             111
      PARAMETER (NWPAW=1 000 000, LREC=8100, BSIZE=LREC)                  112
      COMMON /PAWC/ IPAW(NWPAW)                                           113
      COMMON /QUEST/ IQUEST(100)                                          113
* Hepevnt related:
      INTEGER IS,L,MREF,LUH/99/                                           116
      CHARACTER CR*8, CP*8, OPTION*1, GENER*20, FILE*20/' '/              117
      INTEGER NP,IDRUN,IEVT,IDAT,ITIM,IGEN,PID,IRNDM                      118
      COMMON /HEP_HEAD/ NP,IDRUN,IEVT,IDAT,ITIM,IGEN,PID,IRNDM            119
      INTEGER IP,ISTAT,IPDG,MOT1,MOT2,IDA1,IDA2                           120
      REAL PXYZ,ENER,MASS,VXYZ,VTIME                                      121
      COMMON /HEP_PART/ IP,ISTAT,IPDG,MOT1,MOT2,IDA1,IDA2,PXYZ(3),ENER,   123
     *MASS, VXYZ(3),VTIME                                                 123
* Local:
      PARAMETER (K=9)                                                     125
      INTEGER I,CC(K)                                                     126
      CHARACTER*20 GG(K),FF(K)                                            127
      DATA (GG(I),FF(I),CC(I),I=1,K) / 'nexus' , 'nexus.optns' , 9, 'sta  137
     *rpom' , 'starpom.inp' , 8, 'starlight', 'starlight.in', 7,          137
     *'venus' , 'optns.dat' , 6, 'hijing' , 'hijev.inp' , 5, 'mevsim' ,   137
     *'mult_gen.in' , 4, 'rqmd' , 'rqmd.inp' , 3, 'pythia' , 'pythia.dat  137
     *a' , 1, 'user' , 'user.input' , 0/                                  137
      LOGICAL FIRST/.TRUE./                                               138
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Check FIRST                                                             141
      IF (FIRST) THEN                                                     141
      FIRST=.FALSE.                                                       142
      MREF = MXRF*MXPA                                                    143
      GENER= GENERATOR                                                    144
      CALL CUTOL(GENER)                                                   145
C Check RUN > 0                                                           146
      IF (RUN .GT. 0) IDRUN = RUN                                         146
C Check IDRUN<=0                                                          148
      IF (IDRUN.LE.0) CALL HEPRUN(IDRUN)                                  148
      CALL HEPNUMBER (IDRUN,CR)                                           149
      PID = GETPID()                                                      151
      CALL HEPNUMBER (PID,CP)                                             152
*   Is HBOOK and memory initialised ?
C *                                                                       154
C    Check IPAW(1)==0                                                     154
         IF (IPAW(1).EQ.0) THEN                                           154
         PRINT *,' HBOOK initialised for HEP'                             154
         CALL HLIMIT(NWPAW)                                               155
      END IF                                                              155
*   print *,' hbook initialised with len = ',Ipaw(1)
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      VXMM=0                                                              159
C Check VXMX>0                                                            160
      IF (VXMX.GT.0) VXMM=-VXMX                                           160
      FILE='evgen.'//CR(1:LENOCC(CR))//'.nt'                              161
C  anyway truncated in HRFILE to 65000                                    163
      IQUEST(10) = 66000                                                  163
      CALL HROPEN (LUH,'HEPEVNT',FILE,'QN7',LREC,IS)                      164
C Check IS!=0                                                             165
      IF (IS.NE.0) STOP ' HEPTUPLE: Can not open output file '            165
      CALL RZCDIR ('//HEPEVNT', ' ')                                      167
      CALL HBSET ('BSIZE',BSIZE,IS)                                       168
C Check IS!=0                                                             169
      IF (IS.NE.0) STOP ' HEPTUPLE: Can not set buffer size '             169
      CALL HBNT (ID,'HEPEVNT',' ')                                        171
      CALL HEPBNAME (ID,IP, 'itrac' , 0, -1, MXPA)                        172
      CALL HEPBNAME (ID,ISTAT,'istat' , 0, -1, 15)                        173
      CALL HEPBNAME (ID,IPDG, 'ipdg' , 0, -IPMX,IPMX)                     174
      CALL HEPBNAME (ID,MOT1, 'moth1' , 0, -1, MREF)                      175
      CALL HEPBNAME (ID,MOT2, 'moth2' , 0, -MREF, 1)                      176
      CALL HEPBNAME (ID,IDA1, 'idau1' , 0, -1, MREF)                      177
      CALL HEPBNAME (ID,IDA2, 'idau2' , 0, -1, MREF)                      178
      CALL HEPBNAME (ID,PXYZ, 'Pxyz(3)' , 0, 0, 0)                        179
      CALL HEPBNAME (ID,ENER, 'ener' , 0, 0, 0)                           180
      CALL HEPBNAME (ID,MASS, 'mass:R:' ,16, -1, 10)                      181
C  mm                                                                     182
      CALL HEPBNAME (ID,VXYZ, 'Vxyz(3):R:' ,NV, VXMM, VXMX)               182
C mm/c                                                                    183
      CALL HEPBNAME (ID,VTIME,'Vtime:R:' ,NV, 0, VXMX)                    183
*   1 mm/c=0.33 ns;   ct=3.e11: tmax=5000 -> 17 ns
C *                                                                       187
C    Loop here                                                            187
         DO 5011 IGEN=1,K-1                                               187
         L=MIN(LENOCC(GENER),LENOCC(GG(IGEN)))                            187
         IF (GENER(1:L).EQ.GG(IGEN)(1:L))GO TO 5012                       188
5011  CONTINUE                                                            189
5012  CONTINUE                                                            189
      CALL HEPINPUT(FF(IGEN))                                             189
      ENDIF                                                               190
*
      CALL VZERO (IP,16)                                                  192
*  Call RLUXAT(Lux,Irndm,Ida1,Ida2)
* if (MRef>999) { ida1=Pid/Mref; ida2=Mod(Pid,Mref) }
      IEVT=IEVT+1                                                         195
      IP=NPART                                                            195
      ISTAT=IVER                                                          195
      IPDG=IPMX                                                           195
      CALL DATIME (IDAT,ITIM)                                             195
      IPDG=IPDG-1                                                         196
      PXYZ(1)=IDRUN                                                       196
      PXYZ(2)=IEVT                                                        196
      PXYZ(3)=IDAT                                                        196
      PXYZ(4)=ITIM                                                        196
      PXYZ(5)=CC(IGEN)                                                    196
      CALL HFNT(ID)                                                       196
      IPDG=IPDG-1                                                         197
      PXYZ(1)=B                                                           197
      PXYZ(2)=F                                                           197
      PXYZ(3)=ET                                                          197
      PXYZ(4)=AT                                                          197
      PXYZ(5)=1                                                           197
      CALL HFNT(ID)                                                       197
      IPDG=IPDG-1                                                         198
      PXYZ(1)=A1                                                          198
      PXYZ(2)=Z1                                                          198
      PXYZ(3)=A2                                                          198
      PXYZ(4)=Z2                                                          198
      PXYZ(5)=2                                                           198
      CALL HFNT(ID)                                                       198
      NP=NPART                                                            199
      RETURN                                                              201
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ENTRY HEPINFO (I1, I2, I3, I4)                                      204
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IPDG=IPDG-1                                                         205
      PXYZ(1)=I1                                                          205
      PXYZ(2)=I2                                                          205
      PXYZ(3)=I3                                                          205
      PXYZ(4)=I4                                                          205
      PXYZ(5)=3                                                           205
      CALL HFNT(ID)                                                       205
      RETURN                                                              207
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ENTRY HEPPART (IPA,IST,PDG,MOTH,IDAU,PP,EP,AM,VV,VT)                210
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL RZCDIR('//HEPEVNT',' ')                                        213
      IP = IPA                                                            214
      ISTAT = MIN(IST,9)                                                  215
      IPDG = PDG                                                          216
      MOT1 = MOTH(1)                                                      217
      MOT2 = MOTH(2)                                                      218
      IDA1 = IDAU(1)                                                      219
      IDA2 = IDAU(2)                                                      220
      CALL UCOPY(PP,PXYZ,3)                                               221
      CALL UCOPY(VV,VXYZ,3)                                               222
      VTIME = VT                                                          223
      MASS = AM                                                           224
      ENER = EP                                                           225
*  if (ipa==Np) Ip=-1
      CALL HFNT(ID)                                                       227
      RETURN                                                              228
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ENTRY HEPEND(OPTION)                                                231
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL HROUT(0,IC,'NT')                                               233
      CALL HREND('HEPEVNT')                                               235
C Check OPTION=='z' | OPTION=='Z'                                         236
      IF (OPTION.EQ.'z' .OR. OPTION.EQ.'Z') I=SYSTEMF('gzip -f '//        236
     *FILE(1:LENOCC(FILE)))                                               236
      RETURN                                                              237
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ENTRY HEPNORMAL                                                     239
      MXRF=1                                                              239
      NV=16                                                               239
      RETURN                                                              240
      ENTRY HEPDENSE                                                      240
      MXRF=0                                                              240
      NV= 1                                                               240
      RETURN                                                              241
      ENTRY HEPFAT                                                        241
      MXRF=1                                                              241
      VXMX=0                                                              241
      RETURN                                                              242
      ENTRY HEPMAX (MAXIP, MAXRF, MAXPA, VXMAX, MAXNV)                    243
      IPMX=MAXIP                                                          243
      MREF=MAXRF                                                          243
      MXPA=MAXPA                                                          243
      VXMX=VXMAX                                                          243
      NV=MAXNV                                                            243
      RETURN                                                              245
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END                                                                 247
*************************************************************************
      FUNCTION HEPNUMB (LINE,VAR)                                         251
      IMPLICIT NONE                                                       252
      INTEGER HEPNUMB,LENOCC,INDEX,ICHAR,I,J                              253
      CHARACTER*(*) LINE,VAR                                              254
      HEPNUMB = 0                                                         255
      I=INDEX(LINE,VAR(1:LENOCC(VAR)))                                    255
C Skip Unless I>0                                                         256
      IF (I.LE.0)GO TO 1                                                  256
C *                                                                       257
      DO 5011 I=I+LENOCC(VAR),LENOCC(LINE)                                257
C    Skip Unless LINE(I:I)!=' '                                           257
         IF (LINE(I:I).EQ.' ')GO TO 5011                                  257
         J=ICHAR(LINE(I:I))-ICHAR('0')                                    257
C    Check J<0 | J>9                                                      258
         IF (J.LT.0 .OR. J.GT.9) RETURN                                   258
         HEPNUMB=10*HEPNUMB+J                                             259
5011  CONTINUE                                                            260
5012  CONTINUE                                                            260
1     CONTINUE                                                            261
      END                                                                 261
*******************************************************************************
      SUBROUTINE HEPINPUT (INPUT)                                         265
      CHARACTER INPUT*(*),LINE*128                                        266
      INTEGER LENOCC,LI/98/,ID/998/                                       271
      CLOSE (LI)                                                          271
      CALL HBNT (ID,'HEPinput',' ')                                       271
      CALL HBNAMC (ID,'HEPinput',LINE, 'line(4):C*32:')                   271
      OPEN (LI,FILE=INPUT(1:LENOCC(INPUT)),STATUS='OLD',ERR=  5010)       271
C *                                                                       271
C    Loop here                                                            271
5021     CONTINUE                                                         271
         READ (LI,'(a)',ERR=5010,END=5010) LINE                           271
         CALL HFNT(ID)                                                    272
      GO TO 5021                                                          272
5022  CONTINUE                                                            272
5010  CLOSE (LI)                                                          273
      END                                                                 274
*************************************************************************
      SUBROUTINE HEPBNAME(ID,VAR,FORM,NB,IA,IB)                           278
      IMPLICIT NONE                                                       279
      INTEGER LENOCC,INDEX,NB,ID,IA,IB,VAR,L                              280
      CHARACTER C*8,CC*80,FORM*(*)                                        281
      CC=FORM                                                             281
      L=INDEX(FORM,':')                                                   281
C Check L>0                                                               282
      IF (L.GT.0) CC=FORM(1:L-1)                                          282
C *                                                                       284
C    Check IA!=0 | IB!=0                                                  284
         IF (IA.NE.0 .OR. IB.NE.0) THEN                                   284
         CC = FORM                                                        285
         CALL HEPNUMBER(NB,C)                                             286
C    Check NB>0                                                           287
         IF (NB.GT.0) CC=CC(1:LENOCC(CC))//C(1:LENOCC(C))                 287
C    Check INDEX(CC,':')>0                                                288
         IF (INDEX(CC,':').GT.0) CC=CC(1:LENOCC(CC))//':'                 288
         CALL HEPNUMBER(IA,C)                                             288
         CC=CC(1:LENOCC(CC))//'['//C(1:LENOCC(C))                         289
         CALL HEPNUMBER(IB,C)                                             289
         CC=CC(1:LENOCC(CC))//','//C(1:LENOCC(C))//']'                    290
      END IF                                                              291
      CALL HBNAME(ID,'particle',VAR,CC(1:LENOCC(CC)))                     292
      END                                                                 293
*************************************************************************
      SUBROUTINE HEPNUMBER(NUM,CNUM)                                      297
      IMPLICIT NONE                                                       298
      CHARACTER CNUM*(*),S*14                                             299
      INTEGER ANUM,NUM,L,I,I1,I2                                          300
      REAL RNUM                                                           301
      EQUIVALENCE (RNUM,ANUM)                                             302
      ANUM=NUM                                                            304
C *                                                                       304
C    Check ABS(NUM)<=1000000                                              304
         IF (ABS(NUM).LE.1000000) THEN                                    304
         WRITE (S, * ) ANUM                                               304
      ELSE                                                                305
         WRITE (S,'(f14.6)') RNUM                                         305
      END IF                                                              306
      I1=14                                                               306
      I2=1                                                                306
C *                                                                       307
C    Loop here                                                            307
         DO 5011 I=1,14                                                   307
C    Skip Unless S(I:I)!=' '                                              307
         IF (S(I:I).EQ.' ')GO TO 5011                                     307
         I1=MIN(I1,I)                                                     307
         I2=MAX(I2,I)                                                     307
5011  CONTINUE                                                            308
5012  CONTINUE                                                            308
      CNUM=S(I1:I2)                                                       308
      L=I2-I1+1                                                           309
1     CONTINUE                                                            310
      END                                                                 310
*************************************************************************
      FUNCTION RNDM(SEED)                                                 314
      REAL RNDM,B                                                         315
      INTEGER IRNDM,SEED,IB,LUX,INI,K1,K2,IVEC(25)                        316
      EQUIVALENCE (IB,B)                                                  317
      LOGICAL FIRST/.TRUE./                                               318
C *                                                                       319
C    Check (FIRST)                                                        319
         IF (FIRST) THEN                                                  319
         FIRST=.FALSE.                                                    319
         PRINT *,'*** ','RNDM HAS BEEN REPLACED BY RANLUX',' ***'         319
      END IF                                                              319
      CALL RANLUX(B,1)                                                    321
      RNDM = B                                                            322
      RETURN                                                              323
      ENTRY IRNDM (SEED)                                                  325
C *                                                                       326
C    Check (FIRST)                                                        326
         IF (FIRST) THEN                                                  326
         FIRST=.FALSE.                                                    326
         PRINT *,'*** ','RNDM HAS BEEN REPLACED BY RANLUX',' ***'         326
      END IF                                                              326
      CALL RANLUX(B,1)                                                    328
      IRNDM = IB                                                          329
      RETURN                                                              330
      ENTRY RDMIN (SEED)                                                  332
C *                                                                       333
C    Check (FIRST)                                                        333
         IF (FIRST) THEN                                                  333
         FIRST=.FALSE.                                                    333
         PRINT *,'*** ','RNDM HAS BEEN REPLACED BY RANLUX',' ***'         333
      END IF                                                              333
      CALL RLUXGO(2,SEED,0,0)                                             334
      PRINT *,' RANLUX restarted using ',2,SEED,0,0                       335
      PRINT *,' Use RLUXIN(Ivec) to a direct restart'                     336
      RETURN                                                              337
      ENTRY RDMOUT (SEED)                                                 339
C *                                                                       340
C    Check (FIRST)                                                        340
         IF (FIRST) THEN                                                  340
         FIRST=.FALSE.                                                    340
         PRINT *,'*** ','RNDM HAS BEEN REPLACED BY RANLUX',' ***'         340
      END IF                                                              340
      CALL RLUXAT(LUX,INI,K1,K2)                                          341
      CALL RLUXUT(IVEC)                                                   342
      PRINT *,' to restart RANLUX use RLUXGO(',LUX,INI,K1,K2,')'          343
      PRINT *,' or use RLUXIN with the following vector: '                344
      PRINT *,IVEC                                                        345
      RETURN                                                              346
      END                                                                 347
*******************************************************************************
      SUBROUTINE RANNOR(A,B)                                              349
      IMPLICIT NONE                                                       350
      REAL Y(2),A,B,X,R                                                   351
      LOGICAL FIRST/.TRUE./                                               352
C *                                                                       353
C    Check (FIRST)                                                        353
         IF (FIRST) THEN                                                  353
         FIRST=.FALSE.                                                    353
         PRINT *,'*** ','RANNOR HAS BEEN REPLACED BY F(RANLUX)',' ***'    353
      END IF                                                              353
      CALL RANLUX(Y,2)                                                    355
C Check Y(1).EQ.0.                                                        356
      IF (Y(1).EQ.0.) CALL RANLUX(Y,2)                                    356
      X = 6.283185*Y(2)                                                   357
      R = SQRT (-2.0*LOG(Y(1)))                                           358
      A = R*SIN (X)                                                       359
      B = R*COS (X)                                                       360
      RETURN                                                              361
      END                                                                 362
