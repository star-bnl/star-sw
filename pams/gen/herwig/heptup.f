C: definitions from /star/u2c/nevski/bin/geant3.def
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
            CALL HEPEVENT ('hijing',0,NP, 3.,1.5,100.,0.1, 197.,97.,      18
     *      197.,97.)                                                     18
C       *                                                                 19
            DO 5031 I=1,NP                                                19
               PP(1)=1                                                    19
               PP(2)=2                                                    19
               PP(3)=3*RNDM()                                             19
               VV(1)=0                                                    19
               VV(2)=0                                                    20
               VV(3)=.01*RNDM()                                           20
               CALL HEPPART (I,1,421,MM,DD,PP,10.,1.,VV,0.)               21
5031        CONTINUE                                                      22
5032        CONTINUE                                                      22
5021     CONTINUE                                                         23
5022     CONTINUE                                                         23
5011  CONTINUE                                                            24
5012  CONTINUE                                                            24
      N=2*N                                                               25
*   call hepend('z')
      END                                                                 27
*******************************************************************************
      SUBROUTINE HEPHELP                                                  31
      PRINT *,'*********************************************************  33
     ******************'                                                  33
      PRINT *,'* A utility set to write a standard HEPEVNT n-tuple 999 i  34
     *n evgen.run.nt  *'                                                  34
      PRINT *,'*********************************************************  35
     ******************'                                                  35
      PRINT *,'*          mandatory Calles:                               36
     *                *'                                                  36
      PRINT *,'* HEPEvent (generator, run, Npart, B,F,Et,At, A1,Z1,A2,Z2  37
     *) - new event   *'                                                  37
      PRINT *,'* HEPPart  (ipa,ist,pdg, moth,idau,pp, Ep,Am,vv,vt) - wri  38
     *te new particle *'                                                  38
      PRINT *,'* HEPEnd   (option) - close ntuple and compress it on "z"  39
     * option         *'                                                  39
      PRINT *,'*          optional Calls:                                 40
     *                *'                                                  40
      PRINT *,'* HEPdens  - dense packing: no mother-daughter relations,  41
     * no vertex info *'                                                  41
      PRINT *,'* HEPfat   - fat packing: precise vertex info              42
     *                *'                                                  42
      PRINT *,'* HEPnormal- return to default packing: vertex limited wi  43
     *thin 1 mk       *'                                                  43
      PRINT *,'*          experts Call:                                   44
     *                *'                                                  44
      PRINT *,'* HEPmax (IPdg, IRef, NPart, Vxyzt, Nbit) - set limits on  45
     * HEP variables  *'                                                  45
      PRINT *,'*********************************************************  46
     ******************'                                                  46
      END                                                                 47
*******************************************************************************
      SUBROUTINE HEPRUN (RUN)                                             51
      IMPLICIT NONE                                                       52
      INTEGER SYSTEMF,GETPID,LENOCC,HEPNUMB,RUN,RID,IS,LUN/98/,LUX/2/     53
      INTEGER NP,IDRUN,IEVT,IDAT,ITIM,IGEN,PID,IRNDM                      54
      COMMON /HEP_HEAD/ NP,IDRUN,IEVT,IDAT,ITIM,IGEN,PID,IRNDM            55
      LOGICAL BTEST                                                       57
      CHARACTER*8 CP,CR                                                   58
      CHARACTER LINE*80/' '/                                              59
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Check RUN>0                                                             62
      IF (RUN.GT.0) RETURN                                                62
      RUN = 0                                                             62
      IDRUN = -1                                                          62
*                                  create new lock
C *                                                                       65
C    Loop here                                                            65
         DO 5011 IS=1,1000000                                             65
         PID = GETPID()                                                   65
         CALL HEPNUMBER (PID,CP)                                          66
         OPEN (LUN,FILE='run.lock',STATUS='NEW',ERR=  5020)               67
         CLOSE (LUN)                                                      67
         GO TO 5012                                                       67
5020     CONTINUE                                                         68
5011  CONTINUE                                                            69
5012  CONTINUE                                                            69
C Check IS>100000                                                         69
      IF (IS.GT.100000) STOP 'cannot create a run.lock file'              69
*                                  increment run number
C *                                                                       73
C    Loop here                                                            73
5031     CONTINUE                                                         73
         OPEN (LUN,FILE='run.data', STATUS='OLD', ERR=  5040)             73
C    *                                                                    73
C       Loop here                                                         73
5051        CONTINUE                                                      73
            READ (LUN,'(a)', END=5040 ) LINE                              73
         GO TO 5051                                                       74
5052     CONTINUE                                                         74
5040     CLOSE (LUN)                                                      74
         RUN=HEPNUMB(LINE,'run=')                                         76
         RID=HEPNUMB(LINE,'pid=')                                         76
C    Check RUN==IDRUN & RID==PID                                          78
         IF (RUN.EQ.IDRUN .AND. RID.EQ.PID)GO TO 5032                     78
         RUN=RUN+1                                                        79
         CALL HEPNUMBER (RUN,CR)                                          80
         LINE = 'echo `date` `uname -n` pid=' //CP(1:LENOCC(CP))// ' run  81
     *=' //CR(1:LENOCC(CR))                                               81
         IS = SYSTEMF (LINE(1:LENOCC(LINE))//' >> run.data')              81
         IDRUN=RUN                                                        81
      GO TO 5031                                                          83
5032  CONTINUE                                                            83
*                                   release lock
      IS = SYSTEMF ('/bin/rm -f run.lock')                                85
*
      IRNDM = PID                                                         87
C *                                                                       87
C    Loop here                                                            87
         DO 5061 IS=0,31                                                  87
C    Check BTEST(IDRUN,IS)                                                87
         IF (BTEST(IDRUN,IS)) IRNDM=IBSET(IRNDM,30-IS)                    87
5061  CONTINUE                                                            88
5062  CONTINUE                                                            88
      CALL RLUXGO (LUX,IRNDM,0,0)                                         89
      PRINT *,'CALL RANLUX(RVEC,LEN) to generate random numbers (V115)'   90
*
      END                                                                 92
*******************************************************************************
      SUBROUTINE HEPEVENT (GENERATOR, RUN, NPART, B,F,ET,AT, A1,Z1,A2,    96
     *Z2)                                                                 96
      IMPLICIT NONE                                                       97
      INTEGER GETPID,IVER/11/,IPMX/1000000/,MXRF/1/,MXPA/32000/,NV/16/    98
      INTEGER MAXIP,MAXRF,MAXPA,MAXNV,K,IC/0/,ID/999/,LUX/2/              99
      INTEGER I1,I2,I3,I4
      REAL VXMAX,VXMX/20.00/,VXMM                                         100
* Input parameters:
      CHARACTER GENERATOR*(*)                                             102
      INTEGER RUN,NPART,IPA,IST,PDG,MOTH(2),IDAU(2)                       103
      REAL B,F,ET,AT,A1,Z1,A2,Z2,PP(3),EP,AM,VV(3),VT                     104
* Cernlib related:
      INTEGER NWPAW,IPAW, LENOCC, SYSTEMF, LREC,BSIZE, IQUEST             107
      PARAMETER (NWPAW=1 000 000, LREC=8100, BSIZE=LREC)                  108
      COMMON /PAWC/ IPAW(NWPAW)                                           109
      COMMON /QUEST/ IQUEST(100)                                          109
* Hepevnt related:
      INTEGER IS,L,MREF,LUH/99/                                           112
      CHARACTER CR*8, CP*8, OPTION*1, GENER*20, FILE*20                   113
      INTEGER NP,IDRUN,IEVT,IDAT,ITIM,IGEN,PID,IRNDM                      114
      COMMON /HEP_HEAD/ NP,IDRUN,IEVT,IDAT,ITIM,IGEN,PID,IRNDM            115
      INTEGER IP,ISTAT,IPDG,MOT1,MOT2,IDA1,IDA2                           116
      REAL PXYZ,ENER,MASS,VXYZ,VTIME                                      117
      COMMON /HEP_PART/ IP,ISTAT,IPDG,MOT1,MOT2,IDA1,IDA2,PXYZ(3),ENER,   119
     *MASS, VXYZ(3),VTIME                                                 119
* Local:
      PARAMETER (K=7)                                                     121
      INTEGER I,CC(K)                                                     122
      CHARACTER*20 GG(K),FF(K)                                            123
      DATA (GG(I),FF(I),CC(I),I=1,K) / 'starlight', 'starlight.in', 7, '  131
     *venus' , 'optns.dat' , 6, 'hijing' , 'hijev.inp' , 5, 'herwig' , '  131
     *herwig.in' , 4, 'rqmd' , 'rqmd.inp' , 3, 'pythia' , 'pythia.data '  131
     * , 1, 'user' , 'user.input' , 0/                                    131
      LOGICAL FIRST/.TRUE./                                               132
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C Check FIRST                                                             135
      IF (FIRST) THEN                                                     135
      FIRST=.FALSE.                                                       136
      MREF = MXRF*MXPA                                                    137
      GENER= GENERATOR                                                    138
      CALL CUTOL(GENER)                                                   139
C Check RUN > 0                                                           140
      IF (RUN .GT. 0) IDRUN = RUN                                         140
C Check IDRUN<=0                                                          142
      IF (IDRUN.LE.0) CALL HEPRUN(IDRUN)                                  142
      CALL HEPNUMBER (IDRUN,CR)                                           143
      PID = GETPID()                                                      145
      CALL HEPNUMBER (PID,CP)                                             146
*   Is HBOOK and memory initialised ?
C *                                                                       148
C    Check IPAW(1)==0                                                     148
         IF (IPAW(1).EQ.0) THEN                                           148
         PRINT *,' HBOOK initialised for HEP'                             148
         CALL HLIMIT(NWPAW)                                               149
      END IF                                                              149
*   print *,' hbook initialised with len = ',Ipaw(1)
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      VXMM=0                                                              153
C Check VXMX>0                                                            154
      IF (VXMX.GT.0) VXMM=-VXMX                                           154
      FILE='evgen.'//CR(1:LENOCC(CR))//'.nt'                              155
C  anyway truncated in HRFILE to 65000                                    157
      IQUEST(10) = 66000                                                  157
      CALL HROPEN (LUH,'HEPEVNT',FILE,'QN7',LREC,IS)                      158
C Check IS!=0                                                             159
      IF (IS.NE.0) STOP ' HEPTUPLE: Can not open output file '            159
      CALL RZCDIR ('//HEPEVNT', ' ')                                      161
      CALL HBSET ('BSIZE',BSIZE,IS)                                       162
C Check IS!=0                                                             163
      IF (IS.NE.0) STOP ' HEPTUPLE: Can not set buffer size '             163
      CALL HBNT (ID,'HEPEVNT',' ')                                        165
      CALL HEPBNAME (ID,IP, 'itrac' , 0, -1, MXPA)                        166
      CALL HEPBNAME (ID,ISTAT,'istat' , 0, -1, 200)                       167
      CALL HEPBNAME (ID,IPDG, 'ipdg' , 0, -IPMX,IPMX)                     168
      CALL HEPBNAME (ID,MOT1, 'moth1' , 0, -1, MREF)                      169
      CALL HEPBNAME (ID,MOT2, 'moth2' , 0, -1, MREF)                      170
      CALL HEPBNAME (ID,IDA1, 'idau1' , 0, -1, MREF)                      171
      CALL HEPBNAME (ID,IDA2, 'idau2' , 0, -1, MREF)                      172
      CALL HEPBNAME (ID,PXYZ, 'Pxyz(3)' , 0, 0, 0)                        173
      CALL HEPBNAME (ID,ENER, 'ener' , 0, 0, 0)                           174
      CALL HEPBNAME (ID,MASS, 'mass:R:' ,16, -300, 300)                   175
C  mm                                                                     176
      CALL HEPBNAME (ID,VXYZ, 'Vxyz(3):R:' ,NV, -VXMX, VXMX)              176
C mm/c                                                                    177
      CALL HEPBNAME (ID,VTIME,'Vtime:R:' ,NV, -VXMX, VXMX)                177
*   1 mm/c=0.33 ns;   ct=3.e11: tmax=5000 -> 17 ns
C *                                                                       181
C    Loop here                                                            181
         DO 5011 IGEN=1,K-1                                               181
         L=MIN(LENOCC(GENER),LENOCC(GG(IGEN)))                            181
         IF (GENER(1:L).EQ.GG(IGEN)(1:L))GO TO 5012                       182
5011  CONTINUE                                                            183
5012  CONTINUE                                                            183
      CALL HEPINPUT(FF(IGEN))                                             183
      ENDIF                                                               184
*
      CALL VZERO (IP,16)                                                  186
*  Call RLUXAT(Lux,Irndm,Ida1,Ida2)
* if (MRef>999) { ida1=Pid/Mref; ida2=Mod(Pid,Mref) }
      IEVT=IEVT+1                                                         189
      IP=NPART                                                            189
      ISTAT=IVER                                                          189
      IPDG=IPMX                                                           189
      CALL DATIME (IDAT,ITIM)                                             189
      IPDG=IPDG-1                                                         190
      PXYZ(1)=IDRUN                                                       190
      PXYZ(2)=IEVT                                                        190
      PXYZ(3)=IDAT                                                        190
      PXYZ(4)=ITIM                                                        190
      PXYZ(5)=CC(IGEN)                                                    190
      CALL HFNT(ID)                                                       190
      IPDG=IPDG-1                                                         191
      PXYZ(1)=B                                                           191
      PXYZ(2)=F                                                           191
      PXYZ(3)=ET                                                          191
      PXYZ(4)=AT                                                          191
      PXYZ(5)=1                                                           191
      CALL HFNT(ID)                                                       191
      IPDG=IPDG-1                                                         192
      PXYZ(1)=A1                                                          192
      PXYZ(2)=Z1                                                          192
      PXYZ(3)=A2                                                          192
      PXYZ(4)=Z2                                                          192
      PXYZ(5)=2                                                           192
      CALL HFNT(ID)                                                       192
      NP=NPART                                                            193
      RETURN                                                              195
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
      ENTRY HEPPART (IPA,IST,PDG,MOTH,IDAU,PP,EP,AM,VV,VT)                198
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL RZCDIR('//HEPEVNT',' ')                                        201
      IP = IPA                                                            202
      ISTAT = IST                                                         203
      IPDG = PDG                                                          204
      MOT1 = MOTH(1)                                                      205
      MOT2 = MOTH(2)                                                      206
      IDA1 = IDAU(1)                                                      207
      IDA2 = IDAU(2)                                                      208
      CALL UCOPY(PP,PXYZ,3)                                               209
      CALL UCOPY(VV,VXYZ,3)                                               210
      VTIME = VT                                                          211
      MASS = AM                                                           212
      ENER = EP                                                           213
*  if (ipa==Np) Ip=-1
      CALL HFNT(ID)                                                       215
      RETURN                                                              216
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ENTRY HEPEND(OPTION)                                                219
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL HROUT(0,IC,'NT')                                               221
      CALL HREND('HEPEVNT')                                               223
C Check OPTION=='z' | OPTION=='Z'                                         224
      IF (OPTION.EQ.'z' .OR. OPTION.EQ.'Z') I=SYSTEMF('gzip -f '//        224
     *FILE(1:LENOCC(FILE)))                                               224
      RETURN                                                              225
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ENTRY HEPNORMAL                                                     227
      MXRF=1                                                              227
      NV=16                                                               227
      RETURN                                                              228
      ENTRY HEPDENSE                                                      228
      MXRF=0                                                              228
      NV= 1                                                               228
      RETURN                                                              229
      ENTRY HEPFAT                                                        229
      MXRF=1                                                              229
      VXMX=0                                                              229
      RETURN                                                              230
      ENTRY HEPMAX (MAXIP, MAXRF, MAXPA, VXMAX, MAXNV)                    231
      IPMX=MAXIP                                                          231
      MREF=MAXRF                                                          231
      MXPA=MAXPA                                                          231
      VXMX=VXMAX                                                          231
      NV=MAXNV                                                            231
      RETURN                                                              233
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END                                                                 235
*************************************************************************
      FUNCTION HEPNUMB (LINE,VAR)                                         239
      IMPLICIT NONE                                                       240
      INTEGER HEPNUMB,LENOCC,INDEX,ICHAR,I,J                              241
      CHARACTER*(*) LINE,VAR                                              242
      HEPNUMB = 0                                                         243
      I=INDEX(LINE,VAR(1:LENOCC(VAR)))                                    243
C Skip Unless I>0                                                         244
      IF (I.LE.0)GO TO 1                                                  244
C *                                                                       245
      DO 5011 I=I+LENOCC(VAR),LENOCC(LINE)                                245
C    Skip Unless LINE(I:I)!=' '                                           245
         IF (LINE(I:I).EQ.' ')GO TO 5011                                  245
         J=ICHAR(LINE(I:I))-ICHAR('0')                                    245
C    Check J<0 | J>9                                                      246
         IF (J.LT.0 .OR. J.GT.9) RETURN                                   246
         HEPNUMB=10*HEPNUMB+J                                             247
5011  CONTINUE                                                            248
5012  CONTINUE                                                            248
1     CONTINUE                                                            249
      END                                                                 249
*******************************************************************************
      SUBROUTINE HEPINPUT (INPUT)                                         253
      CHARACTER INPUT*(*),LINE*128                                        254
      INTEGER LENOCC,LI/98/,ID/998/                                       259
      CLOSE (LI)                                                          259
      CALL HBNT (ID,'HEPinput',' ')                                       259
      CALL HBNAMC (ID,'HEPinput',LINE, 'line(4):C*32:')                   259
      OPEN (LI,FILE=INPUT(1:LENOCC(INPUT)),STATUS='OLD',ERR=  5010)       259
C *                                                                       259
C    Loop here                                                            259
5021     CONTINUE                                                         259
         READ (LI,'(a)',ERR=5010,END=5010) LINE                           259
         CALL HFNT(ID)                                                    260
      GO TO 5021                                                          260
5022  CONTINUE                                                            260
5010  CLOSE (LI)                                                          261
      END                                                                 262
*************************************************************************
      SUBROUTINE HEPBNAME(ID,VAR,FORM,NB,IA,IB)                           266
      IMPLICIT NONE                                                       267
      INTEGER LENOCC,INDEX,NB,ID,IA,IB,VAR,L                              268
      CHARACTER C*8,CC*80,FORM*(*)                                        269
      CC=FORM                                                             269
      L=INDEX(FORM,':')                                                   269
C Check L>0                                                               270
      IF (L.GT.0) CC=FORM(1:L-1)                                          270
C *                                                                       272
C    Check IA!=0 | IB!=0                                                  272
         IF (IA.NE.0 .OR. IB.NE.0) THEN                                   272
         CC = FORM                                                        273
         CALL HEPNUMBER(NB,C)                                             274
C    Check NB>0                                                           275
         IF (NB.GT.0) CC=CC(1:LENOCC(CC))//C(1:LENOCC(C))                 275
C    Check INDEX(CC,':')>0                                                276
         IF (INDEX(CC,':').GT.0) CC=CC(1:LENOCC(CC))//':'                 276
         CALL HEPNUMBER(IA,C)                                             276
         CC=CC(1:LENOCC(CC))//'['//C(1:LENOCC(C))                         277
         CALL HEPNUMBER(IB,C)                                             277
         CC=CC(1:LENOCC(CC))//','//C(1:LENOCC(C))//']'                    278
      END IF                                                              279
      CALL HBNAME(ID,'particle',VAR,CC(1:LENOCC(CC)))                     280
      END                                                                 281
*************************************************************************
      SUBROUTINE HEPNUMBER(NUM,CNUM)                                      285
      IMPLICIT NONE                                                       286
      CHARACTER CNUM*(*),S*14                                             287
      INTEGER ANUM,NUM,L,I,I1,I2                                          288
      REAL RNUM                                                           289
      EQUIVALENCE (RNUM,ANUM)                                             290
      ANUM=NUM                                                            292
C *                                                                       292
C    Check ABS(NUM)<=1000000                                              292
         IF (ABS(NUM).LE.1000000) THEN                                    292
         WRITE (S, * ) ANUM                                               292
      ELSE                                                                293
         WRITE (S,'(f14.6)') RNUM                                         293
      END IF                                                              294
      I1=14                                                               294
      I2=1                                                                294
C *                                                                       295
C    Loop here                                                            295
         DO 5011 I=1,14                                                   295
C    Skip Unless S(I:I)!=' '                                              295
         IF (S(I:I).EQ.' ')GO TO 5011                                     295
         I1=MIN(I1,I)                                                     295
         I2=MAX(I2,I)                                                     295
5011  CONTINUE                                                            296
5012  CONTINUE                                                            296
      CNUM=S(I1:I2)                                                       296
      L=I2-I1+1                                                           297
1     CONTINUE                                                            298
      END                                                                 298
