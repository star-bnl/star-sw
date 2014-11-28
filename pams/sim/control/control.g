* $Id: control.g,v 1.17 2001/05/02 16:44:51 nevski Exp $
* $Log: control.g,v $
* Revision 1.17  2001/05/02 16:44:51  nevski
* forecast EOD with IQUEST(11) passed to IQUEST(99)
*
* Revision 1.16  2000/08/10 23:54:45  nevski
* 1st EVNT entry checked on CONTROL SIMU=1 only
*
* Revision 1.15  2000/03/10 23:03:52  nevski
* accept both old and new EVNT headers
*
* Revision 1.14  2000/03/09 00:35:09  nevski
* bad ReadNt events rejected
*
* Revision 1.13  2000/03/04 18:20:55  nevski
* add filter for events with wrong ITRA in hits
*
* Revision 1.12  2000/01/12 00:10:44  nevski
* add cvs headers
*
*
      subroutine control
+CDE,gcbank,gcnum,gcflag,gcunit,quest.
      <w>; (' new control ready ')
      IQUEST(100) = JVOLUM
      call agsbegm('CONTROL',IPRIN)
      if (ISLFLAG('CONT','HIST')<=0) return
      call hbook1(11,'all particle PID',           60, 0.5, 60.5, 0) 
      call hbook1(12,'primary particle PID',       60, 0.5, 60.5, 0) 
      call hbook1(13,'all particles with Nhit>10', 60, 0.5, 60.5, 0) 
      call hbook1(14,'secondary processes       ', 50, 0.5, 50.5, 0) 
      call hbook1(15,'particle theta  ',           50,   0, 3.14, 0) 
      call hbook1(16,'particle rapidity  ',        50, -10,  10, 0) 
      call hbook1(21,'particle vertex (LOG) ',     50, -15,  5, 0) 
      end

      subroutine agudigi
      implicit   none
+CDE,gcbank,gcnum,gcflag,gcunit,quest.
      integer ISLFLAG,AGDIGC,IEV/0/,Itrac,Ipart,Ivert,Nu,NtpcHit,Iad,
     >        Iw,Ihit,Ltra,Ntbeam,Nttarg,Nhit,N10,Iprin,Isimu,
     >        Nvs(15)/15*0/,NBV(15),IP,ISTAT,IDPDG,MOTH(2),IDAU(2)
      real    VMOD,Tofg,vert(4),pvert(4),ubuf(100),Digi(15),
              P(5),V(5),theta,y,R,AMASS,TIME,pt
*
* do not allow run without geometry
      Iprin = ISLFLAG('CONT','PRIN')
      Isimu = ISLFLAG('CONT','SIMU')
* forecast next End_of_data - IQUEST(11) is set to 0 when last event is read:
      IQUEST(99) = IQUEST(11)

      if (JVOLUM<=0) STOP ' NO GEOMETRY LOADED '
      if (JHEAD <=0) then
         Prin1; ('CONTROL: event without header rejected ');   go to :e:
      endif
      if (IQ(JHEAD+5)>0 & IQ(JHEAD+6)==0) then
         Prin1; ('CONTROL: splitted event header skipped ');   go to :r:
      endif
      if (JVERTX<=0 | JKINE<=0) then
         Prin1; ('CONTROL: empty event rejected');             go to :e:
      endif
* 
      if (Isimu>1) then
      call AgNZGETP(1,1,1,ISTAT,IDPDG,P,AMASS,MOTH,TIME,IDAU,V)
      unless (ISTAT==10&IdPDG==9999999) | (ISTAT==11&IdPDG==999997)
      {  Prin1 istat,IdPDG;('CONTROL: bad EVNT bank ',2i8,', event rejected') 
                                                               go to :e: }
      endif     

      if  AGDIGC(0) != 0
      {  Prin1; ('CONTROL: bad HITS bank, event rejected');    go to :e: }

      if (ISLFLAG('CONT','HIST')<=0) goto :r:

      call xntup ('Ieotri',Ieotri)
      call xntup ('Ntrack',Ntrack)
      call xntup ('Nvertx',Nvertx)
      call gfnhit('TPCH','TPAD',NtpcHit)
      call xntup ('NtpcHit',NtpcHit)

* fill PID histogramm
      do Itrac=1,Ntrack
         call GFKINE(Itrac,vert,Pvert, Ipart,Ivert,Ubuf,Nu)
         if (0>=Ivert | Ivert > Nvertx | Ipart<=0 ) then
             Prin1 Itrac,Ivert,Ipart;('CONTROL: error in Itrac,Ivert,Ipart=',3i8)
             go to :e:
         endif
         call GFVERT(Ivert,vert,Nttarg,Ntbeam,Tofg,Ubuf,Nu)
* all particles:
         call hfill (11,float(Ipart),1.,1.)
* separate histogram for primary and secondary
         if (Nttarg<=0) then
            call hfill(12,float(Ipart),1.,1.)
            pt    = VMOD(pvert,2)
            theta = atan2(pt,pvert(3))
            call hfill(15,theta,1.,1.)

            y     = sign(10.,pvert(3))
            if (pt>0.001) y=-alog(tan(theta/2))
            call hfill(16,y,1.,1.)
         endif
         if (Nttarg>0)  call hfill(14,float(mod(Ntbeam,50)),1.,1.)
      enddo

      do IP=1,100000
         call AgNZGETP(1,1,IP,ISTAT,IDPDG,P,AMASS,MOTH,TIME,IDAU,V)
         if (ISTAT<=0) break;  check ISTAT==1
         R=VMOD(V,3); if (R>0) call HFILL(21,Alog(R)/2.3,1.,1.)
      enddo
      call xntup ('NtrEG',IP-1)

* count hits:

      Iw    = 0
      Iad   = 0
      Nhit  = 0      
      Itrac = 0
      N10   = 0

      do Ihit=1,NtpcHit
         call AgFDIGI('TPCH','TPAD',NVS,LTRA,NBV,DIGI,Iw,Iad)
         if (iw<=0) break

         if (Itrac!=Ltra) then 
* save counts from previous track in a histigramm
            if (Itrac>0 & Nhit>10) then
               N10 = N10+1
               call GFKINE(Itrac,vert,Pvert, Ipart,Ivert,Ubuf,Nu)
               call hfill (13,float(Ipart),1.,1.)   
            endif
* start new hit counter for new track
            Nhit  = 0
            Itrac = Ltra
         endif
         nhit = Nhit +1  
                     
      enddo

      call xntup ('Ntr10',N10)
      call xntup ('*** end_of_event ***',1)

:r:   IEV=IEV+1
      iquest(100)=Iev
      Prin1 IEV;(' EVENT ',i6,'  DONE ')
      return
:e:
      IEOTRI = 1
      iquest(100)=Iev 
      end

**********************************************************************
*                                                                    *
   function   AGDIGC (dummy)
*                                                                    *
**********************************************************************
+CDE,TYPING,GCBANK,GCUNIT,GCNUM,GCFLAG.
  CHARACTER*4 Cset
  Integer     AGDIGC,Iset,Idet,JS,JD,JH,JX,JXD,NW,I,X,dummy,Last,LTRA
* - - - - - - - - - - - - - - - - - - - - - - - - -

   AGDIGC = 0
   Check Jset>0
   do Iset = 1,IQ(Jset-1)
   { JS   = LQ(Jset-Iset);                                    Check Js  > 0;
     do Idet = 1,IQ(JS-1)
     { JD  = LQ(JS-Idet);                                     Check JD  > 0;
       CALL UHTOC(IQ(Jset+Iset),4,Cset,4)
       if Cset(4:4)=='H' {X=1;JH=JHITS} else {X=2;JH=JDIGI};  Check JH  > 0;
       Jx  = LQ(JH-Iset);                                     Check Jx  > 0;
       JXD = LQ(JX-Idet);                                     Check JXD > 0;
       Last= IQ(JX+Idet);                                     Check Last> 0;
       NW  = IQ(JD+1)+IQ(JD+2*X+1)+1

       I=0; if (Mod(Last,Nw)>0)  go to :e:
       DO i=1,Last,Nw { LTRA=IQ(JXD+i); if (0>=Ltra|Ltra>Ntrack) go to :e:; }
   } }
   return
:e:AGDIGC = Iset*100+Idet
   END





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   call AgNZGETE(ILK,IDE,NPART,IRUN,IEVT,CGNAM,VERT,IWTFL,WEIGH)

* Input : ILK   - Link number  : 1 = primary, 2 = secondary (obsolete)    *
*         IDE   - ID of event in gate ( ZEBRA IDN)                        *
* Output: NPART - Number of particles in event record                     *
*         IRUN  - run number as recorded by generator                     *
*         IEVT  - event number as recorded by generator                   *
*         CGNAM - generator name                                          *
*         VERT(4)- x,y,z,t of event (metres,seconds or mm,mm/c)           *
*         IWTFL - weight flag                                             *
*         WEIGH - event weight                                            *

   call AgNZGETP(1,1,IP,ISTAT,IDPDG,P,AMASS,MOTH,TIME,IDAU1,V)

* Input : IL    - Link number : 1 = primary, 2 = secondary (obsolete)      *
*         IDE   - ID of event in gate                                      *
*         IP    - Number of particle in event record                       *
* Output: ISTAT - HEPEVT status flag. Returns 0 if record not found.       *
*         IDPDG - PDG code for particle                                    *
*         P     - 4-momentum (px,py,pz,E)                                  *
*         AMASS - particle (quark) mass                                    *
*         MOTH  - mothers. If MOTH(2)<0 the range is MOTH(1)-ABS(MOTH(2))  *
*         TIME  - Start time of particle relative to interaction (sec)     *
*         IDAU1 - a daughter pointer                                       *
*         V     - vertex coordinates (in meter)                            *

