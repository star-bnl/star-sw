      subroutine control
+CDE,gcbank,gcnum,gcflag,quest.
      print *,' control ready '
      call hbook1(11,'all particle PID',           60, 0.5, 60.5, 0) 
      call hbook1(12,'primary particle PID',       60, 0.5, 60.5, 0) 
      call hbook1(13,'all particles with Nhit>10', 60, 0.5, 60.5, 0) 
      call hbook1(14,'secondary processes       ', 50, 0.5, 50.5, 0) 
      call hbook1(15,'particle theta  ',           50,   0, 3.14, 0) 
      call hbook1(16,'particle rapidity  ',        50, -10,  10, 0) 
      call hbook1(21,'particle vertex (LOG) ',     50, -15,  5, 0) 
      IQUEST(100)=JVOLUM
      end

      subroutine agudigi
      implicit   none
+CDE,gcbank,gcnum,gcflag,quest.
      integer IEV/0/,Itrac,Ipart,Ivert,Nu,NtpcHit,Iad,
     >        Iw,Ihit,Ltra,Ntbeam,Nttarg,Nhit,N10,
     >        Nvs(15)/15*0/,NBV(15),IP,ISTAT,IDPDG,MOTH(2),IDAU(2)
      real    VMOD,Tofg,vert(4),pvert(4),ubuf(100),Digi(15),
              P(5),V(5),theta,y,R,AMASS,TIME
*
   
* do not allow run without geometry
      if (JVOLUM<=0) STOP ' NO GEOMETRY LOADED '
      if (JVERTX<=0 | JKINE<=0) then
         print *,'CONTROL: empty event rejected'
         go to :e:
      endif
* 
      call xntup ('Ieotri',Ieotri)
      call xntup ('Ntrack',Ntrack)
      call xntup ('Nvertx',Nvertx)
      call gfnhit('TPCH','TPAD',NtpcHit)
      call xntup ('NtpcHit',NtpcHit)

* fill PID histogramm
      do Itrac=1,Ntrack
         call GFKINE(Itrac,vert,Pvert, Ipart,Ivert,Ubuf,Nu)
         if (0>=Ivert | Ivert > Nvertx | Ipart<=0 ) then
             print *,'CONTROL: error in Itrac,Ivert,Ipart=',
                                        Itrac,Ivert,Ipart
             go to :e:
         endif
         call GFVERT(Ivert,vert,Nttarg,Ntbeam,Tofg,Ubuf,Nu)
* all particles:
         call hfill (11,float(Ipart),1.,1.)
* separate histogram for primary and secondary
         if (Nttarg<=0) then
            call hfill(12,float(Ipart),1.,1.)
            theta=atan2(sqrt(pvert(1)**2+pvert(2)**2),pvert(3))
            call hfill(15,theta,1.,1.)
            y=-alog(tan(theta/2))
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

      IEV=IEV+1
      call xntup ('Ntr10',N10)
      call xntup ('*** end_of_event ***',1)
      if (idebug>0) print *,' EVENT ',IEV,' DONE '
      iquest(100)=Iev
      return
:e:
      IEOTRI = 1
      iquest(100)=Iev 
      end

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

