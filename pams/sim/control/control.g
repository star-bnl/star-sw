      subroutine control
+CDE,gcbank,gcnum,gcflag,quest.
      print *,' control ready '
      call hbook1(11,'all particle PID',           60, 0.5, 60.5, 0) 
      call hbook1(12,'primary particle PID',       60, 0.5, 60.5, 0) 
      call hbook1(13,'all particles with Nhit>10', 60, 0.5, 60.5, 0) 
      call hbook1(14,'secondary processes       ', 50, 0.5, 50.5, 0) 
      IQUEST(100)=JVOLUM
      end

      subroutine agudigi
      implicit   none
+CDE,gcbank,gcnum,gcflag,quest.
      integer IEV/0/,Itrac,Ipart,Ivert,Nu,NtpcHit,Iad,
     >        Iw,Ihit,Ltra,Ntbeam,Nttarg,Nhit,N10,
     >        Nvs(15)/15*0/,NBV(15)
      real    Tofg,vert(4),pvert(4),ubuf(100),Digi(15)
*
   
* do not allow run without geometry
      if (JVOLUM<=0) STOP ' NO GEOMETRY LOADED '
* 
      IEV=IEV+1
      call xntup ('Ieotri',Ieotri)
      call xntup ('Ntrack',Ntrack)
      call xntup ('Nvertx',Nvertx)
      call gfnhit('TPCH','TPAD',NtpcHit)
      call xntup ('NtpcHit',NtpcHit)

* fill PID histogramm
      do Itrac=1,Ntrack
         call GFKINE(Itrac,vert,Pvert, Ipart,Ivert,Ubuf,Nu)
         call GFVERT(Ivert,vert,Nttarg,Ntbeam,Tofg,Ubuf,Nu)
* all particles:
         call hfill (11,float(Ipart),1.,1.)
* separate histogram for primary and secondary
         if (Nttarg<=0) call hfill(12,float(Ipart),1.,1.)
         if (Nttarg>0)  call hfill(14,float(mod(Ntbeam,50)),1.,1.)
      enddo


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
      print *,' EVENT ',IEV,' DONE '
      iquest(100)=Iev
      end

