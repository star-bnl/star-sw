      subroutine sdb
      entry sdb_start
      print *,' V1 calibration and constant data base manager loaded '
      end
*-----------------------------------------------------------------------
      SUBROUTINE CAL2MEM(time)
*
* Interface between user's routines and SDBM package.
* Should be called on event-by-event basis, if any STAR calibration files are
*  supposed to be use. This call will create or update calibration directories
*  in memory according to input parameter 'time'.
*
* Author: V.Tikhomirov
* Creation: June-1998
*
* Input:
*       time (integer) - requested system time for DB files selection (seconds
*                        after 00:00:00 UTC, January 1, 1970)
* Output:
*        none
* 
      implicit none
      integer time, justnow
      COMMON/SDBMTIME/ justnow
C.....
      justnow=time
C: Call main routine of SDBM package
      CALL SDBM

      END
*-----------------------------------------------------------------------
  module sdbm is a Simple Data Base Manager
  author V.Tikhomirov
  created 25.06.98

+cde,gcunit.
      integer maxlof,maxlff
      parameter (maxlof=40)    ! Maximum file name length
      parameter (maxlff=80)    ! Maximum full file name length
      character path*(maxlff),fulldir*(maxlff),vfile*(maxlof)
      character troot*20,calenv*20,flist*16
      integer lun,iret,lenocc,ncfile,ncpath

      structure   myco { version, lun, char flist(4), int ncall, int nupdate,
                      int npath, int nfile, char rtime(4), int stime,
                      int tmin, int tmax}
      structure   reco { char file(10), char mpath(20), char t1(4), char t2(4),
                      int it1, int it2 }

      character   cfile*(maxlof),cpath*(maxlff),ctime*16,ct1*16,ct2*16
      equivalence (cfile,reco_file), (cpath,reco_mpath), (ctime,myco_rtime)
      equivalence (flist,myco_flist), (ct1,reco_t1), (ct2,reco_t2)

      integer ltroot,ind,ind1,ts0,ts1,i,tcmin,tcmax,update
      save calenv,troot,ltroot
      integer iprin/0/,istat/-1/
      integer justnow
      COMMON/SDBMTIME/ justnow
C.....

  begin

   prin1 
   (' Module sdbm - Simple Data Base Manager')
   prin2 first
   (' *** SDBM: first=',l)

********************* Initialization part **********************

      if(first) then
C: Initialization parameters:
        calenv='STAR_CALIB'
        flist=' '; ct1=' '; ct2=' '; cfile=' '; cpath=' '; ctime=' ';
C: Get name of root calibration directory from STAR calibration environment
        CALL T_GET_ROOT(calenv,troot)
        ltroot=lenocc(troot)
        prin2 troot(1:ltroot)
        (' *** SDBM: troot>',a,'<')
        if(ltroot.le.0) then
          write(6,*) ' *** SDBM: Fatal error - cannot find ',
     >               'environment >',calenv(1:lenocc(calenv)),'<  ...Return!'
          R E T U R N
        endif
        
C: Fill header strucrure myco
C: 2147483647 below is maximum system time- 03:14:07, January 19, 2038 UTC
C:                                      (or 22:14:07, January 18, 2038 EST)
        fill myco(1)                   ! Simple Data Base control
         version = 1                   ! Version number
         lun=63                        ! LUN to read file with list of paths
         flist={'sdbm','list','.tmp'}  ! Name of temp file with list of paths
         ncall=0                       ! Number of module calls
         nupdate=0                     ! Number of DB updates
         npath=0                       ! Number of DB paths accepted
         nfile=0                       ! Number of DB files accepted
         rtime={' ',' ',' ',' '}       ! Requested time for DB files selection
         stime=0                       ! Requested time, sec
         tmin=2147483647               ! Minimum validation time
         tmax=0                        ! Maximum validation time
        endfill

C: Fill strucrure reco with dummy parameters
        fill reco             ! Calibration file, path, time
          file={' ',' '}         ! Calibration file
          mpath={' ',' '}        ! Path
          t1={' ',' '}           ! Time of file
          t2={' ',' '}           ! Validation time
          it1      = 0           ! Time of file, sec
          it2      = 0           ! Validation time, sec
        endfill
      endif

      use myco(1)
      lun=myco_lun;    ncpath=myco_npath; ncfile=myco_nfile;
      tcmin=myco_tmin; tcmax=myco_tmax;

********************* End of initialization part **********************

C: Convert time from seconds to character DATE.TIME format
      CALL TIME2CHAR_F(justnow,ctime)
      fill myco(1)          ! Simple Data Base control
        ncall=myco_ncall+1  ! Number of module calls
        stime=justnow       ! Requested time, sec
      endfill

C: Check, if new time is inside old allowed interval
      if(.not.first) then
        if(myco_nfile.le.0 .or. (justnow.ge.myco_tmin .and. 
     >                           justnow.le.myco_tmax      ) ) then
C: Map ZEBRA sructure to STAR table (here only myco_ncall is updated)
          CALL AGSTRUT('sdbm',troot(1:ltroot))
          R E T U R N
        endif
        ncpath=0
      endif
     
      update=0
C: Produce file with path list to calibration DB files
      if(first) CALL T_PROD_PATH_LIST(calenv,troot,flist,0)

 30   if(first) then
C: Get next path from file with list of paths
        CALL T_GET_PATH(lun,flist,troot,path,iret)
C: If cannot open list file, or if error during read or EOF found
        if(iret.ne.0)  G O T O  100
      else
        if(ncpath.lt.myco_npath) then
          use reco(ncpath+2) stat=istat;
          if(istat.ne.0)  G O T O  100;
C: Check, if new time is inside old allowed interval
          if(justnow.ge.reco_it1 .and. justnow.le.reco_it2)  then
            ncpath=ncpath+1
            G O T O  30
          endif
C: Get path to update
          ind=index(cpath,troot(1:ltroot));
          i=1; if(ind.gt.0) i=ind;
          path=cpath(i:)
        else
C: End of path list in structure 'reco'
          G O T O  100
        endif
      endif
      prin3 path(1:lenocc(path))
      (' *** SDBM: path>',a,'<')

C: Create corresponding path in memory
      if(first) CALL T_CREATE_DIR(path)

      cfile=' '
C: Search of DB file corresponding to requested datetime justnow 
      CALL SDB_GET_FILE
     >     (path,calenv,troot,justnow,0,fulldir,vfile,ts0,ts1,iret)
C: If directory is not found not in user's PWD, not in STAR_CALIB path
      if(iret.eq.-1)  G O T O  30
      ncpath=ncpath+1
      ind=index(fulldir,char(0))-1
      ind1=index(vfile,char(0))-1
      prin3  fulldir(1:ind)
      (' *** SDBM: fulldir>',a,'<')
      prin3  vfile(1:ind1),ts0,ts1
      (' *** SDBM: vfile>',a,'<  ts0,ts1=',2i12)

C: Directory scaning is finished, is any DB file selected?
      if(ind1.gt.0) then
C: Full file name here
        fulldir=fulldir(1:ind)//'/'//vfile(1:ind1)
C: Read disk calibration file in XDF format and put it into memory
        CALL T_READXDF(fulldir,path,iret)
        if(iret.eq.0) then
          prin3 
          (' *** SDBM: XDF file is transfered to memory')
          cfile=vfile(1:ind1)
          update=1
          if(first) ncfile=ncfile+1
        endif
      endif

C: Fill sructure reco
      cpath=fulldir(1:ind)
      call time2char_f(ts0,ct1)
      call time2char_f(ts1,ct2)

C: Create structure
      if(first) then
        fill reco         ! Calibration file, path, time
	  it1=ts0         ! Time of file, sec
          it2=ts1         ! Validation time, sec
        endfill
C: Update 
      else
        fill reco(ncpath+1) ! Calibration file, path, time
	  it1=ts0         ! Time of file, sec
          it2=ts1         ! Validation time, sec
        endfill
      endif 

C: Read next line from list file or from structure 'reco'
      G O T O  30

 100  continue

C: Calculate minimum and maximum validation time for all paths
        do i=2, ncpath+1
          use reco(i) stat=istat; if (istat!=0) break;
          ts0=reco_it1; ts1=reco_it2;
	  if(i.eq.2) then
            tcmin=ts0
            tcmax=ts1
          else
            if(ts0.gt.tcmin) tcmin=ts0
            if(ts1.lt.tcmax) tcmax=ts1
          endif
        prin4 i,ts0,ts1,tcmin,tcmax
        (' *** SDBM: i,ts0,ts1,tcmin,tcmax=',i4,4i12)
        enddo

C: Update structure 'myco'
      if(first) call time2char_f(justnow,ctime)
      fill myco(1)       ! Simple Data Base control
        npath   = ncpath ! Number of DB files accepted
        nfile   = ncfile ! Number of DB files accepted
        tmin    = tcmin  ! Minimum validation time
        tmax    = tcmax  ! Maximum validation time
        nupdate=myco_nupdate+update   ! Number of structure updates
      endfill

C: Map ZEBRA sructure to STAR table
      CALL AGSTRUT('sdbm',troot(1:ltroot))
      first=.false.

   use myco(1)
   do i=1,ncpath+1
     use reco(i) stat=istat
     prin3 i,istat,cpath(1:lenocc(cpath)),cfile(1:lenocc(cfile)),
           reco_it1,reco_it2
     (' i,istat=',2i3,' path:>',a,'<',/,' file>',a,'<','  t1,t2= ',2i19,/)
   enddo  

      END
*-----------------------------------------------------------------------
      SUBROUTINE SDB_GET_FILE
     >       (path,calenv,troot,justnow,iopt,fulldir,vfile,ts0,ts1,iret)
*
* Routine to scan dir and get a proper DB file according to the requested time.
*
*
* Author: V.Tikhomirov
* Creation: June-1998
*
* Input:
*       path (character) - path (after 'troot') to DB directory
*       calenv (character) - STAR environment - path to DB files
*       troot (charcater) - root directory of path to DB files
*       justnow (integer) - requested time for DB files selection (seconds)
*       iopt (integer) - option to pass to T_OPEN_DIR routine
* Output:
*        fulldir (character) - full path to DB directory
*        vfile (character) - proper file according to the requested time
*        ts0 (integer) - time of proper calibration file 'vfile'
*        ts1 (integer) - expiration time, i.e. time of next to 'vfile'
*                       calibration file
*        iret (integer) - return code: =0 - OK; =-1 - cannot open dir
*
      implicit  none
      integer   justnow,ts0,ts1,iopt,iret
      character path*(*),calenv*(*),troot*(*),fulldir*(*),vfile*(*)
      integer   idir
      character dbfile*80
C.....
      iret=-1
C: Begin to search of DB file corresponding to requested datetime justnow 
C: Open calibration directory
      CALL T_OPEN_DIR(path,calenv,troot,iopt,fulldir,idir)
C: If directory is not found not in user's PWD, not in STAR_CALIB path
      if(idir.eq.0)  R E T U R N

C: 2147483647 below is maximum system time- 03:14:07, January 19, 2038 UTC
C:                                      (or 22:14:07, January 18, 2038 EST)
      ts0=0
      ts1=2147483647
      vfile=' '
      iret=0

C: Get next file name in the directory 'fulldir'
 50   CALL GET_NEXT_FILE_F(fulldir,idir,dbfile)

      if(idir.ne.0) then
C: Compare DB files according to requested time
        CALL SDB_PROPER_FILE(justnow,dbfile,vfile,ts0,ts1)
C: Read the name of next file from directory 'fulldir'
        G O T O  50
      endif

      END
*-----------------------------------------------------------------------
      SUBROUTINE SDB_PROPER_FILE(rtime,file,vfile,t0,t1)
*
* Routine to get a proper DB file according to the requested time.
*  All files in DB directory have a name like XXX.DATE.TIME. Routine converts
*  DATE.TIME format to system time (in seconds since 1.1.1970) and compares it
*  with 'rtime' parameter. Routine should be called for each 'file' in
*  DB directory.
*
* Author: V.Tikhomirov
* Creation: June-1998
*
* Input:
*       rtime (integer) - requested time for DB files selection (seconds)
*       file (character) - DB file name to check
*       t0 (integer) - time of proper calibration file at the moment of call
*       t1 (integer) - expiration time at the moment of call
* Output:
*        vfile (character) - proper file according to the requested time
*        t0 (integer) - time of proper calibration file 'vfile'
*        t1 (integer) - expiration time, i.e. time of next to 'vfile'
*                       calibration file
*
      implicit none
      integer rtime,t0,t1
      character file*(*),vfile*(*)
      integer ld,stime
C.....
      ld=index(file,char(0))-1
C: Valid DB file name should have at least 16 characters
      if(ld.lt.16)  R E T U R N
C: Extract system time from file name field
      CALL CHAR2TIME_F(file(ld-15:ld),stime)
C: Return, if datetime field is invalid
      if(stime.lt.0)  R E T U R N
C: Compare requested time with file time and with existing interval t0-t1
      if(stime.le.rtime .and. stime.ge.t0) then
        t0=stime
        vfile=file
      endif
      if(stime.ge.rtime .and. stime.le.t1) then
        t1=stime
        if(t0.eq.0) vfile=file
      endif

      END








