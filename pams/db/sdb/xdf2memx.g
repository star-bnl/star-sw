      SUBROUTINE PAR2MEM
*
* Interface between user's routines and XDFMEM package.
* Should be called once in STAF run. This call will create direcories in memory
*  and put in it all XDF files, found in PWD/params or $STAR_PARAMS paths.
*
* Author: V.Tikhomirov
* Creation: June-1998
*
* Input:
*       none
* Output:
*        none
* 
      implicit none
      character troot*20,calenv*20
      COMMON/XDFNAM/ calenv,troot
      integer iopt
      COMMON/XDFOPT/ iopt
C.....
      calenv='STAR_PARAMS'
      troot='params'
      iopt=0
      CALL XDFMEM
      END
*-----------------------------------------------------------------------
      SUBROUTINE XDF2MEM(root)
*
* Interface between user's routines and XDFMEM package.
* Should be called once in STAF run. This call will create direcories in memory
*  and put in it all XDF files, found in PWD/root path.
*
* Author: V.Tikhomirov
* Creation: June-1998
*
* Input:
*       root (character) - directory in PWD to search for XDF files
* Output:
*        none
* 
      implicit none
      character root*(*),troot*20,calenv*20
      COMMON/XDFNAM/ calenv,troot
      integer iopt
      COMMON/XDFOPT/ iopt
C.....
      calenv=' '
      troot=root
      iopt=-1
      CALL XDFMEM
      END
*-----------------------------------------------------------------------
  module xdfmem  Put XDF files to memory
  author V.Tikhomirov
  created 2.07.98

+cde,gcunit.
      integer   maxlof,maxlff
      parameter (maxlof=40)    ! Maximum file name length
      parameter (maxlff=80)    ! Maximum full file name length
      character path*(maxlff)
      character fulldir*(maxlff),vfile*(maxlof) 
      character fullname*(maxlff)
      character flist*16
      integer   lun,justnow,iret,lenocc,ncfile,ncpath

      structure   myco { version, lun, char flist(4), int ncall, int nupdate,
                      int npath, int nfile, char rtime(4), int stime,
                      int tmin, int tmax}
      structure   reco { char file(10), char mpath(20), char t1(4), char t2(4),
                      int it1, int it2 }

      character   cfile*(maxlof),cpath*(maxlff),ctime*16,ct1*16,ct2*16
      equivalence (cfile,reco_file), (cpath,reco_mpath), (ctime,myco_rtime)
      equivalence (flist,myco_flist), (ct1,reco_t1), (ct2,reco_t2)

      integer ind,ind1,idir,ts0,ts1,i,tcmin,tcmax,update,ltroot,jscan,noerr
      integer iprin/0/,istat/-1/
      character troot*20,calenv*20
      COMMON/XDFNAM/ calenv,troot
      integer iopt
      COMMON/XDFOPT/ iopt
      save ltroot,noerr
* setenv STAR_PARAMS /afs/rhic/star/packages/dev/params
C.....

  begin
  prin1 
  (' Module xdfmem - Put XDF files to memory')

C: Check, if XDFMEM is called more than once
      if(first) then
        noerr=0
        first=.false.
      else
        if(noerr.le.20) then
          write(6,*) ' *** XDFMEM: You can call me only once!'
          if(noerr.eq.20) write(6,*) '              Last message'
          noerr=noerr+1
        endif
        R E T U R N
      endif

********************* Initialization part **********************

      flist=' '; ct1=' '; ct2=' '; cfile=' '; cpath=' '; ctime=' ';
      ltroot=lenocc(troot)
      prin2 calenv(1:lenocc(calenv)),troot(1:ltroot)
      (' *** XDFMEM: calenv>',a,'<   troot>',a,'<')

C: Fill header strucrure myco
C: 2147483647 below is maximum system time- 03:14:07, January 19, 2038 UTC
C:                                      (or 22:14:07, January 18, 2038 EST)
        fill myco(1)                   ! Simple Data Base control
         version = 1                   ! Version number
         lun=63                        ! LUN to read file with list of paths
         flist={'xdf2','mem.','tmp '}  ! Name of temp file with list of paths
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
        fill reco                ! XDF file, path, time
          file={' ',' '}         ! File
          mpath={' ',' '}        ! Path
          t1={' ',' '}           ! Time of file
          t2={' ',' '}           ! Validation time
          it1      = 0           ! Time of file, sec
          it2      = 0           ! Validation time, sec
        endfill

      use myco(1)
      lun=myco_lun;    ncpath=myco_npath; ncfile=myco_nfile;
      tcmin=myco_tmin; tcmax=myco_tmax;

********************* End of initialization part **********************

C: Put fiction time parameter (keeped for compatibility with SDBM package)
      justnow=0

C: Convert time from seconds to character DATE.TIME format
      CALL TIME2CHAR_F(justnow,ctime)
      fill myco(1)          ! Simple Data Base control
        ncall=myco_ncall+1  ! Number of module calls
        stime=justnow       ! Requested time, sec
      endfill

      update=0
C: Produce file with path list to calibration DB files
      CALL T_PROD_PATH_LIST(calenv,troot,flist,iopt)

C: Get next path from file with list of paths
 30   CALL T_GET_PATH(lun,flist,troot,path,iret)
C: If cannot open list file, or if error during read or EOF found
      if(iret.ne.0)  G O T O  100
      prin3 path(1:lenocc(path))
      (' *** XDFMEM: path>',a,'<')

C: Create corresponding path in memory
      CALL T_CREATE_DIR(path)

C: Open directory
      CALL T_OPEN_DIR(path,calenv,troot,iopt,fulldir,idir)
C: If directory is not found not in user's PWD, not in STAR path
      if(idir.eq.0)  G O T O  30
      ncpath=ncpath+1
      ind=index(fulldir,char(0))-1
      prin3  fulldir(1:ind)
      (' *** XDFMEM: fulldir>',a,'<')
      jscan=0

C: 2147483647 below is maximum system time- 03:14:07, January 19, 2038 UTC
C:                                      (or 22:14:07, January 18, 2038 EST)
 50   ts0=0
      ts1=2147483647
C: Get next XDF file in directory 'fulldir'
      CALL XDF_GET_FILE(idir,fulldir,vfile,iret)
C: End of directory scan
      if(iret.eq.-1) then
C: Fill sructure reco, if no XDF files was found in current directory
        if (jscan.eq.0) then
          cfile=vfile(1:ind1)
          cpath=fulldir(1:ind)
          CALL TIME2CHAR_F(ts0,ct1)
          CALL TIME2CHAR_F(ts1,ct2)
          fill reco         ! XDF file, path, time
	    it1=ts0         ! Time of file, sec
            it2=ts1         ! Validation time, sec
          endfill
        endif
        G O T O  30
      endif
      ind1=index(vfile,char(0))-1
      prin3  vfile(1:ind1),ts0,ts1
      (' *** XDFMEM: vfile>',a,'<  ts0,ts1=',2i12)
      update=1

C: If file with .xdf or .XDF extension is found
      if(vfile(1:ind1).ne.' ') then
        jscan=1
C: Full file name here
        fullname=fulldir(1:ind)//'/'//vfile(1:ind1)
C: Read disk file in XDF format and put it into memory
        CALL T_READXDF(fullname,path,iret)
        if(iret.eq.0) then
          prin3 
          (' *** XDFMEM: XDF file is transfered to memory')
          ncfile=ncfile+1
C: Fill sructure reco
          cfile=vfile(1:ind1)
          cpath=fulldir(1:ind)
          CALL TIME2CHAR_F(ts0,ct1)
          CALL TIME2CHAR_F(ts1,ct2)
          fill reco         ! XDF file, path, time
	    it1=ts0         ! Time of file, sec
            it2=ts1         ! Validation time, sec
          endfill
        endif
      endif


C: Read next line from list file or from structure 'reco'
      G O T O  50

 100  continue

      prin2 ncpath,ncfile
      (' *** XDFMEM: ncpath,ncfile=',2i6)
      tcmin=0
      tcmax=2147483647

C: Update structure 'myco'
      CALL TIME2CHAR_F(justnow,ctime)
      fill myco(1)       ! Simple Data Base control
        npath   = ncpath ! Number of DB files accepted
        nfile   = ncfile ! Number of DB files accepted
        tmin    = tcmin  ! Minimum validation time
        tmax    = tcmax  ! Maximum validation time
        nupdate=myco_nupdate+update   ! Number of structure updates
      endfill

C: Map ZEBRA sructure to STAR table
      prin2
      (' *** XDFMEM: map ZEBRA structure to STAR table')
      CALL AGSTRUT('xdfm',troot(1:ltroot))

   use myco(1)
   do i=1,ncpath+1
     use reco(i) stat=istat
     prin3 i,istat,cpath(1:lenocc(cpath)),cfile(1:lenocc(cfile)),
           reco_it1,reco_it2
     (' i,istat=',2i3,' path:>',a,'<',/,' file>',a,'<','  t1,t2= ',2i19,/)
   enddo  

      RETURN
      END
*-----------------------------------------------------------------------
      SUBROUTINE XDF_GET_FILE(idir,fulldir,vfile,iret)
*
* Routine to scan dir and get name of XDF file.
*
* Author: V.Tikhomirov
* Creation: June-1998
*
* Input:
*       idir (integer) - C- pointer to directory, got by CALL T_OPEN_DIR
*       fulldir (character) - full path to directory
* Output:
*        vfile (character) - name of XDF file
*        iret (integer) - return code: =0 - OK; =-1 - no more files in dir
*
      implicit none
      integer idir,iret
      character fulldir*(*),vfile*(*)
      integer l
      character dbfile*80
C.....
C: Get next file name in the directory 'fulldir'
      CALL GET_NEXT_FILE_F(fulldir,idir,dbfile)

      if(idir.ne.0) then
        iret=0
C: Check file extension
        l=index(dbfile,char(0))-1
C: Not valid XDF file - check next one
        if(l.lt.4 .or. (dbfile(l-3:l).ne.'.xdf' .and. 
     >                  dbfile(l-3:l).ne.'.XDF'      ) )then
          vfile=' '//char(0)
C: Proper XDF file
        else
          vfile=dbfile
        endif
C: End of directory scan
      else
        iret=-1
      endif

      END







