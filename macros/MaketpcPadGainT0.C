/*
  root.exe lDb.C 'MaketpcPadGainT0.C+("itpc_gains.txt.20200626.084156")'
  root.exe lDb.C 'MaketpcPadGainT0.C+("tpx_gains.txt.20200807.104403")'
    root.exe -q -b lDb.C 'MaketpcPadGainT0.C+("'${f}'")' >& ${f}.log
#  foreach f (`find . -name "*.txt*" -amin -360`)   
   foreach f (`ls -d *txt.2*`)
    echo "${f}"
    root.exe -q -b lRTS.C 'MaketpcPadGainT0.C+("'${f}'")' >& ${f}.log
  end
 */
#if !defined(__CINT__)
#include "TString.h"
#include <stdio.h>
#include "Riostream.h"
#include "TClassTable.h"
#include "TSystem.h"
#include "TFile.h"
#include "TString.h"
#include "tables/St_tpcPadGainT0_Table.h"
#include "tables/St_itpcPadGainT0_Table.h"
#include "RTS/src/DAQ_TPX/tpxGain.h"
#include "RTS/src/DAQ_TPX/tpxCore.h"
#include "RTS/src/DAQ_ITPC/itpcCore.h"
#include "RTS/src/DAQ_ITPC/itpcFCF.h"
#else
class St_tpcPadGainT0;
class St_itpcPadGainT0;
#endif
St_tpcPadGainT0 *tpcPadGainT0 = 0;
St_itpcPadGainT0 *itpcPadGainT0 = 0;
static struct g_s_t {
  float g ;
  float t ;
} g_s[25][41][121] ;

//________________________________________________________________________________
void MaketpcPadGainT0(TString FileName="tpx_gains.txt.20180326.052426"){ //"itpc_gains.txt.20191030.050318") {
  Bool_t itpc = kFALSE;
  Int_t d = 0;
  Int_t t = 0;
  if (FileName.BeginsWith("tpx_gains.txt.")) {
    Int_t n = sscanf(FileName.Data(),"tpx_gains.txt.%0d.%0d",&d,&t);
    if (n != 2) {cout << "Illegal file name " << FileName.Data() << endl; return;}
  } else  if (FileName.BeginsWith("itpc_gains.txt.")) {
    Int_t n = sscanf(FileName.Data(),"itpc_gains.txt.%0d.%0d",&d,&t);
    if (n != 2) {cout << "Illegal file name " << FileName.Data() << endl; return;}
    itpc = kTRUE;
  }
  TFile *fOut = 0;
  if (! itpc) { // tpx
    // $STAR/StRoot/RTS/src/RTS_EXAMPLE/tpx_read_gains.C
    tpxGain tpx_gain ;	// constructor
    
    tpx_gain.init(0) ;	// create and zap storage for the whole TPX
  
    Int_t ret = tpx_gain.from_file((Char_t *) FileName.Data()) ;	// read from file...
    if(ret < 0) {
      printf("********* some failure on file %s!\n",FileName.Data()) ;
      return;
    }
  
    printf("File opened: %s\n",FileName.Data()) ;
    printf("Run used [if available]: %08u\n",tpx_gain.c_run) ;
    printf("Date changed [if available]: date %08u, time %06u\n",tpx_gain.c_date,tpx_gain.c_time) ;
    
    // show how the gains & t0 is obtained and used...
    
    for(int s=1;s<=24;s++) {			// NOTE: sector counts from 1
      for(int r=1;r<=45;r++) {		// NOTE: row counts from 1
	for(int p=1;p<=182;p++) {	// NOTE: pads count from 1
	  float g, t0 ;		// gains and t0 are actually stored as "float"...
	  
	  g = tpx_gain.get_gains(s,r,p)->g ;
	  t0 = tpx_gain.get_gains(s,r,p)->t0 ;
	  
	  //				printf("%d %d %d %.3f %.3f\n",s,r,p,g,t0) ;
	}
      }
    }
    // Fill table
    Int_t run = tpx_gain.c_run;
    Int_t d = tpx_gain.c_date;
    Int_t t = tpx_gain.c_time;
    Bool_t itpc = kFALSE;
    tpcPadGainT0 = new St_tpcPadGainT0("tpcPadGainT0",1);
    tpcPadGainT0_st GainT0; 
    memset (&GainT0, 0, sizeof(GainT0));
    GainT0.run = run;
    for(int s=1;s<=24;s++) {			// NOTE: sector counts from 1
      for(int r=1;r<=45;r++) {		// NOTE: row counts from 1
	for(int p=1;p<=182;p++) {	// NOTE: pads count from 1
	  GainT0.Gain[s-1][r-1][p-1] = tpx_gain.get_gains(s,r,p)->g;
	  GainT0.T0[s-1][r-1][p-1]   = tpx_gain.get_gains(s,r,p)->t0;
	}
      }
    }
    tpcPadGainT0->AddAt(&GainT0);
    //  tpcPadGainT0->Print(0,1);
    TDatime timeET(d,t);
    UInt_t uGMT = timeET.Convert(kTRUE);
    TDatime time(uGMT);
    TString filename(Form("tpcPadGainT0.%08d.%06d",time.GetDate(),time.GetTime()));
    printf("Create %s\n",filename.Data());
    filename += ".root";
    fOut = new TFile(filename.Data(),"recreate");
    tpcPadGainT0->Write();
  } else { // iTPC
    // online/RTS/src/ITPC_SUPPORT/test_gains.C
    // online/RTS/src/ITPC_SUPPORT/gainDbWrite.C
    itpc_fcf_c fcf ;
    fcf.det_type = 1 ;      // itpc
    fcf.init(0,FileName.Data()) ;
    
    int bad_ch = 0 ;
    int all_ch = 0 ;
    int bad_fee = 0 ;
    
    // example of gains; will use file for that
    FILE *f = fopen(FileName.Data(),"r") ;
    if(f==0) {
      LOG(ERR,"Can't open gain file") ;
      return;
    }
    while(!feof(f)) {
      char buff[128] ;
      int sec,rdo,port,ch,row,pad ;
      float g, t ;
      
      if(fgets(buff,sizeof(buff),f)==0) continue ;
      
      if(buff[0]=='#') continue ;
      if(strlen(buff)<1) continue ;
      
      int ret = sscanf(buff,"%d %d %d %d %d %d %f %f",&sec,&rdo,&port,&ch,&row,&pad,&g,&t) ;
      if(ret != 8) continue ;
      
      
      if(ch<0) {
	bad_fee++ ;
	fcf.zap_fee(sec,rdo,port) ;
	continue ;
      }
      
      //if(g<0.01) bad_ch++ ;
      all_ch++ ;
      
      //              g_s[sec][row][pad].g = g ;
      g_s[sec][row][pad].t = t ;
    }


    for(int s=1;s<=24;s++) {
      for(int r=1;r<=40;r++) {
        for(int p=1;p<=fcf.x_max(r,0);p++) {
	  float g = fcf.get_gain(s,r,p) ;
	  
	  g_s[s][r][p].g = g ;
	  
	  if(g<0.01) {
	    bad_ch++ ;
	    
	    printf("BAD CH: S%02d: row %2d, pad %3d\n",s,r,p) ;
	  }
	  
        }}}
    

    LOG(INFO,"From gain file %s: %d bad fees, %d/%d bad channels",FileName.Data(),bad_fee,bad_ch,all_ch) ;
    fclose(f) ;

    itpcPadGainT0 = new St_itpcPadGainT0("itpcPadGainT0",1);
    itpcPadGainT0_st GainT0; 
    memset (&GainT0, 0, sizeof(GainT0));
    GainT0.run = 124; // run;
    for(int s=1;s<=24;s++) {
      for(int r=1;r<=40;r++) {
        for(int p=1;p<=120;p++) {
	  GainT0.Gain[s-1][r-1][p-1] = g_s[s][r][p].g ;
	  GainT0.T0[s-1][r-1][p-1] = g_s[s][r][p].t ;
        }
      }
    }
    itpcPadGainT0->AddAt(&GainT0);
    //  itpcPadGainT0->Print(0,1);
    TDatime  time(d,t);
    TString filename(Form("itpcPadGainT0.%08d.%06d",time.GetDate(),time.GetTime()));
    printf("Create %s\n",filename.Data());
    filename += ".root";
    fOut = new TFile(filename.Data(),"recreate");
    itpcPadGainT0->Write();
  }
  delete fOut;
}
/* Dmitry's scripts
   /star/u/stardb/dbcron/macros-new/Fill_itpcPadGainT0.C
   /star/u/stardb/dbcron/macros-new/Fill_tpcPadGainT0B.C

================================================================================
heston
mysql -h heston.star.bnl.gov --port=3501 -u "" -p""  Conditions_rts


robinsonMySQL [Conditions_rts]> select  entryTime,elementID,beginTime,flavor,deactive,run from tpcPadGainT0 order by beginTime;;
+---------------------+-----------+---------------------+--------+----------+---------+
| entryTime           | elementID | beginTime           | flavor | deactive | run     |
+---------------------+-----------+---------------------+--------+----------+---------+
| 2019-01-19 13:10:44 |         0 | 2019-01-19 13:10:44 | ofl    |        0 | 1105317 |
| 2019-01-19 13:23:00 |         0 | 2019-01-19 13:23:00 | ofl    |        0 | 1105317 |
| 2019-01-29 13:19:17 |         0 | 2019-01-29 13:19:17 | ofl    |        0 | 1105317 |
| 2019-02-27 08:36:38 |         0 | 2019-02-27 08:36:38 | ofl    |        0 | 1105317 |
| 2019-10-30 08:56:56 |         0 | 2019-10-30 04:56:56 | ofl    |        0 | 1106137 |
+---------------------+-----------+---------------------+--------+----------+---------+
5 rows in set (0.06 sec)

MySQL [Calibrations_tpc]> select entryTime,elementID,beginTime,flavor,deactive,run from tpcPadGainT0B where elementID=1 order by beginTime;
+---------------------+-----------+---------------------+--------+------------+---------+
| entryTime           | elementID | beginTime           | flavor | deactive   | run     |
+---------------------+-----------+---------------------+--------+------------+---------+
| 2009-12-01 20:04:27 |         1 | 2000-06-15 00:00:00 | ofl    |          0 |       0 |
| 2009-12-01 20:04:32 |         1 | 2001-06-01 00:00:00 | ofl    |          0 |       0 |
...
| 2018-03-08 18:36:21 |         1 | 2018-02-27 18:25:57 | ofl    |          0 | 1104698 |
| 2018-03-26 09:31:24 |         1 | 2018-03-26 09:24:31 | ofl    |          0 | 1104698 |
| 2017-02-07 11:41:59 |         1 | 2019-01-01 00:00:00 | sim    |          0 |       0 |
| 2019-01-23 17:41:49 |         1 | 2019-01-19 13:10:44 | ofl    |          0 | 1105317 |
| 2019-01-23 17:41:50 |         1 | 2019-01-19 13:23:00 | ofl    |          0 | 1105317 |
| 2019-01-29 13:20:34 |         1 | 2019-01-29 13:19:17 | ofl    |          0 | 1105317 |
| 2019-02-27 08:41:19 |         1 | 2019-02-27 08:36:38 | ofl    |          0 | 1105317 |
| 2019-12-03 19:18:43 |         1 | 2019-10-30 04:56:56 | ofl    |          0 | 1106137 |
+---------------------+-----------+---------------------+--------+------------+---------+
94 rows in set (0.14 sec)

MySQL [Calibrations_tpc]> select entryTime,elementID,beginTime,flavor,deactive,run from itpcPadGainT0 where elementID=1 order by beginTime;
Empty set (0.00 sec)
================================================================================
mysql -h dbbak.starp.bnl.gov -P 3418 Conditions_rts
MySQL [Conditions_rts]> select entryTime,elementID,beginTime,deactive,run from  tpcPadGainT0  order by beginTime;
+---------------------+-----------+---------------------+----------+---------+
| entryTime           | elementID | beginTime           | deactive | run     |
+---------------------+-----------+---------------------+----------+---------+
| 2019-01-19 13:10:44 |         0 | 2019-01-19 13:10:44 |        0 | 1105317 |
| 2019-01-19 13:23:00 |         0 | 2019-01-19 13:23:00 |        0 | 1105317 |
| 2019-01-29 13:19:17 |         0 | 2019-01-29 13:19:17 |        0 | 1105317 |
| 2019-02-27 08:36:38 |         0 | 2019-02-27 08:36:38 |        0 | 1105317 |
+---------------------+-----------+---------------------+----------+---------+
4 rows in set (0.00 sec)
MySQL [Conditions_rts]> select entryTime,elementID,beginTime,deactive,run from  itpcPadGainT0  order by beginTime;
+---------------------+-----------+---------------------+----------+------+
| entryTime           | elementID | beginTime           | deactive | run  |
+---------------------+-----------+---------------------+----------+------+
| 2019-01-19 00:01:34 |         0 | 2019-01-19 00:01:34 |        0 |  123 |
+---------------------+-----------+---------------------+----------+------+
1 row in set (0.00 sec)
================================================================================
mysql -h dbbak.starp.bnl.gov -P 3419 Conditions_rts
MySQL [Conditions_rts]> select entryTime,elementID,beginTime,deactive,run from  tpcPadGainT0  order by beginTime;
+---------------------+-----------+---------------------+----------+---------+
| entryTime           | elementID | beginTime           | deactive | run     |
+---------------------+-----------+---------------------+----------+---------+
| 2019-01-19 13:10:44 |         0 | 2019-01-19 13:10:44 |        0 | 1105317 |
| 2019-01-19 13:23:00 |         0 | 2019-01-19 13:23:00 |        0 | 1105317 |
| 2019-01-29 13:19:17 |         0 | 2019-01-29 13:19:17 |        0 | 1105317 |
| 2019-02-27 08:36:38 |         0 | 2019-02-27 08:36:38 |        0 | 1105317 |
| 2019-10-30 08:56:56 |         0 | 2019-10-30 04:56:56 |        0 | 1106137 |
+---------------------+-----------+---------------------+----------+---------+
5 rows in set (0.00 sec)

MySQL [Conditions_rts]> select entryTime,elementID,beginTime,deactive,run from  itpcPadGainT0  order by beginTime;
+---------------------+-----------+---------------------+----------+------+
| entryTime           | elementID | beginTime           | deactive | run  |
+---------------------+-----------+---------------------+----------+------+
| 2019-01-19 00:01:34 |         0 | 2019-01-19 00:01:34 |        0 |  123 |
+---------------------+-----------+---------------------+----------+------+
1 row in set (0.00 sec)
================================================================================

mysql -h onldb.starp.bnl.gov -P 3501 Conditions_rts

MySQL [Conditions_rts]> select entryTime,elementID,beginTime,deactive,run from  tpcPadGainT0  order by beginTime;
+---------------------+-----------+---------------------+----------+---------+
| entryTime           | elementID | beginTime           | deactive | run     |
+---------------------+-----------+---------------------+----------+---------+
| 2019-01-19 13:10:44 |         0 | 2019-01-19 13:10:44 |        0 | 1105317 |
| 2019-01-19 13:23:00 |         0 | 2019-01-19 13:23:00 |        0 | 1105317 |
| 2019-01-29 13:19:17 |         0 | 2019-01-29 13:19:17 |        0 | 1105317 |
| 2019-02-27 08:36:38 |         0 | 2019-02-27 08:36:38 |        0 | 1105317 |
| 2019-10-30 08:56:56 |         0 | 2019-10-30 04:56:56 |        0 | 1106137 |
+---------------------+-----------+---------------------+----------+---------+
5 rows in set (0.00 sec)
MySQL [Conditions_rts]> select entryTime,elementID,beginTime,deactive,run from  itpcPadGainT0  order by beginTime;
+---------------------+-----------+---------------------+----------+------+
| entryTime           | elementID | beginTime           | deactive | run  |
+---------------------+-----------+---------------------+----------+------+
| 2019-01-19 00:01:34 |         0 | 2019-01-19 00:01:34 |        0 |  123 |
+---------------------+-----------+---------------------+----------+------+
1 row in set (0.00 sec)


================================================================================
 cvs log $STAR/online/RTS/src/ITPC_SUPPORT/itpc_gains.txt > itpc.log
 cvs log $STAR/online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx.log
 awk '{\
       if ($1 == "revision") printf("cvs co -r %s -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >",$2);\
       if ($1 == "Version") printf("itpc_gains.txt.%08i.%06i\n",$4,$5);\
       }' itpc.log

MySQL [Calibrations_tpc]> select entryTime,elementID,beginTime,flavor,deactive,run from  itpcPadGainT0 where beginTime > "2019" order by beginTime desc;
+---------------------+-----------+---------------------+--------+----------+------+
| entryTime           | elementID | beginTime           | flavor | deactive | run  |
+---------------------+-----------+---------------------+--------+----------+------+
| 2021-11-08 19:06:47 |         0 | 2021-11-05 09:14:38 | ofl    |        0 |  123 |20211105.051401
| 2021-04-23 12:30:16 |         0 | 2021-04-23 12:25:22 | ofl    |        0 |  123 |20210423.082447
| 2021-03-15 14:50:14 |         0 | 2021-03-15 14:44:25 | ofl    |        0 |  123 |
| 2021-02-28 14:50:13 |         0 | 2021-02-28 14:48:45 | ofl    |        0 |  123 |
| 2021-02-25 09:50:14 |         0 | 2021-02-25 09:45:40 | ofl    |        0 |  123 |
| 2021-02-20 18:00:15 |         0 | 2021-02-20 17:55:06 | ofl    |        0 |  123 |
| 2021-02-02 10:10:12 |         0 | 2021-02-02 10:02:14 | ofl    |        0 |  123 |

cvs co -r 1.47 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20211130.094528
cvs co -r 1.46 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20211126.030408
cvs co -r 1.45 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20211126.025616
cvs co -r 1.44 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20211126.024751
cvs co -r 1.43 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20211112.113607
cvs co -r 1.42 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20211112.113500
cvs co -r 1.41 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20211111.051138
cvs co -r 1.40 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20211111.050441
cvs co -r 1.39 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20211105.051401
cvs co -r 1.38 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20211105.051325
cvs co -r 1.37 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20210630.082758
cvs co -r 1.36 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20210611.110412
cvs co -r 1.35 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20210517.033610
cvs co -r 1.34 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20210514.010417
cvs co -r 1.33 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20210423.082447
cvs co -r 1.32 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20210407.073736
cvs co -r 1.31 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20210319.064311
cvs co -r 1.30 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20210315.104408
cvs co -r 1.29 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20210228.094832
cvs co -r 1.28 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20210225.044533
cvs co -r 1.27 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20210220.125428
cvs co -r 1.26 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20210201.073202
cvs co -r 1.25 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20210128.023247
cvs co -r 1.24 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20210126.042220
cvs co -r 1.23 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20210126.042117
cvs co -r 1.22 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20201026.102654
cvs co -r 1.21 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20201026.101147
cvs co -r 1.20 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20200626.084156
cvs co -r 1.19 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20200622.084024
cvs co -r 1.18 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20200617.065332
cvs co -r 1.17 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20200610.083104
cvs co -r 1.16 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20200314.111211
cvs co -r 1.15 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20200312.123619
cvs co -r 1.14 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20200309.130417
cvs co -r 1.13 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20200309.094108
cvs co -r 1.12 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20200220.025625
cvs co -r 1.11 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20200130.061524
cvs co -r 1.10 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20200127.105303
cvs co -r 1.9 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20200105.081651
cvs co -r 1.8 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20191202.053238
cvs co -r 1.7 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20191129.034258
cvs co -r 1.6 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20191104.084613
cvs co -r 1.5 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20191104.033112
cvs co -r 1.4 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20191104.032401
cvs co -r 1.3 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20191101.090440
cvs co -r 1.2 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20191101.090256
cvs co -r 1.1 -p online/RTS/src/ITPC_SUPPORT/itpc_gains.txt >itpc_gains.txt.20191030.050318

cvs log $STAR/online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx.log
 awk '{\
       if ($1 == "revision") printf("cvs co -r %s -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >",$2);\
       if ($1 == "Version") printf("tpc_gains.txt.%08i.%06i\n",$4,$5);\
       }' tpx.log
MySQL [Calibrations_tpc]> select entryTime,elementID,beginTime,flavor,deactive,run from  tpcPadGainT0 where beginTime > "2019" order by beginTime desc;
+---------------------+-----------+---------------------+--------+----------+---------+
| entryTime           | elementID | beginTime           | flavor | deactive | run     |
+---------------------+-----------+---------------------+--------+----------+---------+
| 2021-11-08 19:06:02 |         0 | 2021-10-29 08:19:05 | ofl    |        0 | 1107731 |20211029.041901
| 2021-01-29 20:40:53 |         0 | 2021-01-14 16:23:04 | ofl    |        0 |       0 |20210114.112304
| 2021-01-29 20:40:50 |         0 | 2020-08-07 14:44:03 | ofl    |        0 |       0 |
| 2021-01-29 20:40:48 |         0 | 2020-08-07 14:16:59 | ofl    |        0 |       0 |

cvs co -r 1.101 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20211029.041901
cvs co -r 1.102 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20211213.102452  
cvs co -r 1.101 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20211029.041901  
cvs co -r 1.100 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20211029.040810  
cvs co -r 1.99 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20210503.085129    
cvs co -r 1.98 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20210401.085802    
cvs co -r 1.97 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20210226.061422    
cvs co -r 1.96 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20210114.112304    
cvs co -r 1.95 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20200807.104403    
cvs co -r 1.94 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20200807.101659    
cvs co -r 1.93 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20191216.060513    
cvs co -r 1.92 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20191216.055625    
cvs co -r 1.91 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20191216.031341    
cvs co -r 1.90 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20191129.041911    
cvs co -r 1.89 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20191129.041003    
cvs co -r 1.88 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20191129.035320    
cvs co -r 1.87 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20191030.045652    
cvs co -r 1.86 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20191030.044030    
cvs co -r 1.85 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20190623.153905    
cvs co -r 1.84 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20190623.031712    
cvs co -r 1.83 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20190227.033057    
cvs co -r 1.82 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20190226.062805    
cvs co -r 1.81 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20190205.030534    
cvs co -r 1.80 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20190205.030315    
cvs co -r 1.79 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20190205.030024    
cvs co -r 1.78 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20190129.081912    
cvs co -r 1.77 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20190119.082255    
cvs co -r 1.76 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20190119.081038    
cvs co -r 1.75 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20190119.080422    
cvs co -r 1.74 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20190110.060009    
cvs co -r 1.73 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20180326.052426    
cvs co -r 1.72 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20180227.131934    
cvs co -r 1.71 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20180227.123255    
cvs co -r 1.70 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20180227.122857    
cvs co -r 1.69 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20171113.114052    
cvs co -r 1.68 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20170320.075429    
cvs co -r 1.67 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20170320.075111    
cvs co -r 1.66 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20170128.200956    
cvs co -r 1.65 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20170128.193728    
cvs co -r 1.64 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20170128.193322    
cvs co -r 1.63 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20160211.125600    
cvs co -r 1.62 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20160211.121636    
cvs co -r 1.61 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20160211.110340    
cvs co -r 1.60 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20160211.105248    
cvs co -r 1.59 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20160114.082038    
cvs co -r 1.58 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20160107.114627    
cvs co -r 1.57 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20150415.165009    
cvs co -r 1.56 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20150202.091555    
cvs co -r 1.55 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20150121.123646    
cvs co -r 1.54 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20150121.122644    
cvs co -r 1.53 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20150113.073009    
cvs co -r 1.52 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20150113.064031    
cvs co -r 1.50 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20140201.090438    
cvs co -r 1.49 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20140131.121642    
cvs co -r 1.48 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20131029.055015    
cvs co -r 1.47 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20130429.121034    
cvs co -r 1.46 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20130429.120550    
cvs co -r 1.45 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20130429.120404    
cvs co -r 1.44 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20130327.110450    
cvs co -r 1.43 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20130327.110136    
cvs co -r 1.42 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20130107.082939    
cvs co -r 1.41 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20121001.143459    
cvs co -r 1.40 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20120603.141239    
cvs co -r 1.39 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20120308.094947    
cvs co -r 1.38 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20120209.092729    
cvs co -r 1.37 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20120123.134210    
cvs co -r 1.36 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20111201.075406    
cvs co -r 1.35 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20111130.074804    
cvs co -r 1.34 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20111130.073930    
cvs co -r 1.33 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20111007.081206    
cvs co -r 1.32 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20111007.080924    
cvs co -r 1.31 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20110926.030907    
cvs co -r 1.30 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20110204.142155    
cvs co -r 1.29 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20110204.141720    
cvs co -r 1.28 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20110204.132452    
cvs co -r 1.27 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20110203.112539    
cvs co -r 1.26 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20110126.143551    
cvs co -r 1.25 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20100914.033102    
cvs co -r 1.24 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20100212.063502    
cvs co -r 1.23 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20100129.050853    
cvs co -r 1.22 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20100126.145326    
cvs co -r 1.21 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20100126.144144    
cvs co -r 1.20 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20091214.165624    
cvs co -r 1.19 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20091210.144450    
cvs co -r 1.18 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20091210.141015    
cvs co -r 1.17 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20091209.141757    
cvs co -r 1.16 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20091111.022632    
cvs co -r 1.15 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20091111.021120    
cvs co -r 1.14 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20091110.032722    
cvs co -r 1.13 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20091109.050346    
cvs co -r 1.12 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20091109.045446    
cvs co -r 1.11 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20091109.045116    
cvs co -r 1.10 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20091109.044832    
cvs co -r 1.8 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20090830.161716     
cvs co -r 1.7 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20090829.140248     
cvs co -r 1.6 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20090829.134450     
cvs co -r 1.5 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20090829.134056     
cvs co -r 1.4 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20090828.113844     
cvs co -r 1.3 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20093526.133548     
cvs co -r 1.2 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >tpx_gains.txt.20093426.133454     
											     
											     
											     
MySQL [Calibrations_tpc]> select beginTime,flavor, deactive, run from tpcPadGainT0  where deactive = 0  order by beginTime desc;
 mysql -h robinson.star.bnl.gov --port=3306 -u "fisyak" Calibrations_tpc -e 'SELECT beginTime,DATE_FORMAT(beginTime, \'%Y%m%d.%H%m%S\' ) ,run from tpcPadGainT0 order by beginTime desc ; '



MySQL [Calibrations_tpc]>  select entryTime,beginTime,flavor,deactive,run from itpcPadGainT0 where deactive = 0 order by beginTime;
+---------------------+---------------------+--------+----------+----------+
| entryTime           | beginTime           | flavor | deactive | run      |
+---------------------+---------------------+--------+----------+----------+
| 2017-12-20 00:00:00 | 2017-12-20 00:00:00 | ofl    |        0 |      123 |
| 2018-05-11 21:51:44 | 2018-05-11 17:16:58 | ofl    |        0 |      123 |
| 2019-01-23 17:42:27 | 2019-01-19 00:01:34 | ofl    |        0 |      123 |
| 2022-01-19 14:25:36 | 2019-01-19 00:01:35 | ofl    |        0 |      123 |
| 2022-02-03 17:37:14 | 2019-02-26 09:57:02 | ofl    |        0 | 20057003 |
| 2022-02-03 17:38:22 | 2019-02-27 08:51:40 | ofl    |        0 | 20058005 |
| 2022-02-03 17:38:26 | 2019-03-15 08:16:39 | ofl    |        0 | 20074014 |
| 2022-02-03 17:38:31 | 2019-04-17 13:34:01 | ofl    |        0 | 20107029 |
| 2022-02-03 17:38:36 | 2019-06-07 17:48:34 | ofl    |        0 | 20158042 |

| 2021-02-01 12:55:12 | 2021-02-01 07:32:02 | ofl    |        0 |        0 |
| 2021-02-02 10:10:12 | 2021-02-02 10:02:14 | ofl    |        0 |      123 |
| 2021-02-20 18:00:15 | 2021-02-20 17:55:06 | ofl    |        0 |      123 |
| 2021-02-25 09:50:14 | 2021-02-25 09:45:40 | ofl    |        0 |      123 |
| 2021-02-28 14:50:13 | 2021-02-28 14:48:45 | ofl    |        0 |      123 |
| 2021-03-15 14:50:14 | 2021-03-15 14:44:25 | ofl    |        0 |      123 |
| 2021-04-23 12:30:16 | 2021-04-23 12:25:22 | ofl    |        0 |      123 |
| 2021-11-08 19:06:47 | 2021-11-05 09:14:38 | ofl    |        0 |      123 |
| 2021-11-12 16:40:14 | 2021-11-12 16:35:21 | ofl    |        0 |      123 |
| 2021-11-12 16:50:14 | 2021-11-12 16:36:10 | ofl    |        0 |      123 |
| 2021-11-26 08:10:15 | 2021-11-26 08:07:10 | ofl    |        0 |      123 |
| 2021-11-30 14:50:14 | 2021-11-30 14:45:41 | ofl    |        0 |      123 |
| 2021-12-29 16:20:13 | 2021-12-29 16:14:00 | ofl    |        0 |      123 |
| 2022-01-01 17:30:16 | 2022-01-01 17:21:42 | ofl    |        0 |      123 |
| 2022-01-03 13:30:16 | 2022-01-03 13:30:10 | ofl    |        0 |      123 |
| 2022-01-05 13:40:14 | 2022-01-05 13:32:30 | ofl    |        0 |      123 |
| 2022-01-07 14:00:16 | 2022-01-07 13:53:01 | ofl    |        0 |      123 |
| 2022-01-10 09:40:15 | 2022-01-10 09:36:19 | ofl    |        0 |      123 |
| 2022-01-14 17:50:13 | 2022-01-14 17:41:32 | ofl    |        0 |      123 |
+---------------------+---------------------+--------+----------+----------+
28 rows in set (0.03 sec)

MySQL [Calibrations_tpc]>  select entryTime,beginTime,flavor,deactive,run from itpcPadGainT0 where deactive = 0 order by beginTime;
+---------------------+---------------------+--------+----------+----------+
| entryTime           | beginTime           | flavor | deactive | run      |
+---------------------+---------------------+--------+----------+----------+
| 2017-12-20 00:00:00 | 2017-12-20 00:00:00 | ofl    |        0 |      123 |
| 2018-05-11 21:51:44 | 2018-05-11 17:16:58 | ofl    |        0 |      123 |
| 2019-01-23 17:42:27 | 2019-01-19 00:01:34 | ofl    |        0 |      123 |
| 2022-01-19 14:25:36 | 2019-01-19 00:01:35 | ofl    |        0 |      123 |
| 2022-02-03 17:37:14 | 2019-02-26 09:57:02 | ofl    |        0 | 20057003 |
| 2022-02-03 17:38:22 | 2019-02-27 08:51:40 | ofl    |        0 | 20058005 |
| 2022-02-03 17:38:26 | 2019-03-15 08:16:39 | ofl    |        0 | 20074014 |
| 2022-02-03 17:38:31 | 2019-04-17 13:34:01 | ofl    |        0 | 20107029 |
| 2022-02-03 17:38:36 | 2019-06-07 17:48:34 | ofl    |        0 | 20158042 |

| 2022-02-03 17:50:23 | 2019-10-30 05:03:18 | ofl    |        0 |      124 | 20303004	pedAsPhys	2019-10-30 20:12:08	-22222.2	0	-1	2
| 2022-02-03 17:51:28 | 2019-11-01 09:04:40 | ofl    |        0 |      124 | 20305001	CosmicLocalClock	2019-11-01 13:59:39	-22222.2	2	-1	2
| 2022-02-03 17:51:33 | 2019-11-04 03:24:01 | ofl    |        0 |      124 | 20308001	pedAsPhys_tcd_only	2019-11-04 20:05:03	-22222.2	2	-1	2
| 2022-02-03 17:51:38 | 2019-11-04 03:31:12 | ofl    |        0 |      124 |
| 2022-02-03 17:51:43 | 2019-11-04 08:46:13 | ofl    |        0 |      124 |
| 2022-02-03 17:51:47 | 2019-11-29 03:42:58 | ofl    |        0 |      124 | 20332056	CosmicLocalClock	2019-11-29 03:27:45	-1.003	0	0	2
| 2022-02-03 17:51:52 | 2019-12-02 05:32:38 | ofl    |        0 |      124 | 20336001	laser_localclock	2019-12-02 05:32:35	-1.003	0	0	2
| 2022-02-03 17:51:57 | 2020-01-05 08:16:51 | ofl    |        0 |      124 | 21005008	production_11p5GeV_2020	2020-01-05 07:46:29	-1.003	0	0	2
| 2022-02-03 17:52:02 | 2020-01-27 10:53:03 | ofl    |        0 |      124 | 21027022	production_11p5GeV_2020	2020-01-27 10:46:58	-1.003	2	-1	2
| 2022-02-03 17:52:08 | 2020-01-30 06:15:24 | ofl    |        0 |      124 | 21030001	production_9p8GeV_fixedTarget_2020	2020-01-30 05:57:48	-1.003	0	0	2
| 2022-02-03 17:52:13 | 2020-02-20 02:56:25 | ofl    |        0 |      124 | 21050062	production_11p5GeV_2020	2020-02-20 02:29:32	-1.004	0	0	
| 2022-02-03 17:52:17 | 2020-03-09 09:41:08 | ofl    |        0 |      124 | 21069007	production_9p2GeV_2020b	2020-03-09 09:08:59	-1.003	0	0	2
| 2022-02-03 17:52:21 | 2020-03-09 13:04:17 | ofl    |        0 |      124 | 21069014	production_9p2GeV_2020b	2020-03-09 13:04:18	-1.003	0	0	2
| 2022-02-03 17:52:25 | 2020-03-12 12:36:19 | ofl    |        0 |      124 | 21072012	pedAsPhys	2020-03-12 12:34:56	-1.003	0	2	2
| 2022-02-03 17:52:31 | 2020-03-14 11:12:11 | ofl    |        0 |      124 | 21074010	production_9p2GeV_2020b	2020-03-14 10:25:53	-1.003	0	0	2
| 2022-02-03 17:52:35 | 2020-06-10 08:31:04 | ofl    |        0 |      124 | 21162001	pedestal_rhicclock_clean	2020-06-10 15:25:32	-0.001	2	-1	2
| 2022-02-03 17:52:40 | 2020-06-17 06:53:32 | ofl    |        0 |      124 | 21169003	tune_9p2GeV_2020	2020-06-17 06:12:37	-1.002	0	0	2
| 2022-02-03 17:52:45 | 2020-06-22 08:40:24 | ofl    |        0 |      124 | 21174008	production_9p2GeV_2020c	2020-06-22 07:58:03	-1.003	0	0	2
| 2022-02-03 17:52:50 | 2020-06-26 08:41:56 | ofl    |        0 |      124 | 21178010	production_9p2GeV_2020c	2020-06-26 10:39:58	-1.003	0	0	2
| 2022-02-03 17:52:55 | 2020-10-26 10:11:47 | ofl    |        0 |      124 | 21258012	pedAsPhys	2020-09-14 18:20:04	-0	0	0	2
| 2022-02-03 17:52:59 | 2020-10-26 10:26:54 | ofl    |        0 |      124 | 21258012	pedAsPhys	2020-09-14 18:20:04	-0	0	0	2

| 2021-02-01 12:55:12 | 2021-02-01 07:32:02 | ofl    |        0 |        0 |
| 2021-02-02 10:10:12 | 2021-02-02 10:02:14 | ofl    |        0 |      123 |
| 2021-02-20 18:00:15 | 2021-02-20 17:55:06 | ofl    |        0 |      123 |
| 2021-02-25 09:50:14 | 2021-02-25 09:45:40 | ofl    |        0 |      123 |
| 2021-02-28 14:50:13 | 2021-02-28 14:48:45 | ofl    |        0 |      123 |
| 2021-03-15 14:50:14 | 2021-03-15 14:44:25 | ofl    |        0 |      123 |
| 2021-04-23 12:30:16 | 2021-04-23 12:25:22 | ofl    |        0 |      123 |
| 2021-11-08 19:06:47 | 2021-11-05 09:14:38 | ofl    |        0 |      123 |
| 2021-11-12 16:40:14 | 2021-11-12 16:35:21 | ofl    |        0 |      123 |
| 2021-11-12 16:50:14 | 2021-11-12 16:36:10 | ofl    |        0 |      123 |
| 2021-11-26 08:10:15 | 2021-11-26 08:07:10 | ofl    |        0 |      123 |
| 2021-11-30 14:50:14 | 2021-11-30 14:45:41 | ofl    |        0 |      123 |
| 2021-12-29 16:20:13 | 2021-12-29 16:14:00 | ofl    |        0 |      123 |
| 2022-01-01 17:30:16 | 2022-01-01 17:21:42 | ofl    |        0 |      123 |
| 2022-01-03 13:30:16 | 2022-01-03 13:30:10 | ofl    |        0 |      123 |
| 2022-01-05 13:40:14 | 2022-01-05 13:32:30 | ofl    |        0 |      123 |
| 2022-01-07 14:00:16 | 2022-01-07 13:53:01 | ofl    |        0 |      123 |
| 2022-01-10 09:40:15 | 2022-01-10 09:36:19 | ofl    |        0 |      123 |
| 2022-01-14 17:50:13 | 2022-01-14 17:41:32 | ofl    |        0 |      123 |
+---------------------+---------------------+--------+----------+----------+
49 rows in set (0.05 sec)


MySQL [Calibrations_tpc]>  select entryTime,beginTime,flavor,deactive,run from tpcPadGainT0 where deactive = 0 and beginTime > "2019" order by beginTime;
+---------------------+---------------------+--------+----------+---------+
| entryTime           | beginTime           | flavor | deactive | run     |
+---------------------+---------------------+--------+----------+---------+
| 2021-12-26 17:27:03 | 2019-01-10 11:00:09 | ofl    |        0 | 1105199 | tpx_gains.txt.20190110.060009
| 2021-12-26 17:27:09 | 2019-01-19 13:04:22 | ofl    |        0 | 1105317 | tpx_gains.txt.20190119.080422
| 2021-12-26 17:27:14 | 2019-01-19 13:10:38 | ofl    |        0 | 1105317 | tpx_gains.txt.20190119.081038
| 2019-01-23 17:41:03 | 2019-01-19 13:10:44 | ofl    |        0 | 1105317 | tpx_gains.txt.20190119.082255
| 2021-12-26 17:27:20 | 2019-01-19 13:22:55 | ofl    |        0 | 1105317 | 
| 2019-01-23 17:41:04 | 2019-01-19 13:23:00 | ofl    |        0 | 1105317 | 
| 2021-12-26 17:27:25 | 2019-01-29 13:19:12 | ofl    |        0 | 1105317 | tpx_gains.txt.20190129.081912
| 2019-01-29 13:20:33 | 2019-01-29 13:19:17 | ofl    |        0 | 1105317 | 
| 2021-12-26 17:27:31 | 2019-02-05 08:00:24 | ofl    |        0 | 1105317 | tpx_gains.txt.20190205.030024
| 2021-12-26 17:27:36 | 2019-02-05 08:03:15 | ofl    |        0 | 1105317 | tpx_gains.txt.20190205.030315
| 2021-12-26 17:27:42 | 2019-02-05 08:05:34 | ofl    |        0 | 1105317 | tpx_gains.txt.20190205.030534
| 2021-12-26 17:27:47 | 2019-02-26 11:28:05 | ofl    |        0 | 1105317 | 
| 2021-12-26 17:27:53 | 2019-02-27 08:30:57 | ofl    |        0 | 1105317 | tpx_gains.txt.20190226.062805
| 2019-02-27 08:41:20 | 2019-02-27 08:36:38 | ofl    |        0 | 1105317 | tpx_gains.txt.20190227.033057
| 2021-12-26 17:27:59 | 2019-06-23 07:17:12 | ofl    |        0 | 1105317 | tpx_gains.txt.20190623.031712
| 2021-12-26 17:28:04 | 2019-06-23 19:39:05 | ofl    |        0 | 1105317 | 
| 2019-12-03 19:18:05 | 2019-10-30 04:56:56 | ofl    |        0 | 1106137 | tpx_gains.txt.20190623.153905
| 2021-12-26 17:28:10 | 2019-10-30 08:40:30 | ofl    |        0 | 1106137 | tpx_gains.txt.20191030.044030
| 2021-12-26 17:28:15 | 2019-10-30 08:56:52 | ofl    |        0 | 1106137 | tpx_gains.txt.20191030.045652
| 2021-12-26 17:28:21 | 2019-11-29 08:53:20 | ofl    |        0 | 1106137 | tpx_gains.txt.20191129.035320
| 2021-12-26 17:28:27 | 2019-11-29 09:10:03 | ofl    |        0 | 1106137 | tpx_gains.txt.20191129.041003
| 2021-12-26 17:28:33 | 2019-11-29 09:19:11 | ofl    |        0 | 1106137 | tpx_gains.txt.20191129.041911
| 2021-12-26 17:28:38 | 2019-12-16 08:13:41 | ofl    |        0 | 1106137 | tpx_gains.txt.20191216.031341
| 2021-12-26 17:28:44 | 2019-12-16 10:56:25 | ofl    |        0 | 1106137 | tpx_gains.txt.20191216.055625
| 2021-12-26 17:28:50 | 2019-12-16 11:05:13 | ofl    |        0 | 1106137 | tpx_gains.txt.20191216.060513
| 2021-12-26 17:28:55 | 2020-08-07 14:16:59 | ofl    |        0 | 1106137 | tpx_gains.txt.20200807.101659
| 2021-12-26 17:29:01 | 2020-08-07 14:44:03 | ofl    |        0 | 1106137 | tpx_gains.txt.20200807.104403
| 2021-12-26 17:29:06 | 2021-01-14 16:23:04 | ofl    |        0 | 1106827 | tpx_gains.txt.20210114.112304
| 2021-12-26 17:29:11 | 2021-02-26 11:14:22 | ofl    |        0 | 1106827 | tpx_gains.txt.20210226.061422
| 2021-12-26 17:29:17 | 2021-04-01 12:58:02 | ofl    |        0 | 1106827 | tpx_gains.txt.20210401.085802
| 2021-12-26 17:29:22 | 2021-05-03 12:51:29 | ofl    |        0 | 1106827 | tpx_gains.txt.20210503.085129
| 2021-12-26 17:29:28 | 2021-10-29 08:08:10 | ofl    |        0 | 1107731 | tpx_gains.txt.20211029.040810
| 2021-12-26 17:29:34 | 2021-10-29 08:19:01 | ofl    |        0 | 1107731 | tpx_gains.txt.20211029.041901
| 2021-11-08 19:06:02 | 2021-10-29 08:19:05 | ofl    |        0 | 1107731 | 
| 2021-12-26 17:29:39 | 2021-12-13 15:24:52 | ofl    |        0 | 1107731 | tpx_gains.txt.20211213.102452
| 2021-12-13 15:30:14 | 2021-12-13 15:25:01 | ofl    |        0 | 1107731 |
+---------------------+---------------------+--------+----------+---------+
36 rows in set, 1 warning (0.00 sec)
*/
