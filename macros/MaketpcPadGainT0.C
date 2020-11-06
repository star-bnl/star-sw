/*
  root.exe lDb.C 'MaketpcPadGainT0.C+("itpc_gains.txt.20200626.084156")'
  root.exe lDb.C 'MaketpcPadGainT0.C+("tpx_gains.txt.20200807.104403")'
  foreach f (`ls -d *txt.2*`)
    root.exe -q -b lDb.C 'MaketpcPadGainT0.C+("'${f}'")' >& ${f}.log
  end
via RTS
  
root.exe lRTS.C 'MaketpcPadGainT0.C+("itpc_gains.txt.20200626.084156")'
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
#define __USE__RTS__
#ifdef __USE__RTS__
#include "RTS/src/DAQ_TPX/tpxGain.h"
#endif /* __USE__RTS__ */
#endif
void MaketpcPadGainT0(TString FileName="tpx_gains.txt.20180326.052426"){ //"itpc_gains.txt.20191030.050318") {
  if (gClassTable->GetID("TTable") < 0) gSystem->Load("libTable");
#ifndef __USE__RTS__
  if (gClassTable__USE__RTS__->GetID("St_tpcPadGainT0") < 0) gSystem->Load("libStDb_Tables.so");
  FILE *fp = fopen(FileName.Data(),"r");
  if (! fp) {
    cout << "Can't open" << FileName.Data() << endl;
    return;
  }
  Int_t d = 0;
  Int_t t = 0;
  Bool_t itpc = kFALSE;
  if (FileName.BeginsWith("tpx_gains.txt.")) {
    Int_t n = sscanf(FileName.Data(),"tpx_gains.txt.%0d.%0d",&d,&t);
    if (n != 2) {cout << "Illegal file name " << FileName.Data() << endl; return;}
  } else  if (FileName.BeginsWith("itpc_gains.txt.")) {
    Int_t n = sscanf(FileName.Data(),"itpc_gains.txt.%0d.%0d",&d,&t);
    if (n != 2) {cout << "Illegal file name " << FileName.Data() << endl; return;}
    itpc = kTRUE;
  }
  if (! d ) {
    cout << "Illegal file " << FileName.Data() << endl;
    return;
  }
  Char_t line[121];
  TFile *f = 0;
  Int_t nEntry = 0;
  if (! itpc) {
    St_tpcPadGainT0 *tpcPadGainT0 = new St_tpcPadGainT0("tpcPadGainT0",1);
    tpcPadGainT0_st GainT0; 
    memset (&GainT0, 0, sizeof(GainT0));
    Int_t run = 0, sec, row, pad;
    Float_t gain, t0;
    Int_t n = 0;
    while (fgets(&line[0],120,fp)) {
      if (line[0] == '#') {
	TString Line(line);
	Int_t index = Line.Index("Run ");
	if (index < 0) continue;
	n = sscanf(&line[index+4],"%d",&run);
	GainT0.run = run;
	continue;
      }
      n = sscanf(&line[0],"%d%d%d%f%f",&sec,&row,&pad,&gain,&t0);
      if (sec < 1 || sec > 24) continue;
      if (row < 1 || row > 45) continue;
      if (pad < 1 || pad > 182) continue;
      GainT0.Gain[sec-1][row-1][pad-1] = gain;
      GainT0.T0[sec-1][row-1][pad-1] = t0;
      nEntry++;
      if (nEntry%1000 == 1) {
	printf("%7i: %2d %2d %3d %8.3f %8.3f\n",nEntry,sec,row,pad, GainT0.Gain[sec-1][row-1][pad-1], GainT0.T0[sec-1][row-1][pad-1]);
      }
    }
    tpcPadGainT0->AddAt(&GainT0);
    //  tpcPadGainT0->Print(0,1);
    TDatime  time(d,t);
    TString filename(Form("tpcPadGainT0.%08d.%06d",time.GetDate(),time.GetTime()));
    printf("Create %s\n",filename.Data());
    filename += ".root";
    f = new TFile(filename.Data(),"recreate");
    tpcPadGainT0->Write();
  } else {
    St_itpcPadGainT0 *itpcPadGainT0 = new St_itpcPadGainT0("itpcPadGainT0",1);
    itpcPadGainT0_st GainT0; 
    memset (&GainT0, 0, sizeof(GainT0));
    Int_t run = 0, sec, row, pad, rdo,port,ch;
    Float_t gain, t0;
    Int_t n = 0;
    while (fgets(&line[0],120,fp)) {
      if (line[0] == '#') {
	TString Line(line);
	Int_t index = Line.Index("Run ");
	if (index < 0) continue;
	n = sscanf(&line[index+4],"%d",&run);
	GainT0.run = run;
	continue;
      }
      // printf("%d %d %d %d %d %d %.3f %.3f%s\n",s+1,rdo,port,ch,r,p,g,t0,g==0.0?comment:"") ;
      n = sscanf(&line[0],"%d%d%d%d%d%d%f%f",&sec,&rdo,&port,&ch,&row,&pad,&gain,&t0);
      if (sec < 1 || sec > 24) continue;
      if (row < 1 || row > 45) continue;
      if (pad < 1 || pad > 182) continue;
      GainT0.Gain[sec-1][row-1][pad-1] = gain;
      GainT0.T0[sec-1][row-1][pad-1] = t0;
      nEntry++;
      if (nEntry%1000 == 1) {
	printf("%7i: %2d %2d %3d %3d %3d %3d %8.3f %8.3f\n",nEntry,sec,row,pad, rdo, port, ch,GainT0.Gain[sec-1][row-1][pad-1], GainT0.T0[sec-1][row-1][pad-1]);
      }
    }
    itpcPadGainT0->AddAt(&GainT0);
    //  itpcPadGainT0->Print(0,1);
    TDatime  time(d,t);
    TString filename(Form("itpcPadGainT0.%08d.%06d",time.GetDate(),time.GetTime()));
    printf("Create %s\n",filename.Data());
    filename += ".root";
    f = new TFile(filename.Data(),"recreate");
    itpcPadGainT0->Write();
  }
  delete f;
  delete fp;
#else /* __USE__RTS__ */
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
  TFile *f = 0;
  Int_t run = tpx_gain.c_run;
  Int_t d = tpx_gain.c_date;
  Int_t t = tpx_gain.c_time;
  Bool_t itpc = kFALSE;
   if (FileName.BeginsWith("tpx_gains.txt.")) {
  } else  if (FileName.BeginsWith("itpc_gains.txt.")) {
    itpc = kTRUE;
  }
  if (! itpc) {
    St_tpcPadGainT0 *tpcPadGainT0 = new St_tpcPadGainT0("tpcPadGainT0",1);
    tpcPadGainT0_st GainT0; 
    memset (&GainT0, 0, sizeof(GainT0));
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
    TDatime  time(d,t);
    TString filename(Form("tpcPadGainT0.%08d.%06d",time.GetDate(),time.GetTime()));
    printf("Create %s\n",filename.Data());
    filename += ".root";
    f = new TFile(filename.Data(),"recreate");
    tpcPadGainT0->Write();
  } else {
    St_itpcPadGainT0 *itpcPadGainT0 = new St_itpcPadGainT0("itpcPadGainT0",1);
    itpcPadGainT0_st GainT0; 
    memset (&GainT0, 0, sizeof(GainT0));
    for(int s=1;s<=24;s++) {			// NOTE: sector counts from 1
      for(int r=1;r<=40;r++) {		// NOTE: row counts from 1
	for(int p=1;p<=120;p++) {	// NOTE: pads count from 1
	  GainT0.Gain[s-1][r-1][p-1] = tpx_gain.get_gains(s,r,p)->g;
	  GainT0.T0[s-1][r-1][p-1]   = tpx_gain.get_gains(s,r,p)->t0;
	}
      }
    }
    itpcPadGainT0->AddAt(&GainT0);
    //  itpcPadGainT0->Print(0,1);
    TDatime  time(d,t);
    TString filename(Form("itpcPadGainT0.%08d.%06d",time.GetDate(),time.GetTime()));
    printf("Create %s\n",filename.Data());
    filename += ".root";
    f = new TFile(filename.Data(),"recreate");
    itpcPadGainT0->Write();
  }
  delete f;

#endif
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
       if ($1 = "revision") printf("cvs co -r %5.2f -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >",$2);\
       if ($1 == "Version") printf("tpc_gains.txt.%08i.%06i\n",$4,$5);\
       }' tpx.log

cvs co -r  1.22 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20201026.102654
cvs co -r  1.21 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20201026.101147
cvs co -r  1.20 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20200626.084156
cvs co -r  1.19 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20200622.084024
cvs co -r  1.18 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20200617.065332
cvs co -r  1.17 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20200610.083104
cvs co -r  1.16 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20200314.111211
cvs co -r  1.15 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20200312.123619
cvs co -r  1.14 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20200309.130417
cvs co -r  1.13 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20200309.094108
cvs co -r  1.12 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20200220.025625
cvs co -r  1.11 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20200130.061524
cvs co -r  1.10 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20200127.105303
cvs co -r  1.9 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20200105.081651
cvs co -r  1.8 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20191202.053238
cvs co -r  1.7 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20191129.034258
cvs co -r  1.6 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20191104.084613
cvs co -r  1.5 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20191104.033112
cvs co -r  1.4 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20191104.032401
cvs co -r  1.3 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20191101.090440
cvs co -r  1.2 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20191101.090256
cvs co -r  1.1 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20191030.050318

cvs co -r  1.94 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20200807.101659
cvs co -r  1.93 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20191216.060513
cvs co -r  1.92 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20191216.055625
cvs co -r  1.91 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20191216.031341
cvs co -r  1.90 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20191129.041911
cvs co -r  1.89 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20191129.041003
cvs co -r  1.88 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20191129.035320
cvs co -r  1.87 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20191030.045652
cvs co -r  1.86 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20191030.044030
cvs co -r  1.85 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20190623.153905
cvs co -r  1.84 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20190623.031712
cvs co -r  1.83 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20190227.033057
cvs co -r  1.82 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20190226.062805
cvs co -r  1.81 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20190205.030534
cvs co -r  1.80 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20190205.030315
cvs co -r  1.79 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20190205.030024
cvs co -r  1.78 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20190129.081912
cvs co -r  1.77 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20190119.082255
cvs co -r  1.76 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20190119.081038
cvs co -r  1.75 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20190119.080422
cvs co -r  1.74 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20190110.060009
cvs co -r  1.73 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20180326.052426
cvs co -r  1.72 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20180227.131934
cvs co -r  1.71 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20180227.123255
cvs co -r  1.70 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20180227.122857
cvs co -r  1.69 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20171113.114052
cvs co -r  1.68 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20170320.075429
cvs co -r  1.67 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20170320.075111
cvs co -r  1.66 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20170128.200956
cvs co -r  1.65 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20170128.193728
cvs co -r  1.64 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20170128.193322
cvs co -r  1.63 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20160211.125600
cvs co -r  1.62 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20160211.121636
cvs co -r  1.61 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20160211.110340
cvs co -r  1.60 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20160211.105248
cvs co -r  1.59 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20160114.082038
cvs co -r  1.58 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20160107.114627
cvs co -r  1.57 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20150415.165009
cvs co -r  1.56 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20150202.091555
cvs co -r  1.55 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20150121.123646
cvs co -r  1.54 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20150121.122644
cvs co -r  1.53 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20150113.073009
cvs co -r  1.52 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20150113.064031
cvs co -r  1.51 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20150113.003641
cvs co -r  1.50 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20140201.090438
cvs co -r  1.49 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20140131.121642
cvs co -r  1.48 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20131029.055015
cvs co -r  1.47 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20130429.121034
cvs co -r  1.46 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20130429.120550
cvs co -r  1.45 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20130429.120404
cvs co -r  1.44 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20130327.110450
cvs co -r  1.43 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20130327.110136
cvs co -r  1.42 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20130107.082939
cvs co -r  1.41 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20121001.143459
cvs co -r  1.40 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20120603.141239
cvs co -r  1.39 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20120308.094947
cvs co -r  1.38 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20120209.092729
cvs co -r  1.37 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20120123.134210
cvs co -r  1.36 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20111201.075406
cvs co -r  1.35 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20111130.074804
cvs co -r  1.34 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20111130.073930
cvs co -r  1.33 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20111007.081206
cvs co -r  1.32 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20111007.080924
cvs co -r  1.31 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20110926.030907
cvs co -r  1.30 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20110204.142155
cvs co -r  1.29 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20110204.141720
cvs co -r  1.28 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20110204.132452
cvs co -r  1.27 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20110203.112539
cvs co -r  1.26 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20110126.143551
cvs co -r  1.25 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20100914.033102
cvs co -r  1.24 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20100212.063502
cvs co -r  1.23 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20100129.050853
cvs co -r  1.22 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20100126.145326
cvs co -r  1.21 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20100126.144144
cvs co -r  1.20 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20091214.165624
cvs co -r  1.19 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20091210.144450
cvs co -r  1.18 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20091210.141015
cvs co -r  1.17 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20091209.141757
cvs co -r  1.16 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20091111.022632
cvs co -r  1.15 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20091111.021120
cvs co -r  1.14 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20091110.032722
cvs co -r  1.13 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20091109.050346
cvs co -r  1.12 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20091109.045446
cvs co -r  1.11 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20091109.045116
cvs co -r  1.10 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20091109.044832
cvs co -r  1.9 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20091022.161716
cvs co -r  1.8 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20090830.161716
cvs co -r  1.7 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20090829.140248
cvs co -r  1.6 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20090829.134450
cvs co -r  1.5 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20090829.134056
cvs co -r  1.4 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20090828.113844
cvs co -r  1.3 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20090826.133548
cvs co -r  1.2 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.20090826.133454
cvs co -r  1.1 -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx_gains.txt.19910101.000000

 */
