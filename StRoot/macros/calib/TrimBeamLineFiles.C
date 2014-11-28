/*
  TrimBeamLineFiles.C
  
  Author: G. Van Buren, BNL, May 2006
  
  Purpose:
    In order to get an appropriate number of good triggers per
    fill to use in the BeamLine constraint calibration, a query
    is made to the database. This query can do a reasonably good
    job of obtaining a steady number of events per run, but not
    so per fill. This macro does the subsequent work of paring this
    down per fill. An effort is made to use at least one file from
    each run in the fill, and files with few good triggers are
    avoided. Otherwise, selection of files from within fills is
    random.
    
    Because the real number of events which reconstruct primary
    vertices may be less than the predicted "good" events per file,
    a fudgefactor is allowed to adjust for this efficiency.
  
  Usage:
    In this example, we try to get 10k events per run in a query
    and then pare that down to 2k events per fill. A few test runs
    might show that our reconstruction only gives us about 75% as
    many good vertices as predicted from the database. So we make
    the query (I have left the 'where' clause ambiguous for the
    user to replace appropriately), then run this macro:

    > cat > query.txt << EOF
select
floor(bi.blueFillNumber) as fill,
sum(ts.numberOfEvents/ds.numberOfFiles) as gdEvtPerFile,
ds.numberOfFiles,
sum(dt.numberOfEvents*ts.numberOfEvents/ds.numberOfEvents) as gdInFile,
dt.file, rd.glbSetupName
-- 
from daqFileTag as dt
left join l0TriggerSet as ts on ts.runNumber=dt.run
left join runDescriptor as rd on rd.runNumber=dt.run
left join daqSummary as ds on ds.runNumber=dt.run
left join beamInfo as bi on bi.runNumber=dt.run
left join detectorSet as de on de.runNumber=dt.run
left join runStatus as rs on rs.runNumber=dt.run
left join magField as mf on mf.runNumber=dt.run
-- 
where ...
and ...
-- 
group by dt.file
having (@t4:=sum(dt.numberOfEvents*ts.numberOfEvents/ds.numberOfEvents))>60
and (@t3:=mod(avg(dt.filesequence+(10*dt.fileStream)),(@t2:=floor(sum(ts.numberOfEvents)/10000.0)+1)))=0
;
<<EOF
    > cat query.txt | mysql -h onldb.starp.bnl.gov --port=3501 -C RunLog > output.txt
    > root -b -q 'TrimBeamLineFiles.C("output.txt","out.list",2000,0.75)'


*/

//______________________________________________________________________
// Function declarations:

void TrimBeamLineFiles(const char* input, const char* output="out.list",
                       int min=2500, double fudgefactor=1.0);
int flush_fill();

//______________________________________________________________________
// Global variables:

int minfiles=2; // minimum number of files to examine per fill
double minimum=0;
int ct,fill,oldfill;
double nper[1024];
int nfiles[1024];
double good[1024];
TString fname[1024];
ofstream* out;

//______________________________________________________________________
void TrimBeamLineFiles(const char* input, const char* output,
                       int min, double fudgefactor) {
  minimum = min;
  ifstream in(input);
  out = new ofstream(output);
  char line1[1024];
  char line2[1024];

  in >> line1;
  if (strcmp(line1,"fill")) {
    fill = atoi(line1);
  } else {
    in >> line2 >> line2 >> line2 >> line2 >> line2;
    in >> fill;
  }

  int numfiles = 0;
  oldfill = fill;
  ct = 0;
  while (!in.eof() && ct<1024) {
    if (fill != oldfill && fill>0) {
      numfiles += flush_fill();
      oldfill = fill;
      ct=0;
    }
    double goodb;
    in >> nper[ct] >> nfiles[ct] >> goodb >> line1 >> line2;
    good[ct] = fudgefactor * goodb;
    fname[ct] = line1;
    if (fill>0) ct++;
    in >> fill;
  }
  numfiles += flush_fill();
  printf("Total file count = %d\n",numfiles);
  out->close();
  delete out;
  out=0;
}

//______________________________________________________________________
int flush_fill() {
  // ct is the number of files provided to sort through
  
  if (ct<=0) return 0;
  int k;
  double totgood = 0;
  for (k=0;k<ct;k++) totgood += good[k];
  
  int mfiles = minfiles;
  int nused=0;
  double tcount;
  Bool_t happy = kFALSE;
  while (!happy) {

    double mctratio = ((double) mfiles)/((double) ct);
    double fneed = TMath::Max(minimum/totgood,mctratio);
    fneed = TMath::Min(fneed,1.0);

    double using[1024];
    int giveup=0;
    int gaveup=10;
  
    while (nused<mfiles && giveup<gaveup) { // insure using >= mfiles
      gRandom->RndmArray(ct,using);
      int rused = 0;
      nused = 0;
      tcount = 0;

      for (k=0;k<ct;k++) {
        Bool_t last_k = (k==(ct-1) || nper[k] != nper[k+1]);
      
        // Use at least 1 per run where we expect lots of good events
        if (last_k && rused==0 && good[k]>750) using[k] = 0.0;
        if (good[k]<50) using[k]=1.0;
        if (using[k] <= fneed) {
          tcount += good[k];
          rused++;
	  nused++;
        }
        if (last_k) rused=0;
      }
      giveup++;
    }

    if (tcount < minimum && mfiles < ct) {
      // Bump it up more quickly
      mfiles = TMath::Max(mfiles+1,(int) (fneed*ct));
    } else {
      happy = kTRUE;
    }

  }

  for (k=0;k<ct;k++) {
    if (using[k] <= fneed) (*out) << fname[k].Data() << endl;
  }
  printf("Guess for fill %d = %f (%d files)\n",oldfill,tcount,nused);
  if (tcount<minimum)
    printf("^^^^^ WARNING ^^^^^     Below minimum=%d\n",(int) minimum);
  return nused;
}

//______________________________________________________________________
// $Id: TrimBeamLineFiles.C,v 1.1 2006/05/11 19:43:09 genevb Exp $
// $Log: TrimBeamLineFiles.C,v $
// Revision 1.1  2006/05/11 19:43:09  genevb
// Introduce macro
//
//
