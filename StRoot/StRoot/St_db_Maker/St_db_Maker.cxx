/*!
 * class St_db_Maker
 * author Valery Fine(fine@bnl.gov)
 * date 10/08/98
 *
 * This class is C++ implementation of the
 * <a href="/STAR/comp/pkg/dev/pams/db/sdb/doc/">Simple Database Manager</a>.
 *
 */

// Most of the history moved at the bottom
//
// $Id: St_db_Maker.cxx,v 1.144 2020/01/15 02:07:47 perev Exp $
// $Log: St_db_Maker.cxx,v $
// Revision 1.144  2020/01/15 02:07:47  perev
// Cleanup
//
// Revision 1.143  2019/03/21 19:00:37  jeromel
// Added ATTENTION message
//
// Revision 1.142  2019/01/16 23:38:29  perev
// Fix wrong order in if (Jason)
//
// Revision 1.141  2018/01/17 17:15:59  perev
// Enhanced test to skip wrong file name corrected
//
// Revision 1.139  2018/01/16 19:50:44  perev
// Test to skip wrong file name is enhanced
//
// Revision 1.138  2017/04/26 20:20:24  perev
// Hide m_DataSet
//
// Revision 1.137  2015/05/19 20:34:47  perev
// Remove old commented coded
//
// Revision 1.136  2015/05/16 02:34:05  perev
// bug #3101 Cleanup
//
// Revision 1.135  2015/05/05 21:05:52  dmitry
// Updated db disconnect handling. Keep connection if less than 30 sec passed since last data retrieval
//
// Revision 1.134  2015/05/05 20:42:14  dmitry
// dynamic db disconnects handling
//
// Revision 1.133  2014/07/28 14:19:36  dmitry
// fixed templated call to make it compliant with gcc 4.8.2
//
// Revision 1.132  2013/07/18 14:00:32  fisyak
// expand no. of possible databases from 4 to 10
//
// Revision 1.131  2012/05/04 19:42:34  perev
// Ignore wrong timing for RunLog tables
//
// Revision 1.130  2012/04/19 16:20:38  perev
// Add test for unrecognized file
//
// Revision 1.129  2012/03/20 23:44:00  perev
// FullFileName() bug #2303 fix
//
// Revision 1.128  2012/03/16 19:36:18  dmitry
// converted dangled char pointers to std::string objects + fixed typo
//
// Revision 1.127  2012/01/24 02:55:43  perev
// Errors check added
//
// Revision 1.126  2011/11/28 23:23:35  dmitry
// case conversion update for overrides
//
// Revision 1.125  2011/11/28 17:03:09  dmitry
// dbv override support in StDbLib,StDbBroker,St_db_Maker
//
// Revision 1.124  2011/03/19 02:48:07  perev
// blacklist added
//
// Revision 1.122  2010/05/05 20:44:15  dmitry
// Fixed check for db broker in file mode
//
// Revision 1.121  2010/05/05 18:35:03  dmitry
// addon: single datasets also saved
//
// Revision 1.120  2010/05/05 15:25:51  dmitry
// refactored snapshot code, to include Valeri's patch (save .root files)
//
// Revision 1.119  2010/04/28 07:23:40  dmitry
// =new method to save snapshot+one subsequent dataset for each table in db
//
// Revision 1.118  2010/04/21 19:04:31  perev
// Save changed to account changed internal structure
//
// Revision 1.117  2010/04/17 02:08:33  perev
// ::SetDateTime set time also StMaker::SetDateTime
//
// Revision 1.116  2010/01/27 21:34:20  perev
// GetValidity now is static
//
// Revision 1.115  2009/11/16 20:16:23  fine
// Make the TDatime const interfaces
//
// Revision 1.114  2008/04/02 20:22:32  perev
// WarnOff
//
// Revision 1.113  2008/01/20 00:39:40  perev
// improve dbStat print
//
// Revision 1.112  2008/01/09 20:44:46  perev
// Improve printout in Finish()
//
// Revision 1.111  2007/12/29 01:43:32  perev
// More dbStat
//
// Revision 1.110  2007/09/10 02:19:25  perev
// StDbBroker::Release used
//
// Revision 1.109  2007/07/02 19:36:39  fisyak
// Add parameter currenTime (== requested time) in FindLeft
//
// Revision 1.108  2007/05/15 18:45:11  perev
// Add StMaker::Init() into Init
//
// Revision 1.107  2007/04/26 04:17:45  perev
// Correct printout
//
// Revision 1.106  2007/04/17 01:37:05  perev
// assert improved
//
// Revision 1.105  2007/04/16 00:20:23  perev
// Feature in TTable workaround
//
// Revision 1.104  2007/04/15 20:57:42  fisyak
// By pass bug in TTable::New
//
// Revision 1.103  2007/03/22 04:27:47  perev
// BugFix for user time
//
// Revision 1.102  2007/03/09 20:01:03  perev
// Request by user defined time now allowed
//
// Revision 1.101  2007/02/15 18:39:40  perev
// dbSnapshot fixes
//
// Revision 1.100  2006/11/23 01:55:44  perev
// Non init variable fixed. Thanks Yuri
//
// Revision 1.99  2006/08/15 21:42:20  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.98  2006/05/31 03:55:52  fisyak
// Set default fMaxEntryTime='now'
//
// Revision 1.97  2006/02/06 20:07:48  perev
// Decrease level of aliases 2==>1 to avoid name clashes
//
// Revision 1.96  2005/12/31 01:32:10  perev
// test for memory curruption added
//
// Revision 1.95  2005/10/06 18:46:30  fisyak
// Add protection for validity date < 19950101
//
// Revision 1.94  2005/08/29 21:44:20  fisyak
// switch from fBits to fStatus for StMaker control bits; account replacing of UInt_t by Int_t for m_runNumber
//
// Revision 1.93  2005/08/08 18:00:37  perev
// Move test of crazy date in SetFlavor to the end
//
// Revision 1.92  2005/08/05 23:44:33  perev
// Test for unseted time in SetFlavor added
//
// Revision 1.91  2005/07/26 16:48:13  perev
// SetFlavor/fDbBroker bug fixed
//
// Revision 1.90  2005/07/25 03:01:58  perev
// SetFlavor was not called if (fDbBroker==0)
//
// Revision 1.89  2005/07/20 17:41:34  perev
// Cleanup
//
// Revision 1.88  2005/05/13 19:39:11  perev
// Zero validity test added
//
// Revision 1.87  2005/04/01 21:38:31  perev
// call Make after SetFlavor
//
// Revision 1.86  2004/09/16 02:05:18  perev
// Add option to use saved dbConfig file. speedup
//
// Revision 1.85  2004/08/18 20:33:56  perev
// Timers added for MySQL and maker itself
//
// Revision 1.84  2004/07/22 20:47:40  perev
// Cleanup. 0 nrows for no table found
//
// Revision 1.83  2004/04/29 02:03:37  jeromel
// y2004a addded
//
// Revision 1.82  2004/04/14 22:55:56  jeromel
// Bundle changes for chain adjustements
//
// Revision 1.81  2004/04/08 00:28:53  perev
// AliasDate & AliasTime now static methods
//
// Revision 1.80  2004/04/08 00:13:09  perev
// Again move from .data to .const
//
// Revision 1.79  2004/04/07 18:17:45  perev
// Cleanup, DB data now in .const as should be
//
// Revision 1.78  2004/03/16 04:00:16  jeromel
// Improper number of elements times[] array fixed
//
// Revision 1.77  2004/01/06 23:58:01  perev
// (JL) More printing ...
//
// Revision 1.76  2004/01/06 16:44:42  jeromel
// Logic error not(()&&())
//
// Revision 1.75  2003/11/13 02:55:39  perev
// Safe destructor of TDataSet like object used
//
// Revision 1.74  2003/11/09 20:58:33  jeromel
// new timestamps
//
// Revision 1.73  2003/11/07 17:33:19  perev
// Add more clear message if no timestamp
//
// Revision 1.72  2003/10/06 04:05:33  perev
// deletes to be pleasant to Insure
//
// Revision 1.71  2003/10/01 21:51:31  jeromel
// Adjust as well
//
// Revision 1.70  2003/09/28 21:11:30  jeromel
// Unsued variable ldsname removed
//
// Revision 1.69  2003/09/23 01:04:55  jeromel
// READ_BAD_INDEX fixed
//
// Revision 1.68  2003/09/13 00:42:32  perev
// XDF obsolete + small fixes
//
// Revision 1.67  2003/09/02 17:59:25  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.66  2003/08/21 16:11:08  jeromel
// Labelling consistency correction
//
// Revision 1.65  2003/07/16 19:58:34  perev
// Cleanup of StTriggerData2003 at all
//
// Revision 1.64  2003/07/03 18:25:51  jeromel
// y2003x = 20021115 as for year2003
//
// Revision 1.63  2002/12/20 03:28:37  perev
// Save method improved
//
// Revision 1.62  2002/12/16 17:29:56  jeromel
// Fixed timestamp (final)
//
// Revision 1.61  2002/11/27 03:12:26  jeromel
// Expand array for a new geometry time stamp. Beware, timestamp not fixed yet
// (had to revert to the preceeding one as the new timeline does not exists but
// any stamp ended with garbage).Next commit will make it right ...
//

//	Example + explanation of DB maker statistics
//==============================================================================
// St_db_Maker::Init  	//Init without MySQL
// Real time 0:00:00, CP time 0.330, 3 slices
// 
//       MySQL::Init	// It is clear
// Real time 0:00:10, CP time 1.110, 2 slices
// 
// St_db_Maker::Make 	// All work without MySql
// Real time 0:00:03, CP time 3.200, 3177 slices
//       MySQL::Make   // MySql work
// Real time 0:01:10, CP time 6.440, 3104 slices
//       MySQL::Data	// Part of MySQL::Make with data transfer
// Real time 0:01:10, CP time 6.420, 252 slices
// 
// St_db_Maker:INFO  - St_db_Maker::dbStat : Evts = 3 dbEvts=1 Evts/dbEvts =  3.0
// // Evts =number of events, dbEvts = number of DB calls
// 
// St_db_Maker:INFO  - St_db_Maker::dbStat : dbData =      91.1 dbTime=25.3689 dbData/dbTime=3.59187
// // dbData = megabytes read from DB
// 
// St_db_Maker:INFO  - St_db_Maker::dbStat : dbTime =      25.4 dbEvts=1 dbTime/dbEvts =25.3689
// 
// St_db_Maker:INFO  - St_db_Maker::dbStat : dbCpu  =       6.4 dbEvts=1  dbCpu/dbEvts =6.42
// 
// St_db_Maker:INFO  - St_db_Maker::dbStat : dbTime/tot  =     60.81% dbCpu/tot=     15.39%
// // dbTime/tot = percent(%) of total time spent in DB
// // dbCpu/tot  = percent(%) of total CPU spent in DB
//==============================================================================



#define MYSQLON 1999

#include <Stiostream.h>
#include "Stiostream.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "TError.h"
#include "TBrowser.h"
#include "TDatime.h"
#include "TRegexp.h"
#include "TInterpreter.h"
#include "TFile.h"
#include "TSystem.h"
#include "St_db_Maker.h"
#include "TDataSetIter.h"
#include "TFileSet.h"
//VP#include "St_XDFFile.h"
#include "StTree.h"
#include "TTableDescriptor.h"
#include "TTable.h"
#include "TUnixTime.h"
#include "StDbBroker/StDbBroker.h"
#include "TAttr.h"
#include "StValiSet.h"

#include <numeric>

enum eKIND    { kXDFkind = 1, kCkind=2, kROOTkind = 3};
enum eDBMAKER { kUNIXOBJ = 0x2000};

/////////////////////////////////////////////////////////////////////////
//
//  Class St_dbConfig wraps the STAF table dbConfig
//  It has been generated by automatic. Please don't change it "by hand"
//
/////////////////////////////////////////////////////////////////////////
//_____________________________________________________________________________
TString FullFileName(const TDataSet* ds) 
{
  TString dbfile(ds->GetTitle()+5);
  TString dbDir(gSystem->BaseName(dbfile.Data()));
  dbDir.Append("/");
  TString full(ds->Path());
//		
  do { 		//Remove redundant like "*/StarDb/"
    int idx = full.Index(dbDir);if (idx<0) break;
    full.Remove(0,idx+dbDir.Length()-1);
  } while(1);
  
  TString name(ds->GetName());
  int idx = name.Index("."); if (idx>=0) name.Remove(idx,999);
  name.Insert(0,"/.");
  full.ReplaceAll(name,"");
  full.Insert(0,dbfile);
  gSystem->ExpandPathName(full);
  return full;
}  

TableClassImpl(St_dbConfig,dbConfig_st)
//__________________________ class St_db_Maker  ____________________________
//__________________________ class St_db_Maker  ____________________________
ClassImp(St_db_Maker)
//_____________________________________________________________________________
St_db_Maker::St_db_Maker(const char *name
			 , const char *dir0
			 , const char *dir1
			 , const char *dir2
			 , const char *dir3
			 , const char *dir4
			 , const char *dir5
			 , const char *dir6
			 , const char *dir7
			 , const char *dir8
			 , const char *dir9
)
:StMaker(name)
{
   for (int i=0;i<5;i++) {fTimer[i].Stop();}
   fTimer[5].Start(0);

   memset(fEvents,0,sizeof(fEvents)+sizeof(fDataSize));

   fDirs[0] = dir0;
   fDirs[1] = dir1;
   fDirs[2] = dir2;
   fDirs[3] = dir3;
   fDirs[4] = dir4;
   fDirs[5] = dir5;
   fDirs[6] = dir6;
   fDirs[7] = dir7;
   fDirs[8] = dir8;
   fDirs[9] = dir9;

   fDBBroker = 0;

   fHierarchy = 0;
   fIsDBTime = 0;

   fDataBase = 0;
   fUpdateMode = 0;
   TUnixTime ut;
   fQueryTs = time(NULL);
   fMaxEntryTime = ut.GetUTime();
}
//_____________________________________________________________________________
St_db_Maker::~St_db_Maker()
{
delete fDBBroker; fDBBroker =0;
                  fDataBase =0;
delete fHierarchy;fHierarchy=0;
}
//_____________________________________________________________________________
Int_t St_db_Maker::InitRun(int runumber)
{
  if (!fDBBroker || !runumber) return 0;
  fTimer[3].Start(0);
  fDBBroker->SetRunNumber(runumber);
  fTimer[3].Stop();
  return 0;
}
//_____________________________________________________________________________
Int_t St_db_Maker::Init()
{
   TDataSet *fileset;
   TString dir;
   fTimer[0].Start(0);
   SetBIT(kInitBeg);
   fDataBase=0;
   Snapshot(0);
   int snap = fDataBase!=0;
   for (int idir=0;!snap && idir < 10 && !fDirs[idir].IsNull(); idir++) {//loop over dirs

     dir = fDirs[idir];
     gSystem->ExpandPathName(dir);
     if (strncmp("MySQL:",  (const char*)dir,6)==0){
       fileset = OpenMySQL(((const char*)dir)+6);
       if (!fileset) return kStErr;
       fileset->Pass(PrepareDB,0);
     } else {

//              recreate a memory resided data-structure
       fileset = new TFileSet(dir,gSystem->BaseName(dir));
       if (!fileset->First()) {delete fileset; fileset = 0; continue;}
       fileset->Purge();
       fileset->Sort();
       fileset->Pass(PrepareDB,&dir);
       fileset->Purge();
     }
     if (fDataBase) {
       assert(strcmp(fDataBase->GetName(),fileset->GetName())==0);
       fDataBase->Update(fileset); delete fileset;
       } else          {fDataBase = fileset; }
   }

   if (fDataBase) {
     AddConst(fDataBase);
     SetOutputAll(fDataBase,1); // Only 1 level of aliases

     if (Debug()>1) fDataBase->ls("*");
   }
   OnOff();
   SetFlavor(0,0);      // Apply all collected before flavors
   ResetBIT(kInitBeg); SetBIT(kInitEnd);
   fTimer[0].Stop ();
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_db_Maker::Finish()
{
   Snapshot(1);
   for (int i=0;i<6;i++) fTimer[i].Stop();
   Printf("St_db_Maker::Init ");fTimer[0].Print();
   Printf("      MySQL::Init ");fTimer[2].Print();
   Printf("St_db_Maker::Make ");fTimer[1].Print();
   Printf("      MySQL::Make ");fTimer[3].Print();
   Printf("      MySQL::Data ");fTimer[4].Print();

   if (fEvents[1]<=0) return 0;
   double estiTime = fTimer[4].RealTime()*fTimer[5].CpuTime()/fTimer[5].RealTime();
   double eachEvt = double(fEvents[0]) /(fEvents[1]+3e-33);
   double MperTim = (fDataSize[1]*1e-6)/(estiTime+3e-33);
   double timPerE = estiTime/(fEvents[1]+3e-33);
   double cpuPerE = fTimer[4].CpuTime() /(fEvents[1]+3e-33);
   double timPct  = fTimer[4].RealTime()/fTimer[5].RealTime() *100;
   double cpuPct  = fTimer[4].CpuTime() /fTimer[5].CpuTime()  *100;

   Info("dbStat","Evts = %d dbEvts=%d Evts/dbEvts = %4.1f\n"
       ,fEvents[0], fEvents[1],eachEvt);

   Info("dbStat","dbData =%10.1f dbTime=%g dbData/dbTime=%g\n"
       ,fDataSize[1]*1e-6,estiTime,MperTim);

   Info("dbStat","dbTime =%10.1f dbEvts=%d dbTime/dbEvts =%g\n"
       ,estiTime,fEvents[1],timPerE);

   Info("dbStat","dbCpu  =%10.1f dbEvts=%d  dbCpu/dbEvts =%g\n"
       ,fTimer[4].CpuTime() ,fEvents[1],cpuPerE);

   Info("dbStat","dbTime/tot  =%10.2f dbCpu/tot=%10.2f \n"
       ,timPct,cpuPct);

   return 0;
}


//_____________________________________________________________________________
Int_t St_db_Maker::Make()
{

  fTimer[1].Start(0);
  TDatime td = GetDateTime();
  if (td.GetDate() >= 20330101) {
     Error("Make", "TimeStamp not set. Can not make request to DB");
     return kStFatal;
  }
  fUpdateMode = 1;
  UpdateDB(fDataBase);
  fUpdateMode = 0;
  fTimer[1].Stop();
  return kStOK;
}
//_____________________________________________________________________________
void St_db_Maker::Clear(const char *)
{
  if (!fDBBroker) return;
  fEvents[0]++;
  if (fDataSize[0]) fEvents[1]++;
  fDataSize[1]+=fDataSize[0];
  fDataSize[0]=0;

  time_t now = time(NULL);
  if ( ( now - fQueryTs ) < 30 ) { return; } // do not call dbbroker->release if less than 30 sec from last query

  fDBBroker->Release();
}
//_____________________________________________________________________________
TDatime St_db_Maker::Time(const char *filename)
{
  int lfilename,lname,idate,itime;

  TDatime time; time.Set(kMaxTime,0);//time is defined as a far future to ignore wrong file
  TString tfilename(filename);
  tfilename.ReplaceAll(".C","");
  tfilename.ReplaceAll(".root","");

  lfilename = tfilename.Length();
  lname = strcspn(tfilename.Data(),".");
  if (lfilename== lname) { //simple case xxx.C
     time.Set(kMinTime,0);
     return time;
  }
  idate = AliasDate(tfilename.Data()+lname+1);
  itime = AliasTime(tfilename.Data()+lname+1);

  if (idate) {
     time.Set(idate,itime);return time;
  }

  if (lname+16 <= lfilename     &&
      tfilename[lname+0 ]=='.'  &&
      tfilename[lname+9 ]=='.'    ) {// file name format:  <name>.YYYYMMDD.hhmmss.<ext>
       idate  = atoi(tfilename.Data()+lname+ 1);
       itime  = atoi(tfilename.Data()+lname+10);
   } else {                        // file name is wrong
     ::Error("St_db_Maker::Time", "Unrecognised File name %s IGNORED",filename);
     return time;
   }
   time.Set(idate,itime); return time;

}

int St_db_Maker::Kind(const char *filename)
{
   int lfilename;

   lfilename = strlen(filename);
   if (!strcmp(filename+lfilename-4,".xdf" )) return kXDFkind;
   if (!strcmp(filename+lfilename-2,".C"   )) return kCkind;
   if (!strcmp(filename+lfilename-2,".c"   )) return kCkind;
   if (!strcmp(filename+lfilename-5,".root")) return kROOTkind;
   return 0;
}
//_____________________________________________________________________________
TDataSet *St_db_Maker::OpenMySQL(const char *dbname)
{
   int nrows,irow,jrow;
   dbConfig_st *thy,*ihy,*jhy;
   TDataSet *top,*node,*ds;

   fTimer[0].Stop(); fTimer[2].Start(0);
   fDBBroker  = new StDbBroker();
   if (Debug() > 1) fDBBroker->setVerbose(1);
   if (fMaxEntryTime) { 
       fDBBroker->SetProdTime(fMaxEntryTime);
        for (std::map<std::pair<std::string,std::string>,UInt_t>::iterator it = fMaxEntryTimeOverride.begin(); it != fMaxEntryTimeOverride.end(); it++ ) {
            fDBBroker->AddProdTimeOverride((*it).second, (char*)((*it).first.first).c_str(), (char*)((*it).first.second).c_str());                                                                        
        }
   }
   const TAttr *attl = GetAttr();
   if (attl) {
     TIter next(attl);
     TObject *obj;
     while ((obj = next())) {
       if (strcmp("blacklist",obj->GetName())!=0) continue;
       fDBBroker->addBlacklistedDomain(obj->GetTitle());
       Info("OpenMySQL","Block domain %s",obj->GetTitle());
     }
   }
   TString ts(dbname); ts+="_hierarchy";
   fHierarchy = new St_dbConfig((char*)ts.Data());
   thy = fDBBroker->InitConfig(dbname,nrows);
   fTimer[0].Start(0); fTimer[2].Stop();
   if (!thy || !nrows){
       Warning("OpenMySQL","***Can not open MySQL DB %s ***",dbname);
       return 0;}

   fHierarchy->Adopt(nrows,thy);
   if (GetDebug()>1)  fHierarchy->Print(0,nrows);

   top = new TDataSet(thy->parname);
   top->SetTitle("directory");

   TDataSet **dss = new TDataSet*[nrows];
   memset(dss,0,nrows*sizeof(void*));

   //           First pass: directories only
   for (irow=0,ihy=thy; irow <nrows ; irow++,ihy++)
   {
     if (strcmp(ihy->tabtype,".node")==0) {// new node
       ds = new TDataSet(ihy->tabname);
       ds->SetTitle("directory");
     } else {                           // new table
       const char *ty = ihy->tabtype;
       if (ty[0]=='.') ty++;
       ds = TTable::New(ihy->tabname,ty,0,0);
       if (!ds) {
         Warning("OpenMySQL","Unknown table %s/%s.%s",ihy->parname,ihy->tabname,ihy->tabtype); continue;}
     }
     if (ds) ds->SetUniqueID(ihy->tabID);
     dss[irow] = ds;
   }

//              Second pass: set relations
   for (irow=0,ihy=thy; irow <nrows ; irow++,ihy++)
   {
     ds = dss[irow]; if (!ds)                   continue;
     if (strcmp(ihy->parname,top->GetName())==0) top->Add(ds);
     if(ds->GetParent())                        continue;
     for (jrow=0,jhy=thy; jrow <nrows ; jrow++,jhy++)
     {
       if (jrow==irow)                          continue;
       if (ihy->parID != jhy->tabID)            continue;
       if (strcmp(".node"     ,jhy->tabtype))   continue;
       if (strcmp(ihy->parname,jhy->tabname))   continue;
       node = dss[jrow]; if (!node)             continue;
       node->Add(ds); break;
     }/*end j for*/
     if (ds->GetParent())                       continue;
     delete ds; dss[irow]=0;
     Error("OpenMySQL","WRONG parent %s/%s\n",ihy->parname,ihy->tabname);
   }/*end i for*/


   delete [] dss;
   //   top->ls(99);
   return top;
}


//_____________________________________________________________________________
TDataSet *St_db_Maker::UpdateDB(TDataSet* ds)
{
  if(!ds) return 0;
  ds->Pass(&UpdateDB,this);
  return ds;
}
//_____________________________________________________________________________
int St_db_Maker::UpdateTable(UInt_t parId, TTable* dat
                            ,const TDatime &req,TDatime val[2] )
{

  assert(fDBBroker);assert(dat);

  

  fDBBroker->SetDateTime(req.GetDate(),req.GetTime());
  TTableDescriptor *rowTL = ((TTable*)dat)->GetRowDescriptors();
  fTimer[1].Stop();
  fTimer[3].Start(0);
  fTimer[4].Start(0);
  fDBBroker->SetDictionary(rowTL);
  fDBBroker->SetTableName (dat->GetName());
  fDBBroker->SetStructName(dat->GetTitle());
  fDBBroker->SetStructSize(dat->GetRowSize());

  //            if descriptor filled, no need for newdat
  void *dbstruct = fDBBroker->Use(dat->GetUniqueID(),parId);
//  printf("FLAVOR: %s.%s\n",dat->GetName(),fDBBroker->GetFlavor());
  Int_t d1 = fDBBroker->GetBeginDate();
  Int_t t1 = fDBBroker->GetBeginTime();
  if (d1 < 19950101) {
    Warning("UpdateTable","Table %s.%s Unacceptable Begin Date/Time %d/%d reset to 19950101/000001",
            dat->GetName(),dat->GetTitle(),d1,t1);
    d1 = 19950101; t1 = 1;
  }
  Int_t d2 = fDBBroker->GetEndDate  ();
  Int_t t2 = fDBBroker->GetEndTime  ();
  if (d2 < 19950101) {
    Warning("UpdateTable","Table %s.%s Unacceptable End Date/Time %d/%d reset to 19950101/000001",
            dat->GetName(),dat->GetTitle(),d2,t2);
    d2 = 19950101; t2 = 1;
  }
  val[0].Set(d1,t1);
  val[1].Set(d2,t2);

  // small debug statement
  if ( val[0].Get() >= val[1].Get()) {
    Error("UpdateTable","val[0].Get() = %u >= val[1].Get() = %u\n",val[0].Get(),val[1].Get());
    Error("UpdateTable:","Table %s.%s Suspicious Ranges Date/Time %d/%d->%d/%d\n",
            dat->GetName(),dat->GetTitle(),d1,t1,d2,t2);
    Error("UpdateTable","Table %s.%s Suspicious Ranges Date/Time %d/%d->%d/%d",
          dat->GetName(),dat->GetTitle(),d1,t1,d2,t2);
    assert(! ( val[0].Get() >= val[1].Get()));
  }

  fTimer[1].Start(0); fTimer[3].Stop();fTimer[4].Stop();

  if (!dbstruct) {
    dat->SetNRows(0);
    if(Debug()>1)  Warning("UpdateTable","Table %s.%s Not FOUND in DB",dat->GetName(),dat->GetTitle());
    return 1;
  }

  int nRows = fDBBroker->GetNRows();
  //            Adopt DB data in the new TTable
  dat->Adopt(nRows,dbstruct);
//      check size.
  fDataSize[0]+=nRows*dat->GetRowSize();
  if (!nRows || ((char*)dbstruct)[dat->GetRowSize()*nRows-1]) {}

  //  dat->Print(0,1);

  //  printf("BegVal=%s\n",val[0].AsString());
  //  printf("EndVal=%s\n",val[1].AsString());
  return 0;

}
//_____________________________________________________________________________
EDataSetPass St_db_Maker::UpdateDB(TDataSet* ds,void *user )
{
  StValiSet *val;
  if (strcmp("directory",ds->GetTitle())==0)    return kContinue;
  if (strcmp(".Val"     ,ds->GetTitle())!=0)    return kPrune;
  //
  //    It is our place.
  val = (StValiSet*)ds;
  St_db_Maker *mk = (St_db_Maker*)user;
  if (mk->fUpdateMode && !val->IsModified())    return kPrune;

  TDatime currenTime = mk->GetDateTime();
  UInt_t uevent = currenTime.Get();

  //            Check validity

    if (val->fTimeMin.Get() <= uevent
     && val->fTimeMax.Get() >  uevent)          return kPrune;

   TObjectSet set("dbSnapshot",0);                                                                                                                   
   const char *fname = mk->SAttr("dbSnapshot");                                                                                                      
   if (!fname || !*fname) {                                                                                                                          
     // not in a snapshot mode                                                                                                                       
   } else { 
    if (!mk->fDBBroker) {
      mk->Error("UpdateDB","DbSnapshot mode: wrong validity for %s ignored( ???? )"
               ,val->GetName());
      return kPrune;
    }
  }
  TDataSet *par = val->GetParent();
  par->Remove(val->fDat);
  mk->UpdateValiSet(val,currenTime);
  if (val->fGood) par->AddFirst(val->fDat);
  return kPrune;
}
//_____________________________________________________________________________
int St_db_Maker::UpdateValiSet(StValiSet *val,const TDatime &currenTimep)
{
static int nCall=0; nCall++;
  TDatime currenTime(currenTimep);
  if (currenTime.GetDate() >=kMaxTime) currenTime.Set(kMaxTime-1,0);
  TDataSet *left;
  TDatime valsCINT[2],valsSQL[2];
  UInt_t uevent = currenTime.Get();
//      Start loop

  val->fTimeMin.Set(kMaxTime,0);
  val->fTimeMax.Set(kMinTime,0);
  val->fGood=0;
  int kase = 0;
  valsSQL[0].Set(kMinTime,0);
  valsSQL[1].Set(kMaxTime,0);
  if (fDBBroker && val->fTabId ) {      // Try to load from MySQL
    assert(val->fTabId==val->fDat->GetUniqueID());
    int ierr = UpdateTable(val->fParId,(TTable*)val->fDat,currenTime,valsSQL );
    if (!ierr) kase = 1;
	fQueryTs = time(NULL);
  }

  left = FindLeft(val,valsCINT,currenTime);
  if (left) kase+=2;
  TDataSet *newGuy=0;
SWITCH:  switch (kase) {

    case 0:   // No SQL or CINT objects
              val->fTimeMin = valsSQL[0];  val->fTimeMax = valsSQL[1];
              kase=4; goto SWITCH;

    case 1:   // Only SQL object
              val->fTimeMin = valsSQL[0];  val->fTimeMax = valsSQL[1];
              val->fGood=1; kase=4; goto SWITCH;

    case 2:   // Only CINT object
              newGuy = LoadTable(left);
              if (!val->fDat) { val->fDat = newGuy; val->AddFirst(newGuy);}
              else if(val->fDat->InheritsFrom(TTable::Class()))
	                      { val->fDat->Update(newGuy); delete newGuy ;}
              else            { delete val->fDat; val->fDat = newGuy     ;}
              val->fTimeMin = valsCINT[0];  val->fTimeMax = valsCINT[1];
              val->fGood=1; kase=4; goto SWITCH;

    case 3:   // Both SQL and CINT objects
              if (valsCINT[0].Get()>=valsSQL[0].Get()) {
                kase = 2;
                if (valsCINT[1].Get()>valsSQL[1].Get()) valsCINT[1] = valsSQL[1];
              } else {
                kase = 1;
                if (valsSQL[1].Get()>valsCINT[1].Get()) valsSQL[1] = valsCINT[1];
              }
              goto SWITCH;

    case 4:
      if( ! ((val->fTimeMin.Get()<= uevent) && (uevent<val->fTimeMax.Get()) )){
//	wrong timing. We can forgive it, if it is RunLog. Akio case
        if (val->Path().Contains("RunLog/onl")) break;
	
	//
	// ATTENTION: do not change te format of the next line.
	//   CheckFail is verified by production parsers to detect
	//   fatal DB initialization issues.
	//
        (void) printf("CheckFail:: Assert will fail for Table %s TimeMin=%d TimeMax=%d uevent=%d\n",
                      val->GetName(),val->fTimeMin.Get(),val->fTimeMax.Get(),uevent);
        (void) printf("\tTimeMin "); val->fTimeMin.Print();
        (void) printf("\tuevent  "); currenTime.Print();
        (void) printf("\tTimeMax "); val->fTimeMax.Print();
      }
      assert((val->fTimeMin.Get()<= uevent) && (val->fTimeMax.Get()>uevent));

      break;

    default:  assert(0);
  }
  val->fVers++;
  val->Modified(1);
  if (val->fTimeMin.Get() == val->fTimeMax.Get()) {
    Warning("UpdateDB","Zero size validity for %s",val->GetName());
    printf("\tTimeMin "); val->fTimeMin.Print();
    printf("\tuevent  "); currenTime.Print();
    printf("\tTimeMax "); val->fTimeMax.Print();
    Warning("UpdateDB","Ask Mike DeFillips WHY!!!!!");
  }
  return val->fGood;
}

//_____________________________________________________________________________
TDataSet *St_db_Maker::FindLeft(StValiSet *val, TDatime vals[2], const TDatime &currenTime)
{

//      Search left object among ONLY CINT_DB objects. MySQL objects ignored here
//      Start loop
  UInt_t uevent = currenTime.Get();

  vals[0].Set(kMinTime,0);
  vals[1].Set(kMaxTime,0);
  UInt_t utmp,udifleft=(UInt_t)(-1),udifrite=(UInt_t)(-1);
  TDataSet *left=0,*rite=0,*set=0;
  TListIter next(val->GetList());
  while ((set = (TDataSet*)next())) {
    if (set == val->fDat) continue;  //MySQL object ignored
    const char *filename = set->GetName();
    UInt_t ucur = St_db_Maker::Time(filename).Get();
    if (!ucur) {
      Warning("FindLeft","*** Unrecognozed file %s ***",filename);
      continue;
    } else if (uevent < ucur) {
      utmp = ucur - uevent;
      if (utmp <= udifrite) { udifrite=utmp; rite=set;}
    }else{
      utmp = uevent - ucur;
      if (utmp <= udifleft) { udifleft=utmp; left=set;}
    }
  }// end while


  if (left) vals[0] = St_db_Maker::Time(left->GetName());
  if (rite) vals[1] = St_db_Maker::Time(rite->GetName());
  return left;
}


//_____________________________________________________________________________
TDataSet *St_db_Maker::LoadTable(TDataSet* left)
{
  TFile *tf =0;
  TObject *to =0;
  TString command;
  TDataSet *newdat = 0;
  TString dbfile(FullFileName(left));
  assert(dbfile.Length()>0);

  int kind = Kind(left->GetName());
  switch (kind) {

    case kXDFkind: // .xdf file
    assert(0);
 //    newdat = St_XDFFile::GetXdFile(dbfile);assert (newdat);
    if (GetDebug()) printf("Load XdfFile:   %s\n",(const char*)dbfile);
    break;

    case kCkind: // .C file

      command = ".L "; command += dbfile;
      if (GetDebug()) printf("LoadTable: %s\n",(const char*)command);
      TInterpreter::EErrorCode ee;
      gInterpreter->ProcessLine(command,&ee);
      assert(!ee);
      newdat = (TDataSet *) gInterpreter->Calc("CreateTable()",&ee);
      assert(!ee);
      command.ReplaceAll(".L ",".U ");
      gInterpreter->ProcessLine(command,&ee);
      assert(!ee);

      break;

    case kROOTkind: // .root file

      tf = new TFile(dbfile);
      to = StIO::Read (tf, "*");
      delete tf;
      if (!to) break;
      if (GetDebug()) printf("Load TFile:   %s\n",(const char*)dbfile);
      if (strcmp(to->ClassName(),"StIOEvent")==0) to = ((StIOEvent*)to)->fObj;
      if (!to) break;
      if (to->InheritsFrom(TDataSet::Class()))  {
        newdat = (TDataSet*)to;
      } else                                    {
        newdat = new TObjectSet(to->GetName());
        newdat->SetObject(to);
      }
    break;

    default: assert(0);
  }
  if (newdat) newdat->SetUniqueID(kUNIXOBJ+kind);

  return newdat;
}

//_____________________________________________________________________________
EDataSetPass St_db_Maker::PrepareDB(TDataSet* ds, void *user)
{
  TDataSet *set;
  StValiSet *pseudo = 0;
  const char *dsname,*filename,*dot;
  char psname[100];
  //int ldsname,lpsname;
  int lpsname;

  TList *list = ds->GetList();
  if (!list) return kContinue;
  if (strcmp("directory",ds->GetTitle())!=0)    return kPrune;
  dsname = ds->GetName(); ;
  if (!strcmp("CVS",dsname)) { delete ds;       return kPrune;}
  //ldsname= strlen(dsname);
  TString newTitle = "file ";
  if (user) newTitle += *((TString*)user);

//      Start loop
  pseudo = 0; psname[0]='.'; psname[1]=0;
  TListIter next(list);
  while ((set = (TDataSet*)next())) {
    filename = set->GetName();
    int isSql =(set->InheritsFrom(TTable::Class())!=0);
    if (isSql) {                        // Sql object
      lpsname = strlen(filename);
    } else {                            // Cint object
      if (strncmp("file",set->GetTitle(),4)!=0) continue;
      if (!(dot = strchr(filename,'.')))        continue;
      int k = Kind(filename);
      if (k==kCkind) {	//case of *.C
      }
      if (!k){ delete set;         continue;}
      set->SetTitle(newTitle);
      lpsname = dot - filename;
    }
    if (strncmp(filename,psname+1,lpsname)) {// make new pseudo directory
      psname[1]=0; strncat(psname,filename,lpsname);
      pseudo = new StValiSet(psname,ds); //VP strcat(psname,".");
      pseudo->fParId = ds->GetUniqueID();
    }

    set->Shunt(pseudo);
    if (isSql) {
      assert(pseudo);
      pseudo->fTabId = set->GetUniqueID();      // save SQL  Id
      pseudo->fDat=set;                         // save SQL  object
                                                // for future validity requests
    }
  }
  return kContinue;
}
//______________________________________________________________________________
TDataSet *St_db_Maker::GetDataBase(const char* logInput,const TDatime *td)
{
  fTimer[1].Start(0);
  TString ts;
  TDataSet *ds=0;
  int idir = 1,lst;
  ds = GetDataSet(logInput);
  if (!ds || strncmp(ds->GetTitle(),"directory",9)!=0)
  {             // We did not find it or it is not a directory.
                // May be concrete object name is here
     idir = 0;
     lst  =-1;
     for (int i=0;logInput[i];i++) if (logInput[i]=='/') lst=i;
     if (lst<0) { ds=0;         goto RETN;}
//              path/obj  ==> path/.obj
     ts = logInput;
     ts.Insert(lst+1,".");
     ds =  GetDataSet(ts.Data());
     if (!ds)                   goto RETN;
  }

  if (td) { //Case when time is defined by user. User will be owner
    if (idir) {
      Error("GetDataBase","Request for directory %s with user time is FORBIDDEN"
           ,ds->GetName());
      assert(!idir);
    }
    assert(!strcmp(".Val",ds->GetTitle()));
    StValiSet *vs = (StValiSet*)ds;
    StValiSet *myVS = new StValiSet(vs->GetName(),0);
    myVS->fTabId = vs->fTabId;
    myVS->fParId = vs->fParId;
    myVS->Modified(1);
    if (vs->fParId) {
      TTable *tb = (TTable *)vs->fDat;
      TString ty(tb->GetType());
      if (ty.EndsWith("_st")) ty.Remove(ty.Length()-3,99);
      myVS->fDat = TTable::New(tb->GetName(),ty,0,0);
      myVS->fDat->SetUniqueID(myVS->fTabId);
    }
    TDataSetIter next(vs);
    TDataSet *to=0;
    while ((to=(TDataSet*)next())){ myVS->Add(to);}

    UpdateValiSet(myVS,*td);
    myVS->GetList()->Clear("nodelete");
    if (myVS->fGood) { //object is found
     myVS->fDat->Add(myVS);
     return myVS->fDat;
    }
    delete myVS->fDat;
    delete myVS;
    return 0;
  }


  UpdateDB(ds);
  if (idir)                     goto RETN;
  ds = GetDataSet(logInput);
RETN: fTimer[1].Stop();         return ds;
}
//_____________________________________________________________________________
const TDatime &St_db_Maker::GetDateTime() const
{
  if (!fIsDBTime) return StMaker::GetDateTime();
  //(void )printf("**** fIsDBTime is set, returning its value\n");
  return fDBTime;
}
//_____________________________________________________________________________
void St_db_Maker::SetDateTime(Int_t idat,Int_t itim)
{
  fIsDBTime=0; if (idat==0) return;
  fIsDBTime=1; fDBTime.Set(idat,itim);
  Info("SetDateTime","Setting Startup Date=%d Time=%d",idat,itim);
  StMaker::SetDateTime(idat,itim);
}
//_____________________________________________________________________________
void   St_db_Maker::SetDateTime(const char *alias)
{
  fIsDBTime=1;
  int idat = AliasDate(alias);// <name>.YYYYMMDD.hhmmss.<ext>
  int itim = AliasTime(alias);
  assert(idat);
  Info("SetDateTime","(\"%s\") == Startup Date=%d Time=%d",alias,idat,itim);
  fDBTime.Set(idat,itim);
  StMaker::SetDateTime(idat,itim);
}
//_____________________________________________________________________________
void   St_db_Maker::SetOn(const char *path)
{ AddAlias("On" ,path,".onoff"); OnOff();}
//_____________________________________________________________________________
Int_t  St_db_Maker::Save(const char *path,const TDatime *newtime)
{
  ofstream out;
  int nakt=0,i,l;
  TDataSet *top,*ds;
  TTable   *tb;
  TString ts,dir;
  TDatime val[2];
  char cbuf[20];
  top = GetDataBase(path);
  if (!top) return 1;
  TDataSetIter nextDS(top,999);
  TDataSet::EDataSetPass  mode = TDataSet::kContinue;
  while((ds = nextDS(mode))) {
    mode = TDataSet::kContinue;
    if (ds->GetName()[0]=='.') { mode = TDataSet::kPrune; continue; }
    if (!ds->InheritsFrom(TTable::Class()))continue;
    ts = ds->Path();
    i = ts.Index(".const/"); assert(i>0); ts.Replace(0  ,i+7,"");
    int jdot = ts.Index(".",(Ssiz_t)1,(Ssiz_t)0   ,TString::kExact);
    assert(jdot>0);
    int jsla = ts.Index("/",(Ssiz_t)1,(Ssiz_t)jdot,TString::kExact);
    assert(jsla>0);
    ts.Remove(jdot,jsla-jdot+1);
    l = ts.Length();
    for (i=0;i<l;i++) {
      if (ts[i]!='/') continue;
      dir.Replace(0,999,ts,i);
      gSystem->MakeDirectory(dir);
    }
    tb = (TTable*)ds;
    if (newtime) { val[0] = *newtime;}
    else         {i = GetValidity(tb,val); assert(i>=0);}
    sprintf(cbuf,".%08d.%06d.C",val[0].GetDate(),val[0].GetTime());
    ts += cbuf;
    out.open((const char*)ts);
    tb->SavePrimitive(out,"");
    out.close();
    nakt++;
  }
  return (!nakt);
}
//_____________________________________________________________________________
Int_t St_db_Maker::SaveDataSet(TDataSet* ds, int type, bool savenext) {
    if (!ds || !ds->InheritsFrom(TTable::Class())) {                                                                                                        
        // dataset is not inherited from TTable                                                                                                      
        return 0;                                                                                                                                    
    }                                                                                                                                                
    int i = 0;                                                                                                                                         
    int jdot = 0, jsla = 0; // used for recursive directory creation                                                                                   
	TString ts; 
	TString dir; // DataSet directory on disk                                                                                                          
    ts = ds->Path();                                                                                                                                 
    i = 0; i = ts.Index(".const/"); assert(i>0); ts.Replace(0,i+7,"");                                                                                                                                                                                                                                   
    jdot = 0; jdot = ts.Index(".",(Ssiz_t)1,(Ssiz_t)0   ,TString::kExact); assert(jdot>0);                                                           
    jsla = 0; jsla = ts.Index("/",(Ssiz_t)1,(Ssiz_t)jdot,TString::kExact); assert(jsla>0);                                                           
    ts.Remove(jdot, jsla-jdot + 1);                                                                                                                  
    int l = ts.Length();                                                                                                                             

    for (i = 0; i < l; i++) {                                                                                                                        
      if (ts[i]!='/') continue;                                                                                                                      
      dir.Replace(0,999,ts,i);                                                                                                                       
      gSystem->MakeDirectory(dir);                                                                                                                   
    } 
	switch(type) {
		case 0:
			return SaveDataSetAsRootFile((TTable*)ds, ts, savenext);
			break;
		case 1:
			return SaveDataSetAsCMacro((TTable*)ds, ts, savenext);
			break;
        default:
		// requested format is not known to St_db_Maker, sorry!
		break;
	}                                                                                                                                      	
	return 1;
}

//_____________________________________________________________________________
Int_t St_db_Maker::SaveDataSetAsCMacro(TTable* tb, TString ts, bool savenext) {
    TDatime val[2]; // validity of original sets                                                                                                       
    std::ofstream out; // output file handle                                                                                                           
    int i = 0; i = GetValidity(tb,val); assert(i>=0);                                                                                                    
    std::stringstream ostr;                                                                                                                          
    ostr << ts << "." << std::setw(6) << std::setfill('0') <<val[0].GetDate() << "." << std::setw(6) << std::setfill('0')                            
         << val[0].GetTime() << ".C"; 
    out.open( ostr.str().c_str() );                                                                                                                  
    tb->SavePrimitive(out,"");                                                                                                                       
    out.close();                                                                                                                                     

	if (savenext == true) {
    	std::string tbname( std::string(ts.Data()).substr(7) ); // name of data set to search for
    	if (val[1].GetDate() < 20330101 ) { // table has more values, let's store +1 dataset 
        	TDataSet* ds_r = 0;                                                                                                                          
        	ds_r = GetDataBase(tbname.c_str(), &val[1]);                                                                                                 
			if (ds_r) {
				SaveDataSetAsCMacro((TTable*)ds_r, ts, false);
        		delete ds_r;
			}                                                                                                                                 
    	}                                                                                                                                                
	}
	return 0;
}

Int_t St_db_Maker::SaveDataSetAsRootFile(TTable* tb, TString ts, bool savenext) {
    TDatime val[2]; // validity of original sets                                                                                                       
    int i = 0; i = GetValidity(tb,val); assert(i>=0);                                                                                                    
    std::stringstream ostr;                                                                                                                          
    ostr << ts << "." << std::setw(6) << std::setfill('0') <<val[0].GetDate() << "." << std::setw(6) << std::setfill('0')                            
         << val[0].GetTime() << ".root";                                                                                                             
    TFile ofile(ostr.str().c_str(),"RECREATE" );                                                                                                   
    tb->Write();                                                                                                                                 
    ofile.Close(); 

	if (savenext == true) {
    	std::string tbname( std::string(ts.Data()).substr(7) ); // name of data set to search for
    	if (val[1].GetDate() < 20330101 ) { // table has more values, let's store +1 dataset 
        	TDataSet* ds_r = 0;                                                                                                                          
        	ds_r = GetDataBase(tbname.c_str(), &val[1]);                                                                                                 
			if (ds_r) {
				SaveDataSetAsRootFile((TTable*)ds_r, ts, false);
        		delete ds_r;
			}                                                                                                                                 
    	}
	}                                                                                                                                                

	return 0;
}

//_____________________________________________________________________________
Int_t  St_db_Maker::SaveSnapshotPlus(char* path, int type)                                                                            
{                                                                                                                                                    
  TDataSet *top = 0, *ds = 0;                                                                                                                        
  TString ts; // DataSet path                                                                                                                        
  TDataSet::EDataSetPass  mode = TDataSet::kContinue;                                                                                                
  TDatime maxDbTime; 
  maxDbTime.Set(kMaxTime,0);                                                                                                                                 

  // request DataSets from database                                                                                                                  
  top = GetDataBase(path);                                                                                                                           
  if (!top) {                                                                                                                                        
    // can't find requested database name                                                                                                            
    return 1;                                                                                                                                        
  }                 
  TDataSetIter nextDS(top,999); 
  ds = nextDS(mode);
  if (!ds) {
	 // no subtrees found, let's save this lonely dataset 
	 SaveDataSet(top, type, true);
	 return 0;
  }
  // cycle through available datasets                                                                                                                
  while ((ds = nextDS(mode))) {                                                                                                                      
    mode = TDataSet::kContinue;                                                                                                                      
    if (ds->GetName()[0]=='.') { mode = TDataSet::kPrune; continue; }                                                                                
	SaveDataSet(ds, type, true);
  }                                                                                                                                                  
  return 0;                                                                                                                                          
}   

//_____________________________________________________________________________
void St_db_Maker::SetFlavor(const char *flav,const char *tabname)
{
   TDataSet *fl=0;
   TDataSet *flaDir =0;
   if (flav) {
     fl = 0;
     flaDir = Find(".flavor");
     if (flaDir) fl = flaDir->Find(tabname);
     if (fl && strcmp(fl->GetTitle(),flav)==0)  return;
     if (fl) delete fl;
     fl = new TDataSet(tabname);
     fl->SetTitle(flav);
     AddData(fl,".flavor");
   }
   if (!fDBBroker)                              return;
   int nAkt = 0;
   flaDir = Find(".flavor");
   if (!flaDir)                                 return;
   StValiSet *val;
   TDataSetIter  valNext(GetConst(),999);
   while ((val = (StValiSet*)valNext())) {      //DB objects loop
     const char *tabName = val->GetName();
     if (tabName[0] != '.')                     continue;
     if (strcmp(val->GetTitle(),".Val")!=0)     continue;
     tabName++;
     if (val->fDat==0)                          continue;
     if (val->fDat->GetUniqueID() >= kUNIXOBJ)  continue;

     TDataSetIter  flaNext(flaDir);
     while((fl = flaNext())) {                  // Flavor loop
       const char *flaName = fl->GetName();
       if (strcmp(flaName,".all" )!=0
       &&  strcmp(flaName,tabName)!=0)          continue;
       const char *flaType = fl->GetTitle();
       if (val->fFla == flaType)                continue;

       TDataSet *par = val->GetParent();
       val->fFla = flaType;
       nAkt++;
       val->fTimeMin.Set(kMaxTime,0);
       val->fTimeMax.Set(kMinTime,0);
       int tabID = (int)val->fDat->GetUniqueID();
       int parID = (int)par->GetUniqueID();

       fTimer[1].Stop(); fTimer[3].Start(0);
       fDBBroker->SetTableFlavor(flaType,tabID, parID);
       fTimer[1].Start(0); fTimer[3].Stop();
       fl->SetUniqueID(fl->GetUniqueID()+1);

       if (Debug()<2)                           continue;
       if (strcmp("ofl",flaType)==0)            continue;
         printf("<St_db_Maker::SetFlavor> Set flavor %s to %s\n",flaType,tabName);

     }// End Flavor loop

   }//End DB objects loop

   TDataSetIter  flaNext(flaDir);
   while((fl = flaNext())) {                    // 2nd Flavor loop
     if (fl->GetUniqueID()) continue;
     if (strcmp(fl->GetName(),".all" )==0)      continue;
     Warning("SetFlavor","Flavor %s for table %s was NOT USED",
              fl->GetTitle(),fl->GetName());
   }

   delete flaDir;
   if (GetDateTime().GetDate() >= 20330101)     return;
   if(nAkt) Make();

}
//_____________________________________________________________________________
Int_t  St_db_Maker::GetValidity(const TTable *tb, TDatime *const val)
{
   if (!tb)                             return -1;
   TString ts("."); ts+=tb->GetName();
   TDataSet *pa = tb->GetParent();
   const StValiSet *vs =0;
   if (pa) { vs = (StValiSet*)pa->Find(ts);}
   else    { vs = (StValiSet*)tb->Find(ts);}
   if (!vs)                             return -2;
   if (val) {
     val[0] = vs->fTimeMin;
     val[1] = vs->fTimeMax;
   }
   return vs->fVers;
}
//_____________________________________________________________________________
Int_t  St_db_Maker::Drop(TDataSet *ds)
{
   if (!ds)                             return -1;
   TString ts("."); ts+=ds->GetName();
   TDataSet *pa = ds->GetParent();
   assert(pa);				
   assert(ts == pa->GetName());
   StValiSet *vs = (StValiSet*)pa;
   assert(ds == vs->fDat);
   TDataSet *ppa = pa->GetParent();
   ppa->Remove(ds); 
   pa->Remove(ds); 
   vs->fDat=0; 
   delete ds;
   return 0;
}
//_____________________________________________________________________________
void   St_db_Maker::SetOff(const char *path)
{ AddAlias("Off",path,".onoff"); OnOff();}
//_____________________________________________________________________________
void St_db_Maker::OnOff()
{
  int Off,len;
  if (!fDataBase) return;
  TDataSet *onoff = Find(".onoff");
  if (!onoff) return;

  TString tsBase,tsDir,tsTit;
  TDataSet *ono;
  TDataSetIter onoffNext(onoff);
  while((ono=onoffNext())) {// loop onoffs
    Off = (strcmp("Off",ono->GetName())==0);
    tsDir  = gSystem->DirName(ono->GetTitle());
    tsBase = gSystem->BaseName(ono->GetTitle());
    TRegexp rex(tsBase,1);
    TDataSet *dsDir = GetDataSet(tsDir);
    if (!dsDir) continue;
    if (GetMaker(dsDir) != this) continue;
    TDataSetIter nextVal(dsDir);
    TDataSet *val;
    while ((val=nextVal())) {//loop over val's
      const char *name = val->GetName();
      if(name[0]!='.')                          continue;
      tsTit = val->GetTitle();
      int ival = tsTit.Index(".Val");
      if (ival<0)                               continue;
      if (Off != (ival==0))                     continue;
      if (rex.Index(name+1,&len)!=0)            continue;
      if ( Off) tsTit.Replace(0,0,".Off");
      if (!Off) tsTit.Replace(0,4,""    );
      val->SetTitle(tsTit);
      printf("<%s::Set%s>   %s/%s\n"
      ,ClassName(),ono->GetName(),(const char*)tsDir,val->GetName()+1);
    }//end loop over val's
  }// end loop onoffs
}
//_____________________________________________________________________________
void St_db_Maker::SetMaxEntryTime(Int_t idate,Int_t itime)
{
  TUnixTime ut;
  ut.SetGTime(idate,itime);
  fMaxEntryTime = ut.GetUTime();
}

//_____________________________________________________________________________
void St_db_Maker::AddMaxEntryTimeOverride(Int_t idate,Int_t itime, char* dbType, char* dbDomain) {

	std::string mDbType = "";
	std::string mDbDomain = "";

	if (dbType) {
		for (char* p = dbType; *p != '\0'; p++) {
			*p = (char)std::tolower(*p);
		}
		dbType[0] = std::toupper(dbType[0]);
		mDbType = dbType;
	}

	if (dbDomain) {
		for (char* p = dbDomain; *p != '\0'; p++) {
			*p = (char)std::tolower(*p);
		}
		mDbDomain = dbDomain;
	}

    TUnixTime ut;
    ut.SetGTime(idate,itime);
    fMaxEntryTimeOverride.insert( std::make_pair( std::make_pair(mDbType,mDbDomain), ut.GetUTime() ) );
}

// Now very UGLY trick
#define private public
#include "TGeoManager.h"
//_____________________________________________________________________________
int St_db_Maker::Snapshot (int flag)
{
   int ians=0;
   TFile *tfSnap = 0;
   TObjectSet set("dbSnapshot",0);
   const char *fname = SAttr("dbSnapshot");
   if (!fname || !*fname)                                       return 0;
   switch (flag) {
     case 0://Read
     if (gSystem->AccessPathName(fname, kFileExists))           return 0;
     if (gSystem->AccessPathName(fname, kReadPermission))       return 0;
     tfSnap = TFile::Open(fname,"READ");
     if (!tfSnap)                                               return 0;
     if (tfSnap->IsZombie()) {delete tfSnap;                    return 0;}

     Info("Snapshot","Use DB from file %s\n",fname);
     ians = set.Read("dbSnapshot");
     fDataBase = (TDataSet*)set.GetObject();
     set.DoOwner(0);
     assert(fDataBase);
//      {
// //	Special case TGeo geometry must  be touched
//      TDataSet *geo = (TDataSet*)fDataBase->FindObject(".Geometry");
//      if (geo) {
//        geo = ((StValiSet*)geo)->fDat;
//        TGeoManager *gm = (TGeoManager*)geo->GetObject();
//        if (gm) { gm->fClosed=0; gm->CloseGeometry(); }
//      } }
     break;

     case 1://Write
     if (!gSystem->AccessPathName(fname, kFileExists))          return 0;
//// if (gSystem->AccessPathName(fname, kWritePermission))      return 0;
     tfSnap = TFile::Open(fname,"NEW");
     if (!tfSnap || tfSnap->IsZombie()) {
       Error("Snapshot","Can not open file for write");
       return 0;
     }
// //	TGeo geometry must not be saved
//      TDataSet *geo = (TDataSet*)fDataBase->FindObject(".Geometry");
//      if (geo) {
//        geo = ((StValiSet*)geo)->fDat;
//        Drop(geo);
//      }
     TDataSet *parent = fDataBase->GetParent();
     fDataBase->Shunt(0);
     set.SetObject(fDataBase);
     Info("Snapshot","Save DB to file %s\n",fname);
     ians = set.Write("dbSnapshot",TObject::kOverwrite);
     fDataBase->Shunt(parent);
     tfSnap->Close();
     break;
   }
   delete tfSnap;
   return ians;
}



//*-- Author :    Valery Fine(fine@bnl.gov)   10/08/98
//
// Revision 1.60  2002/06/14 14:48:00  jeromel
// Last change moved after Use()
//
// Revision 1.59  2002/06/14 14:03:11  jeromel
// Added warning if start=end fro date and time (i.e. 0 everywhere) for a table.
//
// Revision 1.58  2002/05/18 01:04:52  jeromel
// Small modif adding more debugging of what it's doing ... In perticular,
// the association string <-> DateTime is shown.
//
// Revision 1.57  2002/03/04 16:38:58  jeromel
// More info printed before assert()
//
// Revision 1.56  2002/03/01 14:56:42  jeromel
// Finalized the timestamp as per June 15th.
//
// Revision 1.55  2002/02/28 23:34:04  jeromel
// Since the svt geant geometry appears to have changed in Spectember 5th 2001,
// the definition of y2001 geometry must have changed since MDC4 (where y2001
// was used but the geometry did not have those recent updates). Furthermore,
// y2001 equates to year_2b for St_db_Maker, that is, timestamp 20010501 that's
// May 1st 2001. The 'db' entries for svt should also be different. So, we
// added a hacked entry to correct this very very very confusing issue.
//
//      MDC4New chain added
//      Timestamp for this y2001n, will equate to egant geometry y2001
//      but a new timestamp in St_db_maker .
//
// This allows for re-running through the geant MDC4 produced files using
// MDC4 chain option in the reco pass and find the same result.
//
// Revision 1.54  2002/01/18 16:18:03  perev
// TimeStamp for absent table fix
//
// Revision 1.53  2002/01/17 23:45:32  perev
// TimeStamp for nonexisting table fixed
//
// Revision 1.52  2001/11/07 18:02:28  perev
// reurn into InitRun added
//
// Revision 1.51  2001/10/27 21:48:32  perev
// SetRunNumber added
//
// Revision 1.50  2001/10/13 20:23:16  perev
// SetFlavor  working before and after Init()
//
// Revision 1.49  2001/09/26 23:24:04  perev
// SetFlavor for table added
//
// Revision 1.48  2001/04/14 02:03:46  perev
// const added
//
// Revision 1.47  2001/04/13 21:21:34  perev
// small fix (fine) + cons's
//
// Revision 1.46  2001/04/13 01:28:42  perev
// Small bug fixed (fine found)
//
// Revision 1.45  2001/03/21 23:06:13  didenko
// Set time for year to 20000614.175430 instead of 20000501.0
//
// Revision 1.44  2001/03/07 20:43:46  perev
// assert for wrong time added
//
// Revision 1.43  2001/02/18 20:09:58  perev
// Distinction between UNIX FS objects and MySql added
//
// Revision 1.42  2001/01/31 17:12:05  fisyak
// Introduce year-2b for STAR 2001
//
// Revision 1.41  2000/11/07 15:02:05  fisyak
// Fix typo
//
// Revision 1.40  2000/11/07 01:48:25  fisyak
// Add one more protection agaist fDataBase==NULL
//
// Revision 1.39  2000/11/02 16:02:20  fisyak
// Jeff request to allow a top level directory (e.g. database) have ID=0 which is ok
//
// Revision 1.38  2000/09/16 02:45:09  didenko
// commit Victor's changes
//
// Revision 1.37  2000/07/14 02:39:21  perev
// SetMaxEntryTime method added
//
// Revision 1.36  2000/06/29 15:58:04  perev
// bug in Init fixed, wrong {}
//
// Revision 1.35  2000/06/26 20:58:41  perev
// multiple DBs
//
// Revision 1.33  2000/06/20 20:39:49  fisyak
// Add debug print out
//
// Revision 1.32  2000/05/20 01:00:43  perev
// SetFlavor() added
//
// Revision 1.31  2000/05/12 15:10:19  fine
// new Table macro introduced
//
// Revision 1.30  2000/05/11 01:27:54  fisyak
// Change y1h from 03/01/2000 to 05/01/2000
//
// Revision 1.29  2000/04/14 14:49:59  perev
// Communication via descriptor struct -> class
//
// Revision 1.28  2000/04/13 02:58:47  perev
// Method Save is added & default CintDB loaded if exists
//
// Revision 1.27  2000/04/07 15:44:42  perev
// Error return when MySQL is not available
//
// Revision 1.26  2000/03/23 14:55:55  fine
// Adjusted to libSTAR and ROOT 2.24
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_db_Maker class for Makers                                      //
//                                                                      //
// This class is C++ implementation of the Begin_html <a href="http://www.rhic.bnl.gov/afs/rhic.bnl.gov/star/doc/www/packages_l/pro/pams/db/sdb/doc/index.html">Simple Database Manager</a> End_html    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
