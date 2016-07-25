/*
  root.exe sqltpcAnodeCurrent.C+
 */
#ifndef __CINT__
#include <assert.h>
#include <string.h>
#include "Riostream.h"
#include "TSQLServer.h"
#include "TSQLResult.h"
#include "TSQLRow.h"
#include "TStopwatch.h"
#include "TString.h"
#include "TFile.h"
#include "TTree.h"
#include "StarRoot/StMemStat.h"
#endif
class Lecroy_t : public TNamed {
public:
  Lecroy_t() : TNamed("","") {}
  void Clear(const Option_t* /* opt */ ="") {memset(Beg, 0, End-Beg+1);}
  Char_t  Beg[1];//!
  Int_t   time;
  Int_t   io; 
  Int_t   module;
  Int_t   channel;
  Int_t   sector;  // [96];
  Int_t   row;     //[96]; // ;
  Float_t Voltage;//s [96]; // [12][8]
  Float_t Current;//s[96]; // [12][8]
  Int_t   Status;  //[96]; // [12][8]
  Int_t   Reason;
  Char_t  End[1];  //!
  //  ClassDef(Lecroy_t,1)
};
//ClassImp(Lecroy_t);
//________________________________________________________________________________
Int_t sector(Int_t lecroy = 0, Int_t channel = 0) {
  return 1 + 2*lecroy + channel/4;
}
//________________________________________________________________________________
Int_t channel(Int_t row = 1) {
  //                     1  2  3  4  5  7  8  9 10 
  Int_t channels[45] = { 0, 0, 1, 1, 1, 1, 2, 2, 3, //  0
			 3, 3, 3, 0, 0, 0, 0, 0, 0, // 10
			 0, 1, 1, 1, 1, 1, 1, 1, 2, // 20 
			 2, 2, 2, 2, 2, 2, 3, 3, 3, // 30
			 3, 3, 3, 3, 3};            // 40
  Int_t ch = -1;
  if (row >= 1 && row <= 45) ch = channels[row-1];
  return ch;
}
//________________________________________________________________________________
Int_t rowI(Int_t chan){
  Int_t rows[4] = {1, 3, 8, 10};
  Int_t row = -1;
  if (chan >=0 && chan <=3) row = rows[chan];
  return row;
}
//________________________________________________________________________________
Int_t rowO(Int_t chan){
  Int_t rows[4] = {14, 22, 30, 38};
  Int_t row = -1;
  if (chan >=0 && chan <=3) row = rows[chan];
  return row;
}
//________________________________________________________________________________
Int_t ParseString (const Char_t *string, TObjArray &Opt) {
  Opt.Clear();
  TString tChain(string);
  TObjArray *obj = tChain.Tokenize("[^ ;,]+");
  Opt = *obj;
  Int_t nParsed = Opt.GetEntries();
#if 0
  for (Int_t k = 0; k < nParsed; k++) {
    if (Opt.At(k)) {
      cout << k << "\t" << ((TObjString *) Opt.At(k))->GetName() << endl;
    }
  }
#endif
  return nParsed;
}
//________________________________________________________________________________
Int_t readF(const Char_t *string, Float_t *array) {
  TObjArray Opt;
  Int_t N = ParseString(string,Opt);
  for (Int_t k = 0; k < N; k++) {
    array[k] = TString(((TObjString *) Opt.At(k))->GetName()).Atof();
  }
  return N;
}
//________________________________________________________________________________
Int_t readI(const Char_t *string, Int_t *array) {
  TObjArray Opt;
  Int_t N = ParseString(string,Opt);
  for (Int_t k = 0; k < N; k++) {
    array[k] = TString(((TObjString *) Opt.At(k))->GetName()).Atoi();
  }
  return N;
}
//_____________________________________________________________________________
static void doPs(const Char_t *who, const Char_t *where)
{
  static const Char_t *ps = "yes";
  printf("doPs for %20s:%12s \t",who,where);
  StMemStat::PrintMem(0);
  //  printf("\n");
}
#include "OnDb.h"
//________________________________________________________________________________
void sqltpcAnodeCurrent(Int_t year = 2014, Int_t n1 = 0, Int_t n2 = 100000) {
  //  TSQLServer *db = TSQLServer::Connect("mysql://onldb2.starp.bnl.gov:3502/Conditions_daq","", "");
  TString database = OnDb(year);
  TString server("mysql://"); server += database; server += "/Conditions_daq";
  TSQLServer *db = TSQLServer::Connect(server.Data(),"", "");
  if (! db) {cout << "server " << server.Data() << " is not found" << endl; return;}
  printf("Server info: %s\n", db->ServerInfo());
 // Book TTree
  TFile *fOut = new TFile(Form("tpcAnode%4i_%i_%i.root",year,n1,n2),"recreate");
  TTree *ftree = new TTree("FitP","Lecroy tree");
  ftree->SetAutoSave(1000000);  // autosave when 1 Mbyte written
  Int_t bufsize = 64000;
  Int_t split = 99;
  if (split)  bufsize /= 4;
  Lecroy_t lecroy;
  Lecroy_t *flecroy = &lecroy;
  TTree::SetBranchStyle(1); //new style by default
  ftree->Branch("LecroyB", "Lecroy_t", &flecroy, bufsize, split);
  
  TSQLRow *row;
  TSQLResult *res;
   // start timer
   TStopwatch timer;
   timer.Start();
   const Char_t *IO[2] = {"Inner","Outer"};
   for (Int_t io = 0; io < 2; io++) {
     TString Sql(Form("select UNIX_TIMESTAMP(beginTime),Voltages,Currents,Status,Reason from Conditions_daq.tpcPowerSupply%s "
		      "WHERE beginTime > \"%4i-01-03\"",IO[io],year));
     //		      "WHERE beginTime > \"2012-01-03\"",IO[io]));
     cout << Sql.Data() << endl;
     res = db->Query(Sql.Data());
     
     int nrows = res->GetRowCount();
     printf("\nGot %d rows in result\n", nrows);
   
     int nfields = res->GetFieldCount();
     TString names[nfields];
     for (int i = 0; i < nfields; i++) {
       names[i] = res->GetFieldName(i);
       printf("%20s", names[i].Data());
     }
     printf("\n");
     for (int i = 0; i < nfields*40; i++)
       printf("=");
     printf("\n");
     Int_t time, Status[96], Reason;
     Float_t Voltages[96], Currents[96];
     
     for (int i = 0; i < nrows; i++) {
       row = res->Next();
       if (i < n1) {delete row; continue;}
       if (i > n2) {delete row; break;}
       Int_t N = 0;
       lecroy.Clear();
       lecroy.io = io;
       for (int j = 0; j < nfields; j++) {
	 //	 printf("%20s = %s\n", names[j].Data(), row->GetField(j));
	 switch (j) {
	 case 0: N = readI(row->GetField(j), &time);     assert(N ==  1); break;
	 case 1: N = readF(row->GetField(j),  Voltages); assert(N == 96); break;
	 case 2: N = readF(row->GetField(j),  Currents); assert(N == 96); break;
	 case 3: N = readI(row->GetField(j),  Status);   assert(N == 96); break;
	 case 4: N = readI(row->GetField(j), &Reason);   assert(N ==  1); break;
	 default: break;
	 }
       }
       delete row;
       if (! (i%1000)) doPs("Case",Form("%i",i));
       if (Reason != 1) continue;
       for (Int_t j = 0; j < 96; j++) {
	 if (Currents[j] < 0.02) Currents[j] = 0;
	 lecroy.time    = time;
	 lecroy.channel = i%8;
	 lecroy.module  = i/8;
	 lecroy.sector  = sector(lecroy.module,lecroy.channel);
	 lecroy.row     = (io == 0) ? rowI(lecroy.channel) : rowO(lecroy.channel);
	 lecroy.Voltage = Voltages[j];
	 lecroy.Current = Currents[j];
	 lecroy.Status  = Status[j];
	 lecroy.Reason  = Reason;
	 ftree->Fill();
       }
     }
     
     delete res;
   }
   delete db;

   // stop timer and print results
   timer.Stop();
   Double_t rtime = timer.RealTime();
   Double_t ctime = timer.CpuTime();

   printf("\nRealTime=%f seconds, CpuTime=%f seconds\n", rtime, ctime);
   fOut->Write();
}
/*
20 wires per channel / card
Subsector  Row  Radius   Pads  Pad 1    Pad 2	 Anode wires  Anode-wire    Notes (Lecroy Card slot#.channel# => (l.c)) 
                   (cm)        fraction fraction              termination
Inner        1   60.000   88   0.778    1.000       17 -  19  Card 1        1  (0.0)                         sector 1 + 2*l + c/4
"            2   64.800   96   0.652   	"     	    29 -  31  Card 2        2
"    	     3   69.600  104   0.499  	"     	    41 -  43  Card 3        "  (0.1)
"    	     4   74.400  112   0.347    0.992 	    53 -  54  "       	    "   
"    	     5   79.200  118   0.957  	1.000 	    65 -  67  Card 4        "   
"      	     6   84.000  126   0.893    "     	    77 -  79  "       	    "   
"    	     7   88.800  134   0.802  	"     	    89 -  91  Card 5        "  (0.2)
"    	     8   93.600  142   0.683  	"     	   101 - 103  Card 6        "
"    	     9   98.800  150   0.801    "     	   114 - 116  "       	    "   
"    	    10  104.000  158   0.892  	"     	   127 - 129  Card 7        "  (0.3)
"    	    11  109.200  166   0.955  	"     	   140 - 142  Cards 7, 8    " 
"    	    12  114.400  174   0.991  	"     	   153 - 155  Card 8        "     
"    	    13  119.600  182   1.000  	"     	   166 - 168  LOAB/ISOR     
 
Outer       14  127.195   98   0.764    1.000       10 -  14  Card 9        2  (0.0)	          
"           15  129.195  100   0.583  	"      	    15 -  19  "  "		 	  
"           16  131.195  102   0.383  	"      	    20 -  24  "  "		 	  
"     	    17  133.195  104   0.210  	0.973  	    25 -  29  Cards 9, 10   "          
"           18  135.195  106   0.089    0.894  	    30 -  34  Card 10       "	    	  
"           19  137.195  "     0.764  	1.000  	    35 -  39  "             "		 	  
"           20  139.195  108   0.582  	"      	    40 -  44  "  	    "     	  
"           21  141.195  110   0.382  	"      	    45 -  49  Cards 10, 11  "         
"           22  143.195  112   0.210    0.972  	    50 -  54  Card 11       "  (0.1)	    	  
"           23  145.195  "     0.893  	1.000  	    55 -  59  "             "		 	  
"           24  147.195  114   0.763  	"      	    60 -  64  "  	    "     	  
"           25  149.195  116   0.582  	"      	    65 -  69  Cards 11, 12  "         
"     	    26  151.195  118   0.382    "      	    70 -  74  Card 12       "	   	  
"     	    27  153.195  120   0.209  	0.972  	    75 -  79  "             "	    	 	  
"     	    28  155.195  122   0.088  	0.893  	    80 -  84  "  	    "	 	  
"     	    29  157.195  "     0.762    1.000  	    85 -  89  Cards 12, 13  "         
"     	    30  159.195  124   0.581    "      	    90 -  94  Card 13       "  (0.2)	   	  
"           31  161.195  126   0.381  	"      	    95 -  99  "             "		 	  
"     	    32  163.195  128   0.209  	0.972  	   100 - 104  "             "	    	 	  
"     	    33  165.195  "     0.893    1.000  	   105 - 109  Cards 13, 14  "         
"           34  167.195  130   0.762  	"      	   110 - 114  Card 14       "	          
"           35  169.195  132   0.580    "      	   115 - 119  "             "		      	  
"           36  171.195  134   0.380    "      	   120 - 124  "             "		      	  
"  "  	    37  173.195  136   0.208  	0.972  	   125 - 129  Cards 14, 15  "	  
"     	    38  175.195  138   0.088  	0.892  	   130 - 134  Card 15       "  (0.3)    	  
"     	    39  177.195  "     0.761  	1.000  	   135 - 139  "             "	    	 	  
"           40  179.195  140   0.579    "      	   140 - 144  "             "	              	  
"     	    41  181.195  142   0.379  	"      	   145 - 149  Cards 15, 16  "   	  
"     	    42  183.195  144   0.208  	0.972  	   150 - 154  Card 16       "    	  
"     	    43  185.195  "     0.892  	1.000  	   155 - 159  "             "	    	 	  
"           44  187.195  "     1.000    "      	   160 - 164  "             "		      	  
"     	    45  189.195  "      "       "      	   165 - 169  Card 16, LOAB/OSOR  2, 3

Notes:

1. MWPC socket 1 was unfilled during Summer 1999 running. In Fall 1999, a (modified FEE) 
grounding card will be installed, but the socket will eventually be instrumented.
2. The MWPC cards were not powered during the Summer 1999 test run, so their input 
impedance was not well-defined: the inputs were, effectively, partway between floating and ground.
3. Over row 45, the three innermost anode wires feed MWPC 16, while the outer two go to the 
LOAB. The next three wires also go to the LOAB, making edge effects, per se, negligible at 
the outer edge of the sector.

ISOR N = 10 (Inner sector, outer radius)
OSIR N =  7 (Outer sector, inner radius)
OSOR N = 5  (Outer sector, outer radisu)
 */
