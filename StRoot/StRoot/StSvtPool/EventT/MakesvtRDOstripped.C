#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include <string.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TString.h"
#include "TObjString.h"
#include "tables/St_svtRDOstripped_Table.h"

#endif
//#define DEBUG
class svtRDOstripped_st;
struct RDO_t {
  Char_t *name;
  Int_t ladNum, barNum;
  Char_t *rdo;
  Int_t ndet;
};
const Double_t DeltahVolt = 5;
Int_t nCFW = 0;
Int_t nCFWFault = 0;
Char_t tags[72][14];
static const Int_t NRDOS = 72;
static const RDO_t RDOS[72] = {
  {"L01B1E", 1,1, "E1", 2},{"L02B1E", 2,1, "E2", 2},{"L03B1E", 3,1, "E4", 2},{"L04B1E", 4,1, "E5", 2},{"L05B1E", 5,1, "E7", 2},
  {"L06B1E", 6,1, "E8", 2},{"L07B1E", 7,1,"E10", 2},{"L08B1E", 8,1,"E11", 2},
  {"L01B2E", 1,2, "E3", 2},{"L02B2E", 2,2, "E3", 3},
  {"L03B2E", 3,2, "E3", 3},{"L04B2E", 4,2, "E6", 3},{"L05B2E", 5,2, "E6", 3},{"L06B2E", 6,2, "E6", 3},{"L07B2E", 7,2, "E9", 3},
  {"L08B2E", 8,2, "E9", 3},{"L09B2E", 9,2, "E9", 3},{"L10B2E",10,2,"E12", 3},{"L11B2E",11,2,"E12", 3},{"L12B2E",12,2,"E12", 3},
  
  {"L01B3E", 1,3, "E1", 3},{"L02B3E", 2,3, "E1", 4},{"L03B3E", 3,3, "E2", 3},{"L04B3E", 4,3, "E2", 4},{"L05B3E", 5,3, "E4", 3}, 
  {"L06B3E", 6,3, "E4", 4},{"L07B3E", 7,3, "E5", 3},{"L08B3E", 8,3, "E5", 4},{"L09B3E", 9,3, "E7", 3},{"L10B3E",10,3, "E7", 4}, 
  {"L11B3E",11,3, "E8", 3},{"L12B3E",12,3, "E8", 4},{"L13B3E",13,3,"E10", 3},{"L14B3E",14,3,"E10", 4},{"L15B3E",15,3,"E11", 3},
  {"L16B3E",16,3,"E11", 4}, 	   	    
  
  {"L01B1W", 1,1, "W1", 4},{"L02B1W", 2,1, "W2", 4},{"L03B1W", 3,1, "W4", 4},{"L04B1W", 4,1, "W5", 4},{"L05B1W", 5,1, "W7", 4},
  {"L06B1W", 6,1, "W8", 4},{"L07B1W", 7,1,"W10", 4},{"L08B1W", 8,1,"W11", 4},
  {"L01B2W", 1,2, "W3", 4},{"L02B2W", 2,2, "W3", 6},
  {"L03B2W", 3,2, "W3", 6},{"L04B2W", 4,2, "W6", 6},{"L05B2W", 5,2, "W6", 6},{"L06B2W", 6,2, "W6", 6},{"L07B2W", 7,2, "W9", 6},
  {"L08B2W", 8,2, "W9", 6},{"L09B2W", 9,2, "W9", 6},{"L10B2W",10,2,"W12", 6},{"L11B2W",11,2,"W12", 6},{"L12B2W",12,2,"W12", 6},
  
  {"L01B3W", 1,3, "W1", 7},{"L02B3W", 2,3, "W1", 7},{"L03B3W", 3,3, "W2", 7},{"L04B3W", 4,3, "W2", 7},{"L05B3W", 5,3, "W4", 7}, 
  {"L06B3W", 6,3, "W4", 7},{"L07B3W", 7,3, "W5", 7},{"L08B3W", 8,3, "W5", 7},{"L09B3W", 9,3, "W7", 7},{"L10B3W",10,3, "W7", 7}, 
  {"L11B3W",11,3, "W8", 7},{"L12B3W",12,3, "W8", 7},{"L13B3W",13,3,"W10", 7},{"L14B3W",14,3,"W10", 7},{"L15B3W",15,3,"W11", 7},
  {"L16B3W",16,3,"W11", 7} 	   	    
};
//________________________________________________________________________________
Int_t CheckRows(svtRDOstripped_st *rows) {
  Int_t iok = 0;
  for (Int_t i = 0; i < NRDOS; i++, rows++) {  
    if (! rows->lvFault) iok++;
  }
  return iok;
}
//________________________________________________________________________________
void WritesvtRDOstripped(svtRDOstripped_st *rows, Int_t date, Int_t time) {
  TString fOut =  Form("svtRDOstripped.%8i.%06i.C",date,time);
  if (! CheckRows(rows)) {
    fOut += ".Fault";
    if (nCFWFault) fOut += Form("_%i",nCFWFault);
    nCFWFault++;
  } else nCFWFault = 0;
  ofstream out; 
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_svtRDOstripped\")) return 0;" << endl;
  out << "  svtRDOstripped_st row[" << NRDOS << "] = {" << endl; 
  for (Int_t i = 0; i < NRDOS; i++, rows++) {  
    out << "\t{" << Form("%1i,%2i,\"%s\",\t",rows->barNum,rows->ladNum, rows->rdo); 
    out << Form("%13.7f, %13.7f, %13.7f,%13.7f, %13.7f", rows->northTemp, rows->southTemp,rows->hvBoardTemp,rows->hvVolt,rows->hvCurr);
    out << Form(", %2i, %8i, %6i, %8i, %6i}" ,rows->lvFault, rows->date, rows->time, rows->dateOff, rows->timeOff); 
    if (i != NRDOS - 1) out << ","; 
    if (tags[i]) out << "// " << tags[i];
    out << endl; 
  } 
  out << "  };" << endl; 
  out << "  St_svtRDOstripped *tableSet = new St_svtRDOstripped(\"svtRDOstripped\"," << NRDOS << ");" << endl;  
  out << "  for (Int_t i = 0; i < " << NRDOS << "; i++) tableSet->AddAt(&row[i].barNum, i);" << endl; 
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl; 
  out.close();  
  nCFW++;
}
//________________________________________________________________________________
void MakesvtRDOstripped(Char_t *FileName="./svtRDOs.txt") {
  /* file svtRDOs.txt is created by
 mysql Conditions_svt -h onldb.starp.bnl.gov -P 3502 \
 -e 'select beginTime,flavor,deactive,barNum,ladNum,rdo,northTemp,southTemp,hvBoardTemp,hvVolt,hvCurr,lvFault from Conditions_svt.svtRDOs where deactive=0 and beginTime > "2007-01-01"' > svtRDOs.txt
  */  
  ///afs/rhic.bnl.gov/star/users/fisyak/.dev/DB/svtRDOs.txtdb01"
  Int_t Year = 2007;
  gSystem->Load("libStDb_Tables.so");
  FILE *fp = fopen(FileName,"r");
  if (! fp) return;
  svtRDOstripped_st rows[72];
  memset(rows, 0, NRDOS*sizeof(svtRDOstripped_st));
  memset(tags, 0, 72*14);
  for (Int_t i = 0; i < NRDOS; i++) {
    rows[i].barNum  = RDOS[i].barNum;
    rows[i].ladNum  = RDOS[i].ladNum;
    strcpy(rows[i].rdo,RDOS[i].rdo);
    rows[i].lvFault = -1;
    //    cout << i << "\tB" <<  rows[i].barNum << "\tL" << rows[i].ladNum << "\t" << rows[i].rdo << endl;
  }
  svtRDOstripped_st row;
  Int_t year, month, day;
  Int_t hour, mins, secs;
  Int_t date, time, dateOld = 0, timeOld = 0;
  Double_t datimeOld = 0;
  char flavor[3];
  Int_t deactive;
  char line[121];
  
  fgets(&line[0],120,fp); // skip the first line
  Int_t nRowsModified = 0;
  while (fgets(&line[0],120,fp)) {
#ifdef DEBUG
    if (nCFW > 100) break;
#endif
    memset (&row, 0, sizeof(svtRDOstripped_st));
    
    //    sscanf(&line[0],"%8d.%06d %s %d %d %d %s %f %f %f %f %f %d", 
    //	   &date,&time,
    /*
beginTime	flavor	deactive	barNum	ladNum	rdo	hvBoardTemp	hvVolt    	hvCurr  	lvFault
2007-03-24 04:37:23	ofl	0	1	1	W1	0.00000000	0.10000000	-0.89999998	0
     */
    sscanf(&line[0],"%4d-%2d-%2d  %02d:%02d:%02d %s %d %d %d %s %f %f %f %f %f %d", 
	   &year,&month,&day, &hour,&mins,&secs,
	   flavor, &deactive,
	   &row.barNum,&row.ladNum,row.rdo,&row.northTemp,
	   &row.southTemp,&row.hvBoardTemp,&row.hvVolt,&row.hvCurr,&row.lvFault);
#ifdef DEBUG
    printf("%4i-%02i-%02i  %02i:%02i:%02i %s %i %i %i %s %f %f %f %f %f %i\n", 
 	   year,month,day, hour,mins,secs,
 	   flavor, deactive,
 	   row.barNum,row.ladNum,row.rdo,row.northTemp,
 	   row.southTemp,row.hvBoardTemp,row.hvVolt,row.hvCurr,row.lvFault);
#endif
    date = day + 100*(month + 100*year);
    time = secs + 100*(mins + 100*hour);
#ifdef DEBUG
    cout << line;
    printf("%8d.%06d %s %d %d %d %s %f %f %f %f %f %d", 
	   date,time,
	   flavor, deactive,
	   row.barNum,row.ladNum,row.rdo,row.northTemp,
	   row.southTemp,row.hvBoardTemp,row.hvVolt,row.hvCurr,row.lvFault);
#endif
    TString Ofl(flavor);
    if (Ofl != "ofl") continue;
    if (year !=  Year) {
      cout << "Wrong date: " << line;
      continue;
    }
    Double_t datime = date + ((Double_t) time)/1000000.;
    if (TMath::Abs(row.hvVolt+1500) >= DeltahVolt)  row.hvVolt = 0;   
    if (datimeOld > 0  && datime < datimeOld) {
      cout << "incosistent date time new " << date << "/" << time  << " and old " << dateOld << "/" << timeOld  << endl;
      cout << date << "\t" << time << "\t"
	   << row.barNum << "\t" << row.ladNum << "\t" << row.rdo << "\t" << row.northTemp << "\t" 
	   << row.southTemp << "\t" << row.hvBoardTemp << "\t" << row.hvVolt << "\t" << row.hvCurr << "\t" 
	   << row.lvFault << endl;
      //      cout << Form("Read %i-%i-%i  %i:%i:%i",year,month,day,hour,mins,secs) << endl;
      cout << Form("Read %8d.%06d",date,time) << endl;
    }
    if (row.barNum < 1 || row.barNum >  3 ||
	row.ladNum < 1 || row.ladNum > 16) {
      cout << line;
      cout << date << "\t" << time << endl;
      cout << "Wrong barel or ladder number "
	   << row.barNum << "\t" << row.ladNum << "\t" << row.rdo << "\t" << row.northTemp << "\t" 
	   << row.southTemp << "\t" << row.hvBoardTemp << "\t" << row.hvVolt << "\t" << row.hvCurr << "\t" 
	   << row.lvFault << endl;
      return;
    }
    if (date != dateOld || time != timeOld) { // new beginTime
      if (nRowsModified > 10) {
	// Create Cint Table
	WritesvtRDOstripped(rows,dateOld,timeOld);
	nRowsModified = 0;
      }
      dateOld = date; timeOld = time;
      datimeOld = dateOld + ((Double_t) timeOld)/1000000.;
    }
    // Find row
    Int_t irow = -1;
    for (Int_t i = 0; i <= NRDOS; i++) {
      if (row.barNum != rows[i].barNum ||
	  row.ladNum != rows[i].ladNum ||
	  strcmp(row.rdo,rows[i].rdo)) continue;
      irow = i;
      if ( rows[i].lvFault == -1 && row.lvFault) {break;}
      if ( rows[i].lvFault ==  row.lvFault && TMath::Abs(rows[i].hvVolt - row.hvVolt) < DeltahVolt) {break;}
      
      //	rows[i] = row;
      if ( TMath::Abs(rows[i].hvVolt - row.hvVolt) >= DeltahVolt) {
	if (TMath::Abs(rows[i].hvVolt + 1500) <  DeltahVolt && 
	    TMath::Abs(    row.hvVolt + 1500) >= DeltahVolt) {
	  rows[i].dateOff = date;
	  rows[i].timeOff = time;
	} else {
	  rows[i].date = date;
	  rows[i].time = time;
	}
	nRowsModified++;
      }
      if (TMath::Abs(rows[i].hvVolt + 1500) <  DeltahVolt && rows[i].lvFault !=  row.lvFault) nRowsModified++;
      rows[i].northTemp   = row.northTemp;
      rows[i].southTemp   = row.southTemp;
      rows[i].hvBoardTemp = row.hvBoardTemp;
      rows[i].hvVolt      = row.hvVolt;
      rows[i].hvCurr      = row.hvCurr;
      rows[i].lvFault     = row.lvFault;
      strcpy(tags[i],flavor);
      break;
    }
    if (irow < 0) {
      cout << line;
      cout << date << "\t" << time << endl;
      cout << "Match has not been found for "
	   << row.barNum << "\t" << row.ladNum << "\t" << row.rdo << "\t" << row.northTemp << "\t" 
	   << row.southTemp << "\t" << row.hvBoardTemp << "\t" << row.hvVolt << "\t" << row.hvCurr << "\t" 
	   << row.lvFault << endl;
      return;
    }
  }   
  if (nRowsModified > 0) WritesvtRDOstripped(rows,dateOld,timeOld);
  fclose(fp);
}
