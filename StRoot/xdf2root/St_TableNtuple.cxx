// $Id: St_TableNtuple.cxx,v 1.3 1999/02/18 00:25:52 genevb Exp $
// $Log: St_TableNtuple.cxx,v $
// Revision 1.3  1999/02/18 00:25:52  genevb
// St_TableNtuple: Histogramming ranges fixed, buffer size increased
//
// Revision 1.2  1999/02/17 22:54:13  genevb
// Fixed errors when no tables/datasets found in St_TableNtuple
//
//
// Revision 1.1 1999/01/27 10:28:29 genevb
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_TableNtuple                                                       //
//                                                                      //
// St_TableNtuple is a class to convert STAR Tables into ntuples        //
// The class inherits from TTree, so it can be used just as a           //
// TTree would (Draw(), etc.). Columns are not made for table           //
// entries which are not basic numerical entities. Table entries        //
// which are arrays are spread into multiple columns with names         //
// given by the entry and index (e.g. chisq[3] => chisq0, chisq1,       //
// and chisq2). The class constructor defines the TTree and its         //
// TBranches. You must use the Fill() member function to fill the       //
// TTree from a table.                                                  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
// 
// Basically, this is a TTree with some add-ons. It is very similar to a TNtuple
// except that it requires using tables to fill it, and entries are not forced
// to be floats. Here's how to use it...
// 
// The constructor must be called with an St_Table-derived table.
// Here for example, I'll use the St_tpt_track table. You don't
// have to do the first line if you already have a table:
// 
// St_tpt_track track();
// St_TableNtuple t1(track);
// 
// Now t1 is essentially an ntuple with columns
// as defined by the table specified. However, columns are not included
// for table entries which are not numbers (int, short, long, float, etc
// are allowed). Table entries which are arrays to begin with are spread out
// into additional columns with names identical to the original table element
// name with an index at the end (e.g. array element chisq[3] => chisq0, chisq1,
// chisq2). Now you are ready to fill the ntuple, and there's two
// ways to do it. If you have a specific table, just call:
// 
// t1.Fill(track,firstRow,nRows);
// 
// Here, firstRow [default=0] is the first row you'd like to add from the
// table, and nRows [default=-1] is the number of rows you'd like to add
// (nRows<0 goes to end-of-table). If you just do Fill(track) it will
// read the whole table.
// 
// If you have a root file with tables for many events, you can call:
// 
// t1.AddTFile("filename","datasetname","tablename",firstEvent,nEvents);
// or
// TFile f("filename");
// t1.AddTFile(f,"datasetname","tablename",firstEvent,nEvents);
// 
// (AddXDFFile exists for adding tables from XDF files (class St_XDFFile).)
// 
// You can specify a certain number of events with nEvents [default=-1]
// and a first event with firstEvent [default=1], or leave them
// off and all the events on file will be added (nEvents < 0 goes to
// end-of-file). An example with the tpt_track table would be:
// 
// t1.AddTFile(f,"tpc_data","track");
// 
// Once filled, you can use t1 just like a regular TTree or TNtuple:
// 
// t1.Draw("var1:var2","condition1 && condition2");
// etc.
//
#include <iostream.h>
#include "TROOT.h"
#include "St_TableNtuple.h"
#include "St_Table.h"
#include "TClass.h"
#include "TDataMember.h"
#include "TDataType.h"
#include "TFile.h"
#include "St_XDFFile.h"
#include "TKey.h"
#include "TBranch.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

ClassImp(St_TableNtuple)

//_____________________________________________________________________________
St_TableNtuple::St_TableNtuple() : TTree() {
//
// Default constructor for St_TableNtuple()
//
  mNvar = 0;
  mArgs = 0;
  mClassPtr = 0;
  mType = 0;
  mOffset = 0;
  mTableClass = "";
}
//_____________________________________________________________________________
St_TableNtuple::St_TableNtuple(const St_Table &table, Int_t bufsize) :
  TTree(table.GetName(),table.GetTitle()) {
//
// St_TableNtuple() constructs a TTree as an ntuple with columns as determined
// by a table.
//
  mNvar = 0;
  mArgs = 0;
  mClassPtr = 0;
  mType = 0;
  mOffset = 0;
  mTableClass = "";
  LearnTable(table,kTRUE,bufsize);
  PrintInfo();
}
//_____________________________________________________________________________
St_TableNtuple::~St_TableNtuple() {
}
//_____________________________________________________________________________
Int_t St_TableNtuple::AddTFile(const Char_t *file, Char_t *dataset, Char_t *tname, Int_t firstEvent, Int_t nEvents) {
//
// AddTFile() allows a specific table to be read from a TFile.
//  file       - the name of the TFile to read
//  dataset    - name of the dataset in which the table is to be found
//  tname      - name of the table to put in the ntuple
//  firstEvent - first event to read from the file
//               (first event on a TFile is usually event # 0)
//  nEvents    - number of events to read from the file
//               -1 events (default) means read the entire file
// Number of events actually read is returned
//
  TFile f(file);
  return AddTFile(f,dataset,tname,firstEvent,nEvents);
}
//_____________________________________________________________________________
Int_t St_TableNtuple::AddTFile(TFile &f, Char_t *dataset, Char_t *tname, Int_t firstEvent, Int_t nEvents) {
//
// AddTFile() allows a specific table to be read from a TFile.
//  f          - a pointer to the TFile to read
//  dataset    - name of the dataset in which the table is to be found
//  tname      - name of the table to put in the ntuple
//  firstEvent - first event to read from the file
//               (first event on a TFile is usually event # 0)
//  nEvents    - number of events to read from the file
//               -1 events (default) means read the entire file
// Number of events actually read is returned
//
  St_DataSet *set=0;
  Int_t keys = f.GetNkeys();
  if (firstEvent > keys) return 0;
  Int_t stopEvent = firstEvent + nEvents;
  if ((stopEvent > ++keys) || (nEvents < 0)) {
    nEvents = keys - firstEvent;
    stopEvent = keys;
  }

  St_Table *table=0;
  Int_t j=0;
  for (Int_t i=firstEvent; i<stopEvent; i++) {
    printf("Adding event %d to ntuple.\n",i);
    if (set = (St_DataSet *) f.GetKey(dataset,i)->ReadObj()) {
      St_DataSetIter iter(set);
      if (table = (St_Table *) iter(tname)) {
        Fill(*table);
        j++;
      } else {
        printf("St_TableNtuple Warning: table %s not found in dataset %s.\n",tname,dataset);
      }
    } else {
      printf("St_TableNtuple Warning: dataset %s not found in event.\n",dataset);
    }
    delete set; set=0;
  }

  return j;
}
//_____________________________________________________________________________
Int_t St_TableNtuple::AddXDFFile(const Char_t *file, Char_t *dataset, Char_t *tname, Int_t firstEvent, Int_t nEvents) {
//
// AddXDFFile() allows a specific table to be read from an XDF file.
//  file       - the name of the XDF file to read
//  dataset    - name of the dataset in which the table is to be found
//  tname      - name of the table to put in the ntuple
//  firstEvent - first event to read from the file
//               (first event on an XDF file is usually event # 1)
//  nEvents    - number of events to read from the file
//               -1 events (default) means read the entire file
// Number of events actually read is returned
//
  St_XDFFile f(file);
  return AddXDFFile(f,dataset,tname,firstEvent,nEvents);
  delete &f;
}
//_____________________________________________________________________________
Int_t St_TableNtuple::AddXDFFile(St_XDFFile &f, Char_t *dataset, Char_t *tname, Int_t firstEvent, Int_t nEvents) {
//
// AddXDFFile() allows a specific table to be read from an XDF file.
//  f          - a pointer to the XDF file to read
//  dataset    - name of the dataset in which the table is to be found
//  tname      - name of the table to put in the ntuple
//  firstEvent - first event to read from the file
//               (first event on an XDF file is usually event # 1)
//  nEvents    - number of events to read from the file
//               -1 events (default) means read the entire file
// Number of events actually read is returned
//
  St_DataSet *event=0;

  for (Int_t i=0; i<(firstEvent-1); i++) {
    if (!(event = f.NextEventGet())) return 0;
  }
    
  i=0;
  Int_t j=0;
  St_DataSet *set=0;
  St_Table *table=0;
  while ((event = f.NextEventGet()) && ((i < nEvents) || (nEvents < 0))) {
    printf("Adding event %d to ntuple.\n",(firstEvent+(i++)));
    St_DataSetIter event_iter(event);
    if (set = event_iter(dataset)) {
      St_DataSetIter iter(set);
      if (table = (St_Table *) iter(tname)) {
        Fill(*table);
        j++;
      } else {
        printf("St_TableNtuple Warning: table %s not found in dataset %s.\n",tname,dataset);
      }
      set=0;
    } else {
      printf("St_TableNtuple Warning: dataset %s not found in event.\n",dataset);
    }
    delete event; event=0;
  }

  return j;
}
//_____________________________________________________________________________
void St_TableNtuple::Browse(TBrowser *b) {
//
// Browse() starts a browser for the class
//
  fLeaves.Browse(b);
}
//_____________________________________________________________________________
Int_t St_TableNtuple::Fill(const St_Table &table, Int_t firstRow, Int_t nRows) {
//
// Fill() fills the ntuple with the contents of the specified table
//  table    - name of the table to add
//  firstRow - first row to add from the table
//  nRows    - number of rows to add from the table
//                  -1 rows (default) means add the entire table
// Number of rows actually added is returned
//
  Int_t j;
  Int_t i;
  void *pointer;
  Int_t thisRow;
  Int_t rowSize = (Int_t) table.GetRowSize();
  Int_t tRows = table.GetNRows();

// Check for knowledge of table
  if (!mClassPtr) LearnTable(table);

// Check for table compatibility
  if (!mClassPtr || (mClassPtr != table.GetRowClass())) {
    printf("St_TableNtuple Error: Table incompatible with constructed St_TableNtuple.\n");
    return 0;
  }
  if (firstRow > (tRows-1)) return 0;
  if (((firstRow + nRows) > tRows) || (nRows < 0)) {
    nRows = tRows - firstRow;
  }

// Loop over table rows
  thisRow = ((Int_t) table.GetArray()) + (firstRow * rowSize);
  for (i=0; i<nRows; i++) {
    for (j=0; j<mNvar; j++) {
      pointer = thisRow + mOffset[j];
      switch (mType[j]) {
        case kFloat :{ Float_t *pF1  =  (Float_t *) mArgs[j];
                       Float_t *pF2  =  (Float_t *) pointer;
                       *pF1 = *pF2; break;}
        case kLong  :{ }
        case kInt   :{ Int_t *pI1    =    (Int_t *) mArgs[j];
                       Int_t *pI2    =    (Int_t *) pointer;
                       *pI1 = *pI2; break;}
        case kShort :{ Short_t *pS1  =  (Short_t *) mArgs[j];
                       Short_t *pS2  =  (Short_t *) pointer;
                       *pS1 = *pS2; break;}
        case kUShort:{ UShort_t *ps1 = (UShort_t *) mArgs[j];
                       UShort_t *ps2 = (UShort_t *) pointer;
                       *ps1 = *ps2; break;}
        case kDouble:{ Double_t *pD1 = (Double_t *) mArgs[j];
                       Double_t *pD2 = (Double_t *) pointer;
                       *pD1 = *pD2; break;}
        case kULong :{ }
        case kUInt  :{ UInt_t *pi1   =   (UInt_t *) mArgs[j];
                       UInt_t *pi2   =   (UInt_t *) pointer;
                       *pi1 = *pi2; break;}
        default     :{ cout << "*** Warning: Fill() tried to add invalid data type." << endl; }
      }
    }
    TTree::Fill();
    thisRow += rowSize;
  }
  TTree::SetEstimate(TTree::GetEntries());
  return nRows;
}
//_____________________________________________________________________________
void St_TableNtuple::LearnTable(const St_Table &table, Bool_t buildTree, Int_t bufsize) {
//
// LearnTable() allows the St_TableNtuple to learn the structure of the
// tables used to fill the ntuple.
//  table     - the name of the table
//  buildTree - if kTRUE, then add TBranches to the TTree for each table
//              column (default=kFALSE)
//
  mClassPtr = table.GetRowClass();
  if (!mClassPtr) return;
  if (buildTree)
    mTableClass = mClassPtr->GetName();
  else if (mTableClass != mClassPtr->GetName()) {
    mClassPtr = 0;
    return;
  }

  if (!mClassPtr->GetListOfRealData()) mClassPtr->BuildRealData();
  if (!(mClassPtr->GetNdata())) return;
  Int_t rowSize = table.GetRowSize();

  mArgs = new void*[rowSize];
  mType = new NumType[rowSize];
  mOffset = new Int_t[rowSize];
  Int_t *pvars = new Int_t[(rowSize+1)];
  Char_t **ty = new Char_t*[rowSize];
  Char_t *types;
  Char_t *varname;
  Char_t varlist[1000];
  Char_t vartemp[80];
  Char_t *tempvar;
  static Char_t *colon=":";
  Int_t i;
  Int_t pv;
  Int_t memberDim;
  Int_t memberSize;
  Int_t count = 0;

  pvars[0] = 0;
  TIter next(mClassPtr->GetListOfDataMembers());
  next.Reset();
  TDataMember *member = 0;
  while (member = (TDataMember *) next()) {
    varname = (Char_t *) member->GetName();   
    TDataType *memberType = member->GetDataType();
    types = memberType->GetTypeName();
    if (!strcmp("float", types)) {
      mType[count] = kFloat ; ty[count]="/F"; }
    else if (!strcmp("int", types)) {
      mType[count] = kInt   ; ty[count]="/I"; }
    else if (!strcmp("long", types)) {
      mType[count] = kLong  ; ty[count]="/I"; }
    else if (!strcmp("short", types)) {
      mType[count] = kShort ; ty[count]="/S"; }
    else if (!strcmp("double", types)) {
      mType[count] = kDouble; ty[count]="/D"; }
    else if (!strcmp("unsigned int", types)) {
      mType[count] = kUInt  ; ty[count]="/i"; }
    else if (!strcmp("unsigned long", types)) {
      mType[count] = kULong ; ty[count]="/i"; }
    else if (!strcmp("unsigned short", types)) {
      mType[count] = kUShort; ty[count]="/s"; }
    else {
      if (buildTree) {
        cout << "Not including table column " << varname;
        cout << ", type " << types << endl;
      }
      mType[count] = kNAN;
    }
    if (mType[count]) {
      if (memberDim = member->GetArrayDim()) {
        memberSize = memberType->Size();
        tempvar = varname;
        varname = vartemp;
      }
      mOffset[count] = member->GetOffset();
      for (i=0; i<(memberDim+1); i++) {
        if (memberDim) sprintf(vartemp,"%s%d",tempvar,i);
        if (i) {
          mOffset[count] = mOffset[count-1] + memberSize;
          mType[count] = mType[count-1]; ty[count] = ty[count-1];
        }
        mArgs[count] = memberType->Class()->New();
        pvars[count+1] = pvars[count] + 1 + strlen(varname);
        if (!count++) {
          strcpy(varlist,varname);
        } else {
          strcat(varlist,colon);
          strcat(varlist,varname);
        }
      }
    }
  }

  if (buildTree) mNvar = count;
  else if (mNvar != count) {
    mClassPtr = 0;
    return;
  }

  TBranch *branch;
  for (i=0; i<mNvar; i++) {
    pv = pvars[i+1] - 1;
    varlist[pv] = 0;
    pv = pvars[i];
    tempvar = &varlist[pv];
    if (buildTree) {
      strcpy(vartemp,tempvar);
      strcat(vartemp,ty[i]);
      TTree::Branch(tempvar,mArgs[i],vartemp,bufsize);
    } else {
      if (!(branch = TTree::GetBranch(tempvar))) {
        mClassPtr = 0;
        return;
      }
      branch->SetAddress(mArgs[i]);
    }
  }
}
//_____________________________________________________________________________
void St_TableNtuple::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: St_TableNtuple.cxx,v 1.3 1999/02/18 00:25:52 genevb Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("* Using %d columns from table with:\n",mNvar);
  printf("*   Name: %s\n",GetName());
  printf("*  Title: %s\n",GetTitle());
  printf("**************************************************************\n");
}
