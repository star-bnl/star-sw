//*-- Author :    Valery Fine   27/04/98
// $Id: St_XDFFile.cxx,v 1.32 1999/03/15 00:10:14 perev Exp $ 
// $Log: St_XDFFile.cxx,v $
// Revision 1.32  1999/03/15 00:10:14  perev
// For new Maker schema
//
// Revision 1.31  1999/03/11 01:29:53  perev
// New schema xdf2root
//
// Revision 1.30  1999/03/02 03:19:02  fine
// re-commit, the obsolte version was in use
//
// Revision 1.28  1999/02/20 22:15:40  fine
// Clean up to avoid g++ compilation warnings
//
// Revision 1.27  1999/01/21 20:55:07  fine
// Some comments have been added fro the new Browse method
//
// Revision 1.26  1999/01/21 18:12:47  fine
// Browse and dir methods have been introduced
//
// Revision 1.25  1999/01/21 02:46:36  fine
// New method dir and Browse to navigate XDF files
//
// Revision 1.24  1999/01/21 00:21:41  fine
// Some print statement have been introduced
//
// Revision 1.23  1999/01/02 21:28:57  fine
// St_XDFFile::MakeDataSet(DS_DATASET_T *ds): some protection against of the "wrong" tables
//
// Revision 1.22  1999/01/02 19:08:27  fisyak
// Add ctf
//
// Revision 1.21  1998/12/30 22:28:59  fine
// St_XDFFile Some protection against of the wrong tables has been introduced
//
// Revision 1.20  1998/12/19 02:54:04  fine
// ST_XDFFile::NextEventList() - fast scan of the XDF files method has been introduced
//
// Revision 1.19  1998/11/25 21:58:38  fisyak
// Cleanup
//
// Revision 1.18  1998/10/31 00:21:57  fisyak
// Add record counter
//
// Revision 1.17  1998/10/05 21:21:22  fine
//  #include <errno.h>
// fErrorCode data-member has been activated
//
// Revision 1.16  1998/09/21 23:50:47  fine
// Some cosmetic improvements
//
// Revision 1.15  1998/09/20 22:50:38  fine
// New method to read XDF file has been introduced
//
// Revision 1.14  1998/09/15 20:55:37  fisyak
// Split St_DataSet -> St_DataSet + St_DataSetIter
//
// Revision 1.13  1998/08/28 21:55:10  fine
// TSocket data-memebr and "socket" methods have been introduced to accept the XDF file
// over TCP/IP
//
// Revision 1.12  1998/08/26 12:15:23  fisyak
// Remove asu & dsl libraries
//
// Revision 1.11  1998/08/18 14:05:07  fisyak
// Add to bfc dst
//
// Revision 1.10  1998/08/14 16:49:42  fisyak
// reduce level of print out
//
// Revision 1.9  1998/08/10 02:33:09  fisyak
// Add St_fileSet
//
// Revision 1.8  1998/07/23 22:12:01  fisyak
// Recover after Correction for root 2.09
//
// Revision 1.7  1998/07/23 21:09:14  fisyak
// Adjust for ROOT 2.09
// 
//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//   St_XDFFile                                                             //
//                                                                          //
//   DESCRIPTION:  Create,Read/Write,Fill the C++ objects from XDF files    //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <errno.h>

#include "table_header.h"
#include "dsxdr.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TClass.h"
#include "TSocket.h"
#include "TBrowser.h"
#include "St_XDFFile.h"
#include "St_DataSetIter.h"
#include "St_DataSet.h"
#include "St_Table.h"

ClassImp(St_XDFFile)

//______________________________________________________________________________
St_XDFFile::St_XDFFile(){
  fName   = 0;
  fStream = 0;
  fFile   = 0;
  fDataSet= 0;
  fSocket = 0;
  fErrorCode=0;
  fRecordCount =0;
  fBrowsable = 0;
  fDebug=0;
}

//______________________________________________________________________________
St_XDFFile::St_XDFFile(const Char_t *filename,const Char_t *mode)
{
  //
  // Open the XDF file
  //
  //   Char_t *filename - name of the file to open
  //   Char_t *mode     - mode to open this file
  //                      "r"  - read only
  //                      "w"  - write mode
  //

  fName   = 0;
  fStream = 0;
  fFile   = 0;
  fDataSet= 0;
  fSocket = 0;
  fErrorCode=0;
  fRecordCount =0;
  fBrowsable = 0;
  fDebug=0;
  if (filename && strlen(filename)) {
     fStream = (XDR*) malloc(sizeof(XDR));
     OpenXDF(filename,mode);
     if (fFile) CreateXDFStream();
  }
}

//______________________________________________________________________________
St_XDFFile::~St_XDFFile(){
    if (fStream) {
      CloseXDF();
      free (fStream); 
      fStream = 0;
    } 
    SafeDelete(fSocket);
    SafeDelete(fBrowsable);
    if (fName) delete [] fName;
    fName = 0;
}
//______________________________________________________________________________
void St_XDFFile::Browse(TBrowser *b)
{
  // St_XDFFile::Browse(TBrowser *b)
  //
  // Reads the next XDF file record and inserts it into the current ROOT TBrowser
  //
  // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/XDFBrowser.gif"> </P> End_Html 
  //
  // Macro begin_html <a href="../examples/XDFBrowser.C.html"><i>XDFBrowser.C</i></a> end_html shows how this method can be used.
  //
  // Note:
  // ====
  //   For this case St_XDFFile::NextEventGet takes in account
  //   the usre's interactive action.
  //   This means the first call of St_XDFFile::NextEventGet 
  //   just after the user applied ROOT Browser to his/her XDF file
  //   will return the fBrowsable object and will read the next XDF
  //   record with the second call only.
  //
  //   It is GetSelected() method that return the fBrowsable only
  //
 
 
  if (!fFile) TObject::Browse(b);
  if (fBrowsable) {delete fBrowsable; fBrowsable = 0;}
  fBrowsable = NextEventGet();
  b->Add(fBrowsable);
}

//______________________________________________________________________________
void St_XDFFile::Delete(DS_DATASET_T *ds)
{
 //  Delete(DS_DATASET_T *ds)
 //
 //  Delete DS_DATASET_T structure recursively
 //
  DS_DATASET_T *dt;
  if (ds->tid) {
     ds->p.data  = 0;  // unlink table data 
     ds->elcount = 0;
  }
  else
    for (UInt_t j=0; j< ds->elcount; j++)
      if ( (dt=ds->p.link[j]) ) { 
            Delete(dt);
            ds->p.link[j] = dt;
      }
  if (ds) {
     if (ds->p.link) free (ds->p.link);
     ds->p.link  = 0;
     ds->elcount = 0;
     dsFreeDataset(ds); 
  }
  ds = 0;
}
//______________________________________________________________________________
Int_t St_XDFFile::OpenXDF(TInetAddress address, const char *service,const Char_t *mode)
{
  fSocket = new TSocket(address,service);
  fFile   = (FILE *)fSocket->GetDescriptor();
  fRecordCount =0;
  return  CreateXDFStream();
}
//______________________________________________________________________________
Int_t St_XDFFile::OpenXDF(TInetAddress address, Int_t port,const Char_t *mode)
{
   fSocket = new TSocket(address,port);
   fFile = (FILE *)fSocket->GetDescriptor();
   fRecordCount =0;
   return  CreateXDFStream();
}

//______________________________________________________________________________
Int_t St_XDFFile::OpenXDF(const char *host, Int_t port,const Char_t *mode)
{
  fSocket = new TSocket(host,port);
  fFile = (FILE *)fSocket->GetDescriptor();
  fRecordCount =0;
  return  CreateXDFStream();
}
//______________________________________________________________________________
Int_t St_XDFFile::OpenXDF(Int_t descriptor,const Char_t *mode)
{
  fSocket = new TSocket(descriptor);
  fFile = (FILE *)fSocket->GetDescriptor();
  fRecordCount =0;
  return  CreateXDFStream();
}

//______________________________________________________________________________
Int_t St_XDFFile::OpenXDF(const Char_t *filename,const Char_t *mode) 
{
  //
  // OpenXDF(Char_t *filename,Char_t *mode) 
  //
  //  open file <filename> with C "fopen" subroutine
  //
  //  filename - the name of the file to be opened
  //  mode     - the "C" mode this file should opened with
  //             this method append the "b" - binary option itself
  //             for any mode supplied
  //

  fMethodName = "OpenXDF(Char_t *filename,Char_t *mode)";
  if (!(filename || strlen(filename))) return 3;
  Char_t *expfile = gSystem->ExpandPathName(filename); 
  if (expfile) 
        fName =  expfile;

  strcpy(fType,"r"); 
  if (mode && mode[0] && mode[0]!=' ') strcpy(fType,mode);
  strcat(fType,"b");

  fErrorCode=0;
  fFile = fopen(fName,fType);
  
  fRecordCount =0;
  if(!fFile) {                    // open error 
    fErrorCode=errno;
    Printf("xdf_open. Error, can not open file %s %s\n",fName,fType);
    return 2;
  }
  return  CreateXDFStream();
}
//______________________________________________________________________________
Int_t St_XDFFile::CreateXDFStream(){

  if (!fFile) return 1;
  
  if(!dsNewDataset(&fDataSet,"NEVSKI")) {  
    printf("xdf_open. Error, can not create data set for file %s %s\n",fName,fType);
    CloseXDF();
    return 2;
  }

  if (!fStream) fStream = (XDR*) malloc(sizeof(XDR));

  if (strchr(fType,'w'))  xdrstdio_create(fStream, fFile, XDR_ENCODE);   
  else xdrstdio_create(fStream, fFile, XDR_DECODE);   

  return 0;
} 
//______________________________________________________________________________
Int_t St_XDFFile::WriteEvent(St_DataSet *dataset)
{
 // 
 //  WriteEvent(St_DataSet *dataset)
 // 
 // The WriteEvent writes the St_DataSet objected defined with the dataset
 // pointer as XDR dataset
 //
 //  dataset - the pointer to the St_DataSet object to be saved with XDF file
 //            recursively
 //
 // Note: 
 // The method creates temporary the XDR DS_DATASET_T C structure and deletes
 // it upon exit
 //

 fMethodName = "WriteEvent";
 if (fDebug) printf("%s \n",fMethodName);
 if (!( dataset && fFile) ) return 0;
 if (strchr(fType,'w')) {
   DS_DATASET_T *ds = MakeDataSet(dataset);

   if (ds) { 
     fRecordCount++;
     if (!::xdr_dataset(fStream,&ds))
     {
        printf("*** Error: WriteEvent() xdf_next_record: %s in Record %d\n",fName,fRecordCount); 
        return 0;
     };
     Delete(ds);
   }
   return kTRUE;
 } else 
   return kFALSE;
}

//______________________________________________________________________________
St_DataSet *St_XDFFile::ReadEvent()
{
 // 
 //  ReadEvent()
 // 
 //  The ReadEvent reads the next XDR events and creats St_DataSet object
 //  and returns its pointer.
 //
 //  It returns ZERO if failed.
 //
 // Note: 
 // ----
 // It is the calling method responsibility to delete the object created with
 // this method to avoid any memory leak
 //

 if (fBrowsable) return GetSelected(); // returh the slected with the ROOT Browser dataset
 fMethodName = "NextEvent()";
 // printf("%s \n",fMethodName);
 if (!fFile) return 0;
 if (strchr(fType,'r')) {
   if (!::xdr_dataset(fStream,&fDataSet))
   {
//    xdr_getpos(&fDataSet)
       printf("*** Warning: NextEvent() xdf_next_record: end of file %s\n",fName); 
       return 0;
   };
   fRecordCount++;
    if (fDebug) printf("%s from %s record %d \n",fMethodName,fName,fRecordCount);
   St_DataSet *set = MakeDataSet(fDataSet);
   Delete(fDataSet);
   return set;
 } else 
   return 0;
}
//______________________________________________________________________________
St_DataSet *St_XDFFile::NextEventList()
{
 //   STILL UNDER CONSTRUCTION !!!!
 //   This method requires a special version of dsl library
 //   -----------------------------------------------------
 //  NextEventList()
 // 
 //  The NextEventGet reads the next XDR events and creats St_DataSet object
 //  and returns its pointer.
 //
 //  It returns ZERO if failed.
 //
 // Note: 
 // ----
 // It is the calling method responsibility to delete the object created with
 // this method to avoid any memory leak
 //
 return 0;
#if 0
 fMethodName = "NextEvent()";
 // printf("%s \n",fMethodName);
 if (!fFile) return 0;
 if (strchr(fType,'r')) {
   if (    !xdr_getpos(fStream) 
        && !::xdr_dataset_type(fStream,&fDataSet)
        && !xdr_getpos(fStream)
        && !xdr_dataset_data(fStream,fDataSet)
      ) 
  {
//   
       printf("*** Warning: NextEvent() xdf_next_record: end of file %s\n",fName); 
       return 0;
   };
   fRecordCount++;
    printf("%s from %s record %d \n",fMethodName,fName,fRecordCount);
   St_DataSet *set = MakeDataSet(fDataSet);
   Delete(fDataSet);
   return set;
 } else 
   return 0;
#endif // 0
}
//______________________________________________________________________________
St_DataSet *St_XDFFile::MakeDataSet(DS_DATASET_T *ds)
{
 //
 //  MakeDataSet(DS_DATASET_T *ds)
 //
 //  Convert: DS_DATASET_T -> St_DataSet.   
 //
 //  Create and return the pointer to the St_DataSet object 
 //  from DS_DATASET_T *ds C-structure
 //
 // This method moves the pointers of the STAF tables from 
 // DS_DATASET_T *ds into St_Table objects it is creating. 
 // So NO real copy of the C-structure is performed. 
 // This means the original DS_DATASET_T *ds can not be used 
 // for the second time since it has no useful information anymore.
 //

  DS_DATASET_T *dt;
  St_DataSet *dataset = 0;

  //   printf (" dir: ds=0x%x  \n", ds);
  //   printf ("      tid  - %d     \n", ds->tid);
  //   printf ("      name - %s     \n", ds->name);

  if (ds->tid) 
  {
      Char_t *data;
      UInt_t nrows; 
      UInt_t rsize;
      const Char_t *name;
      const Char_t *type;
      const Char_t *classprefix="St_";
      const Int_t extralen = strlen(classprefix) + 1;
 
      if (!dsTableDataAddress(&data,ds)) {/* fErrorCode = 2;*/ return 0; }
      if (!dsTableRowCount(&nrows,ds)) {/* fErrorCode = 3; */ return 0; }
      if (!dsTableRowSize(&rsize,ds)) {/* fErrorCode = 4;*/ return 0; }
      if (!dsTableName(&name, ds)){/* fErrorCode = 5; */ return 0; }
      if (!dsTableTypeName(&type, ds)){/* fErrorCode = 6; */ return 0; }

      Char_t *classname = new Char_t[strlen(type)+extralen];
      St_Table *table = 0;
      strcpy(classname,classprefix);
      strcat(classname,type);

      TClass *cl = gROOT->GetClass(classname);
      if (cl) {
        table = (St_Table *)cl->New();
        // check the real table size against of the "dictionary" size
        cl = table->GetRowClass();
        UInt_t cCount=0;;
        dsTableColumnCount(&cCount, ds);
        if (UInt_t(cl->GetNdata()) == cCount)
        {
          table->SetTablePointer(data);
          table->SetName(name);
          table->SetfN(nrows);
          table->SetUsedRows(nrows);
        }
        else {
         table->PrintHeader();
         fprintf(stderr," ** Error ** There are <%d> columns found in the <%s> table ALREADY in the current dictionary\n     They do not match <%d> columns in the input file\n"
                ,cl->GetNdata(),table->GetType(),cCount);
         const Char_t *tableSpec=0;
         if (dsTableTypeSpecifier(&tableSpec, ds)) {
           fprintf(stderr," ------------------------------\n");
           fprintf(stderr," The original table definition (as seen in xdf file) contains %d row%sof :\n%s\n",nrows,nrows>1?"s ":" ",tableSpec);
           fprintf(stderr," -----------------------------\n");
           fprintf(stderr," The current table (as defined by IDL) definition is :\n");
           fprintf(stderr," ===============================\n");
           table->Print();
           fprintf(stderr," -------------------------------\n\n");
         }
         fprintf(stderr," ** Error ** This table has been discarded !\n");
         SafeDelete(table);
        }
      }
      else 
      {
        table = new St_Table((char *)name,(char *)type, nrows, data, rsize);
        Printf(" Warning: MakeDataSet the share lib /DLL for the table <%s> was not loaded\n",type);
        SafeDelete(table);
      }

      dataset = table;
      delete [] classname;

     // remove the the STAF table from the original dataset

      ds->elcount  = 0;
      ds->p.data   = 0;
      ds->maxcount = 0;
  }
  else {
    dataset = new St_DataSet(ds->name);
    for (UInt_t j=0; j< ds->elcount; j++)
      if ( (dt=ds->p.link[j]) )
          dataset->Add(MakeDataSet(dt));
  }
  return dataset;
 }
//______________________________________________________________________________
DS_DATASET_T *St_XDFFile::MakeDataSet(St_DataSet *dataset)
{
 //
 //  MakeDataSet(St_DataSet *dataset)
 //
 //  Convert: St_DataSet -> DS_DATASET_T
 //
 //  Allocate and return the pointer to the DS_DATASET_T C-structure
 //  from St_DataSet *dataset object
 //
 // So NO real copy of the C-structure kept within St_Table's is 
 // performed. 
 //
  if (!dataset) return 0;
  DS_DATASET_T *ds=0;
  St_Table *ta = (St_Table *)dataset;
  if (dataset->HasData()) {
    Char_t tablespec[2000];
    if(!dsNewTable(&ds,(Char_t *)ta->GetName(),ta->Print(tablespec,2000), 
                       ta->GetNRows(), ta->GetArray())) {  
        printf("MakeDataSet. Error, can not create table \"%s\"\n %s \n",ta->GetName(), tablespec);
        return 0;
    }
  }
  else {
    if(!dsNewDataset(&ds,(Char_t *)dataset->GetName())) {  
      printf("xdf_open. Error, can not create data set for dataset \"%s\"\n",dataset->GetName());
      return 0;
    }
    St_DataSetIter next(dataset);
    Int_t i = 0;
    St_DataSet *set = 0;
    ds->tid      = 0;                           // tid - a table row type
    ds->elcount  = dataset->GetListSize();      // the number of the links = dataset->GetListSize();
    ds->maxcount = ds->elcount;                 // the maximum number of rows for a table
    ds->p.link   = (ds_dataset_t **)malloc(sizeof(void *)*ds->elcount);
    while( (set = next()) )
          ds->p.link[i++] = MakeDataSet(set);
  }
  return ds;
 }

//______________________________________________________________________________
Int_t St_XDFFile::CloseXDF()
{
 //
 //  CloseXDF()
 //
 //  Close the current XDF file 
 //
  int ians;   
  if (fFile)
    ians = fclose(fFile);
  fFile = 0;
  if (fDataSet) Delete(fDataSet);
  fDataSet = 0;
  return ians;
}
//______________________________________________________________________________
void St_XDFFile::GetXdFile(const Char_t *filename, St_DataSet *dataset)
{
  if (!(dataset && filename && strlen(filename))) return;
  St_XDFFile xdf;
  if(xdf.GetDebug()) printf(" GetXdfFile: read from %s to DataSet %s \n",filename,dataset->GetName());
  if (xdf.OpenXDF(filename) == 0){
    St_DataSet *set = xdf.ReadEvent();
    if (set) dataset->Add(set);
  }
}

//______________________________________________________________________________
Int_t St_XDFFile::dir(const Char_t *filename, UInt_t firstRecord, UInt_t numberOfRecords)
{
 //
 //  Usage:
 //  -----
 //    gSystem->Load("St_base");
 //    gSystem->Load("xdf2root");
 //    gSystem->Load("St_Tables");
 //    St_XDFFile::dir(const Char_t *filename, UInt_t firstRecord=1 UInt_t numberOfRecords=1);
 //       where:
 //           firstRecord    =0 - means ALL record from the very first one by the "end_of_file"
 //           numberOfRecords=0 - means ALL records fron the firstRecord by the "end_of_file"
 //
  if (!(filename && strlen(filename))) return 0;
  printf("\n  Directory of  %s \n",filename);
  St_XDFFile xdf;
  Int_t counter=1;
  if (xdf.OpenXDF(filename) == 0)
  {
    St_DataSet *set = 0;
    while ( (set = xdf.NextEventGet()) && (!numberOfRecords || counter < Int_t (firstRecord+numberOfRecords) ) ) 
    {
      if (!firstRecord || (counter >= Int_t (firstRecord-1)))  // Skip first "firstRecords" records
      {
        St_DataSetIter dir(set);
        dir.Du();
      }
       counter++;
    }
    printf(" %d records have been read\n",counter);
    return counter; 
  }
  else
    printf(" Can't open file %s \n",filename);
  return 0;
}

//______________________________________________________________________________
St_DataSet *St_XDFFile::GetXdFile(const Char_t *filename)
{
  St_DataSet *set = 0;
  if (filename && strlen(filename)) {
    St_XDFFile xdf;
    if(xdf.GetDebug()) printf("GetXdfFile: read from %s\n",filename);
    if (xdf.OpenXDF(filename) == 0)
      set = xdf.ReadEvent();
  }
  return set;
}
