//*-- Author :    Valery Fine   27/04/98
// $Id: St_XDFFile.h,v 1.23 2000/03/22 23:52:06 fine Exp $
// $Log: St_XDFFile.h,v $
// Revision 1.23  2000/03/22 23:52:06  fine
// Adjusted to libSTAR for ROOT 2.24
//
// Revision 1.22  1999/03/15 00:10:15  perev
// For new Maker schema
//
// Revision 1.21  1999/03/02 03:19:03  fine
// re-commit, the obsolte version was in use
//
// Revision 1.19  1999/01/21 18:12:48  fine
// Browse and dir methods have been introduced
//
// Revision 1.18  1999/01/21 02:46:38  fine
// New method dir and Browse to navigate XDF files
//
// Revision 1.17  1998/12/19 02:54:05  fine
// ST_XDFFile::NextEventList() - fast scan of the XDF files method has been introduced
//
// Revision 1.15  1998/10/31 00:21:57  fisyak
// Add record counter
//
// Revision 1.14  1998/10/06 18:00:56  perev
// cleanup
//
// Revision 1.13  1998/10/05 21:21:23  fine
//  #include <errno.h>
// fErrorCode data-member has been activated
//
// Revision 1.12  1998/10/01 17:09:51  fine
// St_XDFFile::GetName() method has been introduced
//
// Revision 1.11  1998/09/20 22:50:38  fine
// New method to read XDF file has been introduced
//
// Revision 1.10  1998/08/28 21:55:11  fine
// TSocket data-memebr and "socket" methods have been introduced to accept the XDF file
// over TCP/IP
//
// Revision 1.9  1998/08/10 02:33:10  fisyak
// Add St_fileSet
//
// Revision 1.8  1998/07/20 15:08:17  fisyak
// Add tcl and tpt
//


#ifndef STAF_St_XDFFile
#define STAF_St_XDFFile

#include <stdio.h>
#include "Rtypes.h"
#include "TSocket.h"
#ifndef __CINT__
#include "dsxdr.h"
#else
typedef void XDR;
typedef void DS_DATASET_T; 
#endif

class TDataSet;
class TClass;


class St_XDFFile : public TObject
 {
  private:
    FILE 		*fFile;  	// pointer to C file descriptor 
    XDR  		*fStream; 	// XDR stream      
    DS_DATASET_T 	*fDataSet; 	// "root" dataset  
    char 		*fName;	        // File Name	
    char 		 fType[8];	// File type    
    Int_t                fErrorCode;    // Error code of the last operation
    const Char_t        *fMethodName;   // The name of the current method (to debug code)
    TSocket             *fSocket;       // Socket to XDF I/O
    Int_t                fRecordCount;  // No. of records read/written
    TDataSet          *fBrowsable;    // The pointer to the record selected with ROOT Browser
    Int_t                fDebug;        // Debug Level
  protected:
    static TDataSet   *MakeDataSet(DS_DATASET_T *ds);    // DS_DATASET_T -> TDataSet. Create TDataSet object from DS_DATASET_T C-structure
    static DS_DATASET_T *MakeDataSet(TDataSet *dataset); // TDataSet -> DS_DATASET_T. Create DS_DATASET_T C-structure from TDataSet object
    static void  Delete(DS_DATASET_T *ds);                 // Delete DS_DATASET_T structure
           Int_t CreateXDFStream();
  public:
    St_XDFFile();                                          // Default ctor
    St_XDFFile(const Char_t *filename,const Char_t *mode="r");                  // Create object and open file
    virtual ~St_XDFFile();
    virtual void        Browse(TBrowser *b);
    virtual Int_t       CloseXDF();                                             // close the XDF file (it is called from dtor)
    static  Int_t       dir(const Char_t *filename, UInt_t firstRecord=1, UInt_t numberOfRecords=1);
            Int_t       GetErrorCode() const { return fErrorCode;}
    virtual const Char_t *GetName() const { return fName;}
    virtual TDataSet *GetSelected() { TDataSet *s = fBrowsable; fBrowsable=0; return s;}
    virtual Int_t       OpenXDF(const Char_t *filename,const Char_t *mode="r"); // Open file and read create the "root" TDataSet (it is called from ctor)
    virtual Int_t       OpenXDF(TInetAddress address, const char *service,const Char_t *mode="r");      // Create object and open file
    virtual Int_t       OpenXDF(TInetAddress address, Int_t port,const Char_t *mode="r");      // Create object and open file
    virtual Int_t       OpenXDF(const char *host, Int_t port,const Char_t *mode="r");      // Create object and open file
    virtual Int_t       OpenXDF(Int_t descriptor,const Char_t *mode="r");       // Create object and open file
    virtual Bool_t    IsFolder(){ return fFile?kTRUE:kFALSE;}
    virtual TDataSet *ReadEvent();                                         // create TDataSet and read the next event in it.
    virtual TDataSet *NextEventGet(){return ReadEvent();};                                         // create TDataSet and read the next event in it.
    virtual TDataSet *NextEventList();
    virtual Int_t       WriteEvent(TDataSet *dataset);                      // create DS_DATASET_T from TDataSet and write it out in XDR format
    virtual Int_t       NextEventPut(TDataSet *dataset){return WriteEvent(dataset);};                      // create DS_DATASET_T from TDataSet and write it out in XDR format
    virtual Int_t       GetCount(){return fRecordCount;}
    virtual Int_t       GetDebug(){return fDebug;};
    virtual Int_t       Debug(){return GetDebug();};
    virtual void        SetDebug(Int_t dbl=1){fDebug=dbl;};
    static  void        GetXdFile(const Char_t *filename, TDataSet *dataset); // open, read and close file file
    static  TDataSet *GetXdFile(const Char_t *filename);                      // open, read and close file file

    ClassDef(St_XDFFile,0)

};


#endif
