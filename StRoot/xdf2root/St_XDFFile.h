//*-- Author :    Valery Fine   27/04/98
// $Id: St_XDFFile.h,v 1.14 1998/10/06 18:00:56 perev Exp $
// $Log: St_XDFFile.h,v $
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
#include "dstype.h"
#else
typedef void XDR;
typedef void DS_DATASET_T; 
#endif

class St_DataSet;
class TClass;


class St_XDFFile 
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

  protected:
    static St_DataSet   *MakeDataSet(DS_DATASET_T *ds);    // DS_DATASET_T -> St_DataSet. Create St_DataSet object from DS_DATASET_T C-structure
    static DS_DATASET_T *MakeDataSet(St_DataSet *dataset); // St_DataSet -> DS_DATASET_T. Create DS_DATASET_T C-structure from St_DataSet object
    static void  Delete(DS_DATASET_T *ds);                 // Delete DS_DATASET_T structure
           Int_t CreateXDFStream();
  public:
    St_XDFFile();                                          // Default ctor
    St_XDFFile(const Char_t *filename,const Char_t *mode="r");                  // Create object and open file
    virtual ~St_XDFFile();
    virtual Int_t       CloseXDF();                                             // close the XDF file (it is called from dtor)
            Int_t       GetErrorCode() const { return fErrorCode;}
    virtual const Char_t *GetName() const { return fName;}
    virtual Int_t       OpenXDF(const Char_t *filename,const Char_t *mode="r"); // Open file and read create the "root" St_DataSet (it is called from ctor)
    virtual Int_t       OpenXDF(TInetAddress address, const char *service,const Char_t *mode="r");      // Create object and open file
    virtual Int_t       OpenXDF(TInetAddress address, Int_t port,const Char_t *mode="r");      // Create object and open file
    virtual Int_t       OpenXDF(const char *host, Int_t port,const Char_t *mode="r");      // Create object and open file
    virtual Int_t       OpenXDF(Int_t descriptor,const Char_t *mode="r");       // Create object and open file
    virtual St_DataSet *NextEventGet();                                         // create St_DataSet and read the next event in it.
    virtual Int_t       NextEventPut(St_DataSet *dataset);                      // create DS_DATASET_T from St_DataSet and write it out in XDR format
   static  void        GetXdFile(const Char_t *filename, St_DataSet *dataset); // open, read and close file file
    static  St_DataSet *GetXdFile(const Char_t *filename);                      // open, read and close file file
    ClassDef(St_XDFFile,0)

};


#endif
