//*-- Author :    Valery Fine   27/04/98

#ifndef STAF_St_XDFFile
#define STAF_St_XDFFile

#include <stdio.h>
#include "Rtypes.h"
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
    char 		fType[8];	// File type    
    Int_t               fErrorCode;     // Error code of the last operation
    const Char_t       *fMethodName;    // The name of the current method (to debug code)
  protected:
    static St_DataSet   *MakeDataSet(DS_DATASET_T *ds);    // DS_DATASET_T -> St_DataSet. Create St_DataSet object from DS_DATASET_T C-structure
    static DS_DATASET_T *MakeDataSet(St_DataSet *dataset); // St_DataSet -> DS_DATASET_T. Create DS_DATASET_T C-structure from St_DataSet object
    static void Delete(DS_DATASET_T *ds);                  // Delete DS_DATASET_T structure
    St_XDFFile();                                          // Default ctor (only for dictionary)
  public:
    St_XDFFile(Char_t *filename,Char_t *mode="r");         // Create object and open file
    virtual ~St_XDFFile();
    Int_t    GetErrorCode(){ return fErrorCode;}
    virtual Int_t       OpenXDF(Char_t *filename,Char_t *mode="r"); // Open file and read create the "root" St_DataSet (it is called from ctor)
    virtual St_DataSet *NextEventGet();                         // create St_DataSet and read the next event in it.
    virtual Int_t       NextEventPut(St_DataSet *dataset);      // Create DS_DATASET_T from St_DataSet and write it out in XDR format
    virtual Int_t       CloseXDF();                             // close the XDF file (it is called from dtor)
    ClassDef(St_XDFFile,0)

};


#endif
