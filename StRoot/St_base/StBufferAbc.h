//*CMZ :  1.03/06 25/06/99  13.25.14  by  Valery Fine
//*-- Author :    Valery Fine   25/06/99
 
#ifndef STAR_StBufferAbc
#define STAR_StBufferAbc
  
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StBufferAbc                                                          //
//                                                                      //
// Buffer abstract base class used for serializing objects.             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
  
#ifndef ROOT_TObject
#include "TObject.h"
#endif
 
class TClass;
  
class StBufferAbc : public TObject {
 
protected:
   Bool_t  fMode;       //Read or write mode
   Int_t   fVersion;    //Buffer format version
 
   // Default ctor
   StBufferAbc() : fMode(0){;}
 
   // StBufferAbc objects cannot be copied or assigned
   StBufferAbc(const StBufferAbc &) { }
   void operator=(const StBufferAbc &) { }
 
public:
   enum EMode { kRead = 0, kWrite = 1 };
 
   StBufferAbc(EMode mode);
   virtual ~StBufferAbc(){}
//  From Pasha

    virtual Int_t    GetDescriptor(const Char_t *tableName,Int_t timeStart, Int_t timeEnd) = 0;  // return the number of the rows for the object "tableName" with valididty (t1,t2)
    virtual void     ReadFastArray(Char_t   *c, Int_t n1, Int_t n2, const Char_t *)        = 0;
    virtual void     ReadFastArray(Char_t   *c, Int_t n1, Int_t n2, Int_t n3, const Char_t *) = 0;
    virtual void     SetRecordNumber(Int_t recNum) = 0;
//
 
   void     SetReadMode(){fMode = !kWrite;}
   void     SetWriteMode(){fMode = kWrite;}
 
   Bool_t   IsReading() const { return (fMode & kWrite) == 0; }
   Bool_t   IsWriting() const { return (fMode & kWrite) != 0; }
  
   virtual Text_t  *ReadString(Text_t *s, Int_t max) = 0;
   virtual void     WriteString(const Text_t *s) = 0;
 
   virtual  Version_t ReadVersion(UInt_t *start = 0, UInt_t *bcnt = 0) = 0;
   virtual  UInt_t    WriteVersion(const TClass *cl, Bool_t useBcnt = kFALSE) = 0;
 
   virtual  TClass  *ReadClass(const TClass *cl = 0, UInt_t *objTag = 0) = 0;
   virtual  void     WriteClass(const TClass *cl) = 0;
 
   virtual  TObject *ReadObject(const TClass *cl) = 0;
   virtual  void     WriteObject(const TObject *obj) = 0;
 
   virtual  Int_t    ReadArray(Char_t   *&c, const Char_t *) = 0;
   virtual  Int_t    ReadArray(UChar_t  *&c, const Char_t *) = 0;
   virtual Int_t    ReadArray(Short_t  *&h, const Char_t *) = 0;
   virtual Int_t    ReadArray(UShort_t *&h, const Char_t *) = 0;
   virtual Int_t    ReadArray(Int_t    *&i, const Char_t *) = 0;
   virtual Int_t    ReadArray(UInt_t   *&i, const Char_t *) = 0;
   virtual Int_t    ReadArray(Long_t   *&l, const Char_t *) = 0;
   virtual Int_t    ReadArray(ULong_t  *&l, const Char_t *) = 0;
   virtual Int_t    ReadArray(Float_t  *&f, const Char_t *) = 0;
   virtual Int_t    ReadArray(Double_t *&d, const Char_t *) = 0;
 
   virtual Int_t    ReadStaticArray(Char_t   *c, const Char_t *) = 0;
   virtual Int_t    ReadStaticArray(UChar_t  *c, const Char_t *) = 0;
   virtual Int_t    ReadStaticArray(Short_t  *h, const Char_t *) = 0;
   virtual Int_t    ReadStaticArray(UShort_t *h, const Char_t *) = 0;
   virtual Int_t    ReadStaticArray(Int_t    *i, const Char_t *) = 0;
   virtual Int_t    ReadStaticArray(UInt_t   *i, const Char_t *) = 0;
   virtual Int_t    ReadStaticArray(Long_t   *l, const Char_t *) = 0;
   virtual Int_t    ReadStaticArray(ULong_t  *l, const Char_t *) = 0;
   virtual Int_t    ReadStaticArray(Float_t  *f, const Char_t *) = 0;
   virtual Int_t    ReadStaticArray(Double_t *d, const Char_t *) = 0;
 
   virtual void     WriteArray(const Char_t   *c, Int_t n, const Char_t *) = 0;
   virtual void     WriteArray(const UChar_t  *c, Int_t n, const Char_t *) = 0;
   virtual void     WriteArray(const Short_t  *h, Int_t n, const Char_t *) = 0;
   virtual void     WriteArray(const UShort_t *h, Int_t n, const Char_t *) = 0;
   virtual void     WriteArray(const Int_t    *i, Int_t n, const Char_t *) = 0;
   virtual void     WriteArray(const UInt_t   *i, Int_t n, const Char_t *) = 0;
   virtual void     WriteArray(const Long_t   *l, Int_t n, const Char_t *) = 0;
   virtual void     WriteArray(const ULong_t  *l, Int_t n, const Char_t *) = 0;
   virtual void     WriteArray(const Float_t  *f, Int_t n, const Char_t *) = 0;
   virtual void     WriteArray(const Double_t *d, Int_t n, const Char_t *) = 0;
 
   virtual void     ReadFastArray(Char_t   *c, Int_t n, const Char_t *) = 0;
   virtual void     ReadFastArray(UChar_t  *c, Int_t n, const Char_t *) = 0;
   virtual void     ReadFastArray(Short_t  *h, Int_t n, const Char_t *) = 0;
   virtual void     ReadFastArray(UShort_t *h, Int_t n, const Char_t *) = 0;
   virtual void     ReadFastArray(Int_t    *i, Int_t n, const Char_t *) = 0;
   virtual void     ReadFastArray(UInt_t   *i, Int_t n, const Char_t *) = 0;
   virtual void     ReadFastArray(Long_t   *l, Int_t n, const Char_t *) = 0;
   virtual void     ReadFastArray(ULong_t  *l, Int_t n, const Char_t *) = 0;
   virtual void     ReadFastArray(Float_t  *f, Int_t n, const Char_t *) = 0;
   virtual void     ReadFastArray(Double_t *d, Int_t n, const Char_t *) = 0;
 
   virtual void     WriteFastArray(const Char_t   *c, Int_t n, const Char_t *) = 0;
   virtual void     WriteFastArray(const UChar_t  *c, Int_t n, const Char_t *) = 0;
   virtual void     WriteFastArray(const Short_t  *h, Int_t n, const Char_t *) = 0;
   virtual void     WriteFastArray(const UShort_t *h, Int_t n, const Char_t *) = 0;
   virtual void     WriteFastArray(const Int_t    *i, Int_t n, const Char_t *) = 0;
   virtual void     WriteFastArray(const UInt_t   *i, Int_t n, const Char_t *) = 0;
   virtual void     WriteFastArray(const Long_t   *l, Int_t n, const Char_t *) = 0;
   virtual void     WriteFastArray(const ULong_t  *l, Int_t n, const Char_t *) = 0;
   virtual void     WriteFastArray(const Float_t  *f, Int_t n, const Char_t *) = 0;
   virtual void     WriteFastArray(const Double_t *d, Int_t n, const Char_t *) = 0;
 
   virtual Char_t   ReadScalar(Char_t   &c, const Char_t *) = 0;
   virtual UChar_t  ReadScalar(UChar_t  &c, const Char_t *) = 0;
   virtual Short_t  ReadScalar(Short_t  &h, const Char_t *) = 0;
   virtual UShort_t ReadScalar(UShort_t &h, const Char_t *) = 0;
   virtual Int_t    ReadScalar(Int_t    &i, const Char_t *) = 0;
   virtual UInt_t   ReadScalar(UInt_t   &i, const Char_t *) = 0;
   virtual Long_t   ReadScalar(Long_t   &l, const Char_t *) = 0;
   virtual ULong_t  ReadScalar(ULong_t  &l, const Char_t *) = 0;
   virtual Float_t  ReadScalar(Float_t  &f, const Char_t *) = 0;
   virtual Double_t ReadScalar(Double_t &d, const Char_t *) = 0;
   virtual Char_t   ReadScalar(Char_t   *c, const Char_t *) = 0;
 
   virtual void     WriteScalar(Char_t   c, const Char_t *) = 0;
   virtual void     WriteScalar(UChar_t  c, const Char_t *) = 0;
   virtual void     WriteScalar(Short_t  h, const Char_t *) = 0;
   virtual void     WriteScalar(UShort_t h, const Char_t *) = 0;
   virtual void     WriteScalar(Int_t    i, const Char_t *) = 0;
   virtual void     WriteScalar(UInt_t   i, const Char_t *) = 0;
   virtual void     WriteScalar(Long_t   l, const Char_t *) = 0;
   virtual void     WriteScalar(ULong_t  l, const Char_t *) = 0;
   virtual void     WriteScalar(Float_t  f, const Char_t *) = 0;
   virtual void     WriteScalar(Double_t d, const Char_t *) = 0;
   virtual void     WriteScalar(const Char_t  *c, const Char_t *) = 0;
  
   ClassDef(StBufferAbc,0)  //Buffer base class used for serializing objects
};
 
#endif

