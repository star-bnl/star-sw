//*-- Author :    Valery Fine   24/03/98  (E-mail: fine@bnl.gov)
 
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TStafTable                                                           //
//                                                                      //
// Array of the STAF structures longs (one STAF Table per element).     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
#include <iostream.h>
// #include <sstream>
#include <string.h> 

#include "TROOT.h"
#include "TBuffer.h"
#include "TMath.h"
#include "TClass.h"
#include "Api.h"

#include "St_Table.h"
 
ClassImp(St_Table)
 

//______________________________________________________________________________
Int_t St_Table::MakeWrapClass(Text_t *name)
{
 //
 // It is a static method wich must be ROOT macro
 // But I failed to pass through ROOT though and did this method instead
 //
  Char_t filename[500];
  if (!name) { return -1;}
  strcpy(filename,name);
  strcat(filename,".h");
  gROOT->LoadMacro(filename);
  St_Table t(name,1);
  t.StafStreamer();
  return 0;
}
//______________________________________________________________________________
St_Table::St_Table(Text_t *name, Int_t size)
{
   // Default St_Table ctor.
   s_TableHeader = new table_head_st;
   s_Size = &s_TableHeader->rbytes;
   s_MaxIndex = &s_TableHeader->nok;
   *s_Size = size;
   SetHeadFields(name);          // Define the default name and type as "unknown"
   s_Table = 0;
   *s_MaxIndex = 0;
   if (size == 0) Warning("St_Table(0)","Wrong table format");
}
 
//______________________________________________________________________________
St_Table::St_Table(Text_t *name, Int_t n,Int_t size)
{
   // Create St_Table object and set array size to n longs.
 
   s_Table = 0;
   s_TableHeader = new table_head_st;
   LinkHeader();
   *s_Size = size;
   SetHeadFields(name);          // Define the default name and type as "unknown"
   if (n > 0) Set(n);
   *s_MaxIndex = 0;
}
 
//______________________________________________________________________________
St_Table::St_Table(Text_t *name, Int_t n, Char_t *table,Int_t size)
{
   // Create St_Table object and initialize it with values of array.
 
   s_Table = 0;
   s_TableHeader = new table_head_st; 
   LinkHeader();
   *s_Size = size;
   SetHeadFields(name);          // Define the default name and type as "unknown"
   Set(n, table);
}
 
//______________________________________________________________________________
St_Table::St_Table(Text_t *name, Text_t *type, Int_t n, Char_t *array, Int_t size)
{
   // Create St_Table object and initialize it with values of array.
 
   s_Table = array;
   s_TableHeader = new table_head_st; 
   LinkHeader();
   *s_Size = size;
   SetHeadFields(name);
   SetType(type);
   SetfN(n); 
}

//______________________________________________________________________________
St_Table::St_Table(const St_Table &table)
{
   // Copy constructor.
 
   s_Table = 0;
   *s_TableHeader = *(table.s_TableHeader);
   Set(table.fN, table.s_Table);
}
 
//______________________________________________________________________________
St_Table &St_Table::operator=(const St_Table &rhs)
{
   // St_Table assignment operator.
   if (this != &rhs)
      Set(rhs.fN, rhs.s_Table);
   return *this;
}
 
//______________________________________________________________________________
St_Table::~St_Table()
{
   // Delete St_Table object.
   Delete();
}
 
//______________________________________________________________________________
void St_Table::Adopt(Int_t n, Char_t *arr)
{
   // Adopt array arr into St_Table, i.e. don't copy arr but use it directly
   // in St_Table. User may not delete arr, St_Table dtor will do it.
 
   Delete();
 
   SetfN(n);
   s_Table = arr;
}
 
//______________________________________________________________________________
void St_Table::AddAt(ULong_t *row, Int_t i)
{
   // Add long c at position i. Check for out of bounds.
 
   if (!BoundsOk("St_Table::AddAt", i))
      i = 0;
   memcpy(s_Table+i*(*s_Size),row,*s_Size);
   *s_MaxIndex = TMath::Max((Int_t)i,Int_t(*s_MaxIndex));
}
 
//______________________________________________________________________________
void St_Table::Copy(Char_t *dest, Char_t *src)
{
    ::memcpy(dest,src,*s_Size*fN);
}
//______________________________________________________________________________
void St_Table::Copy(St_Table &array)
{
  array.Set(fN); 
  Copy(array.s_Table,s_Table); 
 *(array.s_TableHeader) = *s_TableHeader; 
}

//______________________________________________________________________________
Char_t *St_Table::Create()
{
  if (s_Size) 
       return (Char_t *)malloc(*s_Size*fN);
  return 0;
}
//______________________________________________________________________________
void St_Table::Browse(TBrowser *b){
#ifndef WIN32
  Inspect();
#endif
  ls("*");
}
//______________________________________________________________________________
void St_Table::Delete()
{
  if (s_Table)
  {
    free(s_Table);
    s_Table = 0;
   *s_MaxIndex = 0;
    fN = 0;
  }       
}

#if 0
//______________________________________________________________________________
void St_Table::Dump()
{
   // Dump contents of object on stdout.
   // Using the information in the object dictionary (class TClass)
   // each data member is interpreted.
   // If a data member is a pointer, the pointer value is printed
   //
   // The following output is the Dump of a TArrow object:
   //   fAngle                   0           Arrow opening angle (degrees)
   //   fArrowSize               0.2         Arrow Size
   //   fOption.*fData
   //   fX1                      0.1         X of 1st point
   //   fY1                      0.15        Y of 1st point
   //   fX2                      0.67        X of 2nd point
   //   fY2                      0.83        Y of 2nd point
   //   fUniqueID                0           object unique identifier
   //   fBits                    50331648    bit field status word
   //   fLineColor               1           line color
   //   fLineStyle               1           line style
   //   fLineWidth               1           line width
   //   fFillColor               19          fill area color
   //   fFillStyle               1001        fill area style
 
   char parent[256];
   parent[0] = 0;
   TDumpMembers dm;
   ShowMembers(dm, parent);
}
#endif 
//______________________________________________________________________________
void St_Table::LinkHeader()
{
  // Link some class data members with the STAF table header
   if (s_TableHeader)
   {
     s_Size = &s_TableHeader->rbytes;
     s_MaxIndex = &s_TableHeader->nok;
   }
   else 
     Error("LinkHeader","wrong header !");
}

//______________________________________________________________________________
void St_Table::ls(Option_t *option)
{
  TNamed::ls(option);
  IncreaseDirLevel();
  IndentLevel();
  cout       <<"Allocated rows: "<<fN
       <<'\t'<<"Used rows: "     <<*s_MaxIndex
       <<'\t'<<"Row size: "      <<*s_Size
       <<endl;
  Print();
  DecreaseDirLevel();
}
//______________________________________________________________________________
Char_t *St_Table::Print(Char_t *strbuf,Int_t lenbuf)
{
  Char_t buffer[100];
  strcpy(buffer,GetTitle());
  strcat(buffer,"_st");

//  ostrstream  out(strbuf,lenbuf);
  Int_t iOut = 0; 
 
  G__ClassInfo m(buffer);
  G__DataMemberInfo data(m);

  if (!m.Name()) {
      Error("Print"," No dictionary entry for <%s> structure", buffer);
      if (lenbuf>0) iOut += sprintf(strbuf+iOut," *** Errror ***");
      return strbuf;
  }
  IndentLevel();

  if (lenbuf>0) {
  // cut of the "_st" suffix 
      Char_t *typenam =  (Char_t *)malloc(strlen(m.Name()));
      strcpy(typenam,m.Name());
      Char_t *eon = strstr(typenam,"_st");
      if (eon) *eon = '\0';
//      out << "struct " << typenam << " {";
        iOut += sprintf(strbuf+iOut,"struct %s {",typenam);

      free(typenam);
   }
   else
      cout << "struct " << m.Name() << " {" << endl;

  while(data.Next())
  {
    Int_t dim = data.ArrayDim();

    G__TypeInfo *t = data.Type();

    IndentLevel();

    if (lenbuf>0) 
//        out << " " << t->Name() << " " << data.Name();
        iOut += sprintf(strbuf+iOut," %s %s",t->Name(),data.Name());
    else
        cout << '\t'<< t->Name() << '\t'<< data.Name();

    Int_t indx = 0;
    while (indx < dim) {
          if (lenbuf>0)
  //             out <<  "[" << data.MaxIndex(indx)<<"]";
                 iOut += sprintf(strbuf+iOut,"[%d]",data.MaxIndex(indx));
          else
               cout <<  "[" << data.MaxIndex(indx)<<"]";
          indx++;
    }
    if (lenbuf>0) 
//        out << "; " << data.Title();
          iOut += sprintf(strbuf+iOut, "; %s", data.Title());
    else 
        cout << ";" << endl;
  }
  IndentLevel();
  if (lenbuf>0) {
//     out << "}";
          iOut += sprintf(strbuf+iOut, "}");
    printf("%s\n",strbuf);
  }
  else
     cout << "}" << endl;
 
  return strbuf;
}

//______________________________________________________________________________
Int_t St_Table::ReadGenericArray(TBuffer &b, void *&ii, EBufSizes membersize)
{
   // Read array of ANY type with pre-defined size from the I/O buffer. 
   // Returns the number of that read. 
   // If argument is a 0 pointer then space will be allocated for the
   // array.

  switch ( membersize) {
    case kChar1Byte:    return b.ReadArray((Char_t *&)ii);
         break;
    case kShort2Bytes:  return b.ReadArray((Short_t *&)ii);
         break;
    case kLong4Bytes:   return b.ReadArray((Long_t *&)ii);
         break;
    case kDouble8Bytes: return b.ReadArray((Double_t *&)ii); 
         break;
   default: 
         break;
  };
  return 0;
}

//______________________________________________________________________________
void St_Table::Set(Int_t n)
{
   // Set array size of St_Table object to n longs. If n<0 leave array unchanged.
   if (n < 0) return;
   if (fN != n)  Delete();
   SetfN(n);
   if (fN == 0) return;
   if (!s_Table) s_Table = Create();
   Reset();
}
//______________________________________________________________________________
void St_Table::SetHeadFields(Text_t *name)
{
   if (name) SetName(name);      // Define "name" if supplied
   else SetName("unknown");      // Define the default name 
   SetType("unknown");           // Define the default type 
}

//______________________________________________________________________________
void St_Table::SetName(const Text_t *const name)
{
   strcpy(s_TableHeader->name,name);
   TNamed::SetName(s_TableHeader->name);
}
//______________________________________________________________________________
void St_Table::SetTablePointer(void *table)
{ 
   if (s_Table) free(s_Table);
   s_Table = (Char_t *)table;
}

//______________________________________________________________________________
void St_Table::SetType(const Text_t *const type)
{
   strcpy(s_TableHeader->type,type);
   TNamed::SetTitle(s_TableHeader->type);
}

//______________________________________________________________________________
int St_Table::PointerToPointer(G__DataMemberInfo &m)
{
   if (strstr(m.Type()->Name(), "**")) return 1;
   return 0;
}

//______________________________________________________________________________
void St_Table::MakeHeader(const Char_t *prefix,const Char_t *tablename,
                         const Char_t *suffix, FILE *fl)
{
  Char_t *include = new Char_t [strlen(prefix)+strlen(tablename)+strlen(suffix)+5];
  sprintf(include,"%s%s%s.h",prefix,tablename,suffix);
  //*-*
  //*-* Open file if that is not supplied via parameter
  //
   FILE *fp = (fl == 0 ? fopen(include, "w") : fl) ;
   fprintf(fp, "#ifndef STAF_%s%s_Table\n",prefix,tablename);
   fprintf(fp, "#define STAF_%s%s_Table\n",prefix,tablename);
   fprintf(fp, "\n");
   fprintf(fp, "#include \"St_Table.h\"\n");
   fprintf(fp, "\n");
   fprintf(fp, "#include \"%s.h\"\n",tablename);
   fprintf(fp, "\n");
   fprintf(fp, "class St_%s : public St_Table\n",tablename);
   fprintf(fp, "{\n");
   fprintf(fp, "public:\n");
   fprintf(fp, "  %s%s() : St_Table(\"%s\",sizeof(%s_st)) {SetType(\"%s\");}\n"
          ,prefix,tablename,tablename,tablename,tablename);
   fprintf(fp, "  %s%s(Text_t *name) : St_Table(name,sizeof(%s_st)) {SetType(\"%s\");}\n"
          ,prefix,tablename,tablename,tablename);
   fprintf(fp, "  %s%s(Int_t n): St_Table(\"%s\",n,sizeof(%s_st)) {SetType(\"%s\");}\n"
          ,prefix,tablename,tablename,tablename,tablename);
   fprintf(fp, "  %s%s(Text_t *name,Int_t n): St_Table(name,n,sizeof(%s_st)) {SetType(\"%s\");}\n"
          ,prefix,tablename,tablename,tablename);
   fprintf(fp, "  %s_st *GetTable(){ return (%s_st *)s_Table;}\n"
          ,tablename,tablename);
   fprintf(fp, "\n");
   fprintf(fp, "  ClassDef(%s%s,0) // class particle STAF tables\n"
          ,prefix,tablename);
   fprintf(fp, "};\n");
   fprintf(fp, "\n");
   fprintf(fp, "#endif  \n");
  fclose(fp);
  delete [] include;
}
//______________________________________________________________________________
void St_Table::StafStreamer(Char_t *structname, FILE *fl)
{

  ///////////////////////////////////////////////////////////////////////////////
  //
  // Method StafStreamer(Char_t *structname, FILE *fl)
  // Creates the implemetation of the class to wrap the "plain"
  // C data strucutre.
  //
  //  It is assumes if "name" is the name of the C-structure
  // 
  //    1. The name of this class is created as "prefix"+"name"
  //    2. The real name of the base C-structure is "name"+"tabsuffux"
  //    3. The of the generetared file with this class implementation
  //       is "prefix" + "name" + "suffix" + ".cxx" if any
  //
  //  Parameters:
  //      Char_t *structname  - The "name" of the "extrernal C data structure
  //                            (see p.1 above)
  //                      = 0  - The name is defined with GetType() method
  //      FILE *fl            - the FILE handler used to write the generated 
  //                            C++ code out, otherwise this method opens file
  //                            as p.3 defines alone and close it before return
  //
  ///////////////////////////////////////////////////////////////////////////////

   Char_t datamem[256];
   Char_t *tablename=0;;
   Char_t datatype[256];
   Char_t *thisclassname=0;
   const Char_t *prefix = "St_";      // Prefix for all STAF wrapper (table & modules) classes
   const Char_t *suffix = "_Table";   // Siffix to distinguish "module" and "table" wrappers
   const Char_t *tabsuffix ="_st";    // Suffix defined with STAF C structures
   Bool_t thisclassbase;
   const Char_t row[]="row";
   
   Bool_t typeunknown = strcmp(GetType(),"unknown") == 0;

   if (structname || typeunknown ) 
   {
        if (!structname) {
    //
    // The name of the original structure has not been supplied
    // Try to guess it
    //
          tablename = new Char_t [strlen(GetName())+1];
          strcpy(tablename,GetName());
        }
        else {
    //
    // The name of the original structure has been supplied
    //
          tablename = new Char_t [strlen(structname)+1];
          strcpy(tablename,structname);
        }
        strcpy(datamem, prefix);
        strcat(datamem, tablename);
        thisclassbase = kTRUE;
   }
   else if (!typeunknown)
   {
       tablename = new Char_t[strlen(GetType())+1];
       strcpy(tablename,GetType());
       strcpy(datamem, ClassName());
       thisclassbase = kFALSE;
   }
   else
   {
     Error("StafStreamer", "Can't guess the C-structure typedef");
     return;
   }
   thisclassname = new Char_t[strlen(datamem)+1]; 
   strcpy(thisclassname,datamem);
   strcat(datamem,suffix);
   strcat(datamem,".cxx");
  //*-*
  //*-* Open file if that is not supplied via parameter
  //
   FILE *fp = (fl == 0 ? fopen(datamem, "w") : fl) ;

//    Create ClassInfo
   strcpy(datatype,tablename);
   strcat(datatype,tabsuffix);

   G__ClassInfo cl(datatype);

   strcpy(datamem,"((");
   strcat(datamem,tablename);
   strcat(datamem,tabsuffix);
   strcat(datamem," *)s_Table)");

   // Create include file

   MakeHeader(prefix,tablename,suffix);

   fprintf(fp, "#include \"%s%s%s.h\"\n",prefix,tablename,suffix);
   fprintf(fp, "#include \"%s%s%s.h\"\n",prefix,tablename,suffix);
   fprintf(fp, "/////////////////////////////////////////////////////////////////////////\n");
   fprintf(fp, "//\n");
   fprintf(fp, "//  Class %s wraps the STAF table %s \n",thisclassname,datatype);
   fprintf(fp, "//  It has been generated \"by automatic\". Please don\'t change it \"by hand\"\n");
   fprintf(fp, "//\n");
   fprintf(fp, "///////////////////////////////////////////////////////////////////////// \n\n");
   fprintf(fp, "ClassImp(%s%s)\n",prefix,tablename);

   fprintf(fp, "//_______________________________________");
   fprintf(fp, "_______________________________________\n");
   fprintf(fp, "void %s::Streamer(TBuffer &R__b)\n{\n", thisclassname);
   fprintf(fp, "   // Stream an array of the \"plain\" C-structures <%s>.\n\n", cl.Name());
 
   // In case of VersionID<=0 write dummy streamer only calling
   // its base class Streamer(s). If no base class(es) let Streamer
   // print error message, i.e. this Streamer should never have been called.
   char a[80];
//   int i = pass;
//   sprintf(a, "%s::Class_Version()", cl.Name());
//   version = (int)G__int(G__calc(a));
 
   // loop twice: first time write reading code, second time writing code
   for (int i = 0; i < 2; i++) {
 
      int decli = 0;

      if (i == 0) {
         fprintf(fp, "   if (R__b.IsReading()) {\n");
         fprintf(fp, "      Version_t R__v = R__b.ReadVersion(); if (R__v) { }\n");
      } else {
         fprintf(fp, "   } else {\n");
         fprintf(fp, "      R__b.WriteVersion(%s::IsA());\n",thisclassname);
      }

      // Stream base classes if any
      if (!thisclassbase)
      {
        TIter next(IsA()->GetListOfBases());
        TClass *obj;
        while (obj = (TClass *)next()) 
           fprintf(fp, "      %s::Streamer(R__b);\n", obj->GetName());
      }
      else 
           fprintf(fp, "      %s::Streamer(R__b);\n", IsA()->GetName());

     // Create a loop for IMaxIndex

      fprintf(fp, "      if (*s_MaxIndex <= 0) return; \n\n");
      fprintf(fp, "      for (Int_t indx=0;indx<*s_MaxIndex;indx++) {\n");
      fprintf(fp, "        %s &%s=*(%s+indx);\n",datatype, row, datamem);

      // Stream base class(es) when they have the Streamer() method
      G__BaseClassInfo b(cl);
 
      while (b.Next())
         if (b.HasMethod("Streamer"))
            fprintf(fp, "        %s::Streamer(R__b);\n", b.Name());
 
      // Stream data members
      G__DataMemberInfo m(cl);
 
      while (m.Next()) {
 
         // we skip:
         //  - static members
         //  - members with an ! as first character in the title (comment) field
         //  - the member G__virtualinfo inserted by the CINT RTTI system
 
         if (!(m.Property() & G__BIT_ISSTATIC) &&
             strncmp(m.Title(), "!", 1)        &&
             strcmp(m.Name(), "G__virtualinfo")) {
 
            // fundamental type: short, int, long, etc....
            if (((m.Type())->Property() & G__BIT_ISFUNDAMENTAL) ||
                ((m.Type())->Property() & G__BIT_ISENUM)) {
               if (m.Property() & G__BIT_ISARRAY &&
                   m.Property() & G__BIT_ISPOINTER) {
                  int s = 1;
                  for (int dim = 0; dim < m.ArrayDim(); dim++)
                     s *= m.MaxIndex(dim);
                  if (!decli) {
                     fprintf(fp, "        int R__i;\n");
                     decli = 1;
                  }
                  fprintf(fp, "        for (R__i; R__i < %d; R__i++)\n", s);
                 if (i == 0) {
                     fprintf(stderr,"*** Datamember %s::%s: array of pointers to fundamental type (need manual intervention)\n", cl.Name(), m.Name());
                     fprintf(fp, "           ;//R__b.ReadArray(%s.%s);\n",row, m.Name());
                  } else {
                     fprintf(fp, "           ;//R__b.WriteArray(%s.%s, __COUNTER__);\n",row, m.Name());
                  }
               } else if (m.Property() & G__BIT_ISPOINTER) {
                  if (i == 0) {
                     fprintf(stderr,"*** Datamember %s::%s: pointer to fundamental type (need manual intervention)\n", cl.Name(), m.Name());
                     fprintf(fp, "        //R__b.ReadArray(%s.%s);\n",row, m.Name());
                  } else {
                     fprintf(fp, "        //R__b.WriteArray(%s.%s, __COUNTER__);\n",row, m.Name());
                  }
               } else if (m.Property() & G__BIT_ISARRAY) {
                  if (i == 0) {
                     if (m.ArrayDim() > 1) {
                        if ((m.Type())->Property() & G__BIT_ISENUM)
                           fprintf(fp, "        R__b.ReadStaticArray((Int_t*)(%s.%s));\n", row,m.Name());
                        else
                           fprintf(fp, "        R__b.ReadStaticArray((%s*)(%s.%s))%s;\n", m.Type()->TrueName(), row, m.Name());
                     } else {
                        if ((m.Type())->Property() & G__BIT_ISENUM)
                           fprintf(fp, "        R__b.ReadStaticArray((Int_t*)(%s.%s));\n", row, m.Name());
                        else
                           fprintf(fp, "        R__b.ReadStaticArray(%s.%s);\n",row, m.Name());
                      }
                  } else {
                     int s = 1;
                     for (int dim = 0; dim < m.ArrayDim(); dim++)
                        s *= m.MaxIndex(dim);
                     if (m.ArrayDim() > 1) {
                        if ((m.Type())->Property() & G__BIT_ISENUM)
                           fprintf(fp, "        R__b.WriteArray((Int_t*)(%s.%s), %d);\n", row,m.Name(), s);
                        else
                           fprintf(fp, "        R__b.WriteArray((%s*)(%s.%s), %d);\n", m.Type()->TrueName(), row, m.Name(), s);
                     } else {
                        if ((m.Type())->Property() & G__BIT_ISENUM)
                           fprintf(fp, "        R__b.WriteArray((Int_t*)(%s.%s), %d);\n", row, m.Name(), s);
                        else
                           fprintf(fp, "        R__b.WriteArray(%s.%s, %d);\n", row, m.Name(), s);
                     }
                  }
               } else if ((m.Type())->Property() & G__BIT_ISENUM) {
                  if (i == 0)
                     fprintf(fp, "        R__b >> (Int_t&)(%s.%s);\n", row, m.Name());
                  else
                     fprintf(fp, "        R__b << (Int_t)(%s.%s);\n", row, m.Name());
               } else {
                  if (i == 0)
                     fprintf(fp, "        R__b >> %s.%s;\n", row, m.Name());
                  else
                     fprintf(fp, "        R__b << %s.%s;\n", row, m.Name());
               }
            } else {
               // we have an object
               if (m.Property() & G__BIT_ISARRAY &&
                   m.Property() & G__BIT_ISPOINTER) {
                  int s = 1;
                  for (int dim = 0; dim < m.ArrayDim(); dim++)
                     s *= m.MaxIndex(dim);
                  if (!decli) {
                     fprintf(fp, "        int R__i;\n");
                     decli = 1;
                  }
                  fprintf(fp, "        for (R__i; R__i < %d; R__i++)\n", s);
                  if (i == 0) {
                     fprintf(fp, "           R__b >> (%s.%s)[R__i];\n", row, m.Name());
                  } else {
                     fprintf(fp, "           R__b << (%s.%s)[R__i];\n", row, m.Name());
                  }
               } else if (m.Property() & G__BIT_ISPOINTER) {
                  // This is always good. However, in case of a pointer
                  // to an object that is guarenteed to be there and not
                  // being referenced by other objects we could use
                  //     xx->Streamer(b);
                  // Optimize this with control statement in title.
                  if (PointerToPointer(m)) {
                     if (i == 0) {
                        fprintf(stderr,"*** Datamember %s::%s: pointer to pointer (need manual intervention)\n", cl.Name(), m.Name());
                        fprintf(fp, "        //R__b.ReadArray(%s.%s);\n", row, m.Name());
                     } else {
                        fprintf(fp, "        //R__b.WriteArray(%s.%s, __COUNTER__);\n", row, m.Name());
                     }
                  } else {
                     if (strstr(m.Type()->Name(), "TClonesArray")) {
                        fprintf(fp, "        (%s.%s)->Streamer(R__b);\n", row, m.Name());
                     } else {
                        if (i == 0)
                           fprintf(fp, "        R__b >> %s.%s;\n", row, m.Name());
                        else {
                           if (m.Type()->IsBase("TObject") && m.Type()->IsBase("TArray"))
                              fprintf(fp, "        R__b << (TObject*)(%s.%s);\n", row, m.Name());
                           else
                              fprintf(fp, "        R__b << %s.%s;\n", row, m.Name());
                        }
                     }
                  }
               } else if (m.Property() & G__BIT_ISARRAY) {
                  int s = 1;
                  for (int dim = 0; dim < m.ArrayDim(); dim++)
                     s *= m.MaxIndex(dim);
                  if (!decli) {
                     fprintf(fp, "        int R__i;\n");
                     decli = 1;
                  }
                  fprintf(fp, "        for (R__i; R__i < %d; R__i++)\n", s);
                  fprintf(fp, "           (%s.%s)[R__i].Streamer(R__b);\n", row, m.Name());
               } else {
                  if ((m.Type())->HasMethod("Streamer"))
                     fprintf(fp, "        (%s.%s).Streamer(R__b);\n", row, m.Name());
                  else {
                     if (i == 0)
                        fprintf(stderr, "*** Datamember %s::%s: object has no Streamer() method (need manual intervention)\n",
                                cl.Name(), m.Name());
                     fprintf(fp, "        //(%s.%s).Streamer(R__b);\n", row, m.Name());
                  }
               }
            }
         }
      }

   fprintf(fp, "     }\n");
   }
   fprintf(fp, "   }\n");
   fprintf(fp, "}\n\n");
   if (!fl) fclose(fp);  // Close file if that was opened with this method

   // Clean things

   if (thisclassname) delete [] thisclassname;
   if (tablename) delete [] tablename;
}

//______________________________________________________________________________
void St_Table::Reset(Int_t c)
{
  if (s_Table) ::memset(s_Table,c,*s_Size*fN);
}

//______________________________________________________________________________
void St_Table::Set(Int_t n, Char_t *array)
{
   // Set array size of St_Table object to n longs and copy array.
   // If n<0 leave array unchanged.
 
   if (n < 0) return;
   if (fN != n) Delete();

   SetfN(n);

   if (fN == 0) return;
   if (!s_Table) s_Table = Create();
   Copy(s_Table,array);
   *s_MaxIndex = n;
}
 
//_______________________________________________________________________
void St_Table::Streamer(TBuffer &b)
{
   // Stream a St_Table object.
   // Stream an object of class St_Table.


   if (b.IsReading()) {
     TNamed::Streamer(b);
     b >> fN;
     StreamerHeader(b);
     LinkHeader();
    //   Create a table to fit nok rows
     Set(*s_MaxIndex);
     printf(" Read:s_Size = %i fN = %i \n", *s_Size, fN);
   } else {
      TNamed::Streamer(b);
      b << fN;
      StreamerHeader(b);
      printf(" Write: s_Size = %i  fN = %i head size =  %i \n", *s_Size, fN, sizeof(table_head_st));
   }
}
 
//_______________________________________________________________________
void St_Table::StreamerHeader(TBuffer &b)
{
  if (b.IsReading()) 
  {
   b.ReadStaticArray(s_TableHeader->name);         /* table name */
   b.ReadStaticArray(s_TableHeader->type);         /* table type */
   b >> s_TableHeader->maxlen;       /* # rows allocated */
   b >> s_TableHeader->nok;          /* # rows filled */
   b >> s_TableHeader->rbytes;       /* number of bytes per row */
   b >> s_TableHeader->dsl_pointer;  /* swizzled (DS_DATASET_T*) */
   b >> s_TableHeader->data_pointer; /* swizzled (char*) */
  }
  else {
   b.WriteArray(s_TableHeader->name,20);         /* table name */
   b.WriteArray(s_TableHeader->type,20);         /* table type */
   b << s_TableHeader->maxlen;       /* # rows allocated */
   b << s_TableHeader->nok;          /* # rows filled */
   b << s_TableHeader->rbytes;       /* number of bytes per row */
   b << s_TableHeader->dsl_pointer;  /* swizzled (DS_DATASET_T*) */
   b << s_TableHeader->data_pointer; /* swizzled (char*) */
  }
}

#if 0
//_______________________________________________________________________
TBuffer &operator>>(TBuffer &buf, St_Table *&obj)
{
   // Read St_Table object from buffer. Declared in ClassDef.
 
   obj = (St_Table *) TArray::ReadArray(buf, St_Table::Class());
   return buf;
}

#endif

//_______________________________________________________________________
Int_t St_Table::SetfN(Long_t len)
{
   fN = len;
   if (s_TableHeader)
   {
//     s_TableHeader->nok = fN;
     s_TableHeader->maxlen = fN;
   }
    return fN;
}


