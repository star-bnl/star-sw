#ifndef _PDF_INDEX_HH_
#define _PDF_INDEX_HH_


//***********************************************************
//  Utility Class For adding index to existing PDF file...
//***********************************************************

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

class index_entry
{
public:
  char name[100];
  index_entry *child;
  index_entry *next;
  int page;
  float position;
  
  // filled by construct objects...
  int obj;
  int prevobj;
  int parobj;
  int nextobj;
  int nobj;
  int fobj;
  int lobj;
  char data[256];
  int sz;
  
  index_entry(char *n, int p, float pos);
};

struct XrefEntry {
  int offset;
  int object;
};

class PdfIndex
{
public:
  // (1) Add index elements with "add"
  index_entry *add(index_entry *p, char *name, int page, float pos);

  index_entry *add_child(index_entry *p, char *name, int page, float pos);
  index_entry *add_sibling(index_entry *p, char *name, int page, float pos);
  
  // Then
  // *   Create index from file to file in one shot...
  //
  int CreateIndexedFile(char *input_fn, char *output_fn);


  // (Or)
  // *   Read a file, add_index, then write file...
  //
  int ReadFile(char *fn);
  void add_index();
  int WriteFile(char *fn);
 
  // (Or)
  // *   Set a buffer with the original file, then add_index() and WriteFile.
  void set_orig_buff(char *buff, int sz);


  PdfIndex();
  ~PdfIndex();

  void dump(index_entry *e=NULL, int tabs=0, int format=0);

  // If creating more than one file, call reset before adding entries for the second file
  void reset();

private:
  index_entry *index;

  char *orig_buff;
  char *add_buff;
  XrefEntry *xref_array;
  int orig_size;
  int add_size;
  int add_alloc_size;
  int n_constructed;
  char *orig_xref;

  void remove(index_entry *e);
  void set_object(int *obj, index_entry *idx);
  void fillObject(index_entry *idx, int parobj, int prevobj);
  index_entry *find_entry(index_entry *c, int obj); 
  void fillNext(index_entry *c);
  int construct_objects(int obj, int dict_obj);
  int get_trailer_token(char *xref, char *str);
  char *get_xref_pos(char *start, int obj);
  void writeDictionary(index_entry *e, XrefEntry *xref, int *xsz, char *data, int *dsz, int osz);
  int lastTopLevelIndexEntryObject();
  int nTopLevelEntries();
  int getObjectOffset(XrefEntry *xref, int obj);
};

#endif
