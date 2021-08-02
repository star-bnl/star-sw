#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>


#include "PdfIndex.hh"

index_entry::index_entry(char *n, int p, float pos) {
  strcpy(name, n);
  page = p;
  position = pos;
  child = NULL;
  next = NULL; 
}

int PdfIndex::CreateIndexedFile(char *input_fn, char *output_fn)
{
  int ret = ReadFile(input_fn);
  if(ret < 0) {
    return -1;
  }
 
  add_index();

  ret = WriteFile(output_fn);
  return ret;
}
  
void PdfIndex::set_orig_buff(char *buff, int sz)
{
  orig_buff = buff;
  orig_size = sz;
}


int PdfIndex::ReadFile(char *fn)
{
  struct stat64 statbuff;
  if(stat64(fn, &statbuff) < 0) {
    perror("stat");
    return -1;
  }

  orig_size = statbuff.st_size;
  orig_buff = (char *)malloc(orig_size);
  if(!orig_buff) {
    perror("malloc");
    return -1;
  }
    
  int fd = open(fn, O_RDONLY);
  if(fd < 0) {
    perror("open");
    return -1;
  }

  int ret = read(fd, orig_buff, orig_size);
  if(ret != orig_size) {
    printf("Only read %d of %d\n",ret,orig_size);
    close(fd);
    return -1;
  }
    
  //char *c = orig_buff + orig_size - 5;
  //printf("Read %d %c%c%c%c\n",ret,*c,*(c+1),*(c+2),*(c+3));
  close(fd);

  return 0;
}

int PdfIndex::WriteFile(char *fn)
{
  int fd = open(fn, O_WRONLY | O_CREAT, 0666);
  if(fd < 0) {
    perror("open");
    return -1;
  }
  int ret = write(fd, orig_buff, orig_size);
  if(ret != orig_size) {
    printf("Only wrote %d of %d\n", ret, orig_size);
    //perror("write");
    close(fd);
    return -1;
  }

  ret = write(fd, add_buff, add_size);
  if(ret != add_size) {
    // perror("write2");
    printf("Only wrote %d of %d\n", ret, orig_size);
    close(fd);
    return -1;
  }

  close(fd);
  return 0;
}
  
PdfIndex::PdfIndex()
{
  index = NULL;
  orig_buff = NULL;
  add_buff = NULL;
  xref_array = NULL;
  orig_size = 0;
  add_size = 0;
  n_constructed = 0;
}

void PdfIndex::remove(index_entry *e)
{
  if(!e) return;
  remove(e->child);
  remove(e->next);
    
  //printf("Deleting element %s\n",e->name);
  delete e;
} 

void PdfIndex::set_object(int *obj, index_entry *idx)
{
  idx->obj = *obj;
  //printf("[%d] %s\n",idx->obj, idx->name);

  if(idx->child) {
    (*obj)++;
    set_object(obj, idx->child);
  }
  if(idx->next) {
    (*obj)++;
    set_object(obj, idx->next);
  }
}

void PdfIndex::fillObject(index_entry *idx, int parobj, int prevobj)
{
  idx->parobj = parobj;
  if(prevobj > 0) idx->prevobj = prevobj;
    
  idx->nobj = 0;
  index_entry *c = idx->child;
  if(c) {
    idx->fobj = c->obj;
    idx->nobj++;
    while(c->next) {
      c = c->next;
      idx->nobj++;
    }
    idx->lobj = c->obj;
  }
  if(idx->next) idx->nextobj = idx->next->obj;
  else idx->nextobj = -1;
    
  char prev[26];
  char next[26];
  char first[26];
  char last[26];
  char num[26];
  if(idx->prevobj > 0) sprintf(prev, "/Prev %d 0 R ", idx->prevobj);
  else prev[0] = '\0';

  if(idx->nextobj > 0) sprintf(next, "/Next %d 0 R ", idx->nextobj);
  else next[0] = '\0';

  if(idx->nobj > 0) sprintf(num, "/Count -%d ", idx->nobj);
  else num[0] = '\0';

  if(idx->nobj > 0) {
    sprintf(first, "/First %d 0 R ", idx->fobj);
    sprintf(last, "/Last %d 0 R ", idx->lobj);
  }
  else {
    first[0] = '\0';
    last[0] = '\0';
  }

  sprintf(idx->data, "%d 0 obj << /Title (%s) /Parent %d 0 R %s%s%s%s%s/Dest [%d /XYZ 0 %d 0] >> endobj\n",
	  idx->obj,
	  idx->name,
	  idx->parobj,
	  prev,
	  next,
	  num,
	  first,
	  last, 
	  idx->page-1, 
	  (int)((1.0 - idx->position)*790.0) );
}

index_entry *PdfIndex::find_entry(index_entry *c, int obj) {
  if(c->obj == obj) return c;
  if(c->child) return find_entry(c->child, obj);
  if(c->next) return find_entry(c->next, obj);
  return NULL;
}

void PdfIndex::fillNext(index_entry *c)
{
  if(!c) return;
  if(c->child) {
    fillObject(c->child, c->obj, -1);
    fillNext(c->child);
  }
  if(c->next) {
    fillObject(c->next, c->parobj, c->obj);
    fillNext(c->next);
  }
}

int PdfIndex::construct_objects(int obj, int dict_obj) {
  int max_obj=obj;
  set_object(&max_obj, index);   // sets all the obj values
    
  fillObject(index, dict_obj,-1);
  fillNext(index);

  n_constructed = max_obj - obj + 1;
  return max_obj;
}

index_entry *PdfIndex::add(index_entry *p, char *name, int page, float pos)
{
  index_entry *n = new index_entry(name, page, pos);
   
  if(index == NULL) {
    index = n;
  }
  else if(p == NULL) {
    index_entry *e = index;
    while(e->next) e = e->next;
    e->next = n;
  }
  else if(p->child == NULL) {
    p->child = n;
  }
  else {
    index_entry *e = p->child;
    while(e->next) e = e->next;
    e->next = n;
  }

  return n;    
}

index_entry *PdfIndex::add_child(index_entry *p, char *name, int page, float pos)
{
  if((p == NULL) && (index == NULL)) {
    index = new index_entry(name, page, pos);
    return index;
  }

  if((index == NULL) || (p == NULL) || (p->child != NULL)) {
    printf("AddChild: Error index=0x%lx p=0x%lx child=0x%lx",(long) index, (long) p, (long) (p ? p->child : 0));
    return NULL;
  }

  index_entry *n = new index_entry(name, page, pos);
   
  p->child = n;
  return n;
}

index_entry *PdfIndex::add_sibling(index_entry *p, char *name, int page, float pos)
{  
  if((p == NULL) && (index == NULL)) {
    index = new index_entry(name, page, pos);
    return index;
  }

  if((index == NULL) || (p == NULL) || (p->next != NULL)) {
    printf("AddSib:  Error index=0x%lx p=0x%lx child=0x%lx",(long) index,(long) p, (long) (p ? p->next : 0));
    return NULL;
  }
  
  index_entry *n = new index_entry(name, page, pos);
  p->next = n;
  return n;
}


void PdfIndex::dump(index_entry *e, int tabs, int format) {
  if(!e) e = index;

  if(format == 0) {
    for(int i=0;i<tabs;i++) {
      printf("    ");
    }
    printf("%s (%d %f)\n",e->name,e->page,e->position);
  }

  if(format == 1) {
    printf("%ld --------------\n%s------------\n",(long) strlen(e->data), e->data);
  }
  if(e->child) dump(e->child, tabs+1, format);
  if(e->next) dump(e->next, tabs, format);
}
    
void PdfIndex::reset()
{
  if(index) { remove(index); index = NULL; };
  if(orig_buff) { free(orig_buff); orig_buff = NULL; };
  if(add_buff) { free(add_buff); add_buff = NULL; };
  if(xref_array) { free(xref_array); xref_array = NULL; };

  orig_size = 0;
  add_size = 0;
  add_alloc_size = 0;
  n_constructed = 0;
}

PdfIndex::~PdfIndex() {
  reset();

}
  
int PdfIndex::get_trailer_token(char *xref, char *str)
{
  char *buff = orig_buff;
  char *start = xref;
    
  char *x = start;
  int prev=-1;
    
  for(;;) {
    if(memcmp(x, "%%EOF", 5) == 0) {
      if(prev > 0) {
	return get_trailer_token(buff + prev, str);
      }
      else {
	return -1;
      }
    }
      
    if(memcmp(x, "/Prev", 5) == 0) {
      x += 6;
      //int prev = atoi(x);
    }
      
    if(memcmp(x, str, strlen(str)) == 0) {
      x += strlen(str)+1;
      int obj = atoi(x);
      return obj;
    } 
      
    x++;
      
  }   
    
  return -1;
}
  
char *PdfIndex::get_xref_pos(char *start, int obj)
{
  char *buff = orig_buff;
  
  char *x = start;
  while(isalnum(*x)) x++; while(!isalnum(*x)) x++;

  //int off = (x-start);
  
  for(;;) {
    if(memcmp(x, "trailer", 7) == 0) {
      break;   // Move on to previous xref...
    }
    
    int first = atoi(x);
    //    char *f=x;
    while(isalnum(*x)) x++; while(!isalnum(*x)) x++;
    //  char *l=x;
    
    int n = atoi(x);
    while(!isspace(*x)) x++; while(isspace(*x)) x++;
 
    
    if((obj >= first) && (obj < first + n)) {
      x += 20 * (obj-first);
      int offset = atoi(x);
      return buff + offset;
    }
    
    x += 20 * (n);
  }

  // we are at the next trailer...
  // look for /Prev and up through next %%EOF
  for(;;) {
    x++;
    if(memcmp(x, "/Prev", 5) == 0) {
      x += 6;
      int off = atoi(x);
      return get_xref_pos(buff + off, obj);
    }

    if(memcmp(x, "%%EOF", 5) == 0) {
      //printf("Error.. no object %d\n",obj);
      return NULL;
    } 
  }   
}

void PdfIndex::writeDictionary(index_entry *e, XrefEntry *xref, int *xsz, char *data, int *dsz, int osz)
{
  if(!e) return;

  //printf("wd: %d %d %d\n",e->obj, *xsz, *dsz);
  xref[*xsz].offset = *dsz + osz;
  xref[*xsz].object = e->obj;
  strcpy(data + *dsz, e->data);
  (*dsz) += strlen(e->data);
  (*xsz)++;
    
  writeDictionary(e->child, xref, xsz, data, dsz, osz);
  writeDictionary(e->next, xref, xsz, data, dsz, osz);
}

int PdfIndex::lastTopLevelIndexEntryObject()
{
  index_entry *e = index;
  while(e->next) {
    e = e->next; 
  }

  return e->obj;
}

int PdfIndex::nTopLevelEntries()
{
  int n=1;
  index_entry *e = index;
  while(e->next) {
    n++;
    e = e->next;
  }
  return n;
} 
  
int PdfIndex::getObjectOffset(XrefEntry *xref, int obj)
{
  //printf("get %d\n", obj);
  while(xref->object != obj) {
    //printf("   obj %d off %d",xref->object, xref->offset);
    xref++;
  }
  return xref->offset;
}

void PdfIndex::add_index()
{
  // find orig_xref
  // Get trailer...
  char *startxref = orig_buff + orig_size - 9;
  while(memcmp(startxref, "startxref", 9) != 0) {

    // printf("::: %c%c%c%c\n",*startxref,*(startxref+1),*(startxref+2),*(startxref+3));
    startxref--;
    if(startxref == orig_buff) {
      perror("bad startxref");
      return;
    }
  }
  int orig_xref_offset = atoi(startxref + 10);
  orig_xref = orig_buff + orig_xref_offset;
    
  int root_obj = get_trailer_token(orig_xref, (char *)"/Root");
  int f_obj = get_trailer_token(orig_xref, (char *)"/Size");  
  char *root_buff = get_xref_pos(orig_xref, root_obj);

  //f_obj++;

  int dict_obj = f_obj;
    
  construct_objects(f_obj+1, dict_obj);

  add_alloc_size = (n_constructed + 10) * 256;
  add_buff = (char *)malloc(add_alloc_size);   // size is heuristic...
  add_size = 0;
  xref_array = (XrefEntry *)malloc(sizeof(XrefEntry)*(n_constructed + 10));
  int xref_sz = 0;

  // write dictionary entries
  writeDictionary(index, xref_array, &xref_sz, add_buff, &add_size, orig_size);
    
  // write dictionary 
  // printf("after writeDictionary:  add_size=%d\n",add_size);

  char *dict = add_buff + add_size;
  sprintf(dict, "%d 0 obj << /Type /Outlines /First %d 0 R /Last %d 0 R /Count %d >> endobj\n",
	  dict_obj, f_obj+1, lastTopLevelIndexEntryObject(), nTopLevelEntries());
  xref_array[xref_sz].object = dict_obj;
  xref_array[xref_sz].offset = add_size + orig_size;
  add_size += strlen(dict);
  xref_sz++;

  // Write new ROOT
  char *x = root_buff;
  char *nx = add_buff + add_size;
    
  int new_root_pos = orig_size + add_size;

  while(memcmp(x, ">>", 2) != 0) {

    if(memcmp(x, "/Outlines", 9) == 0) {
      x++;
      while ((*x != '/') &&
	     (memcmp(x,">>",2) != 0)) {
	x++;
      }
      continue;
    }
    
    *nx = *x;
    x++;
    nx++;
  }
  sprintf(nx, " /Outlines %d 0 R\n\t/PageMode /UseOutlines\n>> \n endobj\n",dict_obj);
  add_size += strlen(add_buff + add_size);
    
  // write xref
  int xref_offset = orig_size + add_size;
  char *cpos = add_buff + add_size;
  sprintf(cpos, "xref\n%d 1\n%010d 00000 n\n%d %d\n",root_obj, new_root_pos, f_obj, n_constructed+1);
  add_size += strlen(cpos);
  cpos += strlen(cpos);

  for(int i=f_obj;i<dict_obj + n_constructed+1;i++) {
    int off = getObjectOffset(xref_array, i);
    sprintf(cpos, "%010d 00000 n\n",off);
    //  printf("(%s) strlen cpos = %d  // off=%d\n",cpos, strlen(cpos),off);
    add_size += strlen(cpos);
    cpos += strlen(cpos);
  }

  // write trailer
  sprintf(cpos, "trailer\n<< /Size %d /Root %d 0 R /Prev %d >>\nstartxref\n%d\n%%%%EOF\n",
	  f_obj + n_constructed + 1, root_obj, orig_xref_offset, xref_offset);
  add_size += strlen(cpos);	    
  cpos += strlen(cpos);



  //printf("cpos-add_buff = %d   add_size = %d\n",cpos - add_buff, add_size);
  //dump();
}
