#include <stdio.h>
#include "PdfIndex.hh"

int main(int argc, char *argv[])
{
  PdfIndex idx;

  index_entry *a, *b, *c;
  a = idx.add(NULL, "page 1", 1, 0);
  b = idx.add(NULL, "page 2", 2, 0);
  c = idx.add(NULL, "page 3", 3, 0);

  if(idx.CreateIndexedFile(argv[1], argv[2]) < 0) {
    printf("Could not create indexed file: %s\n",strerror(errno));
  }
  else {
    printf("Created indexed file %s from %s\n",argv[2],argv[1]);
  }
}
