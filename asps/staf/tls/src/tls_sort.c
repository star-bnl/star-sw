/* f77 interface to QSortTable	

INTEGER FUNCTION tls_index_sort_i( nrows, el1, el2, index, ind_size )
INTEGER FUNCTION tls_index_sort_r( nrows, el1, el2, index, ind_size )
INTEGER FUNCTION tls_quick_sort_i( nrows, el1, el2, table )
INTEGER FUNCTION tls_quick_sort_r( nrows, el1, el2, table )
*/
#include <stdlib.h>
#include <stdio.h>

void QSortTable(void *key,void *base, size_t count, size_t size, int flag);

int tls_index_sort_ii_(int* nrows,char *el1, char *el2, int* index, int* ind_size)
{ 
  int size;
  
  size = el2 - el1;
  QSortTable(el1,el1, *nrows, size, 0x30);
  return 3585;
}
int tls_index_sort_i_(int* nrows,char *el1, char *el2, int* index, int* ind_size)
{ 
  int size;
  
  size = el2 - el1;
  QSortTable(el1,el1, *nrows, size, 0x31);
  return 3585;
}
int tls_index_sort_r_(int* nrows,char *el1, char *el2, int* index, int* ind_size)
{ 
  int size;
  
  size = el2 - el1;
  QSortTable(el1,el1, *nrows, size, 0x32);
  return 3585;
}
int tls_index_sort_d_(int* nrows,char *el1, char *el2, int* index, int* ind_size)
{ 
  int size;
  
  size = el2 - el1;
  QSortTable(el1,el1, *nrows, size, 0x33);
  return 3585;
}

int tls_quick_sort_ii_( int* nrows, char *el1, char* el2, char *table )
{
  int size;
  size = el2 - el1;
  QSortTable(el1,table, *nrows, size, 0x00);
  return 3585;
}

int tls_quick_sort_i_( int* nrows, char *el1, char* el2, char *table )
{
  int size;
  size = el2 - el1;
  QSortTable(el1,table, *nrows, size, 0x01);
  return 3585;
}
int tls_quick_sort_r_( int* nrows, char *el1, char* el2, char *table )
{
  int size;
  size = el2 - el1;
  QSortTable(el1,table, *nrows, size, 0x02);
  return 3585;
}
int tls_quick_sort_d_( int* nrows, char *el1, char* el2, char *table )
{
  int size;
  size = el2 - el1;
  QSortTable(el1,table, *nrows, size, 0x03);
  return 3585;
}
