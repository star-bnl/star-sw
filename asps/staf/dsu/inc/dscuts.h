
int Array(
  DS_DATASET_T *dsPtr, size_t colNum,int tentSubscript,
  int *off, size_t *dim, size_t *array_size_t, char **typeName);
#define DSU_FLOAT   0
#define DSU_INTEGER 1
#define DSU_STRING  2
#define DSU_HEX   3

#define DSU_SIZE_OF_GSTR 100

#define SUB_SOURCE_GENERIC 0
#define SUB_SOURCE_WINDOW 1
#define SUB_SOURCE_CUTS 2
int TableValue(int *dataType,char *xx,float *fv,long *iv,size_t row,
  size_t colNum,DS_DATASET_T *tp,int subscript);
int DoCuts(size_t,char*,char*,DS_DATASET_T*);
int RowPassedCuts(char *ba,long rowIndex);

