
myBool Array(
  DS_DATASET_T *dsPtr, size_t colNum,int tentSubscript,
  int *off, size_t *dim, size_t *array_size_t, char **typeName);
#define FLOAT   0
#define INTEGER 1
#define STRING  2
#define HEX	3

#define SUB_SOURCE_GENERIC 0
#define SUB_SOURCE_WINDOW 1
#define SUB_SOURCE_CUTS 2
myBool TableValue(int *dataType,char *xx,float *fv,long *iv,size_t row,
  size_t colNum,DS_DATASET_T *tp,int subscript);
myBool DoCuts(size_t,char*,char*,DS_DATASET_T*);
myBool RowPassedCuts(char *ba,long rowIndex);

