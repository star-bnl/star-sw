#define MAX_LINES_CLICK_PART 800
typedef int myBool;
#define VWSTRING	0
#define VWNUMBER	1
#define VWHEX		2
void Format(int,char*,float);
float ValueWrapper(int wh_gDs,size_t colNum,int row,int subscript);
myBool DoCutsWrapper(int max8,char *ba,char *cuts,int wh_gDs);
void SetToDatasetInfo(int,char*,int);
void SetToTableInfo(char*,size_t*,int,char*,int);
void DatasetList(int*,int*,int,char *x,int size);
void TableList(char*,int,int*,int*,int,char *x,int size);
void ColumnList(char*,int,int*,int*,int,char*,int,int*);
/* #define STANDALONE */
#define PP printf(
void Err(int x);
#define T41 49
#define TRI_LEAF       0
#define TRI_CONTRACTED 1
#define TRI_EXPANDED   2
EXTERN int gNumDatasetWindows,gVWType,gDataType;
EXTERN char gStr[100],gPass[2000];
EXTERN myBool gCalculateAverages,gTableValueError;
