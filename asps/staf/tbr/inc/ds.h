int gNDs,gIndent;
#define FORMAT 15
#define COLUMN_LIST_FORMAT "%11s %6s %9s %9s %9s %9s"
#define TABLE_LIST_FORMAT "%11s %4s %7s %7s"
#define TYPE_DS_DATASETS 0
#define TYPE_DS_DATASET  1
#define TYPE_DS_TABLE    2
#define INDENT_INIT (-1)
#define MAX_DATASET        200
#define DATASET_NAME_SIZE   42
#define SLACK 5
typedef struct {
  char triText[3]; /* two-letter substitute for Mac's rotating triangles */
  int triStat;
  char name[DATASET_NAME_SIZE+1];
  char parentName[DATASET_NAME_SIZE+1];
  DS_DATASET_T *dsPtr;
  int indent;		/* for showing hierarchy */
  int type;		/* 0 datasets, 1 dataset, 2 table */
} DATASET_INFO;
DATASET_INFO *gDs[MAX_DATASET];
