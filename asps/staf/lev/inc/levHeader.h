void levOse(void);
void levExeName(char *out);
boolean levFindVersionTable(DS_DATASET_T **ppVer);
boolean levFindEnvironmentTable(DS_DATASET_T **ppEnv);
boolean levNewTable(DS_DATASET_T *parent,char *name,char *colType,size_t nRow,
  DS_DATASET_T **handleOut);
void levErrorMess(int xx);
boolean levAddOneRow(int *nRow,const char *col0,const char *col1,
  DS_DATASET_T *pTable);
void ConvertToDigits(char *xx);
boolean levStopTime(DS_DATASET_T *pEnv,const char *comment);
boolean levRegisterVersion(DS_DATASET_T *pVer,char *name,enum type,char *ver);
boolean levMake_gConfigBranch(void);
boolean levAddEnvironmentTable(DS_DATASET_T **ppEnv);
boolean levAddVersionTable(DS_DATASET_T **ppVer);
boolean levRegisterEnvironment(DS_DATASET_T *pEnv,const char *comment);
