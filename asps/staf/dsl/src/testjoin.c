/* Copyright 1995, Lawrence Berkeley Laboratory */

/* testjoin.c - routines to test join functions */
/*
modification history
--------------------
20feb95,whg  written.
*/

/*
DESCRIPTION
TBS ...
*/
#include <stdlib.h>
#define DS_PRIVATE
#include "dstype.h"
static void dumpTable(DS_DATASET_T *pTable, char *msg);
/****************************************************************************
* 
* definitions for self-join tests
*/
typedef struct staff_t {
	DS_CHAR name[15];
	DS_SHORT empNum;
	DS_LONG salary;
	DS_SHORT supNum;
}STAFF_T;

#define STAFF_S "struct staff_t {"\
	"char name[15];"\
	"short empNum;"\
	"long salary;"\
	"short supNum;}"
	
static STAFF_T staff[] = {
	{"Charles",	 1, 90000,  0},
	{"Bob",		19, 50000, 11},
	{"Mary",	10, 80000,  1},
	{"John",	17, 85000, 10},
	{"James",	11, 95000,  1},
	{"Ted",		15, 40000, 10},
	{"Jane",	22, 60000, 11},
	{"Fred",	13, 70000, 10}};

static size_t staffRowCount = sizeof(staff)/sizeof(staff[0]);

static char *selfAlias = "emp sup";

static char *selfJoin = "{sup.empNum emp.supNum}";

static char *selfProject = "{emp.salary empSalary, emp.name empName,"
      "sup.name supName, sup.salary supSalary}";
      
static char *removeName = "{selfJoin.empName myName, empSalary mySalary, supSalary}";               

/****************************************************************************
*
* selfJoin - test of dsEquijoin and dsProjectTable
*
*/
int testEquijoin()
{
	DS_DATASET_T *pProject, *pSelfJoin;
	DS_DATASET_T staffTable, *pStaffTable = &staffTable;
	
	/*
	 * create staff table
	 */
	if (!dsInitTable(pStaffTable, "staff", STAFF_S, staffRowCount, staff)) {
		dsPerror("dsNewTable failed for staff");
		return FALSE;
	}                                     
	dumpTable(pStaffTable, "staff table:");
	/*
	 * create target for self join
	 */
	if (!dsTargetTable(&pSelfJoin, "selfJoin", "self_t", pStaffTable,
		pStaffTable, selfAlias, selfProject)) {
		dsPerror("dsTargetTable failed for selfJoin");
		return FALSE;
	}
	/*
	 * do join
	 */
	if (!dsEquijoin(pSelfJoin, pStaffTable, pStaffTable,
		selfAlias, selfJoin, selfProject)) { 
		dsPerror("dsEquijoin failed");
		return FALSE;
	}
	dumpTable(pSelfJoin, "selfJoin table:");
	/* 
	 * create target table for test of dsProjectTable
	 */
	if (!dsTargetTable(&pProject, "project", "proj_t", pSelfJoin, NULL,
		NULL, removeName)) {
		dsPerror("dsTargetTable failed for removeName");
		return FALSE;
	}
	/*
	 * do project
	 */
	if (!dsProjectTable(pProject, pSelfJoin, removeName)) {
		dsPerror("dsProjectTable failed");
		return FALSE;
	}
	dumpTable(pProject, "project table:");
	dsFreeDataset(pSelfJoin);
	dsFreeDataset(pProject);
	return TRUE;
}
/***************************************************************************/
		
	/* typedefs for variables */
typedef struct one_t {DS_FLOAT x, t; DS_LONG key;} TYPE_ONE_T;
typedef struct two_t {DS_LONG key; DS_FLOAT z, y;} TYPE_TWO_T;
typedef struct join_t {DS_FLOAT x, y, z, t;} JOIN_TYPE_T;

	/* type specifiers for tables */
#define TYPE_ONE_S "struct one_t {float x, t; long key;}"
#define TYPE_TWO_S "struct two_t {long key; float z, y;}"
#define JOIN_TYPE_S "struct join_t {float x, y, z, t;}"

#define DIM_ONE 10
#define DIM_TWO 20
#define KEY_ONE(n) ((n)%3)
#define KEY_TWO(n) ((n)%5)
#define T_VAL(n) (float)((n) + 100)
#define X_VAL(n) (float)((n) + 200)
#define Y_VAL(n) (float)((n) + 300)
#define Z_VAL(n) (float)((n) + 400)

	/* prototypes fot test functions */
static void fillVarOne(TYPE_ONE_T *var, unsigned count);
static void fillVarTwo(TYPE_TWO_T *var, unsigned count);
static int checkJoin(JOIN_TYPE_T *join, unsigned count);
/****************************************************************************
*
* testHashjoin - test program for hash join
*/
typedef struct hash_t {DS_LONG key, row;} HASH_JOIN_T;
typedef struct joined_t {DS_LONG key, row1, row2;} JOINED_T;
#define HASH1_S "struct hash_join1_t {long key, row1;}"
#define HASH2_S "struct hash_join2_t {long key, row2;}"
#define HASH_ROW_COUNT 1000
int testHashjoin()
{
	double t, t0;
	size_t i;
	DS_DATASET_T *join1 = NULL, *join2 = NULL, *table1 = NULL, *table2 = NULL;
	HASH_JOIN_T *p1, *p2;
	JOINED_T *j1, *j2;

	if (!dsNewTable(&table1, "table1", HASH1_S, HASH_ROW_COUNT, NULL) ||
		!dsNewTable(&table2, "table2", HASH2_S, HASH_ROW_COUNT, NULL) ||
		!dsTargetTable(&join1, "j1", "join_t", table1, table2, NULL, NULL) ||
		!dsTargetTable(&join2, "j2", "join_t", table1, table2, NULL, NULL) ||
		!dsAllocTables(table1) ||
		!dsAllocTables(table2)) {
		goto fail;
	}
	table1->elcount = table2->elcount = HASH_ROW_COUNT;
	p1 = (HASH_JOIN_T *)table1->p.data;
	p2 = (HASH_JOIN_T *)table2->p.data;

	for (i = 0; i < HASH_ROW_COUNT; i++) {
		p1[i].row = p2[i].row = i;
		p1[i].key = (32*rand() + i)%HASH_ROW_COUNT;
		p2[i].key = (32*rand() + i)%HASH_ROW_COUNT;
	}
	t0 = msecTime(NULL);
	if (!dsEquijoin(join1, table1, table2, NULL, NULL, NULL)) {
		dsPerror("Hash join test failed");
		goto fail;
	}
	t = msecTime(NULL);
	printf("fast join %d rows, time %f\n", join1->elcount, t - t0);

	t0 = msecTime(NULL);
	if (!dsSlowEquijoin(join2, table1, table2, NULL, NULL, NULL)) {
		dsPerror("Slow join test failed");
		goto fail;
	}
	t = msecTime(NULL);
	printf("slow join %d rows, time %f\n", join2->elcount, t - t0);
	
	if (join1->elcount != join2->elcount) {
		dsError("hash and slow join: bad row count");
		goto fail;
	} 
	j1 = (JOINED_T *)join1->p.data;
	j2 = (JOINED_T *)join2->p.data;
	for (i = 0; i < join1->elcount; i++) {
		if (j1[i].key != j2[i].key ||
			j1[i].key != j2[i].key ||
			j1[i].key != j2[i].key) {
			dsPerror("hash and slow join: diff data error");
			goto fail;
		}
	}
	printf("success for fast/slow join test\n");

	if (table1) dsFreeDataset(table1);
	if (table2) dsFreeDataset(table2);
	if (join1) dsFreeDataset(join1);
	if (join2) dsFreeDataset(join2);
	return TRUE;
fail:
	dsPerror("hashJoin failed");
	if (table1) dsFreeDataset(table1);
	if (table2) dsFreeDataset(table2);
	if (join1) dsFreeDataset(join1);
	if (join2) dsFreeDataset(join2);
	return FALSE;
}
/****************************************************************************
*
* testNatural - program to test dsEquijoin
*
*/
int testNatural()
{
	char *a = "one two", *k = "{key}";
	char *s = "{ one.x, y, two.z z}", *t = "{x, y, z, t t}";
	/* program variables */
	TYPE_ONE_T varOne[DIM_ONE];
	TYPE_TWO_T varTwo[DIM_TWO];
	JOIN_TYPE_T *joinVar;
	size_t joinRowCount;
		/* table structs and pointers */
		/* allocate space for tableOne and tableTwo */
	DS_DATASET_T tableOne, *pTableOne = &tableOne;
	DS_DATASET_T tableTwo, *pTableTwo = &tableTwo;
		/* use dynamic memory for join */
	DS_DATASET_T *pJoinTable = NULL;

	/* set data values for varOne and varTwo */
	fillVarOne(varOne, DIM_ONE);
	fillVarTwo(varTwo, DIM_TWO);
	/* fill in descriptor for table one */
	if (!dsInitTable(pTableOne, "table_one", TYPE_ONE_S, DIM_ONE, varOne)) {
		dsPerror("dsNewTable failed for table_one");
		return FALSE;
	}
	/* fill in descriptor for table two */
	if (!dsInitTable(pTableTwo, "table_two", TYPE_TWO_S, DIM_TWO, varTwo)) {
		dsPerror("dsNewTable failed for table_two");
		return FALSE;
	}
	if (!dsTargetTable(&pJoinTable, "join", "join_t", pTableOne, pTableTwo,
		NULL, NULL)) {
		dsPerror("dsTargetTable failed");
		return FALSE;
	}
	if (!dsEquijoin(pJoinTable, pTableOne, pTableTwo, NULL, NULL, NULL)) {
	dsPerror("naturalJoin failed");
	}
	dumpTable(pJoinTable, "Natural join of table_one and table_two:");
	dsFreeDataset(pJoinTable);
	pJoinTable =NULL;
	/* allocate and fill in descriptor for join */
	if (!dsTargetTable(&pJoinTable, "join", "join_t",
		pTableOne, pTableTwo, NULL, t)) {
		dsPerror("dsTargetTable failed for join");
		return FALSE;
	}
	/* perform join of tableOne and tableTwo, project to joinTable */
	if (!dsEquijoin(pJoinTable, pTableOne, pTableTwo, a, k, s)) {

 		dsPerror("dsEquijoin failed");
 		goto fail;
 	}
 	/* get pointer to data and row count of join */
 	if (!dsCellAddress((char **)&joinVar, pJoinTable, 0, 0) ||
 		!dsTableRowCount(&joinRowCount, pJoinTable)) {
 		dsPerror("get data pointer or row count failed");
 		goto fail;
 	}
 	/* verify that join data is correct */
 	if (!checkJoin(joinVar, joinRowCount)) {
 		dsErrorPrint("checkJoin failed\n");
 		goto fail;
 	}
 	dumpTable(pJoinTable, "table for checkJoin:");
 	/* free join table descriptor and join data */	
 	dsFreeDataset(pJoinTable);
 	return TRUE;
 	
 fail:
 	dsFreeDataset(pJoinTable);
	return FALSE; 	
}
/****************************************************************************
*
*/
int dsTestJoin(void)
{
	if (!testEquijoin() ||
		!testHashjoin() ||
	    !testNatural()) {
	    return FALSE;
	}
	return TRUE;
}
/****************************************************************************
*
* checkJoin - check join data
*
*/
static int checkJoin(JOIN_TYPE_T *join, unsigned count)
{
	size_t i, r1, r2;
	
	for (i = r1 = 0; r1 < DIM_ONE; r1++) {
		for (r2 = 0; r2 < DIM_TWO; r2++) {
			if (KEY_ONE(r1) != KEY_TWO(r2)) {
				continue;
			}
			if (i >= count) {
				printf("to few rows in join\n");
				return FALSE;
			}
			if (join[i].t != T_VAL(r1) ||
				join[i].x != X_VAL(r1) ||
				join[i].y != Y_VAL(r2) ||
				join[i].z != Z_VAL(r2)) {
				printf("join compare failed: r1 %d, r2 %d\n", r1, r2);
				printf("\tt, %g, x %g, y %g, z %g\n",
					join[i].t, join[i].x, join[i].y, join[i].z);
				return FALSE;
			}
			i++;
		}
	}
	if (i != count) {
		printf("to many rows in join\n");
		return FALSE;
	}
	return TRUE; 
}
/****************************************************************************
*
*/
static void dumpTable(DS_DATASET_T *pTable, char *msg)
{
	printf("\n%s\n\n", msg);
	dsPrintTableType(stdout, pTable);
	printf("\n");
	dsPrintTableData(stdout, pTable);
}
/****************************************************************************
*
* fillVarOne - set values for variable one
*
*/
static void fillVarOne(TYPE_ONE_T *var, unsigned count)
{
	size_t i;
	
	for (i = 0; i < count; i++) {
		var[i].t = T_VAL(i);
		var[i].x = X_VAL(i);
		var[i].key = KEY_ONE(i);
	}
}
/****************************************************************************
*
* fillVarOne - set values for variable one
*
*/
static void fillVarTwo(TYPE_TWO_T *var, unsigned count)
{
	size_t i;

	for (i = 0; i < count; i++) {
		var[i].y = Y_VAL(i);
		var[i].z = Z_VAL(i);
		var[i].key = KEY_TWO(i);
	}
}
