/* Copyright 1998, Lawrence Berkeley Laboratory */

/* cpptest.cpp - test C++ wrappers */

/*
modification history
--------------------
20sep98,whg  written.
*/

/*
DESCRIPTION
simple test of C++ API
*/
// TODO test 
#include <iostream.h>
#include <stdlib.h>

#include "dscpp.h"

#define DS_ABORT(m) {cerr << m << " " << __FILE__ << "." << __LINE__ <<\
	 endl << flush; exit(0);}

void printCount(void);
static void basicCppTest(void);
static void joinTestCpp(void);
void xdrTestCpp(dsDataset *dset);
//////////////////////////////////////////////////////////////////////////////
int main(int argc, char *argv[])
{
	basicCppTest();
	joinTestCpp();
	cout << endl  << flush;
	dsAllocStats();
	dsTidHashStats();
	fflush(stdout);
	printCount();
	return 0;
}
/////////////////////////////////////////////////////////////////////////////
static void basicCppTest(void)
{
	char *spec = "struct type_1 {long a, b, c; octet q[100];}";
	char *allSpec = "struct all_t {char c; double d; float f; long l; "
		"octet o; short s; struct aPoint {float x, y, z;}point[100];"
		"unsigned short us; unsigned long ul;}";
	int i;
	DS_LONG lput, lget;
	dsDataset *parent, *child;
	dsField *field;
	dsTable *table;
	dsType *ftype, *type;

	try {
		parent = NULL;
		parent = new dsDataset("12345");
		DS_ABORT("No execption for new dsDataset(\"12345\")");
	}
	catch(dsDataset::dsDatasetConstructFailure) {
		if (parent != NULL) {
			DS_ABORT("Execption failed for new dsDataset(\"12345\")");
		}
		cout << "Success for test of dsDatasetConstructFailure execption\n";
	}
	try {
		table = NULL;
		table = new dsTable("name", "bad spec");
		DS_ABORT("No execption for new dsTable(\"name\", \"bad spec\")");
	}
	catch (dsTable::dsTableConstructFailure) {
		if (table != NULL) {
			DS_ABORT("Execption failed for new dsTable(\"name\", \"bad spec\")");
		}
		cout << "Success for test of dsTableConstructFailure execption\n\n";
	}
	if (!(parent = new dsDataset("parent")) ||
		!(child = dsDataset::create("child")) ||
		!parent->link(child) ||
		!parent->unlink(child) ||
		!parent->link(child) ||
		!(table = new dsTable("table1", spec)) ||
		!child->link(table)  ||
		!child->unlink(table) ||
		!child->link(table)) {
		dsPerror("");
		DS_ABORT("dsDataset::create failed");
	}
	cout << "parent entryCount: "<< parent->entryCount() << endl;
	cout << "parent maxEntryCount: "<< parent->maxEntryCount() << endl;
	cout << "parent refcount: " << parent->refcount() << endl;
	cout << "child refcount: " << child->refcount() << endl;

	cout << "parent->child->table1->rowCount: ";
	cout << parent->findDataset("child")->findTable("table1")->rowCount();
	cout << endl;

	cout << "tableName " << table->name() << endl;
	cout << "maxRowCount " << table->maxRowCount();
	cout << ", rowCount " << table->rowCount() << endl;

	if (table->cellAddress(0, 0) || table->dataAddress()) {
		DS_ABORT("non NULL data address");
	}
	table->realloc(100);
	if (table->setRowCount(200) || !table->setRowCount(2)) {
		DS_ABORT("setRowCount failed");
	}
	if (!table->dataAddress() || !table->cellAddress(1,1)) {
		DS_ABORT("NULL data address");
	}
	lput = 123;
	if (!table->putCell((char *)&lput, 1, 1) ||
		!table->getCell((char *)&lget, 1, 1) || lget != 123) {
		DS_ABORT("putCell/getCell failed");
	}
	cout << "maxRowCount " << table->maxRowCount();
	cout << ", rowCount " << table->rowCount() << endl;

	table->setRowCount(50);
	cout << "maxRowCount " << table->maxRowCount();
	cout << ", rowCount " << table->rowCount() << endl;

	if (!(table = dsTable::create("table2", allSpec))) {
		DS_ABORT("dsTable::create failed");
	}
	type = table->rowType();
	cout << "\nrowTypeSpec: " << type->specifier() << endl;
	cout << "\nrowTypeName " << type->name();
	cout << ", fieldCount " << type->fieldCount();
	cout << ", size " << type->size();
	cout << ", stdsize " << type->stdsize() << "\n\n";
	if (type->isSpecifier(spec) || !type->isSpecifier(allSpec)) {
		DS_ABORT("isSpecifier failed");
	}
	for (i = 0; field = type->field(i); i++) {
		if (type->field(field->name()) != field) {
			DS_ABORT("type->field(field->name) failed");
		}
		ftype = field->type();
		cout << "fieldName " << field->name();
		cout << ", type " << ftype->name();
		cout << ", code " << ftype->code();
		cout << ", dimCount " << field->dimCount();
		if (0 != field->dimCount()) {
			cout << " dim " << field->dim(0);
		}
		cout << ", elcount " << field->elcount() << endl;
		if (ftype->code() == DS_TYPE_STRUCT) {
			cout << "fieldTypeSpecifer for " << field->name() << ":\n";
			cout <<ftype->specifier() << endl;
		}
	}
	cout << endl;
	for (i = 0; field = type->field(i); i++) {
		cout << "fieldName " << field->name();
		cout << ", offset " << field->offset();
		cout << ", size " << field->size();
		cout << ", stdsize " << field->stdsize();
		cout << ", stdoffset " << field->stdoffset() << endl;
	}
	if (!parent->link(table)) {
		DS_ABORT("link failed for table2");
	}
	if (!parent->isAcyclic()) {
		DS_ABORT("isAcyclic failed for parent");
	}
	if (child->linkAcyclic(parent)) {
		DS_ABORT("linkAcyclic incorrect");
	}
	xdrTestCpp(parent);
	delete parent;
}
/////////////////////////////////////////////////////////////////////////////
static void dumpTable(dsTable *pTable, char *msg)
{
	cout << flush;
	printf("\n%s\n\n", msg);
	pTable->printType(stdout);
	printf("\n");
	pTable->printData(stdout);
	fflush(stdout);
}
/////////////////////////////////////////////////////////////////////////////

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
* selfJoinCpp - test of dsEquijoin and dsProjectTable
*
*/
static void joinTestCpp(void)
{
	dsTable *pProject, *pSelfJoin, *pStaffTable;
	
	 // create staff table
	 
	if (!(pStaffTable = new dsTable("staff", STAFF_S, staffRowCount, staff))) {
		dsPerror("joinTestCpp new dsTable failed");
		exit(1);
	}                                     
	dumpTable(pStaffTable, "staff table:");

	// create target for self join
	
	if (!(pSelfJoin = dsTable::targetTable( "selfJoin", "self_t", pStaffTable,
		pStaffTable, selfAlias, selfProject))) {
		dsPerror("dsTable::targetTable failed for selfJoin");
		exit(1);
	}

	// do join
	if (!pSelfJoin->equijoin(pStaffTable, pStaffTable,
		selfAlias, selfJoin, selfProject)) { 
		dsPerror("dsTable::equijoin failed");
		exit(1);
	}
	dumpTable(pSelfJoin, "selfJoin table:");

	 // create target table for test of dsProjectTable
	
	if(!(pProject = dsTable::targetTable("project", "proj_t",
		pSelfJoin, removeName))) {
		dsPerror("dsTargetTable failed for removeName");
		exit(1);
	}
	
	 // do project
	if(!pProject->project(pSelfJoin, removeName)) {
		dsPerror("dsProjectTable failed");
		exit(1);
	}
	dumpTable(pProject, "project table:");
	delete pStaffTable;
	delete pSelfJoin;
	delete pProject;
}
/////////////////////////////////////////////////////////////////////////////
void xdrTestCpp(dsDataset *dset)
{
	char *fileName = "xdrcpp.bin";
	size_t i, j, n = 100;
	dsDataset *d;
	FILE *stream;
	XDR xdr;

	if ((stream = fopen(fileName, "wb")) == NULL) {
		cerr << "xdrTestCpp: fopen for write failed\n";
		exit(0);
	}
	xdrstdio_create(&xdr, stream, XDR_ENCODE);
	for (i = 0; i < n; i++) {
		if (i%2 == 0) {
			if (!dset->encodeBigEndian(&xdr)) {
				cerr << "xdrTestCpp: encodeBigEndian failed\n";
				exit(0);
			}
		}
		else {
			if (!dset->encodeLittleEndian(&xdr)) {
				cerr << "xdrTestCpp: encodeLittleEndian failed\n";
				exit(0);
			}
		}
	}
	fclose(stream);
	if ((stream = fopen(fileName, "rb")) == NULL) {
		cerr << "xdrTestCpp: fopen for read failed\n";
		exit(0);
	}
	xdrstdio_create(&xdr, stream, XDR_DECODE);
	for (i = 0; i < n; i++) {
		if (i%5 != 0) {
			if (!(d = dsDataset::decode(&xdr))) {
				cerr << "xdrTestCpp: decode failed\n";
				exit(0);
			}
		}
		else {
			if (!(d = dsDataset::decodeType(&xdr)) ||
				!d->findTable("table2")->allocTable() ||
				!d->allocTables() || !d->decodeData(&xdr)) {
				cerr << "xdrTestCpp: decodeType, decodeData failed\n";
				exit(0);
			}
		}
		if (d->entryCount() == 0) {
			cerr << "xdrTestCpp: entryCount failed\n";
			exit(0);
		}
		for (j = 0; j < d->entryCount(); j++) {
			if (d->entryIsTable(j)) {
				if (!d->getTable(j)) {
					cerr << "xdrTestCpp: getTable failed\n";
					exit(0);
				}
			}
			else if (d->entryIsDataset(j)) {
				if (!d->getDataset(j)) {
					cerr << "xdrTestCpp: getDataset failed\n";
					exit(0);
				}
			}
			else {
				cerr << "xdrTestCpp: entryIs failed\n";
				exit(0);
			}
		}
		delete d;
	}
	cout << "\nxdrTestCpp: Success\n";
}