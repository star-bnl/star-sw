#include <iostream.h>
#include <stdlib.h>

#include "dscpp.h"

#ifndef DS_ABORT
#define DS_ABORT(m) {cerr << m << " " << __FILE__ << "." << __LINE__ << "\n"; exit(0);}
#endif

void printCount(void);
void cppTest(void);
void xdrTestCpp(dsDataset *dset);
//////////////////////////////////////////////////////////////////////////////
int main(int argc, char *argv[])
{
	cppTest();
	printCount();
	return 0;
	dsAllocStats();
	dsTidHashStats();
	return 0;
}
/////////////////////////////////////////////////////////////////////////////
void cppTest(void)
{
	char *spec = "struct type_1 {long a, b, c; octet q[100];}";
	char *allSpec = "struct all_t {char c; double d; float f; long l; "
		"octet o; short s; struct aPoint {float x, y, z;}point[100];"
		"unsigned short us; unsigned long ul;}";
	int i;
	dsDataset *parent, *child;
	dsField *field;
	dsTable *table;
	dsType *ftype, *type;

	if (!(parent = dsDataset::create("parent")) ||
		!(child = dsDataset::create("child")) ||
		!parent->link(child) ||
		!(table = dsTable::create("table1", spec)) ||
		!child->link(table)) {
		dsPerror("");
		DS_ABORT("dsDataset::create failed");
	}
	cout << "parent entryCount: "<< parent->entryCount() << "\n";

	cout << "parent->child->table1->rowCount: ";
	cout << parent->findDataset("child")->findTable("table1")->rowCount();
	cout << "\n";

	cout << "tableName " << table->name() << "\n";
	cout << "maxRowCount " << table->maxRowCount();
	cout << ", rowCount " << table->rowCount() << "\n";

	table->realloc(100);
	cout << "maxRowCount " << table->maxRowCount();
	cout << ", rowCount " << table->rowCount() << "\n";

	table->setRowCount(50);
	cout << "maxRowCount " << table->maxRowCount();
	cout << ", rowCount " << table->rowCount() << "\n";

	if (!(table = dsTable::create("table2", allSpec))) {
		DS_ABORT("dsTable::create failed");
	}
	type = table->rowType();
	cout << "\nrowTypeSpec: " << type->specifier() << "\n";
	cout << "\nrowTypeName " << type->name() << "\n\n";

	for (i = 0; field = type->field(i); i++) {
		ftype = field->type();
		cout << "fieldName " << field->name();
		cout << ", fieldType " << ftype->name();
		cout << ", typeCode " << ftype->code();
		cout << ", fieldElcount " << field->elcount() << "\n";
		if (ftype->code() == DS_TYPE_STRUCT) {
			cout << "fieldTypeSpecifer for " << field->name() << ":\n";
			cout <<ftype->specifier() << "\n";
		}
	}
	if (!parent->link(table)) {
		DS_ABORT("link failed for table2");
	}
	xdrTestCpp(parent);
	delete parent;
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
		if (!dset->encodeBigEndian(&xdr)) {
			cerr << "xdrTestCpp: encodeBigEndian failed\n";
			exit(0);
		}
	}
	fclose(stream);
	if ((stream = fopen(fileName, "rb")) == NULL) {
		cerr << "xdrTestCpp: fopen for read failed\n";
		exit(0);
	}
	xdrstdio_create(&xdr, stream, XDR_DECODE);
	for (i = 0; i < n; i++) {
		if (!(d = dsDataset::decode(&xdr))) {
			cerr << "xdrTestCpp: decode failed\n";
			exit(0);
		}
		for (j = 0; j < d->entryCount(); j++) {
			if (d->entryIsTable(j)) {
				if (!d->getTable(j)) {
					cerr << "xdrTestCpp: getTable failed\n";
					exit(0);
				}
			}
			else {
				if (!d->getDataset(j)) {
					cerr << "xdrTestCpp: getDataset failed\n";
					exit(0);
				}

			}
		}
		delete d;
	}
	cout << "xdrTestCpp: Success\n";
}