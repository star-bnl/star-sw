#include <iostream>
#include "SANCtable.h"
using std::cout;

int main()
{
	SANCtable f1("table1-1.txt"),f2("table2-2.txt");
	f1.setFlavor(1);
	f2.setFlavor(2);

	//Uncomment for born-level computation
	//f1.setBornLevel(true);
	//f2.setBornLevel(true);

	//Write with fixed precision
	f1.setFixedLength(8);
	f2.setFixedLength(8);

	//Set the basic variables. Only needed once before all computation
	SANCtable::setFlags();
	//The dimensions must match the tauola interface
	SANCtable::setDimensions(100,100,100,21);
	//Define the sqrt mass range for all three data sets
	SANCtable::setRanges(6,17000,85,110,160,220);

	//Add header and additional information to files
	if(!f1.addHeader())
	{
		cout<<"Ranges or dimensions not set.\n";
		return -1;
	}
	if(!f1.addFile("SancLib_v1_02/lib.txt"))
	{
		cout<<"No info file on electrowek library SANC (file SancLib_v1_02/lib.txt missing).\n";
		return -1;
	}

	if(!f1.addFile("var.dump"))
	{
		cout<<"No initialization variables of SANC (file var.dump missing).\n";
		return -1;
	}
	//Same for the second file
	if(!f2.addHeader() || !f2.addFile("SancLib_v1_02/lib.txt") || !f2.addFile("var.dump"))
	{
		cout<<"Second file I/O operations failed.\n";
		return -1;
	}

	//Start computation
	f1.addRange(1,true); //The first range is a logarithmic range
	f1.addRange(2);
	f1.addRange(3);
	f1.close();

	f2.addRange(1,true); //The first range is a logarithmic range
	f2.addRange(2);
	f2.addRange(3);
	f2.close();
	return 0;
}
