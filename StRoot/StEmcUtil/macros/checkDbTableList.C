/*------------------------------------------------------------------------------
| 
| checkDbTableList.C 
| Macro to compare a list of tables saved on STAR database with
| the source files
|
| First argument is the name of the file with the list of files to be
| checked.
|
| Second argument is the output file name. The output file is a list of
| the files with the results of the checking.
| 
| Third argument is the maximum entry time in the database. It means that the
| table had to be *saved* before that time. Do not mistake this as the
| timestamp
| 
| Fourth argument is the flavor the tables is save in the database
|
| It calls the macro with funtion checkDbTable. Argument is a file name. 
| The file names for tables to be saved on DB include information about 
| the kind of table and time stamp. The function gets that information 
| to make the comparison. For the table on Db, an object of class StEmcDbHandler
| is created, which returns table in the appropriate format to be compared 
| to the one in the file.
|
| author: Marcia Maria de Moura 2005-03-22
|
|-----------------------------------------------------------------------------*/

checkDbTableList(char* listFile="table.list", char* outName="out.txt",
                 char* MAXENTRY = "2030-01-01 00:00:00", char* flavor = "ofl")
{
  
  // Loading basic libraries
  gROOT->Macro("loadMuDst.C");
  
  // loading functions macros
  gROOT->LoadMacro("$STAR/StRoot/StEmcUtil/macros/checkDbTable.C");

  // Getting file list to be checked
  ifstream inputFile(listFile); 
  ofstream outputFile(outName);
  char fileName[200];
  
  // Looping over the file list
  while (!inputFile.eof())
  {
    inputFile >> fileName;
    if (strcmp(fileName," "))
      if (!checkDbTable(fileName,MAXENTRY,flavor)) 
      {
        outputFile <<"DBTIME = "<<DBTIME<<" FILE = "<< fileName <<" - *** TABLES DON'T MATCH ***"<< endl;
        cout       <<"DBTIME = "<<DBTIME<<" FILE = "<< fileName <<" - *** TABLES DON'T MATCH ***"<< endl;
      }
      else
      {
         outputFile <<"DBTIME = "<<DBTIME<<" FILE = "<< fileName <<" - Tables match"<< endl;
         cout       <<"DBTIME = "<<DBTIME<<" FILE = "<< fileName <<" - Tables match"<< endl;
      }
  }

  outputFile.close();
  
  // Printing message on terminal
  cout << endl; 
  cout << "_______ DB Table file list checking is done _______" << endl;
  cout << endl;
}
//------------------------------------------------------------------------------

