#include "Sti/Base/Loadable.h"

/// Load this object from file or db
/// This is a steering function to load this object. 
/// It first calls "loadS" to attempt loading the object from a file whose name is provided as argument
/// If the attempt is unsuccessful, it attempts loading from the star db with a call to "loadM". 
/// A maker is to be passed as argument to provide the loader with a path to retrieve the appropriate TDataSet reference.
/// if all fails, use setDefaults
void Loadable::load(const string & userFileName, StMaker & source)
{
  cout << "Loadable::load(const string & userFileName, StMaker & source) -I- Start loading parameters"<<endl;
  // First attempt to read a user file "XXXXXuserFileName"
  try 
    {
      loadS(userFileName);
    }
  catch (runtime_error & e1)
    {
      cout << "Loadable::load() -W- No user file found" << endl;
      cout << "Loadable::load() -I- Load parameters from database" << endl;
      try
	{
	  loadM(source);
	}
      catch (runtime_error & e2)
	{
	  cout << "Loadable::load() -W- No db or error while polling db" << endl;
	  cout << "Loadable::load() -I- Setting default values" << endl;
	  setDefaults();
	}
    }
  cout << "Loadable::load() -I- Done"<<endl;
}

///Load this object from Star database Calibratoins/tracker. Use the give StMaker reference to 
///obtain a reference to the relevant TDataSet object.
///Call "loadDS" of the derived class to actully load the object.
void Loadable::loadM(StMaker & source)
{
  cout << "Loadable::loadM(StMaker & source) -I- Start loading db parameters" << endl;
  TDataSet * ds = source.GetDataBase("Calibrations/tracker");
  if (!ds) throw runtime_error("Loadable::load(StMaker & source) -W- ds==0");
  loadDS(*ds);
  cout << "Loadable::loadM(StMaker & source) -I- Done" << endl;
}

///Load this object from a file with the given name
///Attempt tp open the file with an "ifstream".
///If the file is opened successfully, call the derived class "loadFS" function
///to actually load the object.
void Loadable::loadS(const string & userFileName)
{
  cout << "Loadable::load(const string & userName) -I- Started" << endl;
  cout << "Loadable::load(const string & userName) -I- File name: "<< userFileName<<endl;
  ifstream inF(userFileName.c_str());
  if (!inF.is_open() )
    throw runtime_error("Loadable::load(const string & userName) -W- Could not find/open file");
  loadFS(inF);
  cout << "Loadable::load(const string & userName) -I- Done" << endl;
}

/// Load this object from data set (DS)
/// This is a dummy. It should never be called. It will throw an exception if called.
/// Actually object load must be done in a derived class implementing this method.
void Loadable::loadDS(TDataSet&ds)
{
  throw runtime_error("Loadable::load(TDataSet&) -E- Base class operation not defined");
}

/// Load this object from file stream (FS)
/// This is a dummy. It should never be called. It will throw an exception if called.
/// Actually object load must be done in a derived class implementing this method.
void Loadable::loadFS(ifstream &)
{
  throw runtime_error("Loadable::load(ifstream&) -E- Base class operation not defined");
}

/// Set default values of this object.
/// This is a dummy. It should never be called. It will throw an exception if called.
/// Actually object setting must be done in a derived class implementing this method.
void Loadable::setDefaults()
{
  throw runtime_error("Loadable::setDefaults() -E- Base class operation not defined");
}


