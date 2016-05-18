#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <algorithm>

#include "TH1F.h"

#include "StPicoPrescalesConstants.h"
#include "StPicoPrescales.h"

using namespace std;

ClassImp(StPicoPrescales);

StPicoPrescales::StPicoPrescales(string prescalesFilesDirectoryName): 
  mPrescalesFilesDirectoryName(prescalesFilesDirectoryName)
{

  mTriggersIds = PicoPrescalesConstants::triggerId;
  std::copy(PicoPrescalesConstants::triggerIdMtd.begin(),PicoPrescalesConstants::triggerIdMtd.end(),std::back_inserter(mTriggersIds));

  for(size_t iTrg=0;iTrg<mTriggersIds.size();++iTrg)
  {
    readList(iTrg);
  }
 
  cout<<"StPicoPrescales -  Trigger list: "<<endl;
  for(size_t iTrg=0;iTrg<mTriggersIds.size();++iTrg)
  {
    cout<<iTrg<<"  "<<mTriggersIds[iTrg]<<endl;
  }

   mLastQuery = mTable.end();
}
//___________________________________________
void StPicoPrescales::readList(unsigned int trg)
{
   stringstream st;
   st << mTriggersIds[trg];
   string listFileName = mPrescalesFilesDirectoryName + "/" + st.str() + ".txt";
   cout << "StPicoPrescales - Reading prescale values for trigger " << mTriggersIds[trg] << " from list " << listFileName << endl;

   //Open list
   ifstream runs(listFileName.c_str());

   if(runs.is_open())
   {
     while (!runs.eof())
     {
       string line;
       int run;
       float prescale;

       getline(runs, line);
       if (line == "\0" || line == "\n") continue;

       size_t firstSpace = line.find(" ");
       size_t lastSpace = line.rfind(" ");

       istringstream runBuffer(line.substr(0, firstSpace));
       istringstream prescaleBuffer(line.substr(lastSpace + 1));

       runBuffer >> run;
       prescaleBuffer >> prescale;

       map<unsigned int, vecPrescales>::iterator it = mTable.find(run);

       if (it == mTable.end())
       {
         vecPrescales vec(mTriggersIds.size(), -1);
         vec[trg] = prescale;
         mTable.insert(pair<unsigned int, vecPrescales>(run, vec));
       }
       else
       {
         if (it->second.at(trg) == -1) it->second.at(trg) = prescale;
         else
         {
           cout << "Two prescale values for same run and same trigger." << endl;
           cout << "Run= " << run << " Trigger= " << mTriggersIds[trg] << " StPicoPrescales= " << it->second.at(trg) << " " << prescale << endl;
         }
       }
     }
   }
   else
   {
     cout << "StPicoPrescales -- !!! Cannot find file !!! :" << listFileName << endl;
     exit(EXIT_FAILURE);
   }

   runs.close();
}

//__________________________________
float StPicoPrescales::prescale(unsigned int run, unsigned int trg)
{
   if(trg > mTriggersIds.size())
   {
     cout << "StPicoPrescales requested triggers doesn't exist. See StTRIGGERS.h for triggers definition." << endl;
     return -1;
   }

   if (mLastQuery != mTable.end() && run == mLastQuery->first) return mLastQuery->second.at(trg);
   else
   {
      map<unsigned int, vecPrescales>::iterator it = mTable.find(run);

      if (it != mTable.end())
      {
         mLastQuery = it;
         return it->second.at(trg);
      }
      else
      {
         cout << "StPicoPrescales::GetPrescale: No prescale values available for run " << run << ". Skip it." << endl;
         return -1;
      }
   }
}

//__________________________________
int StPicoPrescales::numberOfRuns() const
{
   return mTable.size();
}
//__________________________________
void StPicoPrescales::fillPrescalesHist(TH1F* hist, unsigned int trg)
{
   if(!hist) return;

   if(trg > mTriggersIds.size())
   {
     cout << "StPicoPrescales requested triggers doesn't exist. See StTRIGGERS.h for triggers definition." << endl;
     return;
   }

   for (map<unsigned int,vecPrescales>::iterator it = mTable.begin(); it != mTable.end(); ++it)
   {
      hist->Fill(std::distance(mTable.begin(), it), it->second.at(trg));
   }
}
//___________________________________
unsigned int StPicoPrescales::runIndex(unsigned int run)
{
   if (mLastQuery != mTable.end() && run == mLastQuery->first) return std::distance(mTable.begin(), mLastQuery);
   else
   {
      map<unsigned int, vecPrescales>::iterator it = mTable.find(run);
      mLastQuery = it;
      return std::distance(mTable.begin(), it);
   }
}

//___________________________________
bool StPicoPrescales::runExists(unsigned int run)
{
   if (mLastQuery != mTable.end() && run == mLastQuery->first) return true;
   else
   {
      map<unsigned int, vecPrescales>::iterator it = mTable.find(run);

      if (it != mTable.end())
      {
         mLastQuery = it;
         return true;
      }
      else return false;
   }
}

