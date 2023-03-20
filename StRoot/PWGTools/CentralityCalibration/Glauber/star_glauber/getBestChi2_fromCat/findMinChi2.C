//#include "/star/u/syang/Macro/headers.h"
//#include "/star/u/syang/Macro/function.C"

//find the minimum Chi2 among all the root files, then print out the root file full name
//the name of the root file  will tell what parameter settings give the minimum chi2

void findMinChi2(const Char_t* inputFileList = "minChi2_file.list")
{
	ifstream fin(inputFileList);
	if(!fin){
		Error("Error, inputfile", "can't open %s", inputFileList);
		return;
	}

	Double_t minChi2 = 1.e9;
	TString minName  = "";

	ofstream outData("minChi2_rootfile_fullname.dat");

	TString name("");

	while( fin >> name )
	{
		TFile* file = TFile::Open(name);
		if(!file || !file->IsOpen() || !file->GetNkeys())
		{
			Error("check chi2", "can't open %s", name.Data());
			continue;
		}
		//cout << "OPEN " << file->GetName() << endl;

		TH3* hchi2 = (TH3D*) file->Get("hChi2");

		for(Int_t i=0; i<hchi2->GetNbinsX(); i++)//parameter n_{pp}
		{
			for(Int_t j=0; j<hchi2->GetNbinsY(); j++)//parameter k
			{
				for(Int_t k=0; k<hchi2->GetNbinsZ(); k++)//parameter x
				{
					Double_t chi2 = hchi2->GetBinContent(i+1, j+1, k+1);

					if(chi2<minChi2)
					{
						minChi2 = chi2;
						minName = name;
						
						//if(minChi2<10) 
						//{
						//	outData<<"minChi2: "<<minChi2<<"    fileName: "<<minName<<endl;
						//	cout<<"minChi2: "<<minChi2<<"    fileName: "<<minName<<endl;
						//}
					}
				}
			}
		}

		file->Close();
	}
	
	outData<<"The minimum Chi2 are found: "<<minChi2<<endl;
	outData<<"the corresponding root fileName: "<<endl;
	outData<<minName<<endl;
	outData.close();

	cout<<endl;
	cout<<"The minimum Chi2: "<<minChi2<<"    fileName: "<<minName<<endl;
}
