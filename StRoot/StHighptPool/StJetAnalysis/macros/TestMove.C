
void TestMove()
{
    cout <<"TestMove()"<<endl;
    char* infile = "Dummy.txt";
    int n=10;

    cout <<"tack int onto filename"<<endl;
    char* newfile = new char[1000];
    sprintf(newfile,"%s_has_%i_events",infile,n);

    cout <<"concat the unix command"<<endl;
    char* command = new char[2000];
    sprintf(command,"cp %s %s",infile, newfile);
    
    cout <<"Get TUnixSystem"<<endl;
    TUnixSystem* sys = static_cast<TUnixSystem*>(gSystem);

    cout <<"copy a file from:\t"<<infile<<"\t to:\t"<<newfile<<endl;
    cout <<"command is:\t"<<command<<endl;
    sys->Exec(command);
    cout <<"Done"<<endl;
}
