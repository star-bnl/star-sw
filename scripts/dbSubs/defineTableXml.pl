#!/opt/star/bin/perl -w
#
# $Id: defineTableXml.pl,v 1.1 2000/04/28 14:08:21 porter Exp $
#
# Author: R. Jeff Porter
#
#***************************************************************************
#
# Description: script to parse c-struct within a header & produce
#              an Xml-table description 
#
#****************************************************************************
# 
# $Log: defineTableXml.pl,v $
# Revision 1.1  2000/04/28 14:08:21  porter
# management perl scripts for db-structure accessible from StDbLib
#
#
#####################################


use FileHandle;

sub defineTableXml(){

my %args = (
            InputFileName => '',
            OutputFileName => '',
            DataBaseName =>'',
            DEBUG => '',
            @_,
            );


  if($args{DEBUG}){
  print $args{InputFileName}, "\n";
  print $args{DataBaseName}, "\n";
}
$infile = $args{InputFileName};
if(!$args{OutputFileName}){ ($outfile = $infile)=~s(\.h)(\.xml); }

$database = join(" ","<StarDataBase>",$args{DataBaseName});
$enddatabase = "</StarDataBase>\n";
$tableDescription=0;
if($args{DEBUG}){
 print "defineTableXml:: input = ",$infile,"\n";
 print "defineTableXml:: ouput = ",$outfile,"\n";
}
open(INFILE,"< $infile");
open(OUTFILE,"> $outfile");

print OUTFILE $database, "\n";

while (<INFILE>){
    chomp;
    $test=$_;
    $test=~s/\#//;
    $test=~s/\'//;
    $test=~s/\&\&/and/;
    $test=~s/\&/and/;
    $test=~s/_st//;

    if($test=~s/\:Description\:/ /){ $tableDescription=$test; }
    if($test=~s/description\:/ /)  { $tableDescription=$test; }
   

    if(!($test=~m/\\/) && !($test=~m/\}\;\"/)){

      if($test=~ m/typedef struct /){
        $test=~s(\{)();
        $test=~s/(\/\*|\/\/)(\s+|\w+).*//;
        $test=~s(typedef struct)(\<StDbTable\>);

        print OUTFILE $test;
        if($tableDescription){
           print OUTFILE " <comment> ",$tableDescription," </comment>";
        }
        print OUTFILE "\n";       
  
      }


      if($test=~ m/struct /){
        $test=~s(\{)();
        $test=~s/(\/\*|\/\/)(\s+|\w+).*//;
        $test=~s(struct)(\<StDbTable\>);

        print OUTFILE $test;
        if($tableDescription){
           print OUTFILE " <comment> ",$tableDescription," </comment>";
        }
        print OUTFILE "\n";       
  
      }


     if($args{DEBUG}){ print $test,"\n";}

     if($test=~m/;/){ 
        if($args{DEBUG}){print "input ", $test,"\n";}

        $commentSet=0;
        if($test=~m/\/\//){  # sort out comments
          @tmp=split /\/\//, $test, 2;
          $test=$tmp[0];
          $tmp[1]=~s/\<\=/\.le\./;
          $tmp[1]=~s/\>\=/\.ge\./;
          $tmp[1]=~s/\</\.lt\./;
          $tmp[1]=~s/\>/\.gt\./;
          $tmp[1]=~s/\[/\(/;
          $tmp[1]=~s/\]/\)/;
          $tmp[1]=~s/\}/\)/;
          $comment=join($tmp[1]," <comment> "," </comment>");
          $commentSet=1;
        }
 
        if(!$commentSet && $test=~m/\/\*/){
          @tmp=split /\/\*/, $test, 2;
          $test=$tmp[0];
          $tmp[1]=~s/\<\=/\.le\./;
          $tmp[1]=~s/\>\=/\.ge\./;
          $tmp[1]=~s/\</\.lt\./;
          $tmp[1]=~s/\>/\.gt\./;
          $tmp[1]=~s/\[/\(/;
          $tmp[1]=~s/\]/\)/;
          $tmp[1]=~s/\}/\)/;
          $comment=join($tmp[1]," <comment> "," </comment>");
          $comment=~ s/\*\///;
          $commentSet=1;
        }            
        if(!$commentSet){ $comment=0;}

        $test=~s/float /Float /;
        $test=~s/long /Long /;
        $test=~s/int /Int /;
        $test=~s/double /Double /;
        $test=~s/short /Short /;       


        if($test =~m/unsigned/){
          $test=~s/unsigned /U/;
          $test=~s/char /Char /;
        } else {
          $test=~s/char\* /String /;
          $test=~s/char \*/String /;
          $test=~s/char /String /;       
        }

# to support utype as an unsigned type definition
#  note that short has already become Short
#
        $test=~s/uShort /UShort /;       
        $test=~s/uInt /UInt /;       
        $test=~s/uLong /ULong /;       

        # print $test,"\n";
        @barray=split(" ",$test);

        if($test =~m/\w/){

         $beginType = $barray[0];
         $endType = $barray[0];
         $beginType =~ s($beginType)(\<db$beginType\>);
         $endType =~ s($endType)( \<\/db$endType\>);
         $test =~ s($barray[0])($beginType);

         if($comment){ $endType = join("",$comment,$endType);}  
         $test =~ s(;)($endType);

#
# array indecis  [n]    -> <length> n </length>
#                [n][m] -> <length> n,m </length>
#
         $test =~ s(\]\[)(\,);
         $test =~ s(\[)( \<length\> );
         $test =~ s(\])( \<\/length\>);

         if($args{DEBUG}){  print "output ", $test,"\n";}
         if(!($test=~m/\}/)){
          print OUTFILE $test ,"\n";
         }
     }

        if(($test =~ m/\}/)){
            print OUTFILE "</StDbTable> \n";
        }
        
    }
}       
}

print OUTFILE $enddatabase;

close(OUTFILE);

}


1;




