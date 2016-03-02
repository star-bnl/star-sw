#!/opt/star/bin/perl -w
#
# $Id: parseXmlTable.pl,v 1.2 2001/02/16 22:11:38 porter Exp $
#
# Author: R. Jeff Porter
#
#***************************************************************************
#
# Description: parses an Xml-table description & loads into memory
#
#****************************************************************************
# 
# $Log: parseXmlTable.pl,v $
# Revision 1.2  2001/02/16 22:11:38  porter
# modified for new low-level table structures
#
# Revision 1.1  2000/04/28 14:08:22  porter
# management perl scripts for db-structure accessible from StDbLib
#
#
#####################################

use XML::Parser;

sub parse_table {

  my %args = (
              DEBUG => '',
              fileName => '',
              DataBase => '',
              @_,
              );

  if($args{DEBUG}){
  print "Input to parse_table = ", $args{fileName}, "\n";
}

  $filename = $args{fileName};

%colormap = (
             directory => "navy",
             file => "forestgreen"
             );

#
# typemap2 = mysql types
#

%typemap2 = (
            dbFloat => "float",
            dbFloatArray => "float",
            dbInt => "int",
            dbIntArray => "int",
            dbUInt => "int unsigned",
            dbUIntArray => "int unsigned ",
            dbUChar => "tinyint unsigned",
            dbUCharArray => "tinyint unsigned ",
            dbString => "char",
            dbDouble => "double",
            dbDoubleArray => "double",
            dbULong => "int unsigned",
            dbULongArray => "int unsigned",
            dbLong => "int",
            dbLongArray => "int",
            dbShort => "smallint",
            dbShortArray => "smallint",
            dbUShort => "smallint unsigned",
            dbUShortArray => "smallint unsigned"
            );

#
# typemap = string reference of stored types
#

%typemap= (
            dbFloat => "float",
            dbFloatArray => "float",
            dbInt => "int",
            dbIntArray => "int",
            dbUInt => "uint",
            dbUIntArray => "uint",
            dbUChar => "uchar",
            dbUCharArray => "uchar",
            dbString => "char",
            dbDouble => "double",
            dbDoubleArray => "double",
            dbULong => "ulong",
            dbULongArray => "ulong",
            dbLong => "long",
            dbLongArray => "long",
            dbShort => "short",
            dbShortArray => "short",
            dbUShort => "ushort",
            dbUShortArray => "ushort"
            );


$depth = 0;
$fullpath = '';
$currentElement = '';
$content=" ";
$inAccessor=0;
$inComment=0;
$commentFlag=0;
$p1 = new XML::Parser(Handlers => {Start => \&handle_start,
                                     End   => \&handle_end,
                                   Char  => \&handle_char});

  

$p1->parsefile($filename);

# reset database name for testing

  if(!$args{DataBase}){ $dbName=$databaseName; }

  return ;
}

######################
sub handle_start {
    local ($expat, $element) = @_;
    $depth = $depth +1;
    $element =~ s/(\s\n)*//g;
    $currentElement = $element;
    if($element eq 'comment'){ $inComment=1; }
#    print "Start ",$element,"\n";
}

######################
sub handle_end {
    local ($expat, $element) = @_;
    $depth = $depth-1;
    if($element eq 'StDbAccessor'){ $inAccessor=0; }
    if($element eq 'comment'){ $inComment=0;}
#    print "End " , $element, "\n";
    $content = join(" ",$content,$element);
}

######################
sub handle_char {
# Here's where the variables are loaded Keyed by "currentElement"
   local ($expat, $element) = @_;
   if(!($currentElement eq 'comment')){
       $element =~ s/[\s\n]*//g;}
#   print $currentElement," ", $element, " & ", $depth, "\n";
    if ( $element ne '' ) {
        if ( $currentElement eq 'StarDataBase' ) {
            $databaseName=$element;
        }        
        if ( $currentElement eq 'StDbTable') {
            $tableName=$element;
            $commentFlag=1;
        }
        if($currentElement eq 'StDbAccessor'){
            $inAccessor=1;
        }

        if ( exists($typemap{$currentElement})) { # Type

            if($inAccessor){

             $#alements++;
             $alements[$#alements] = $element;
             $#atypes++;
             $atypes[$#atypes]=$typemap{$currentElement};
             $#amysqltypes++;
             $amysqltypes[$#amysqltypes]=$typemap2{$currentElement};
             $#alengths++;
             $alengths[$#alengths]=1;
             $#avalues++;

            } else {

             $#elements++;
             $elements[$#elements] = $element;
             $#etypes++;
             $etypes[$#etypes]=$typemap{$currentElement};
             $#mysqltypes++;
             $mysqltypes[$#mysqltypes]=$typemap2{$currentElement};
             $#elengths++;
             $elengths[$#elengths]=1;
             $#evalues++;
             $evalues[$#evalues]='';
             $#ecomments++;
             $ecomments[$#ecomments]='';
             $#eunits++;
             $eunits[$#eunits]='';
             if($commentFlag){$commentFlag=0;} # reset flag if no table comment

            }

        }

        if($currentElement eq 'length'){
 
            if($inAccessor){
             $alengths[$#alengths]=$element;
            } else {
             $elengths[$#elengths]=$element;
            }
        }

        if($currentElement eq 'units'){
              $eunits[$#eunits]=$element;
        }

        if($currentElement eq 'value'){
 
            if($inAccessor){
             $avalues[$#avalues]=$element;
            } else {
              $evalues[$#evalues]=join("",$evalues[$#evalues],$element);
            }
        }

        if($currentElement eq 'namedRef'){
            $namedRef=$element;
        }

        if($currentElement eq 'comment' && $inComment){
            $inComment=0;
            if($commentFlag){
                $tableComment=$element;
                #print $tableComment, " TableComment\n";
            } else {
            if(!$inAccessor){
#            print $element, " at comment\n"; 
                 $ecomments[$#ecomments]=$element;
            }
            }
        }

    } 
}

1;
















