#!/usr/local/bin/perl
#
# This script should be run by root on the STAR Webserver.
# At present (22-Feb-2002) it is started in rc.local on
# connery.star.bnl.gov.
# The purpose is to regularly check (via wget) the availability
# of the STAR web and, if there is a problem, to take corrective
# action, i.e restart server, restart afs, reboot.
#

#
# Various settings, some are system dependent
# 
$workdir       = "/tmp";
$testfile      = "testfile.txt";
$testURL       = "http://www.star.bnl.gov/$testfile";
$mail          = "/bin/mail";
$wget          = "/usr/bin/wget";
$subject       = "Problems with STAR web";
$send_to       = "mm\@bnl.gov";
$httpd_stop    = "/etc/rc.d/init.d/httpd stop &";
$httpd_start   = "/etc/rc.d/init.d/httpd start &";
$afs_stop      = "/etc/rc.d/init.d/afs stop &";
$afs_start     = "/etc/rc.d/init.d/afs start &";

#
# Set work directory
#
chdir $workdir || die "Could not change to $workdir\n";

#
# Assume we have just booted, run reauth, send mail
#
system("su -c /usr/local/bin/my_reauth starweb");
mail("webtest script has started (reboot?)");

# 
# Test webserver in infinite loop
#
while(1) {
    sleep(120);
    if (!testweb()) {
	mail("Could not contact webserver, trying to re-start it");
	system($httpd_stop);
	sleep(30);
	system($httpd_start);
	sleep(30);
	if (testweb()) {
	    mail("Webserver re-start successfull");
	}
	else {
	    mail("Webserver still unreachable, trying to cycle afs");
	    system($httpd_stop);
	    sleep(10);
	    system($afs_stop);
	    sleep(30);
	    system($afs_start);
	    sleep(30);
	    system("killall reauth");
	    system("su -c /usr/local/bin/my_reauth starweb");
	    system($httpd_start);
	    sleep(30);
	    if (testweb()) {
		mail("AFS and Webserver re-start successfull");
	    }
	    else {
		mail("Webserver still unreachable, rebooting");
		system("reboot");
	    }
	}
    }
}

sub testweb()
{
    #
    # Test for existence of file downloaded using wget
    #
    $cmd = "rm -f $testfile";
    system ($cmd);
    $cmd = "$wget $testURL &";
    system ($cmd);
    sleep(30);
    if (-e $testfile) {
	$_ = -1;
    }
    else {
	$_ = 0;
    }
}

sub mail()
{
    local ($message, $send_time, $server);
    $message = $_[0];
    $send_time = gettime();
    $server = `hostname`;
    open(MAIL, "|$mail -s \"$subject\" $send_to");
    print MAIL "\nMessage from $server at $send_time \n\n";
    print MAIL "$message\n"; 
    close(MAIL);
}

sub gettime() 
{
    local(@tarray, $day, $time);
    @tarray = localtime(time());
    $day = join ('.', $tarray[3], $tarray[4]+1, $tarray[5]+1900);
    $time = join (':', @tarray[2,1,0]);
    $_[0] = "$day : $time";
}    




