%define $Id: star-jobtracker-server.spec,v 1.1 2005/12/28 13:51:29 fine Exp $
%define real_name jobtracker-server
%define server_dir html/xmlrpc-test

Summary: STAR GRID Job Tracker server, written in php
Name: star-jobtracker-server
Version: 0.1
Release: 1
License: GPL
Group: Development/Languages
URL: madison.star.bnl.gov

Source: http://madison.star.bnl.gov/xmlrpc/tgz/jobtracker-server-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
BuildArch: noarch
Requires: httpd >= 2.0.46
Requires: MySQL-server >= 4.0.23
Requires: php >= 4.3.8
Requires: php-mysql >= 4.3.8
Requires: php-xmlrpc >= 4.3.8
Obsoletes: jobtracker-server
Provides: jobtracker-server

%description
STAR Job Tracker server description

%package docs
Summary: Documentation for package %{name}
Group: Documentation

%description docs
STAR Job Tracker server docs

This package includes the documentation for %{name}.

%prep
%setup -n %{real_name}-%{version}

%build

%install
%{__rm} -rf %{buildroot}
%{__install} -d -m0755 %{buildroot}%{_localstatedir}/www/%{server_dir}
%{__install} -d -m0755 %{buildroot}%{_localstatedir}/www/%{server_dir}/client
%{__install} -d -m0755 %{buildroot}%{_localstatedir}/www/%{server_dir}/client/xmlrpcutils
%{__install} -d -m0755 %{buildroot}%{_localstatedir}/www/%{server_dir}/list
%{__install} -d -m0755 %{buildroot}%{_localstatedir}/www/%{server_dir}/server
%{__install} -d -m0755 %{buildroot}%{_localstatedir}/www/%{server_dir}/server/engine
%{__install} -d -m0755 %{buildroot}%{_localstatedir}/www/%{server_dir}/server/logs
%{__install} -d -m0755 %{buildroot}%{_localstatedir}/www/%{server_dir}/server/tools
%{__install} -p -m0644 src/client/*.php %{buildroot}%{_localstatedir}/www/%{server_dir}/client/
%{__install} -p -m0644 src/client/xmlrpcutils/*.php %{buildroot}%{_localstatedir}/www/%{server_dir}/client/xmlrpcutils/
%{__install} -p -m0644 src/list/*.php %{buildroot}%{_localstatedir}/www/%{server_dir}/list/
%{__install} -p -m0644 src/server/*.php %{buildroot}%{_localstatedir}/www/%{server_dir}/server/
%{__install} -p -m0644 src/server/.htaccess %{buildroot}%{_localstatedir}/www/%{server_dir}/server/
%{__install} -p -m0644 src/server/logs/*.txt %{buildroot}%{_localstatedir}/www/%{server_dir}/server/logs/
%{__install} -p -m0644 src/server/engine/*.php %{buildroot}%{_localstatedir}/www/%{server_dir}/server/engine/
%{__install} -p -m0644 src/server/tools/*.php %{buildroot}%{_localstatedir}/www/%{server_dir}/server/tools/

%clean
%{__rm} -rf %{buildroot}

%files
%defattr(-, apache, apache, 0755)
%doc GPL.txt README INSTALL jobtracker-server.sql
%{_localstatedir}/www/%{server_dir}/

%changelog
* Tue Dec 27 2005 Dmitry Arkhipkin - 0.1
- Updated to release 0.1

