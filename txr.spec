#
# spec file for TXR
#

Name:           txr
Version:        89
Release:        0
Source:         txr-%{version}.tar.gz
URL:            http://www.nongnu.org/txr
Summary:        Text Extraction and Data Munging Language
License:        BSD
Provides:       txr
BuildRequires:  flex bison
BuildRoot:      %{_tmppath}/%{name}-%{version}-build

%description
TXR is a pattern language for extracting and reporting text, with
a powerful Lisp dialect for advanced data munging.

%prep
%setup  -n txr-%{version}

%build
./configure --prefix=/usr --yacc="bison -y"
make %{?jobs:-j%jobs} 

%install
make install DESTDIR=$RPM_BUILD_ROOT

%files
%defattr(-,root,root)
/usr/bin
/usr/share

%clean
rm -rf $RPM_BUILD_ROOT

%changelog
