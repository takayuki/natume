Summary: an implementation of Kana-Kanji conversion in Haskell
Name: natume
Version: 0.13
Release: 1
Source: natume-%{version}.tar.gz
Group: System
License: GPL
Prefix: /usr
BuildRoot: /var/tmp/%{name}-%{version}-root

%description
Natume is an implementation of Kana-Kanji conversion in Haskell.

%prep
%setup -q

%build
./configure --prefix=%{prefix}
make

%install
rm -rf $RPM_BUILD_ROOT
make prefix=$RPM_BUILD_ROOT%{prefix} install

%clean
rm -rf $RPM_BUILD_ROOT

%files 
%defattr(-,root,root)
%{prefix}/bin/initnatume
%{prefix}/bin/natume
%{prefix}/bin/mknadic
%{prefix}/libexec/natume/natume
%{prefix}/sbin/natumeserver
%{prefix}/share/natume/*

