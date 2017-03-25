!include win\env.nsh

# name the installer
outFile "txr-installer.exe"
Name "TXR"

Icon "win\txr.ico"
UninstallIcon "win\txr.ico"

SetCompressor lzma
CRCCheck on
RequestExecutionLevel admin

Function .onInit
  # default installation dir
  StrCpy $INSTDIR "C:\Program Files"
FunctionEnd

Function .onInstSuccess
  MessageBox MB_YESNO "Expore the TXR installation directory now?" IDNO NoLaunch
    Exec '"$WINDIR\explorer.exe" "$INSTDIR\txr"'
  NoLaunch:
FunctionEnd

PageEx license
  LicenseText "Redistribution of TXR requires agreement with terms of the license." "Acknowledge"
  LicenseData LICENSE
  Caption ": Licensing"
PageExEnd

PageEx license
  LicenseText "Redistribution of this version of TXR also requires agreement with Cygwin's license." "Acknowledge"
  LicenseData LICENSE-CYG
  Caption ": Licensing"
PageExEnd

PageEx directory
  DirVar $INSTDIR
PageExEnd

Page instfiles
UninstPage uninstConfirm
UninstPage instfiles

section "TXR"
  SetShellVarContext current
  SetOutPath $INSTDIR\txr\bin
  File txr.exe
  File txr-win.exe
  File win\cygwin1.dll
  File c:\cygwin\bin\cyggcc_s-1.dll
  SetOutPath $INSTDIR\txr\doc
  File txr-manpage.html
  File txr-manpage.pdf
  SetOutPath $INSTDIR\txr\share\txr
  File LICENSE
  File METALICENSE
  SetOutPath $INSTDIR\txr\share\txr\stdlib
  File share\txr\stdlib\*.txr
  File share\txr\stdlib\*.tl
  Delete /REBOOTOK $INSTDIR\txr\bin\sh.exe
  RmDir /r /REBOOTOK $INSTDIR\txr\share\man
  WriteUninstaller $INSTDIR\txr\uninstall.exe
  CreateDirectory "$SMPROGRAMS\txr"
  SetOutPath $PROFILE
  CreateShortCut "$SMPROGRAMS\txr\txr.lnk" "$INSTDIR\txr\bin\txr.exe"
  CreateShortCut "$SMPROGRAMS\txr\uninstall.lnk" "$INSTDIR\txr\uninstall.exe"
  CreateShortCut "$SMPROGRAMS\txr\install-root.lnk" "$INSTDIR\txr"
  CreateShortCut "$STARTMENU\txr.lnk" "$INSTDIR\txr\bin\txr.exe"
  ${EnvVarUpdate} $0 "PATH" "A" "HKLM" "$INSTDIR\txr\bin"
sectionEnd

section "Uninstall"
  SetShellVarContext current
  # $INSTDIR is now where the uninstaller is installed,
  # not the $INSTDIR that was used during installation!

  ${un.EnvVarUpdate} $0 "PATH" "R" "HKLM" "$INSTDIR\bin"
  RMDir /r "$INSTDIR"
  Delete "$SMPROGRAMS\txr\txr.lnk"
  Delete "$STARTMENU\txr.lnk"
  IfErrors errors removeInstaller
errors:
  MessageBox MB_OK|MB_ICONEXCLAMATION "Some files or folders in were not deleted. Make sure the program is stopped and run the uninstaller again." IDOK quit
removeInstaller:
  Delete "$INSTDIR\uninstall.exe"
  RMDir /r "$INSTDIR"
  Delete "$SMPROGRAMS\txr\uninstall.lnk"
  RmDir /r "$SMPROGRAMS\txr"
  IfErrors fatal done
fatal:
  MessageBox MB_OK|MB_ICONEXCLAMATION "The uninstaller itself was not completely deleted for some reason. It looks like you have to manually remove $SMPROGRAMS\txr and $INSTDIR." IDOK quit
quit:
  Quit
done:
sectionEnd
